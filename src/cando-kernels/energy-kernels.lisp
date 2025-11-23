
(in-package :cando-kernels)

;;; -------------------------------
;;; Small string / name utilities
;;; -------------------------------


(defstruct kernel-layout
  atom->ibase   ; alist, e.g. ((1 . I3X1) (2 . I3X2)) or ((1 . I1) (2 . I2) (3 . I3) (4 . I4))
  axis->offset) ; alist, e.g. ((#\X . 0) (#\Y . 1) (#\Z . 2))


(defun grad-target-name->coord (sym)
  "G_X1 → \"X1\". Non-gradient targets → NIL."
  (let* ((name (symbol-name sym))
         (parts (%split-on-char name #\_)))
    (if (and (= (length parts) 2)
             (string= (first parts) "G"))
        (second parts)
        nil)))

(defun make-energy-write-call-from-assignment (target-sym)
  "If TARGET-SYM is the energy scalar E, return a RAW-C-STATEMENT that writes
it into *Energy; otherwise return NIL."
  (when (eql target-sym 'E)
    (let* ((v-name (string-downcase (symbol-name target-sym)))
           ;; Accumulate into *Energy; if you prefer assignment, change to \"=\".
           (code  (format nil "*Energy += ~A;" v-name)))
      (stmt-ir:make-raw-c-statement code))))

(defun make-force-macro-call-from-grad-target (target-sym layout)
  "If TARGET-SYM is a gradient scalar like G_X1, return a RAW-C-STATEMENT
calling ForceAcc(i_base, offset, g_x1); otherwise return NIL."
  (let ((coord (grad-target-name->coord target-sym)))
    (when coord
      (multiple-value-bind (ibase off) (coord-name->ibase+offset coord layout)
        (let* ((ibase-str (string-downcase (symbol-name ibase)))
               (v-name    (string-downcase (symbol-name target-sym)))
               (code (format nil "ForceAcc(~A, ~D, ~A);"
                             ibase-str off v-name)))
          (stmt-ir:make-raw-c-statement code))))))

(defun make-hess-macro-call-from-target (target-sym layout)
  "If TARGET-SYM is a Hessian scalar like H_X1_X2, return a RAW-C-STATEMENT
  calling DiagHessAcc/OffDiagHessAcc with that variable as v.
  Otherwise return NIL."
  (multiple-value-bind (c1 c2)
      (hess-target-name->coords target-sym)
    (when (and c1 c2)
      (multiple-value-bind (ibase1 off1) (coord-name->ibase+offset c1 layout)
        (multiple-value-bind (ibase2 off2) (coord-name->ibase+offset c2 layout)
          (let* ((macro (if (string= c1 c2)
                            "DiagHessAcc"
                            "OffDiagHessAcc"))
                 (ibase1-str (string-downcase (symbol-name ibase1)))
                 (ibase2-str (string-downcase (symbol-name ibase2)))
                 ;; use the scalar name as the C variable
                 (v-name (string-downcase (symbol-name target-sym)))
                 (code (format nil "~A(~A, ~D, ~A, ~D, ~A);"
                               macro
                               ibase1-str off1
                               ibase2-str off2
                               v-name)))
            (stmt-ir:make-raw-c-statement code)))))))



(defun %split-on-char (string ch)
  "Split STRING on character CH, return list of substrings."
  (let ((parts '())
        (start 0)
        (len (length string)))
    (loop for i from 0 below len do
          (when (char= (aref string i) ch)
            (push (subseq string start i) parts)
            (setf start (1+ i))))
    (push (subseq string start len) parts)
    (nreverse parts)))

(defun coord-name->ibase+offset (coord-name layout)
  "COORD-NAME = \"X1\" / \"Y12\" / \"Z3\".
Return (ibase-symbol, offset-int) using LAYOUT."
  (let* ((s coord-name)
         (len (length s)))
    (when (< len 2)
      (error "coord-name->ibase+offset: bad coord name ~S" s))
    (let* ((axis (char-upcase (char s 0)))
           ;; allow multi-digit atom labels: X10, Y12, etc.
           (atom-id-string (subseq s 1))
           (atom-id (parse-integer atom-id-string))
           (axis->off   (kernel-layout-axis->offset layout))
           (atom->ibase (kernel-layout-atom->ibase layout))
           (offset (or (cdr (assoc axis axis->off))
                       (error "Unknown axis ~C in coord name ~S" axis s)))
           (ibase  (or (cdr (assoc atom-id atom->ibase))
                       (error "Unknown atom id ~D in coord name ~S"
                              atom-id s))))
      (values ibase offset))))


(defun hess-target-name->coords (sym)
  "Given a symbol like H_X1_X2, return two coord-name strings \"X1\" and \"X2\".
If SYM is not a Hessian target, return NIL."
  (let* ((name (symbol-name sym))
         (parts (%split-on-char name #\_)))
    (if (and (= (length parts) 3)
             (string= (first parts) "H"))
        (values (second parts) (third parts))
        (values nil nil))))




(defun transform-eg-h-block (block layout)
  "Walk BLOCK and:
  - keep all assignments,
  - for energy assignment E = ..., append \"*Energy += E;\",
  - for gradient scalar G_* assignments, append ForceAcc(...) macro calls,
  - for Hessian scalar H_*_* assignments, append DiagHessAcc/OffDiagHessAcc calls."
  (unless (typep block 'stmt-ir:block-statement)
    (error "transform-eg-h-block: expected BLOCK-STATEMENT, got ~S" block))
  (let ((new-stmts '()))
    (dolist (st (stmt-ir:block-statements block))
      (typecase st
        (stmt-ir:block-statement
         (push (transform-eg-h-block st layout) new-stmts))

        (stmt-ir:if-statement
         (let* ((cond     (stmt-ir:if-condition st))
                (then-blk (transform-eg-h-block (stmt-ir:if-then-block st) layout))
                (else-blk (and (stmt-ir:if-else-block st)
                               (transform-eg-h-block (stmt-ir:if-else-block st) layout))))
           (push (stmt-ir:make-if-stmt cond then-blk else-blk) new-stmts)))

        (stmt-ir:assignment-statement
         ;; Always keep the assignment itself
         (push st new-stmts)
         (let* ((target (stmt-ir:stmt-target-name st))
                ;; Energy write
                (energy-stmt (make-energy-write-call-from-assignment target))
                ;; Gradient → ForceAcc
                (force-stmt  (make-force-macro-call-from-grad-target target layout))
                ;; Hessian → Diag/OffDiagHessAcc
                (hess-stmt   (make-hess-macro-call-from-target target layout)))
           (when energy-stmt
             (push energy-stmt new-stmts))
           (when force-stmt
             (push force-stmt new-stmts))
           (when hess-stmt
             (push hess-stmt new-stmts))))

        (t
         (push st new-stmts))))
    (stmt-ir:make-block-stmt (nreverse new-stmts))))



(defun transform-eg-h-function (cfun layout)
  "Return a new C-FUNCTION where:
   - energy scalar E is followed by writing into *Energy,
   - gradient scalars G_* are followed by ForceAcc calls,
   - Hessian scalars H_*_* are followed by Diag/OffDiagHessAcc calls."
  (unless (typep cfun 'stmt-ir:c-function)
    (error "transform-stretch-eg-h-function: expected C-FUNCTION, got ~S" cfun))
  (let* ((body (stmt-ir:c-function-body cfun))
         (new-body (transform-eg-h-block body layout)))
    (stmt-ir:make-c-function
     (stmt-ir:c-function-name cfun)
     new-body
     :return-type (stmt-ir:c-function-return-type cfun)
     :parameters  (stmt-ir:c-function-parameters cfun)
     :locals      (stmt-ir:c-function-locals cfun))))



;;; ------------------------------------------------------------
;;; Naming helpers for grad / Hess entries (scalar names for now)
;;; ------------------------------------------------------------

(defun stretch-grad-name (var)
  "Return a symbol naming dE/d(var) for the stretch kernel."
  (intern (format nil "G_~A" (symbol-name var))
          (symbol-package var)))

(defun stretch-hess-name (vi vj)
  "Return a symbol naming d²E/(dvi dvj) for the stretch kernel."
  (intern (format nil "H_~A_~A"
                  (symbol-name vi) (symbol-name vj))
          (symbol-package vi)))


(defun vars (&rest vars)
  (mapcar #'expr-ir:expr-var-symbol vars))

(defun collect-scalar-targets-in-block (block)
  "Return a list of all assignment target symbols in BLOCK (recursively)."
  (labels ((rec (blk acc)
             (dolist (st (stmt-ir:block-statements blk) acc)
               (setf acc
                     (typecase st
                       (stmt-ir:assignment-statement
                        (let ((name (stmt-ir:stmt-target-name st)))
                          (pushnew name acc :test #'eq)))
                       (stmt-ir:block-statement
                        (rec st acc))
                       (stmt-ir:if-statement
                        (let ((acc2 (rec (stmt-ir:if-then-block st) acc)))
                          (if (stmt-ir:if-else-block st)
                              (rec (stmt-ir:if-else-block st) acc2)
                              acc2)))
                       (t
                        acc))))))
    (rec block '())))

(defun make-eg-h-kernel (&key
                           name              ; symbol, e.g. 'stretch_energy_kernel
                           energy-expr       ; expr-ir node
                           coord-vars        ; list of symbols: (x1 y1 z1 x2 y2 z2 ...)
                           layout            ; kernel-layout for macros
                           coord-load-stmts  ; list of RAW-C-STMT for loading coords
                           params)           ; list of (ctype name) parameter specs
  "Build a C-FUNCTION that computes E, grad, and upper-triangular Hessian
for ENERGY-EXPR w.r.t. COORD-VARS, then wraps it with Energy/Force/Hess macros
using LAYOUT.

COORD-LOAD-STMTS are raw C statements that initialize the coord-vars from
the position array / indices. PARAMS is the full C parameter list."
  (let* (;; pure E/G/H block
         (core-block-base
           (stmt-ir:make-energy-grad-hess-block
            :energy-expr    energy-expr
            :coord-vars     coord-vars
            :energy-target  'E
            :grad-target-fn #'stretch-grad-name
            :hess-target-fn #'stretch-hess-name
            :simplify       t))

         ;; multi-pass CSE
         (core-block-cse
           (stmt-ir:cse-block-multi core-block-base
                                    :max-passes 5
                                    :min-uses 2
                                    :min-size 5))

         ;; copy-propagation to remove trivial chains t = u;
         (core-block
           (stmt-ir:copy-propagate-block core-block-cse))

         ;; full body = coord loads + E/G/H core
         (body-block
           (stmt-ir:make-block-stmt
            (append coord-load-stmts
                    (stmt-ir:block-statements core-block))))

         ;; locals:
         ;;  - coordinate locals for each coord var
         ;;  - ALL scalar assignment targets (E, G_*, H_*_*, CSE temps, ...)
         (coord-locals
           (mapcar (lambda (sym)
                     (list "double" sym))
                   coord-vars))
         (scalar-targets (collect-scalar-targets-in-block core-block))
         (scalar-locals
           (mapcar (lambda (sym)
                     (list "double" sym))
                   scalar-targets))
         (locals (append coord-locals scalar-locals))

         ;; plain C function
         (cfun
           (stmt-ir:make-c-function
            name
            body-block
            :return-type "void"
            :parameters  params
            :locals      locals)))
    ;; attach Energy/Force/Hess macros
    (transform-eg-h-function cfun layout)))



(in-package :energy-kernels)

(defun make-stretch-energy-kernel ()
  "Stretch kernel built using the generic EG/H kernel builder."
  (let* ((stretch-layout
           (make-kernel-layout
            :atom->ibase '((1 . I3X1) (2 . I3X2))
            :axis->offset '((#\X . 0) (#\Y . 1) (#\Z . 2))))
         (energy-expr
           (expr-ir:parse-expr
            "kb*(Sqrt((x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2) - r0)^2"))
         (coord-vars (vars "x1" "y1" "z1" "x2" "y2" "z2"))
         (coord-load-stmts
           (list
            (stmt-ir:make-raw-c-statement "x1 = position[i3x1 + 0];")
            (stmt-ir:make-raw-c-statement "y1 = position[i3x1 + 1];")
            (stmt-ir:make-raw-c-statement "z1 = position[i3x1 + 2];")
            (stmt-ir:make-raw-c-statement "x2 = position[i3x2 + 0];")
            (stmt-ir:make-raw-c-statement "y2 = position[i3x2 + 1];")
            (stmt-ir:make-raw-c-statement "z2 = position[i3x2 + 2];")))
         (params
           '(("double" kb)
             ("double" r0)
             ("size_t" i3x1)
             ("size_t" i3x2)
             ("double *" position)
             ("double *" Energy)
             ("double *" force)
             ("double *" hessian)
             ("double *" dvec)
             ("double *" hdvec))))
    (make-eg-h-kernel
     :name 'stretch_energy_kernel
     :energy-expr energy-expr
     :coord-vars coord-vars
     :layout stretch-layout
     :coord-load-stmts coord-load-stmts
     :params params)))



(defun generate-stretch-code (pathname &key (stretch-kernel (make-stretch-energy-kernel)))
  (with-open-file (fout pathname :direction :output :if-exists :supersede)
    (let ((fun stretch-kernel))
      (write-line (stmt-ir:c-function->c-source-string fun) fout))))






(defparameter *dihedral-layout*
  (make-kernel-layout
   :atom->ibase '((1 . I1) (2 . I2) (3 . I3) (4 . I4))
   :axis->offset '((#\X . 0) (#\Y . 1) (#\Z . 2))))

