;;;; ------------------------------------------------------------
;;;; kernel-dsl.lisp
;;;; ------------------------------------------------------------
;;;; Small DSL for defining energy kernels (E, optional grad/Hess) on top of
;;;; expr-ir / stmt-ir and the energy-kernels utilities.

(in-package :energy-kernels)

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
    (let* ((axis (char s 0))
           (atom-id-string (subseq s 1))
           (atom-id (parse-integer atom-id-string))
           (axis->off (kernel-layout-axis->offset layout))
           (atom->ibase (kernel-layout-atom->ibase layout))
           ;; accept either char or symbol keys
           (offset (or (cdr (assoc axis axis->off))
                       (loop for (key . off) in axis->off
                             when (char-equal axis (char (string key) 0))
                               do (return off))
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


;;; ------------------------------------------------------------
;;; Basic building macros: =. and stmt-block
;;; ------------------------------------------------------------

(defmacro =. (var expr-string)
  "Create an assignment statement VAR := parse(EXPR-STRING)."
  `(stmt-ir:make-assignment-stmt
    ',(expr-ir:ev var)
    (expr-ir:parse-expr ,expr-string)))

(defmacro stmt-block (&body forms)
  "Construct a STMT-IR block-statement from FORMS.
Each FORM should evaluate to a statement object (e.g. from =.)."
  `(stmt-ir:make-block-stmt
    (list ,@forms)))

;;; ------------------------------------------------------------
;;; Coordinate load helper: coords-from-position
;;; ------------------------------------------------------------

(defmacro coords-from-position (specs)
  "SPECS = ((x1 y1 z1 i3x1) (x2 y2 z2 i3x2) ...).

Expands to a LIST of RAW-C statements that load xyz from
position[i3xN + {0,1,2}]."
  `(list
    ,@(loop for (x y z idx) in specs
            append
            (list
             `(stmt-ir:make-raw-c-statement
               ,(format nil "~A = position[~A + 0];" x idx))
             `(stmt-ir:make-raw-c-statement
               ,(format nil "~A = position[~A + 1];" y idx))
             `(stmt-ir:make-raw-c-statement
               ,(format nil "~A = position[~A + 2];" z idx))))))


;;; ------------------------------------------------------------
;;; Replacement: extend-block-with-energy-grad-hess
;;; ------------------------------------------------------------

(defun general-grad-name (var)
  "Return a symbol naming dE/d(var) for the stretch kernel."
  (intern (format nil "G_~A" (symbol-name var))
          (symbol-package var)))

(defun general-hess-name (vi vj)
  "Return a symbol naming d²E/(dvi dvj) for the stretch kernel."
  (intern (format nil "H_~A_~A"
                  (symbol-name vi) (symbol-name vj))
          (symbol-package vi)))



(in-package :energy-kernels)

;;; ------------------------------------------------------------
;;; Hessian assignments from derivative env
;;; ------------------------------------------------------------

(in-package :energy-kernels)

(defun make-hessian-assignments-from-block (base-block energy-var coord-vars hess-target-fn)
  "Given a kernel BASE-BLOCK, the scalar ENERGY-VAR symbol, and
the list of coordinate variables COORD-VARS (EXPR-IR variable symbols),
return a list of STMT-IR:ASSIGNMENT-STATEMENTs that assign
H_{qi,qj} := d^2(ENERGY-VAR)/(dqi dqj) for the upper-triangular pairs
(i <= j), using the derivative environment machinery in EXPRESSION-DIFF.LISP.

HESS-TARGET-FN is a function (VI VJ) -> HESS-SYMBOL
(e.g. GENERAL-HESS-NAME, STRETCH-HESS-NAME, etc.)."
  (let* ((n (length coord-vars))
         ;; Precompute derivative envs Env[k] = d(* )/dq_k for all intermediates
         (envs (make-array n)))
    (dotimes (k n)
      (setf (aref envs k)
            (build-deriv-env-for-block base-block (nth k coord-vars))))

    (let ((hess-assignments '()))
      (dotimes (i n)
        (let* ((qi    (nth i coord-vars))
               (env-i (aref envs i))
               ;; dE/dqi from the env for qi
               (dE-dqi (expr-ir:lookup-var-derivative energy-var qi env-i)))
          (dotimes (j n)
            (when (<= i j)  ;; upper triangle only
              (let* ((qj    (nth j coord-vars))
                     (env-j (aref envs j))
                     ;; d/dqj ( dE/dqi ), using Env_j for chain rule
                     (raw-d2E   (expr-ir:differentiate-expr dE-dqi qj env-j))
                     (d2E   (expr-ir:simplify-expr raw-d2E))
                     )
                ;; Skip exact zeros
                (unless (and (typep d2E 'expr-ir:constant-expression)
                             (zerop (expr-ir:expression-value d2E)))
                  (let* ((h-sym (funcall hess-target-fn qi qj))
                         (stmt  (stmt-ir:make-assignment-stmt h-sym d2E)))
                    (push stmt hess-assignments))))))))
      (nreverse hess-assignments))))



;;; ------------------------------------------------------------
;;; New extend-block-with-energy-grad-hess (no inlining)
;;; ------------------------------------------------------------




(defun infer-kernel-locals (block params coord-vars)
  "Given the final BLOCK for a kernel and its PARAMS list
  ((ctype pname) ...), return a list of (ctype name) pairs for locals.

We:
  - collect all scalar assignment targets in BLOCK,
  - subtract parameter names,
  - declare the remainder as DOUBLE locals."
  (let* ((targets      (stmt-ir:collect-scalar-targets-in-block block))
         (targets (append targets coord-vars))
         (param-names  (mapcar #'second params))
         ;; drop anything that is already a parameter
         (local-names  (set-difference targets param-names :test #'eq)))
    ;; everything is a scalar DOUBLE
    (mapcar (lambda (v) (list 'double v))
            local-names)))

(defun make-energy-write-call-from-assignment (target-sym)
  "If TARGET-SYM is the canonical energy scalar (ENERGY),
emit a RAW-C-STMT that accumulates it into *energy_accumulate."
  (let ((name (string-upcase (symbol-name target-sym))))
    (when (string= name "ENERGY")
      (let* ((v-name "energy")          ; C variable holding the value
             (code  (format nil "*energy_accumulate += ~A;" v-name)))
        (stmt-ir:make-raw-c-statement code)))))


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






(defun build-deriv-env-for-block (block base-var)
  "Propagate derivatives d(* )/d(BASE-VAR) through BLOCK and
return the resulting DERIV-ENV (an EXPR-IR derivative environment).

BLOCK is a STMT-IR:BLOCK-STATEMENT whose statements are simple
assignments VAR := EXPR-IR-node.

BASE-VAR is a coordinate symbol like X1, Y1, etc."
  (declare (optimize (debug 3)))
  (let ((deriv-env (expr-ir:make-deriv-env)))
    ;; Walk the assignments in order and update deriv-env.
    (dolist (st (stmt-ir:block-statements block) deriv-env)
      (when (typep st 'stmt-ir:assignment-statement)
        (let* ((var  (stmt-ir:stmt-target-name st))
               (expr (stmt-ir:stmt-expression st))
               ;; Use your existing differentiator with this env
               (dvar (expr-ir:differentiate-expr expr base-var deriv-env)))
          (expr-ir:set-var-derivative var dvar deriv-env))))))


(in-package :energy-kernels)

(defun make-gradient-assignments-from-block (base-block energy-var coord-vars grad-target-fn)
  "Given a kernel BASE-BLOCK, the scalar ENERGY-VAR symbol, and
the list of coordinate variables COORD-VARS (EXPR-IR variable symbols),
return a list of STMT-IR:ASSIGNMENT-STATEMENTs that assign
G_q := d(ENERGY-VAR)/d(q) for each q.

GRAD-TARGET-FN is a function VAR -> GRAD-SYMBOL (e.g. GENERAL-GRAD-NAME
or ANGLE-GRAD-NAME)."
  (declare (optimize (debug 3)))
  (let ((grad-assignments '()))
    (dolist (q coord-vars (nreverse grad-assignments))
      ;; Build derivative environment wrt this coordinate
      (let* ((deriv-env (build-deriv-env-for-block base-block q))
             ;; dE/dq = lookup-var-derivative(energy-var, q, deriv-env)
             (dE-dq   (expr-ir:lookup-var-derivative energy-var q deriv-env))
             (dE-dq-s   (expr-ir:simplify-expr dE-dq)))
        ;; If derivative is non-zero, emit an assignment
        (unless (and (typep dE-dq-s 'expr-ir:constant-expression)
                     (zerop (expr-ir:expression-value dE-dq)))
          (let* ((grad-sym  (funcall grad-target-fn q))
                 (grad-expr dE-dq)
                 (stmt      (stmt-ir:make-assignment-stmt grad-sym grad-expr)))
            (push stmt grad-assignments)))))))

;;; ------------------------------------------------------------
;;; Write out kernel code
;;; ------------------------------------------------------------

(defun make-kernel-from-block
    (&key name pipeline layout coord-vars coord-load-stmts base-block params
       compute-energy compute-grad compute-hess)
  ;; ENERGY is always present in BASE-BLOCK; we ignore COMPUTE-ENERGY.
  (declare (ignore compute-energy))
  (declare (optimize (debug 3)))
  (flet ((the-name (nm)
           (format nil "~a ~a" name nm)))
    (let* ((energy-var 'energy) ;; canonical energy scalar
           ;; Start from the base DSL statements (u1.., v1.., r12_sq.., cos_th, theta, energy)
           (current-stmts (copy-list (stmt-ir:block-statements base-block)))
           (pass-counter (stmt-ir:make-pass-id-counter)))

      ;; 0. optimize the energy
      (let* ((energy-block      (stmt-ir:make-block-stmt current-stmts)))
        (when pipeline
          (multiple-value-bind (energy-block-opt results total-before total-after)
              (stmt-ir:run-optimization-pipeline pass-counter pipeline energy-block
                                                 :name (the-name "energy")
                                                 :log-stream *trace-output*)
            (setf current-stmts (copy-list (stmt-ir:block-statements energy-block-opt)))))

        ;; 1a. Append gradient assignments (primal + grad)
        (when compute-grad
          (let* ((grad-stmts
                   (make-gradient-assignments-from-block
                    base-block energy-var coord-vars #'general-grad-name)))
            (setf current-stmts (append current-stmts grad-stmts)))

          ;; 1b. optimize the core+gradient
          (let* ((grad-block      (stmt-ir:make-block-stmt current-stmts)))
            (when pipeline
              (multiple-value-bind (grad-block-opt results total-before total-after)
                  (stmt-ir:run-optimization-pipeline pass-counter pipeline grad-block
                                                     :name (the-name "e-grad")
                                                     :log-stream *trace-output*)
                (setf current-stmts (copy-list (stmt-ir:block-statements grad-block-opt)))))

            ;; 2a. Append Hessian assignments onto optimized primal+grad
            ;;      We want the hessian calculated only if the gradient was calculated
            (when compute-hess
              (let* ((hess-stmts
                       (make-hessian-assignments-from-block
                        base-block energy-var coord-vars #'general-hess-name)))
                (setf current-stmts (append current-stmts hess-stmts)))
              ;; 2b. optimize the core+gradient+hessian
              (let* ((hess-block      (stmt-ir:make-block-stmt current-stmts)))
                (when pipeline
                  (multiple-value-bind (hess-block-opt results total-before total-after)
                      (stmt-ir:run-optimization-pipeline pass-counter pipeline hess-block
                                                         :name (the-name "e-g-hess")
                                                         :log-stream *trace-output*)
                    (setf current-stmts (copy-list (stmt-ir:block-statements hess-block-opt)))))
                ))))
        ;; 3. Build the core-block
        (let* ((current-block (stmt-ir:make-block-stmt current-stmts))
               ;; 4. Prepend coordinate loads, if any
               (with-loads
                   (if coord-load-stmts
                       (stmt-ir:make-block-stmt
                        (append coord-load-stmts
                                (stmt-ir:block-statements current-block)))
                       current-block))
               ;; 5. Let Energy/Force/Hessian macros consume ENERGY, G_*, H_*_*.
               (eg-h-block (transform-eg-h-block with-loads layout))
               (locals (infer-kernel-locals eg-h-block params coord-vars))
               )

          ;; 6. Wrap everything into a C function
          (stmt-ir:make-c-function
           name
           eg-h-block
           :return-type "void"
           :parameters  params
           :locals      locals))))))




;;; ------------------------------------------------------------
;;; Write out kernel code
;;; ------------------------------------------------------------
(defun generate-kernel-code (pathname &key (kernel (error "Provide kernel")))
  (with-open-file (fout pathname :direction :output :if-exists :supersede)
    (let ((fun kernel))
      (write-line (stmt-ir:c-function->c-source-string fun) fout))))



;;; ------------------------------------------------------------
;;; Top-level DSL: DEFKERNEL
;;; ------------------------------------------------------------


(defun vars (&rest vars)
  (mapcar #'expr-ir:expr-var-symbol vars))

(defmacro defkernel (name &body clauses)
  "Define a kernel described by CLAUSES and bind it as a DEFparameter NAME.

CLAUSES:
  (:compute (energy grad hess))
  (:pipeline pipeline
  (:params  ((ctype pname) ...))
  (:layout  ((atom ibase) ...) ((axis . offset) ...))
  (:coord-vars (x1 y1 z1 ...))
  (:coord-load (coords-from-position ...))
  (:body (stmt-block ...))"
  (labels ((clause-value (key)
             ;; (:key value ...) -> value
             (cadr (assoc key clauses))))
    (let* ((c-function-name (clause-value :c-function-name))
           (compute-list (clause-value :compute))   ; e.g. (energy grad hess)
           (pipeline     (clause-value :pipeline))
           (params       (clause-value :params))
           (layout-cl    (assoc :layout clauses))   ; (:layout atom->ibase axis->offset)
           (coord-vars   (mapcar #'expr-ir:ev (clause-value :coord-vars)))
           (coord-load   (clause-value :coord-load))
           (body-form    (clause-value :body))
           ;; layout: (:layout ((1 I3X1) ...) ((X . 0) ...))
           (atom->ibase  (second layout-cl))
           (axis->offset (third  layout-cl))
           ;; string names of coord vars, e.g. 'X1 -> "X1"
           (coord-names  (mapcar #'symbol-name coord-vars))
           ;; booleans derived from :compute list (coerced to T/NIL)
           (want-grad    (and compute-list
                               (not (null (member 'grad compute-list)))))
           (want-hess    (and compute-list
                               (not (null (member 'hess compute-list))))))
      `(defparameter ,name
         (let* ((layout (make-kernel-layout
                         :atom->ibase ',atom->ibase
                         :axis->offset ',axis->offset))
                ;; expr-ir vars from strings via VARS
                (coord-vars (vars ,@coord-names))
                (coord-load-stmts ,coord-load)
                (base-block ,body-form))
           (make-kernel-from-block
            :name ',c-function-name
            :pipeline ,pipeline
            :layout layout
            :coord-vars coord-vars
            :coord-load-stmts coord-load-stmts
            :base-block base-block
            :params ',params
            :compute-energy t
            :compute-grad   ,want-grad
            :compute-hess   ,want-hess))))))
