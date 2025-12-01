;;;; ------------------------------------------------------------
;;;; kernel-dsl.lisp
;;;; ------------------------------------------------------------
;;;; Small DSL for defining energy kernels (E, optional grad/Hess) on top of
;;;; expr-ir / stmt-ir and the energy-kernels utilities.

(in-package :mathkernel)

;;; ------------------------------------------------------------
;;; Manual derivative specification
;;; ------------------------------------------------------------


(defclass manual-deriv-spec ()
  ((mode
    :initarg :mode
    :accessor manual-deriv-spec-mode)
   (intermediates
    :initarg :intermediates
    :accessor manual-deriv-spec-intermediates)
   (du-dq
    :initarg :du-dq
    :accessor manual-deriv-spec-du-dq)
   (d2u-dq2
    :initarg :d2u-dq2
    :accessor manual-deriv-spec-d2u-dq2)
   (dE-du
    :initarg :dE-du
    :accessor manual-deriv-spec-dE-du)
   (d2E-dudu
    :initarg :d2E-dudu
    :accessor manual-deriv-spec-d2E-dudu)
   (hessian-modes
    :initarg :hessian-modes
    :accessor manual-deriv-spec-hessian-modes)
   (geometry-check-mode
    :initarg :geometry-check-mode
    :accessor manual-deriv-spec-geometry-check-mode)))

(defun make-manual-deriv-spec (&rest initargs)
  (apply #'make-instance 'manual-deriv-spec initargs))



(defun normalize-derivatives-spec (raw-spec)
  "Convert RAW-SPEC from the :DERIVATIVES clause of DEFKERNEL into
a MANUAL-DERIV-SPEC struct, or NIL if RAW-SPEC is NIL.

RAW-SPEC is a property list with keys:
  :MODE
  :INTERMEDIATES
  :INTERMEDIATE->COORD
  :INTERMEDIATE->COORD2
  :ENERGY->INTERMEDIATE
  :HESSIAN-MODES."
  (when raw-spec
    (let* ((plist (if (and (consp raw-spec)
                           (keywordp (car raw-spec)))
                      raw-spec
                      (error "DERIVATIVES spec must be a property list, got ~S"
                             raw-spec)))
           (mode             (getf plist :mode :manual))
           (raw-intermediates (getf plist :intermediates '()))
           ;; normalize intermediates into expr-var package
           (intermediates    (mapcar #'expr-ir:ev raw-intermediates))
           (du-dq            (make-hash-table :test #'equal))
           (d2u-dq2          (make-hash-table :test #'equal))
           (dE-du            (make-hash-table :test #'eq))
           (d2E-dudu         (make-hash-table :test #'equal))
           (hessian-modes    (make-hash-table :test #'eq))
           (geometry-check   (or (getf plist :geometry-check) :none)))

      ;; ∂u/∂q entries
      (dolist (entry (getf plist :intermediate->coord '()))
        ;; entry = (u ((q1 "expr") (q2 "expr") ...))
        (destructuring-bind (u coord-list) entry
          (let ((u* (expr-ir:ev u)))
            (dolist (cd coord-list)
              (destructuring-bind (q expr-str) cd
                (let ((expr (expr-ir:infix->expr-ir expr-str))
                      (q*   (expr-ir:ev q)))
                  (setf (gethash (list u* q*) du-dq) expr)))))))

      ;; ∂²u/(∂qi ∂qj) entries
      (dolist (entry (getf plist :intermediate->coord2 '()))
        ;; entry = (u (((qi qj) "expr") ...))
        (destructuring-bind (u pair-list) entry
          (let ((u* (expr-ir:ev u)))
            (dolist (pd pair-list)
              (destructuring-bind ((qi qj) expr-str) pd
                (let ((expr (expr-ir:infix->expr-ir expr-str))
                      (qi*  (expr-ir:ev qi))
                      (qj*  (expr-ir:ev qj)))
                  (setf (gethash (list u* qi* qj*) d2u-dq2) expr)))))))

      ;; ∂E/∂u and ∂²E/(∂u ∂v)
      (let ((e-int (getf plist :energy->intermediate)))
        (when e-int
          (let ((grad-list (getf e-int :gradient))
                (hess-list (getf e-int :hessian)))
            (dolist (g grad-list)
              ;; g = (u "expr")
              (destructuring-bind (u expr-str) g
                (let ((expr (expr-ir:infix->expr-ir expr-str))
                      (u*   (expr-ir:ev u)))
                  (setf (gethash u* dE-du) expr))))
            (dolist (h hess-list)
              ;; h = ((u v) "expr")
              (destructuring-bind ((u v) expr-str) h
                (let ((expr (expr-ir:infix->expr-ir expr-str))
                      (u*   (expr-ir:ev u))
                      (v*   (expr-ir:ev v)))
                  (setf (gethash (list u* v*) d2E-dudu) expr)))))))

      ;; Hessian modes per intermediate (optional)
      ;; e.g. :hessian-modes ((r :full) (theta :outer-product-only))
      (dolist (entry (getf plist :hessian-modes '()))
        (destructuring-bind (u mode-key) entry
          (setf (gethash (expr-ir:ev u) hessian-modes) mode-key)))

      (make-manual-deriv-spec
       :mode mode
       :intermediates intermediates
       :du-dq du-dq
       :d2u-dq2 d2u-dq2
       :dE-du dE-du
       :d2E-dudu d2E-dudu
       :hessian-modes hessian-modes
       :geometry-check-mode geometry-check))))


(defun build-deriv-env-for-block (block base-var)
  (stmt-ir:build-derivative-env-for-block
   (stmt-ir:block-statements block)
   base-var))




(defun expressions-equivalent-p (expr1 expr2 &optional base-block (verbose nil))
  "Heuristic equivalence test for two EXPR-IR expressions.

Returns three values:
  1) matched-p (T/NIL),
  2) canonical sexpr for expr1,
  3) canonical sexpr for expr2.

Uses:
  - assignment-based rewriting (if BASE-BLOCK given),
  - radial canonicalization via BUILD-RADIAL-MAP-FROM-BLOCK,
  - exponent canonicalization rules,
  - linear canonicalization,
  - SIMPLIFY-EXPR,
  - and a final diff == 0 check with FACTOR-SUM-OF-PRODUCTS."
  (let* ((radial-map (and base-block
                          (build-radial-map-from-block base-block)))
         (e1 (canonicalize-expr-for-equivalence expr1 base-block radial-map verbose))
         (e2 (canonicalize-expr-for-equivalence expr2 base-block radial-map verbose))
         (sx1 (expr-ir:expr->sexpr e1))
         (sx2 (expr-ir:expr->sexpr e2)))
    (cond
      ;; 1. Structural equality after canonicalization
      ((equal sx1 sx2)
       (values t sx1 sx2))

      (t
       ;; 2. Fallback: diff = e1 - e2, factor and simplify, check for constant 0
       (let* ((neg-e2   (expr-ir:make-expr-neg e2))
              (diff     (expr-ir:make-expr-add (list e1 neg-e2)))
              (diff-f   (expr-ir:factor-sum-of-products diff))
              (diff-sim (expr-ir:simplify-expr diff-f)))
         (if (and (typep diff-sim 'expr-ir:constant-expression)
                  (zerop (expr-ir:expression-value diff-sim)))
             (values t sx1 sx2)
             (values nil sx1 sx2)))))))




(defun check-intermediate-geometry!
    (base-block coord-vars manual-deriv-spec &key (reuse-assignments t))
  "If MANUAL-DERIV-SPEC has :geometry-check-mode of :warn or :error and
contains manual :intermediate->coord and/or :intermediate->coord2 entries,
compare them against AD-generated derivatives from BASE-BLOCK.

- BASE-BLOCK: stmt-ir:block-statement
- COORD-VARS: list of expr-ir vars (x1 y1 z1 ...)

Signals warnings or errors if mismatches are found, depending on mode."
  (let ((mode (manual-deriv-spec-geometry-check-mode manual-deriv-spec)))
    (unless (member mode '(:warn :error))
      (return-from check-intermediate-geometry! manual-deriv-spec))
    (let* ((intermediates (manual-deriv-spec-intermediates manual-deriv-spec))
           (du-dq         (manual-deriv-spec-du-dq manual-deriv-spec))
           (d2u-dq2       (manual-deriv-spec-d2u-dq2 manual-deriv-spec))
           ;; derivative envs per coord
           (envs (make-hash-table :test #'eq)))
      ;; build derivative envs once
      (dolist (q coord-vars)
        (setf (gethash q envs)
              (build-deriv-env-for-block base-block q)))
      ;; helper for reporting
      (labels ((report-mismatch (what manual-expr auto-expr sx1 sx2)
                 (let* ((*package* (find-package :expr-var))
                        (msg (format nil
                                     "Geometry derivative mismatch for ~S:~%  manual: ~A~%  auto:   ~A"
                                     what
                                     (expr-ir:expr->infix-string manual-expr)
                                     (expr-ir:expr->infix-string auto-expr))))
                   (ecase mode
                     (:warn (progn
                              (format t "WARN: ~a~%" msg)
                              (let ((*package* (find-package :expr-var)))
                                (format t "transformed manual =~% ~s~%" sx1)
                                (format t "transformed auto =~% ~s~%" sx2)
                                (expr-ir:side-by-side-sexpr sx1 sx2 :label1 "transformed manual" :label2 "transformed auto")
                                )))
                     (:error (error msg))))))
        ;; --- first derivatives: du/dq ---
        (dolist (u intermediates)
          (dolist (q coord-vars)
            (let* ((key    (list u q))
                   (manual (gethash key du-dq nil)))
              (when manual
                (let* ((env-q    (gethash q envs))
                       (auto-raw (expr-ir:lookup-var-derivative u q env-q)))
                  (when auto-raw
                    (let* ((auto-expr (if reuse-assignments
                                          (rewrite-expr-using-assignments auto-raw base-block )
                                          auto-raw)))
                      (multiple-value-bind (matched sx1 sx2)
                          (expressions-equivalent-p manual auto-expr base-block)
                        (unless matched
                          (report-mismatch (list :du-dq u q) manual auto-expr sx1 sx2)
                          ))
                      )))))))
        ;; --- second derivatives: d²u/(dqi dqj) ---
        (dolist (u intermediates)
          (let ((n (length coord-vars)))
            (dotimes (i n)
              (let* ((qi    (nth i coord-vars))
                     (env-i (gethash qi envs))
                     ;; prefer manual du/dqi if present, else AD
                     (du/qi (or (gethash (list u qi) du-dq)
                                (expr-ir:lookup-var-derivative u qi env-i))))
                (when du/qi
                  (dotimes (j n)
                    (when (<= i j)
                      (let* ((qj    (nth j coord-vars))
                             (keyij (list u qi qj))
                             (keyji (list u qj qi))
                             (manual (or (gethash keyij d2u-dq2 nil)
                                         (gethash keyji d2u-dq2 nil))))
                        (when manual
                          (let* ((env-j    (gethash qj envs))
                                 (auto-raw (expr-ir:differentiate-expr du/qi qj env-j)))
                            (when auto-raw
                              (let* ((auto-expr (if reuse-assignments
                                                    (rewrite-expr-using-assignments auto-raw base-block)
                                                    auto-raw)))
                                (multiple-value-bind (matched sx1 sx2)
                                    (expressions-equivalent-p manual auto-expr base-block)
                                  (unless matched
                                    (report-mismatch (list :d2u-dq2 u qi qj) manual auto-expr sx1 sx2)
                                    ))
                                )))))))))))))
        manual-deriv-spec)))

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
  (declare (optimize (debug 3)))
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


(defun build-assignment-map (block)
  "Return a hash table mapping VAR-SYM -> EXPR for all top-level
assignment statements in BLOCK.

Later assignments for the same variable override earlier ones."
  (let ((map (make-hash-table :test #'eq)))
    (dolist (st (stmt-ir:block-statements block) map)
      (when (typep st 'stmt-ir:assignment-statement)
        (setf (gethash (stmt-ir:stmt-target-name st) map)
              (stmt-ir:stmt-expression st))))))


#+(or)
(defun expand-sexpr-with-assignments (sexpr assign-map &optional (max-depth 4))
  "Recursively expand SEXPR by inlining variable definitions from
ASSIGN-MAP, which maps VAR-SYM -> EXPR-IR.

We stop expanding once MAX-DEPTH levels of inlining have been done, to
avoid infinite recursion if there ever were cycles."
  (labels ((rec (s depth)
             (cond
               ((zerop depth)
                s)
               ;; A symbol that we know how to expand
               ((symbolp s)
                (let ((expr (gethash s assign-map nil)))
                  (if expr
                      ;; inline its RHS and keep recursing
                      (rec (expr-ir:expr->sexpr expr) (1- depth))
                      s)))
               ;; A compound form (operator + arguments)
               ((consp s)
                (cons (rec (car s) depth)
                      (mapcar (lambda (sub)
                                (rec sub depth))
                              (cdr s))))
               (t
                s))))
    (rec sexpr max-depth)))




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
            for idxlc = (string-downcase idx)
            for xlc = (string-downcase x)
            for ylc = (string-downcase y)
            for zlc = (string-downcase z)
            append
            (list
             `(stmt-ir:make-raw-c-statement
               ,(format nil "~a ~A = position[~A + 0];" (symbol-name 'double) xlc idxlc))
             `(stmt-ir:make-raw-c-statement
               ,(format nil "~a ~A = position[~A + 1];" (symbol-name 'double) ylc idxlc))
             `(stmt-ir:make-raw-c-statement
               ,(format nil "~a ~A = position[~A + 2];" (symbol-name 'double) zlc idxlc))))))


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


;;; ------------------------------------------------------------
;;; Hessian assignments from derivative env
;;; ------------------------------------------------------------


(defun make-hessian-assignments-from-block
    (base-block energy-var coord-vars hess-target-fn &optional manual-deriv-spec)
  "Return a list of assignment statements H_{qi,qj} := ∂²(ENERGY)/∂qi∂qj
for upper-triangular pairs (i <= j).

If MANUAL-DERIV-SPEC is non-NIL:
  - :mode :manual   -> use manual chain rule.
  - :mode :hybrid   -> use manual dE/du and d²E/(du dv) from the spec,
                       and fill missing du/dq from AD on BASE-BLOCK.

Per-channel Hessian modes (HESSIAN-MODES table) can control how much each
channel contributes:
  :full              -> include both outer-product and geometry-curvature terms.
  :outer-product-only -> include only d²E/(du²) * du/dqi * du/dqj.
  :none              -> ignore this channel in the Hessian.

If MANUAL-DERIV-SPEC is NIL, fall back to the existing pure AD path."
  (declare (optimize (debug 3)))
  (cond
    ;; Manual / hybrid chain-rule
    ((and manual-deriv-spec
          (member (manual-deriv-spec-mode manual-deriv-spec) '(:manual :hybrid)))
     (let* ((mode           (manual-deriv-spec-mode manual-deriv-spec))
            (intermediates  (manual-deriv-spec-intermediates manual-deriv-spec))
            (du-dq          (manual-deriv-spec-du-dq manual-deriv-spec))
            (d2u-dq2        (manual-deriv-spec-d2u-dq2 manual-deriv-spec))
            (dE-du          (manual-deriv-spec-dE-du manual-deriv-spec))
            (d2E-dudu       (manual-deriv-spec-d2E-dudu manual-deriv-spec))
            (hessian-modes  (manual-deriv-spec-hessian-modes manual-deriv-spec))
            (n              (length coord-vars))
            ;; For :hybrid, derivative environments per coord
            (envs (when (eq mode :hybrid)
                    (let ((a (make-array n)))
                      (dotimes (k n)
                        (setf (aref a k)
                              (build-deriv-env-for-block
                               base-block (nth k coord-vars))))
                      a)))
            (hess-assignments '()))
       (dotimes (i n (nreverse hess-assignments))
         (let* ((qi    (nth i coord-vars))
                (env-i (and envs (aref envs i))))
           (dotimes (j n)
             (when (<= i j)
               (let* ((qj    (nth j coord-vars))
                      (env-j (and envs (aref envs j)))
                      (terms '()))
                 ;; First term: Σ_{u,v} d2E/(du dv) * du/dqi * dv/dqj
                 (dolist (u intermediates)
                   (let* ((mode-u (or (gethash u hessian-modes nil) :full)))
                     (unless (eq mode-u :none)
                       (dolist (v intermediates)
                         (let* ((mode-v (or (gethash v hessian-modes nil) :full)))
                           (unless (eq mode-v :none)
                             (let* ((d2E-uv
                                      (or (gethash (list u v) d2E-dudu nil)
                                          (and (not (eq u v))
                                               (gethash (list v u) d2E-dudu nil))))
                                    (du/dqi (or (gethash (list u qi) du-dq nil)
                                                (and env-i
                                                     (expr-ir:lookup-var-derivative
                                                      u qi env-i))))
                                    (dv/dqj (or (gethash (list v qj) du-dq nil)
                                                (and env-j
                                                     (expr-ir:lookup-var-derivative
                                                      v qj env-j)))))
                               (when (and d2E-uv du/dqi dv/dqj)
                                 (push (expr-ir:make-expr-mul
                                        (list d2E-uv du/dqi dv/dqj))
                                       terms)))))))))
                 ;; Second term: Σ_u dE/du * d2u/(dqi dqj),
                 ;; only for channels with :hessian-mode :full.
                 (dolist (u intermediates)
                   (let* ((mode-u (or (gethash u hessian-modes nil) :full)))
                     (when (eq mode-u :full)
                       (let* ((dE/du (gethash u dE-du nil))
                              (d2u   (or (gethash (list u qi qj) d2u-dq2 nil)
                                         (and (not (eq qi qj))
                                              (gethash (list u qj qi) d2u-dq2 nil)))))
                         ;; We *do not* auto-generate d2u via AD here; if it's
                         ;; absent, this term is simply omitted.
                         (when (and dE/du d2u)
                           (push (expr-ir:make-expr-mul
                                  (list dE/du d2u))
                                 terms))))))
                 (when terms
                   (let* ((sum (if (cdr terms)
                                   (expr-ir:make-expr-add terms)
                                   (car terms)))
                          (sum-simplified (expr-ir:simplify-expr sum)))
                     (unless (and (typep sum-simplified 'expr-ir:constant-expression)
                                  (zerop (expr-ir:expression-value sum-simplified)))
                       (let* ((h-sym (funcall hess-target-fn qi qj))
                              (stmt  (stmt-ir:make-assignment-stmt h-sym sum-simplified)))
                         (push stmt hess-assignments))))))))))))
    ;; Default / AD path (existing behavior)
    (t
     (let* ((n    (length coord-vars))
            (envs (make-array n)))
       (dotimes (k n)
         (setf (aref envs k)
               (build-deriv-env-for-block base-block (nth k coord-vars))))
       (let ((hess-assignments '()))
         (dotimes (i n (nreverse hess-assignments))
           (let* ((qi    (nth i coord-vars))
                  (env-i (aref envs i))
                  (dE-dqi (expr-ir:lookup-var-derivative energy-var qi env-i)))
             (dotimes (j n)
               (when (<= i j)
                 (let* ((qj      (nth j coord-vars))
                        (env-j   (aref envs j))
                        (raw-d2E (expr-ir:differentiate-expr dE-dqi qj env-j))
                        (d2E     (expr-ir:simplify-expr raw-d2E)))
                   (unless (and (typep d2E 'expr-ir:constant-expression)
                                (zerop (expr-ir:expression-value d2E)))
                     (let* ((h-sym (funcall hess-target-fn qi qj))
                            (stmt  (stmt-ir:make-assignment-stmt h-sym d2E)))
                       (push stmt hess-assignments)))))))))))))




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
calling KernelGradientAcc(i_base, offset, g_x1); otherwise return NIL."
  (let ((coord (grad-target-name->coord target-sym)))
    (when coord
      (multiple-value-bind (ibase off) (coord-name->ibase+offset coord layout)
        (let* ((ibase-str (string-downcase (symbol-name ibase)))
               (v-name    (string-downcase (symbol-name target-sym)))
               (code (format nil "KernelGradientAcc(~A, ~D, ~A);"
                             ibase-str off v-name)))
          (stmt-ir:make-raw-c-statement code))))))

(defun make-hess-macro-call-from-target (target-sym layout)
  "If TARGET-SYM is a Hessian scalar like H_X1_X2, return a RAW-C-STATEMENT
  calling KernelDiagHessAcc/KernelOffDiagHessAcc with that variable as v.
  Otherwise return NIL."
  (multiple-value-bind (c1 c2)
      (hess-target-name->coords target-sym)
    (when (and c1 c2)
      (multiple-value-bind (ibase1 off1) (coord-name->ibase+offset c1 layout)
        (multiple-value-bind (ibase2 off2) (coord-name->ibase+offset c2 layout)
          (let* ((macro (if (string= c1 c2)
                            "KernelDiagHessAcc"
                            "KernelOffDiagHessAcc"))
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


(defun transform-eg-h-block (block layout &optional coord-vars grad-target-fn hess-target-fn)
  "Walk BLOCK and:
  - keep all assignments,
  - for energy assignment E = ..., append \"*Energy += E;\",
  - for gradient scalar G_* assignments, append KernelGradientAcc(...) macro calls,
  - for Hessian scalar H_*_* assignments, append KernelDiagHessAcc/KernelOffDiagHessAcc calls."
  (unless (typep block 'stmt-ir:block-statement)
    (error "transform-eg-h-block: expected BLOCK-STATEMENT, got ~S" block))
  (let ((new-stmts '()))
    (dolist (st (stmt-ir:block-statements block))
      (typecase st
        (stmt-ir:block-statement
         (push (transform-eg-h-block st layout
                                     coord-vars grad-target-fn hess-target-fn)
               new-stmts))

        (stmt-ir:if-statement
         (let* ((cond     (stmt-ir:if-condition st))
                (then-blk (transform-eg-h-block (stmt-ir:if-then-block st)
                                                layout
                                                coord-vars grad-target-fn hess-target-fn))
                (else-blk (and (stmt-ir:if-else-block st)
                               (transform-eg-h-block (stmt-ir:if-else-block st)
                                                     layout
                                                     coord-vars grad-target-fn hess-target-fn))))
           (push (stmt-ir:make-if-stmt cond then-blk else-blk) new-stmts)))

        (stmt-ir:assignment-statement
         ;; Always keep the assignment itself
         (push st new-stmts)
         (let* ((target (stmt-ir:stmt-target-name st))
                ;; Energy write
                (energy-stmt (make-energy-write-call-from-assignment target))
                ;; Gradient → KernelGradientAcc
                (force-stmt  (make-force-macro-call-from-grad-target target layout))
                ;; Hessian → Diag/KernelOffDiagHessAcc
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







(defun build-deriv-env-for-block-propagate (block base-var)
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
        (let* ((var  (expr-ir:ev (stmt-ir:stmt-target-name st)))
               (expr (stmt-ir:stmt-expression st))
               ;; Use your existing differentiator with this env
               (dvar (expr-ir:differentiate-expr expr base-var deriv-env)))
          (expr-ir:set-var-derivative var dvar deriv-env))))))



(defun make-gradient-assignments-from-block
    (base-block energy-var coord-vars grad-target-fn &optional manual-deriv-spec)
  "Return a list of assignment statements G_q := ∂(ENERGY-VAR)/∂q for each q.

If MANUAL-DERIV-SPEC is non-NIL:
  - :mode :manual   -> use manual chain rule from the spec only.
  - :mode :hybrid   -> use manual dE/du from the spec, and fill missing du/dq
                       automatically via AD from BASE-BLOCK.

Otherwise, fall back to pure AD on ENERGY-VAR wrt each coordinate."
  (declare (optimize (debug 3)))
  (cond
    ;; Manual / hybrid chain rule: dE/dq = Σ_u dE/du * du/dq
    ((and manual-deriv-spec
          (member (manual-deriv-spec-mode manual-deriv-spec) '(:manual :hybrid)))
     (let* ((mode          (manual-deriv-spec-mode manual-deriv-spec))
            (intermediates (manual-deriv-spec-intermediates manual-deriv-spec))
            (du-dq         (manual-deriv-spec-du-dq manual-deriv-spec))
            (dE-du         (manual-deriv-spec-dE-du manual-deriv-spec))
            ;; For :hybrid, precompute derivative environments per coord.
            (envs (when (eq mode :hybrid)
                    (let ((ht (make-hash-table :test #'eq)))
                      (dolist (q coord-vars)
                        (setf (gethash q ht)
                              (build-deriv-env-for-block-propagate base-block q)))
                      ht)))
            (grad-assignments '()))
       (dolist (q coord-vars (nreverse grad-assignments))
         (let* ((terms '())
                (env-q (and envs (gethash q envs nil))))
           (dolist (u intermediates)
             (let* ((dE/du (gethash u dE-du nil))
                    (du/dq (or (gethash (list u q) du-dq nil)
                               (and env-q
                                    (expr-ir:lookup-var-derivative u q env-q)))))
               (when (and dE/du du/dq)
                 (push (expr-ir:make-expr-mul (list dE/du du/dq))
                       terms))))
           (when terms
             (let* ((sum (if (cdr terms)
                             (expr-ir:make-expr-add terms)
                             (car terms)))
                    (sum-simplified (expr-ir:simplify-expr sum)))
               (unless (and (typep sum-simplified 'expr-ir:constant-expression)
                            (zerop (expr-ir:expression-value sum-simplified)))
                 (let* ((grad-sym (funcall grad-target-fn q))
                        (stmt    (stmt-ir:make-assignment-stmt grad-sym sum-simplified)))
                   (push stmt grad-assignments)))))))))
    ;; Default / AD path (existing behavior)
    (t
     (let ((grad-assignments '()))
       (dolist (q coord-vars (nreverse grad-assignments))
         (let* ((deriv-env (build-deriv-env-for-block-propagate base-block q))
                (dE-dq     (expr-ir:lookup-var-derivative energy-var q deriv-env))
                (dE-dq-s   (expr-ir:simplify-expr dE-dq)))
           (unless (and (typep dE-dq-s 'expr-ir:constant-expression)
                        (zerop (expr-ir:expression-value dE-dq-s)))
             (let* ((grad-sym  (funcall grad-target-fn q))
                    (grad-expr dE-dq-s)
                    (stmt      (stmt-ir:make-assignment-stmt grad-sym grad-expr)))
               (push stmt grad-assignments)))))))))


;;; ---------------------------------------------
;;; Helpers: pattern table from base assignments
;;; ---------------------------------------------


(defun build-assignment-pattern-table (block)
  "Scan BLOCK (a STMT-IR:BLOCK-STATEMENT) for top-level assignments
and build a list of (SEXPR . VAR-SYM), where SEXPR is the prefix
S-expression for the RHS and VAR-SYM is the LHS variable symbol.

This table will be used to rewrite derivative expressions to reuse
previous intermediates."
  (let ((entries '()))
    (dolist (st (stmt-ir:block-statements block) (nreverse entries))
      (when (typep st 'stmt-ir:assignment-statement)
        (let* ((var  (stmt-ir:stmt-target-name st))
               (expr (stmt-ir:stmt-expression st))
               ;; use simplified RHS as the pattern
               (simpl (expr-ir:simplify-expr expr))
               (sexp  (expr-ir:expr->sexpr simpl)))
          (push (cons sexp var) entries))))))

(defun %rewrite-sexpr-using-table (sexpr sorted-table)
  "Rewrite SEXPR by replacing any subexpression that exactly matches
one of the keys in SORTED-TABLE (SEXPR . VAR) with that VAR symbol.

SORTED-TABLE should be sorted by decreasing pattern size, so that
larger patterns are matched first."
  (labels ((rec (s)
             ;; First, try to match the whole S-expression against known patterns.
             (dolist (entry sorted-table)
               (when (equal s (car entry))
                 ;; replace entire subtree with the variable symbol
                 (return (cdr entry)))
               )
             ;; No whole-match; recurse into children if list.
             (if (atom s)
                 s
                 (cons (rec (car s))
                       (mapcar #'rec (cdr s))))))
    (rec sexpr)))

(defun rewrite-expr-using-assignments (expr block)
  "Given an EXPR (expression IR node) and a BLOCK of base assignments,
rewrite EXPR by replacing large subexpressions with previously-defined
intermediates from BLOCK.

Return a simplified expression IR node."
  (let* ((patterns (build-assignment-pattern-table block))
         ;; sort patterns by decreasing size (largest first)
         (sorted   (sort (copy-list patterns)
                         #'>
                         :key (lambda (entry)
                                (stmt-ir:sexpr-size (car entry)))))
         (sexp     (expr-ir:expr->sexpr expr))
         (rewritten-sexp (%rewrite-sexpr-using-table sexp sorted))
         (rewritten-expr (expr-ir:sexpr->expr-ir rewritten-sexp)))
    (expr-ir:simplify-expr rewritten-expr)))


(defun rewrite-radial-sexpr (sexpr radial-map)
  "Rewrite occurrences of (EXPT base p) where BASE is in RADIAL-MAP,
meaning BASE = r^2 for some length channel r.

We replace:
  (EXPT base p)  ->  (EXPT r (* 2 p))

in the SEXPR tree."
  (labels
      ((rec (s)
         (cond
           ((consp s)
            (let* ((head (car s))
                   (args (cdr s))
                   (new-args (mapcar #'rec args)))
              (if (and radial-map
                       (symbolp head)
                       (string= (symbol-name head) "EXPT")
                       (>= (length new-args) 2))
                  (let* ((base (first new-args))
                         (exp  (second new-args))
                         (chan (gethash base radial-map)))
                    (if chan
                        ;; (EXPT base p) -> (EXPT chan (* 2 p))
                        (list head chan (list '* 2 exp))
                        (cons head new-args)))
                  (cons head new-args))))
           (t
            s))))
    (rec sexpr)))

(defun build-radial-map-from-block (block)
  "Return a hash table mapping 'radial argument' sexprs to channel vars.

We look for assignments of the form:
  r = sqrt(arg)

and record mappings from:
  - sexpr(arg)          -> expr-var::r
  - sexpr(expanded-arg) -> expr-var::r

where expanded-arg is arg with one level of assignment expansion applied
(e.g. arg = r2, r2 = dx*dx+dy*dy+dz*dz)."
  (let ((assign-map (make-hash-table :test #'eq))
        (radial-map (make-hash-table :test #'equal)))
    ;; First, collect var -> RHS assignments.
    (when block
      (dolist (st (stmt-ir:block-statements block))
        (when (typep st 'stmt-ir:assignment-statement)
          (setf (gethash (stmt-ir:stmt-target-name st) assign-map)
                (stmt-ir:stmt-expression st)))))
    ;; Second, look for sqrt assignments and record keys.
    (when block
      (dolist (st (stmt-ir:block-statements block))
        (when (typep st 'stmt-ir:assignment-statement)
          (let* ((target (stmt-ir:stmt-target-name st))
                 (rhs    (stmt-ir:stmt-expression st)))
            (when (typep rhs 'expr-ir:function-call-expression)
              (let ((fname (expr-ir:function-call-name rhs)))
                (when (eql fname :sqrt)
                  (let ((args (expr-ir:function-call-arguments rhs)))
                    (when (and args (null (cdr args)))
                      (let* ((arg-expr   (first args))
                             (arg-simpl  (expr-ir:simplify-expr arg-expr))
                             (keys       '()))
                        ;; Direct key: sexpr(arg)
                        (push (expr-ir:expr->sexpr arg-simpl) keys)
                        ;; If arg is a variable with its own assignment, also
                        ;; record sexpr(RHS-of-that-variable).
                        (when (typep arg-expr 'expr-ir:variable-expression)
                          (let* ((sym    (expr-ir:variable-name arg-expr))
                                 (var-rhs (gethash sym assign-map)))
                            (when var-rhs
                              (let* ((var-simpl (expr-ir:simplify-expr var-rhs))
                                     (var-sexpr (expr-ir:expr->sexpr var-simpl)))
                                (push var-sexpr keys)))))
                        ;; Store all keys → expr-var::target
                        (let ((chan (expr-ir:ev target)))
                          (dolist (k keys)
                            (setf (gethash k radial-map) chan)))))))))))))
    radial-map))



(defun rewrite-sexpr-fixed-point (sexpr rules &key (max-iterations 5) verbose)
  "Apply RULES to SEXPR repeatedly, top-down, until no rule fires or
MAX-ITERATIONS is reached. Returns (values new-sexpr changed-p)."
  (let ((sx sexpr)
        (changed t)
        (iter 0))
    (loop
      while changed
      while (< iter max-iterations)
      do (incf iter)
         (multiple-value-bind (sx2 ch)
             (expr-ir:apply-rules-to-sexpr-once sx rules :verbose verbose)
           (setf sx sx2
                 changed ch))
      finally (return (values sx changed)))))

(defun canonicalize-expr-for-equivalence-once (expr &optional base-block radial-map verbose)
  "Canonicalize EXPR for equivalence checking.

If BASE-BLOCK is non-nil, use its assignments for reuse. If RADIAL-MAP is
non-nil (from BUILD-RADIAL-MAP-FROM-BLOCK), use it to rewrite (EXPT S -1/2)
forms into channel-based (EXPT R -1)."
  (let ((e expr))
    ;; 1. Reuse assignments (dx, r, etc.) if desired
    (when base-block
      (setf e (rewrite-expr-using-assignments e base-block)))

    ;; 2. Basic simplification
    (setf e (expr-ir:simplify-expr e))

    ;; 3. Sexpr-level rewrites: radial + exponent rules
    (let* ((sx0 (expr-ir:expr->sexpr e))
           (sx1 (if radial-map
                    (progn
                      (rewrite-radial-sexpr sx0 radial-map))
                    sx0))
           sx2)
      (multiple-value-bind (sx-next ignore-changed)
          (rewrite-sexpr-fixed-point sx1 expr-ir:*equivalence-canonicalization-rules* :max-iterations 10 :verbose verbose)
        (declare (ignore ignore-changed))
        (setf sx2 sx-next))
      (setf e (expr-ir:sexpr->expr-ir sx2)))

    ;; 4. Simplify again
    (setf e (expr-ir:simplify-expr e))

    ;; 5. Canonicalize linear numeric subexpressions
    (setf e (expr-ir:canonicalize-linear-subexprs e))

    ;; 6. Final simplify
    (setf e (expr-ir:simplify-expr e))

    e))

(defun canonicalize-expr-for-equivalence (expr &optional base-block radial-map verbose)
  (let ((ee expr))
    (loop for idx below 10
          for sexp-before = (expr->sexpr ee)
          for canon = (canonicalize-expr-for-equivalence-once ee base-block radial-map verbose)
          for sexp-after = (expr->sexpr canon)
          do (when (equal sexp-before sexp-after)
               (return nil))
          do (setf ee canon))
    ee))


(defun auto-fill-intermediate-geometry-from-ad
    (base-block coord-vars manual-deriv-spec
     &key (compute-second-derivs t)
          (reuse-assignments t))
  "For each intermediate u in MANUAL-DERIV-SPEC, automatically populate
du/dq and (optionally) d²u/(dqi dqj) using the AD machinery over BASE-BLOCK.

- BASE-BLOCK: STMT-IR:BLOCK-STATEMENT for the kernel body.
- COORD-VARS: list of expr-ir variable symbols (x1 y1 z1 ...).
- MANUAL-DERIV-SPEC: struct with accessors:
    MANUAL-DERIV-SPEC-INTERMEDIATES
    MANUAL-DERIV-SPEC-DU-DQ
    MANUAL-DERIV-SPEC-D2U-DQ2

If REUSE-ASSIGNMENTS is true, each derivative expression is rewritten to
reuse the largest matching RHS from BASE-BLOCK assignments.

Existing manual entries in DU-DQ / D2U-DQ2 are preserved (never overwritten)."
  (let* ((intermediates (manual-deriv-spec-intermediates manual-deriv-spec))
         (du-dq         (manual-deriv-spec-du-dq manual-deriv-spec))
         (d2u-dq2       (manual-deriv-spec-d2u-dq2 manual-deriv-spec))
         ;; precompute deriv envs for each base coordinate
         (envs (make-hash-table :test #'eq)))
    ;; build derivative envs wrt each coordinate
    (dolist (q coord-vars)
      (setf (gethash q envs)
            (build-deriv-env-for-block-propagate base-block q)))
    ;; For du/dq
    (dolist (u intermediates)
      (dolist (q coord-vars)
        (let ((key (list u q)))
          ;; Don't override manual entries
          (unless (gethash key du-dq)
            (let* ((env-q (gethash q envs))
                   (raw   (expr-ir:lookup-var-derivative u q env-q)))
              (when raw
                (let* ((r1 (if reuse-assignments
                               (rewrite-expr-using-assignments raw base-block)
                               raw))
                       (r2 (expr-ir:simplify-expr r1)))
                  (unless (and (typep r2 'expr-ir:constant-expression)
                               (zerop (expr-ir:expression-value r2)))
                    (setf (gethash key du-dq) r2))))))))
      ;; For d²u/(dqi dqj)
      (when compute-second-derivs
        (let ((n (length coord-vars)))
          (dotimes (i n)
            (let* ((qi   (nth i coord-vars))
                   (env-i (gethash qi envs))
                   (du/qi (or (gethash (list u qi) du-dq)
                              (let* ((raw (expr-ir:lookup-var-derivative u qi env-i))
                                     (r1  (if (and raw reuse-assignments)
                                              (rewrite-expr-using-assignments raw base-block)
                                              raw)))
                                (and r1 (expr-ir:simplify-expr r1))))))
              (when du/qi
                (dotimes (j n)
                  (when (<= i j)
                    (let* ((qj   (nth j coord-vars))
                           (env-j (gethash qj envs))
                           ;; don't override manual entries
                           (key-ij (list u qi qj))
                           (key-ji (list u qj qi)))
                      (unless (or (gethash key-ij d2u-dq2)
                                  (gethash key-ji d2u-dq2))
                        (let* ((raw-d2 (expr-ir:differentiate-expr du/qi qj env-j)))
                          (when raw-d2
                            (let* ((r1 (if reuse-assignments
                                           (rewrite-expr-using-assignments raw-d2 base-block)
                                           raw-d2))
                                   (r2 (expr-ir:simplify-expr r1)))
                              (unless (and (typep r2 'expr-ir:constant-expression)
                                           (zerop (expr-ir:expression-value r2)))
                                ;; store only upper triangle; reader can symmetrize
                                (setf (gethash key-ij d2u-dq2) r2)))))))))))))))
    manual-deriv-spec))

;;; ----------------------------------------------------------------------
;;; Derivative request statements and the D macro
;;; ----------------------------------------------------------------------


(defmacro D! (target base)
  "Insert a derivative request for d(TARGET)/d(BASE) at this point in the kernel body.
TARGET and BASE are symbols naming existing variables in the kernel body.

Example inside a stmt-block:
  (=. e_base \"...\")
  (D e_base r)

This will later be expanded to an assignment computing d(e_base)/d(r)
using the statement-level AD machinery."
  `(stmt-ir:make-derivative-request-stmt
     ',(expr-ir:ev target)
     ',(expr-ir:ev base)))

(defun %expand-derivative-requests-in-block (block seen-assignments)
  "Walk BLOCK and replace derivative-request-statements with explicit
assignment statements computing d(TARGET)/d(BASE), using the sequence
of assignments in SEEN-ASSIGNMENTS and the assignments preceding the
request in this BLOCK.

SEEN-ASSIGNMENTS is a list of assignment-statement objects that are
known to execute before this BLOCK (e.g. assignments in enclosing blocks)."
  (unless (typep block 'stmt-ir:block-statement)
    (error "%expand-derivative-requests-in-block: expected BLOCK-STATEMENT, got ~S"
           block))
  (let ((new-stmts '())
        ;; assignments visible in this block
        (local-assignments (copy-list seen-assignments)))
    (dolist (st (stmt-ir:block-statements block))
      (typecase st
        (stmt-ir:block-statement
         (let ((sub (%expand-derivative-requests-in-block st local-assignments)))
           (push sub new-stmts)))

        (stmt-ir:if-statement
         (let* ((cond     (stmt-ir:if-condition st))
                (then-blk (stmt-ir:if-then-block st))
                (else-blk (stmt-ir:if-else-block st))
                (new-then (%expand-derivative-requests-in-block
                           then-blk local-assignments))
                (new-else (and else-blk
                               (%expand-derivative-requests-in-block
                                else-blk local-assignments))))
           (push (stmt-ir:make-if-stmt cond new-then new-else) new-stmts)))

        (stmt-ir:assignment-statement
         (push st new-stmts)
         (push st local-assignments))

        (stmt-ir:derivative-request-statement
         (let* ((target (stmt-ir:dr-target-var st))
                (base   (stmt-ir:dr-base-var st))
                ;; Use your existing statement-level AD.
                ;; local-assignments are the assignments we've seen so far.
                (d-expr (stmt-ir:differentiate-target-in-block
                         (nreverse local-assignments) base target)))
           (unless d-expr
             (error "No derivative for ~S with respect to ~S; ensure TARGET is assigned before (D ...) is used."
                    target base))
           ;; Name for the derivative variable: you likely already have
           ;; a helper in stmt-ir; if not, define a simple one.
           (let* ((d-name (stmt-ir:make-derivative-name target base))
                  (d-stmt (stmt-ir:make-assignment-stmt d-name d-expr)))
             (push d-stmt new-stmts)
             (push d-stmt local-assignments))))

        (t
         (push st new-stmts))))
    (stmt-ir:make-block-stmt (nreverse new-stmts))))

(defun expand-derivative-requests (block)
  "Top-level entry: expand all derivative requests (D TARGET BASE)
inside BLOCK into explicit derivative assignments."
  (let ((new-block (%expand-derivative-requests-in-block block '())))
    new-block))


;;; ----------------------------------------------------------------------
;;; *post-eg-h-pipeline*
;;; ----------------------------------------------------------------------


(defparameter *post-eg-h-pipeline*
  (make-optimization-pipeline
   :name :post-eg-h
   :optimizations
   (list
    ;; very light structural passes only
    (make-optimization
     :name :copy-propagate-post-eg-h
     :function #'copy-propagate-optimization)
    (make-optimization
     :name :normalize-signs-post-eg-h
     :function #'normalize-signs-optimization))))



;;; ------------------------------------------------------------
;;; Write out kernel code
;;; ------------------------------------------------------------

(defun compile-kernel-to-c-function (kernel)
  "Lower KERNEL-IR to a C function stmt-ir object."
  (let* ((core-block (kernel-core-block kernel))
         (coord-load (kernel-coord-load-stmts kernel))
         (layout     (kernel-layout kernel))
         (coord-vars (kernel-coord-vars kernel))
         (params     (kernel-params kernel))
         ;; inject coord loads
         (block-with-coords
           (if coord-load
               (stmt-ir:make-block-stmt
                (append coord-load (list core-block)))
               core-block))
         ;; turn energy/grad/hess assignments into KernelGradientAcc/KernelDiagHessAcc/... macros
         (transformed
           (transform-eg-h-block
            block-with-coords
            layout
            coord-vars
            #'general-grad-name
            #'general-hess-name))
         (locals (infer-kernel-locals transformed params coord-vars)))
    (stmt-ir:make-c-function
     (kernel-name kernel)
     transformed
     :return-type "void"
     :parameters  params
     :locals      locals)))

(defun write-c-code (kernel pathname)
  (ensure-directories-exist pathname)
  (with-open-file (fout pathname :direction :output :if-exists :supersede)
    (let ((fun (compile-kernel-to-c-function kernel)))
      (write-line (stmt-ir:c-function->c-source-string fun) fout))))



;;; ------------------------------------------------------------
;;; Top-level DSL: DEFKERNEL
;;; ------------------------------------------------------------


(defclass kernel-ir ()
  ((name :accessor kernel-name :initarg :name)
   (layout :accessor kernel-layout :initarg :layout)
   (coord-vars :accessor kernel-coord-vars :initarg :coord-vars)
   (coord-load-stmts :accessor kernel-coord-load-stmts :initarg :coord-load-stmts)
   (params :accessor kernel-params :initarg :params)
   ;; pre-E/G/H block with scalar energy/grad/hess assignments
   (core-block :accessor kernel-core-block :initarg :core-block)
   ;; post-E/G/H block, after transform-eg-h-block and post-EG/H cleanup
   (eg-h-block :accessor kernel-eg-h-block :initarg :eg-h-block :initform nil)
   (compute-energy-p :accessor kernel-compute-energy-p :initarg :compute-energy-p)
   (compute-grad-p   :accessor kernel-compute-grad-p   :initarg :compute-grad-p)
   (compute-hess-p   :accessor kernel-compute-hess-p   :initarg :compute-hess-p)
   (manual-deriv-spec :accessor kernel-manual-deriv-spec :initarg :manual-deriv-spec)
   (pipeline :accessor kernel-pipeline :initarg :pipeline)
   ;; per-kernel algebraic customizations
   (extra-equivalence-rules
    :accessor kernel-extra-equivalence-rules
    :initarg :extra-equivalence-rules
    :initform nil)
   (extra-optimization-rules
    :accessor kernel-extra-optimization-rules
    :initarg :extra-optimization-rules
    :initform nil)))



(defun make-kernel-ir (&rest initargs)
  "Convenience constructor mirroring the old DEFSTRUCT MAKE-KERNEL-IR."
  (apply #'make-instance 'kernel-ir initargs))

;;; ------------------------------------------------------------
;;; Top-level make-kernel-from-block
;;;    Implements what DEFKERNEL does
;;; ------------------------------------------------------------



(defun make-kernel-from-block
    (&key name pipeline layout coord-vars coord-load-stmts base-block params
          compute-energy compute-grad compute-hess derivatives
          extra-equivalence-rules extra-optimization-rules
       post-eg-h-pipeline)
  (declare (optimize (debug 3)))
  (labels ((the-name (nm)
             (format nil "~a ~a" name nm)))
    (let* ((energy-var        'energy)
           (manual-deriv-spec (normalize-derivatives-spec derivatives))
           ;; 1. Lower D! into assignments
           (base-block*       (expand-derivative-requests base-block))
           ;; 2. Counter reused by all optimization pipelines
           (pass-counter      (stmt-ir:make-pass-id-counter))
           ;; 3. Choose a post-EG/H pipeline (kernel can override or disable)
           (post-eg-h-pipeline*
             (or post-eg-h-pipeline *post-eg-h-pipeline*)))
      ;; Dynamically extend rewrite rules for this kernel, if requested
      (expr-ir:with-kernel-rewrite-rules
          (extra-equivalence-rules extra-optimization-rules)

        ;; 3. Geometry consistency checks and auto-fill on lowered block
        (when manual-deriv-spec
          (check-intermediate-geometry!
           base-block* coord-vars manual-deriv-spec)
          (auto-fill-intermediate-geometry-from-ad
           base-block* coord-vars manual-deriv-spec
           :compute-second-derivs t
           :reuse-assignments    t))

        ;; 4. Build up energy + gradient + Hessian statements
        (let ((current-stmts
                (copy-list (stmt-ir:block-statements base-block*))))

          ;; 4a. Gradient
          (when compute-grad
            (let ((grad-stmts
                    (make-gradient-assignments-from-block
                     base-block* energy-var coord-vars #'general-grad-name
                     manual-deriv-spec)))
              (setf current-stmts (append current-stmts grad-stmts)))
            (when pipeline
              (let ((grad-block (stmt-ir:make-block-stmt current-stmts)))
                (multiple-value-bind (grad-opt results total-before total-after)
                    (stmt-ir:run-optimization-pipeline
                     pass-counter pipeline grad-block
                     :name (the-name "e-grad")
                     :log-stream *trace-output*)
                  (declare (ignore results total-before total-after))
                  (setf current-stmts
                        (copy-list (stmt-ir:block-statements grad-opt)))))))

          ;; 4b. Hessian
          (when compute-hess
            (let ((hess-stmts
                    (make-hessian-assignments-from-block
                     base-block* energy-var coord-vars #'general-hess-name
                     manual-deriv-spec)))
              (setf current-stmts (append current-stmts hess-stmts)))
            (when pipeline
              (let ((hess-block (stmt-ir:make-block-stmt current-stmts)))
                (multiple-value-bind (hess-opt results total-before total-after)
                    (stmt-ir:run-optimization-pipeline
                     pass-counter pipeline hess-block
                     :name (the-name "e-g-hess")
                     :log-stream *trace-output*)
                  (declare (ignore results total-before total-after))
                  (setf current-stmts
                        (copy-list (stmt-ir:block-statements hess-opt)))))))

          ;; 5. Coordinate loads + EG/H transform + optional post-EG/H clean-up
          (let* ((core-block
                   (stmt-ir:make-block-stmt current-stmts))
                 (block-with-coords
                   (if coord-load-stmts
                       (stmt-ir:make-block-stmt
                        (append coord-load-stmts (list core-block)))
                       core-block))
                 ;; expands ACCUMULATE-HERE into *Energy/Force/Hessian acc macros
                 (eg-h-block
                   (transform-eg-h-block
                    block-with-coords layout coord-vars
                    #'general-grad-name #'general-hess-name))
                 (eg-h-block*
                   (if post-eg-h-pipeline*
                       (multiple-value-bind (post-block results total-before total-after)
                           (stmt-ir:run-optimization-pipeline
                            pass-counter post-eg-h-pipeline* eg-h-block
                            :name (the-name "post-eg-h")
                            :log-stream *trace-output*)
                         (declare (ignore results total-before total-after))
                         post-block)
                       eg-h-block)))
            ;; 6. Construct kernel IR object
            (make-kernel-ir
             :name name
             :layout layout
             :coord-vars coord-vars
             :coord-load-stmts coord-load-stmts
             :params params
             :core-block core-block
             :eg-h-block eg-h-block*
             :compute-energy-p compute-energy
             :compute-grad-p   compute-grad
             :compute-hess-p   compute-hess
             :manual-deriv-spec manual-deriv-spec
             :pipeline pipeline
             :extra-equivalence-rules extra-equivalence-rules
             :extra-optimization-rules extra-optimization-rules)))))))


;;; ------------------------------------------------------------
;;; Top-level DSL: DEFKERNEL
;;; ------------------------------------------------------------


(defun vars (&rest vars)
  (mapcar #'expr-ir:expr-var-symbol vars))


(defmacro push-kernel (destination c-function-name compute-mode &body clauses)
  (labels ((clause-value (key)
             (cadr (assoc key clauses))))
    (let* ((pipeline        (clause-value :pipeline))
           (params          (clause-value :params))
           (layout-cl       (assoc :layout clauses))
           (coord-vars      (mapcar #'expr-ir:ev (clause-value :coord-vars)))
           (coord-load      (clause-value :coord-load))
           (body-form       (clause-value :body))
           (deriv-cl        (assoc :derivatives clauses))
           (deriv-spec      (and deriv-cl (second deriv-cl)))
           ;; NEW: per‑kernel rewrite rule clauses (optional)
           (extra-eq-rules  (clause-value :extra-equivalence-rules))
           (extra-opt-rules (clause-value :extra-optimization-rules))
           ;; layout pieces
           (atom->ibase     (second layout-cl))
           (axis->offset    (third layout-cl))
           (coord-names     (mapcar #'symbol-name coord-vars))
           (want-grad      (not (null (member compute-mode '(:gradient :hessian)))))
           (want-hess      (eq compute-mode :hessian)))
      `(push 
         (let* ((layout (make-kernel-layout
                         :atom->ibase ',atom->ibase
                         :axis->offset ',axis->offset))
                (coord-vars (vars ,@coord-names))
                (coord-load-stmts ,coord-load)
                (base-block ,body-form))
           (make-kernel-from-block
            :name ',(format nil "~a_~a" c-function-name (string-downcase compute-mode))
            :pipeline ,pipeline
            :layout layout
            :coord-vars coord-vars
            :coord-load-stmts coord-load-stmts
            :base-block base-block
            :params ',params
            :compute-energy t
            :compute-grad ,want-grad
            :compute-hess ,want-hess
            :derivatives ',deriv-spec
            ;; NEW: per‑kernel rewrite rules (may be NIL)
            :extra-equivalence-rules ,extra-eq-rules
            :extra-optimization-rules ,extra-opt-rules))
         ,destination))))

(defmacro build-multiple-kernels ((destination c-function-name compute-mode-list) &body clauses)
  `(progn
     ,@(loop for compute-mode in compute-mode-list
             collect `(push-kernel ,destination ,c-function-name ,compute-mode
                        ,@clauses))))


(defun write-all (kernels &key (pathname (or (uiop/os:getenv "KERNEL_PATH") "/tmp/kernels/")))
  (loop for kernel in kernels
        for name = (string-downcase (kernel-name kernel))
        for pn = (merge-pathnames (make-pathname :name name :type "c") (pathname pathname))
        do (format t "writing = ~s~%" pn)
        do (write-c-code kernel pn)))

(defmacro with-kernels ((destination) &body body)
  `(let ((,destination nil))
     ,@body))
