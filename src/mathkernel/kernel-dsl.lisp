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
    :accessor manual-deriv-spec-mode
    :documentation
    "Overall mode for how this spec is used when building gradients and
Hessians.

Allowed values:
  :manual  – Use only the derivatives explicitly provided in this spec.
             Missing pieces are treated as zero (i.e. the corresponding
             channel simply does not contribute).
  :hybrid  – Use manual dE/du and d²E/(du dv) from this spec, but fill in
             missing du/dq entries automatically using AD on the kernel
             base block. Second‑derivative geometry (d²u/dq²) is never
             auto‑generated; if absent, the corresponding term is omitted.

If NIL or omitted when normalizing the :DERIVATIVES clause, the default
is :manual." )
   (intermediates
    :initarg :intermediates
    :accessor manual-deriv-spec-intermediates
    :documentation
    "List of intermediate scalar variables u that participate in the
manual chain rule.

Each element is an EXPR-VAR symbol (e.g. EXPR-VAR:R, EXPR-VAR:THETA)
corresponding to the intermediates referenced elsewhere in this spec.")
   (du-dq
    :initarg :du-dq
    :accessor manual-deriv-spec-du-dq
    :documentation
    "Hash table mapping (u q) → ∂u/∂q expressions.

Key:
  (list u q) where u and q are EXPR-VAR symbols.
Value:
  EXPR-IR node representing ∂u/∂q.

These entries typically come from the :INTERMEDIATE->COORD section of the
:DERIVATIVES clause, but may also be auto‑filled from AD in :hybrid mode
by AUTO-FILL-INTERMEDIATE-GEOMETRY-FROM-AD.")
   (d2u-dq2
    :initarg :d2u-dq2
    :accessor manual-deriv-spec-d2u-dq2
    :documentation
    "Hash table mapping (u qi qj) → ∂²u/(∂qi ∂qj) expressions.

Key:
  (list u qi qj) with u, qi, qj EXPR-VAR symbols; only the upper
  triangle (i ≤ j) is stored, but lookups are symmetric.
Value:
  EXPR-IR node representing the mixed second derivative.

Entries originate from :INTERMEDIATE->COORD2 in the :DERIVATIVES clause.
They are never auto‑generated; if missing, the dE/du * d²u term in the
Hessian is simply omitted for that channel.")
   (dE-du
    :initarg :dE-du
    :accessor manual-deriv-spec-dE-du
    :documentation
    "Hash table mapping u → ∂E/∂u expressions.

Key:
  u, an EXPR-VAR symbol naming an intermediate.
Value:
  EXPR-IR node for ∂E/∂u.

These come from the :GRADIENT subsection of :ENERGY->INTERMEDIATE in the
:DERIVATIVES clause and are used for both gradient and Hessian chain
rules.")
   (d2E-dudu
    :initarg :d2E-dudu
    :accessor manual-deriv-spec-d2E-dudu
    :documentation
    "Hash table mapping (u v) → ∂²E/(∂u ∂v) expressions.

Key:
  (list u v) with u, v EXPR-VAR symbols; the table is treated
  symmetrically, so (v u) may also be used for lookup.
Value:
  EXPR-IR node for the mixed second derivative.

These come from the :HESSIAN subsection of :ENERGY->INTERMEDIATE.")
   (hessian-modes
    :initarg :hessian-modes
    :accessor manual-deriv-spec-hessian-modes
    :documentation
    "Per‑intermediate flags controlling how each channel contributes to
Hessian entries.

This is a hash table mapping u (an EXPR-VAR symbol) to one of:
  :full               – include both outer‑product and curvature terms
                        for this channel in the Hessian.
  :outer-product-only – include only terms involving d²E/(du²) * du/dqi
                        * du/dqj, i.e. no geometry curvature d²u terms.
  :none               – ignore this intermediate when assembling the
                        Hessian.

If an intermediate has no entry in this table, :full is assumed." )
   (geometry-check-mode
    :initarg :geometry-check-mode
    :accessor manual-deriv-spec-geometry-check-mode
    :documentation
    "Controls how manual intermediate geometry (:INTERMEDIATE->COORD and
:INTERMEDIATE->COORD2) is validated against AD‑generated derivatives.

Allowed values:
  :none  – Do not perform any consistency checks.
  :warn  – Compare manual du/dq and d²u/dq² against AD; print warnings if
           mismatches are detected but continue compilation.
  :error – As for :warn, but signal an error on mismatch.

CHECK-INTERMEDIATE-GEOMETRY! consults this slot when a MANUAL-DERIV-SPEC
is present in a kernel.")))

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

(defun distribute-products-over-sums (sexpr)
  "Recursively expand products over sums in SEXPR (infix-style sexpr).
E.g. (* (+ a b) c) -> (+ (* a c) (* b c))."
  (labels ((make-prod (xs)
             (cond
               ((null xs) 1)
               ((null (cdr xs)) (car xs))
               (t (cons '* xs)))))
    (cond
      ((atom sexpr) sexpr)
      ((and (consp sexpr) (eq (car sexpr) '+))
       (cons '+ (mapcar #'distribute-products-over-sums (cdr sexpr))))
      ((and (consp sexpr) (eq (car sexpr) '*))
       (let* ((args (mapcar #'distribute-products-over-sums (cdr sexpr)))
              (terms (list 1)))
         (dolist (arg args)
           (if (and (consp arg) (eq (car arg) '+))
               (setf terms
                     (loop for term in terms append
                           (loop for summand in (cdr arg)
                                 collect (make-prod (list term summand)))))
               (setf terms
                     (loop for term in terms
                           collect (make-prod (list term arg))))))
         (if (= (length terms) 1)
             (first terms)
             (cons '+ terms))))
      (t
             (cons (distribute-products-over-sums (car sexpr))
                   (mapcar #'distribute-products-over-sums (cdr sexpr)))))))

(defun commutative-canonicalize-sexpr (sexpr)
  "Canonicalize SEXPR for commutative ops + and * by flattening and sorting args."
  (labels ((flatten-op (op args)
             (loop for a in args append
                   (if (and (consp a) (eq (car a) op))
                       (cdr a)
                       (list a)))))
    (cond
      ((atom sexpr) sexpr)
      ((and (consp sexpr) (member (car sexpr) '(+ *)))
       (let* ((op   (car sexpr))
              (raw  (mapcar #'commutative-canonicalize-sexpr (cdr sexpr)))
              (flat (flatten-op op raw))
              (sorted (sort (copy-list flat)
                            (lambda (a b)
                              (string< (prin1-to-string a)
                                       (prin1-to-string b))))))
         (cons op sorted)))
      (t
       (cons (commutative-canonicalize-sexpr (car sexpr))
             (mapcar #'commutative-canonicalize-sexpr (cdr sexpr)))))))

(defun normalize-commutative-sexpr (sexpr)
  "Flatten +/*, drop identities (0 for +, 1 for *), and sort commutative args."
  (labels ((norm (sx)
             (cond
               ((atom sx) sx)
               ((and (consp sx) (member (car sx) '(+ *)))
                (let* ((op (car sx))
                       (args (mapcar #'norm (cdr sx)))
                       ;; flatten nested ops
                       (flat (loop for a in args append
                                   (if (and (consp a) (eq (car a) op))
                                       (cdr a)
                                       (list a))))
                       ;; drop identities
                       (filtered
                         (remove-if (lambda (x)
                                      (and (atom x)
                                           (or (and (eq op '+) (eql x 0))
                                               (and (eq op '*) (eql x 1)))))
                                    flat)))
                  (cond
                    ((null filtered)
                     (if (eq op '+) 0 1))
                    ((null (cdr filtered))
                     (car filtered))
                    (t
                     (cons op
                           (sort (copy-list filtered)
                                 (lambda (a b)
                                   (string< (prin1-to-string a)
                                            (prin1-to-string b)))))))))
               (t
                (cons (norm (car sx)) (mapcar #'norm (cdr sx)))))))
    (norm sexpr)))

(defun fully-normalize-sexpr (sexpr)
  "Distribute, canonicalize commutative ops, drop identities, and simplify."
  (let* ((dist (distribute-products-over-sums sexpr))
         (comm (commutative-canonicalize-sexpr dist))
         (norm (normalize-commutative-sexpr comm))
         (simp (expr-ir:simplify-expr (expr-ir:sexpr->expr-ir norm))))
    (expr-ir:expr->sexpr simp)))




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
         ;; Try inlining assignments first so R2 substitutions are visible
         (e1-raw (if base-block
                     (rewrite-expr-using-assignments expr1 base-block)
                     expr1))
         (e2-raw (if base-block
                     (rewrite-expr-using-assignments expr2 base-block)
                     expr2))
         (e1 (canonicalize-expr-for-equivalence e1-raw base-block radial-map verbose))
         (e2 (canonicalize-expr-for-equivalence e2-raw base-block radial-map verbose))
         (sx1 (expr-ir:expr->sexpr e1))
         (sx2 (expr-ir:expr->sexpr e2))
         (sx1-full (fully-normalize-sexpr sx1))
         (sx2-full (fully-normalize-sexpr sx2)))
    (cond
      ;; 1. Structural equality after canonicalization
      ((equal sx1 sx2)
       (values t sx1 sx2))
      ;; 1b. Equality after aggressive normalization/simplify
      ((equal sx1-full sx2-full)
       (values t sx1-full sx2-full))
      (t
       ;; 2. Fallback: diff = e1 - e2, factor and simplify, check for constant 0
        (let* ((neg-e2   (expr-ir:make-expr-neg e2))
               (diff     (expr-ir:make-expr-add (list e1 neg-e2)))
               (diff-simple (expr-ir:simplify-expr
                             (expr-ir:make-expr-add (list e1-raw (expr-ir:make-expr-neg e2-raw)))))
               (diff-f   (expr-ir:factor-sum-of-products diff))
              (diff-sim (expr-ir:simplify-expr diff-f)))
          (when (and (typep diff-simple 'expr-ir:constant-expression)
                     (zerop (expr-ir:expression-value diff-simple)))
            (return-from expressions-equivalent-p
              (values t (expr-ir:expr->sexpr diff-simple) (expr-ir:expr->sexpr diff-simple))))
          (if (and (typep diff-sim 'expr-ir:constant-expression)
                   (zerop (expr-ir:expression-value diff-sim)))
              (values t sx1 sx2)
              (values nil sx1 sx2)))))))


(defstruct geometry-check-accumulator
  (derivatives (make-hash-table :test #'eq)))

(defclass geometry-check-context (stmt-ir:walk-context)
  ((deriv-env
    :initarg :deriv-env
    :accessor geometry-check-ctx-deriv-env)
   (accumulator
    :initarg :accumulator
    :accessor geometry-check-ctx-accumulator)))

(defclass geometry-check-operation ()
  ((base-var
    :initarg :base-var
    :reader geometry-check-operation-base-var)
   (manual-deriv-spec
    :initarg :manual-deriv-spec
    :reader geometry-check-operation-manual-deriv-spec)
   (base-block
    :initarg :base-block
    :reader geometry-check-operation-base-block)
   (du-dq
    :initarg :du-dq
    :reader geometry-check-operation-du-dq)
   (d2u-dq2
    :initarg :d2u-dq2
    :reader geometry-check-operation-d2u-dq2)
   (intermediate-set
    :initarg :intermediate-set
    :reader geometry-check-operation-intermediate-set)
   (mode
    :initarg :mode
    :reader geometry-check-operation-mode)
   (reuse-assignments
    :initarg :reuse-assignments
    :reader geometry-check-operation-reuse-assignments)
   (accumulator
    :initarg :accumulator
    :reader geometry-check-operation-accumulator)))

(defun make-geometry-check-operation (&rest initargs)
  (apply #'make-instance 'geometry-check-operation initargs))

(defun record-geometry-derivative (acc var expr)
  (let* ((table (geometry-check-accumulator-derivatives acc))
         (existing (gethash var table)))
    (setf (gethash var table) (cons expr existing)))
  acc)

(defun deriv-env-from-accumulator (acc base-var)
  (let ((env (expr-ir:make-deriv-env)))
    (expr-ir:set-var-derivative base-var (expr-ir:make-expr-const 1) env)
    (when acc
      (maphash (lambda (var exprs)
                 (when exprs
                   (expr-ir:set-var-derivative var (car exprs) env)))
               (geometry-check-accumulator-derivatives acc)))
    env))

(defun report-geometry-mismatch (mode what manual-expr auto-expr sx1 sx2)
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
                 (expr-ir:side-by-side-sexpr sx1 sx2
                                             :label1 "transformed manual"
                                             :label2 "transformed auto"))))
      (:error (error msg)))))

(defmethod stmt-ir:clone-context ((operation geometry-check-operation)
                                  (ctx geometry-check-context)
                                  block)
  (make-instance 'geometry-check-context
                 :deriv-env (expr-ir:copy-deriv-env
                             (geometry-check-ctx-deriv-env ctx))
                 :accumulator (geometry-check-ctx-accumulator ctx)))

(defmethod stmt-ir:on-statement ((operation geometry-check-operation)
                                 (ctx geometry-check-context)
                                 stmt)
  (let* ((env (geometry-check-ctx-deriv-env ctx))
         (base-var (geometry-check-operation-base-var operation)))
    (etypecase stmt
      ((or stmt-ir:assignment-statement stmt-ir:anchored-assignment-statement)
       (let* ((target (stmt-ir:stmt-target-name stmt))
              (rhs    (stmt-ir:stmt-expression stmt))
              (dvar   (expr-ir:differentiate-expr rhs base-var env)))
         (expr-ir:set-var-derivative target dvar env)
         (record-geometry-derivative (geometry-check-operation-accumulator operation)
                                     target dvar)
         (when (and dvar
                    (gethash target (geometry-check-operation-intermediate-set operation)))
           (let* ((manual (gethash (list target base-var)
                                   (geometry-check-operation-du-dq operation))))
             (when manual
               (let* ((auto (if (geometry-check-operation-reuse-assignments operation)
                                (rewrite-expr-using-assignments
                                 dvar
                                 (geometry-check-operation-base-block operation))
                                dvar)))
                 (multiple-value-bind (matched sx1 sx2)
                     (expressions-equivalent-p manual auto
                                               (geometry-check-operation-base-block operation))
                   (unless matched
                     (report-geometry-mismatch
                      (geometry-check-operation-mode operation)
                      (list :du-dq target base-var)
                      manual auto sx1 sx2))))))))
       (values stmt ctx))
      (t
       (values stmt ctx)))))

(defun check-intermediate-geometry!
    (base-block coord-vars manual-deriv-spec &key (reuse-assignments t))
  "If MANUAL-DERIV-SPEC has :geometry-check-mode of :warn or :error and
contains manual :intermediate->coord and/or :intermediate->coord2 entries,
compare them against AD-generated derivatives from BASE-BLOCK using the stmt
walker. Signals warnings or errors if mismatches are found, depending on
mode."
  (declare (optimize (debug 3)))
  (let ((mode (manual-deriv-spec-geometry-check-mode manual-deriv-spec)))
    (unless (member mode '(:warn :error))
      (return-from check-intermediate-geometry! manual-deriv-spec))
    (let* ((intermediates (manual-deriv-spec-intermediates manual-deriv-spec))
           (intermediate-set (let ((ht (make-hash-table :test #'eq)))
                               (dolist (u intermediates ht)
                                 (setf (gethash u ht) t))))
           (du-dq         (manual-deriv-spec-du-dq manual-deriv-spec))
           (d2u-dq2       (manual-deriv-spec-d2u-dq2 manual-deriv-spec))
           (acc-by-base   (make-hash-table :test #'eq)))
      ;; Pass 1: walk block per coordinate to collect AD derivatives and check du/dq.
      (dolist (q coord-vars)
        (let* ((acc (make-geometry-check-accumulator))
               (env (expr-ir:make-deriv-env))
               (ctx (make-instance 'geometry-check-context
                                   :deriv-env env
                                   :accumulator acc))
               (op  (make-geometry-check-operation
                     :base-var q
                     :manual-deriv-spec manual-deriv-spec
                     :base-block base-block
                     :du-dq du-dq
                     :d2u-dq2 d2u-dq2
                     :intermediate-set intermediate-set
                     :mode mode
                     :reuse-assignments reuse-assignments
                     :accumulator acc)))
          (expr-ir:set-var-derivative q (expr-ir:make-expr-const 1) env)
          (stmt-ir:walk-block-with-context op ctx base-block)
          (setf (gethash q acc-by-base) acc)))
      ;; Pass 2: check manual d²u entries against AD by reusing collected du/dq.
      (let ((n (length coord-vars)))
        (dolist (u intermediates)
          (dotimes (i n)
            (let* ((qi    (nth i coord-vars))
                   (acc-i (gethash qi acc-by-base))
                   (du/qi (or (gethash (list u qi) du-dq)
                              (let* ((table (and acc-i
                                                 (geometry-check-accumulator-derivatives acc-i))))
                                (and table (car (gethash u table nil)))))))
              (when du/qi
                (dotimes (j n)
                  (when (<= i j)
                    (let* ((qj     (nth j coord-vars))
                           (manual (or (gethash (list u qi qj) d2u-dq2 nil)
                                       (gethash (list u qj qi) d2u-dq2 nil))))
                      (when manual
                        (let* ((acc-j   (gethash qj acc-by-base))
                               (env-j   (and acc-j (deriv-env-from-accumulator acc-j qj)))
                               (auto-raw (and env-j
                                              (expr-ir:differentiate-expr du/qi qj env-j))))
                          (when auto-raw
                            (let* ((auto (if reuse-assignments
                                             (rewrite-expr-using-assignments auto-raw base-block)
                                             auto-raw)))
                              (multiple-value-bind (matched sx1 sx2)
                                  (expressions-equivalent-p manual auto base-block)
                                (unless matched
                                  (report-geometry-mismatch mode
                                                            (list :d2u-dq2 u qi qj)
                                                            manual auto sx1 sx2)))))))))))))))
      manual-deriv-spec)))

(defclass inject-context-base (stmt-ir:walk-context)
  ((seen
    :initarg :seen
    :accessor inject-ctx-seen)
   (coord-vars
    :initarg :coord-vars
    :reader inject-ctx-coord-vars)
   (energy-var
    :initarg :energy-var
    :reader inject-ctx-energy-var)))

(defclass grad-inject-context (inject-context-base)
  ((grad-target-fn
    :initarg :grad-target-fn
    :reader grad-inject-ctx-grad-target-fn)))

(defclass hess-inject-context (inject-context-base)
  ((hess-target-fn
    :initarg :hess-target-fn
    :reader hess-inject-ctx-hess-target-fn)
   (manual-deriv-spec
    :initarg :manual-deriv-spec
    :reader hess-inject-ctx-manual-deriv-spec)))

(defun make-grad-inject-context (&key coord-vars energy-var grad-target-fn)
  (make-instance 'grad-inject-context
                 :seen '()
                 :coord-vars coord-vars
                 :energy-var energy-var
                 :grad-target-fn grad-target-fn))

(defun make-hess-inject-context (&key coord-vars energy-var hess-target-fn manual-deriv-spec)
  (make-instance 'hess-inject-context
                 :seen '()
                 :coord-vars coord-vars
                 :energy-var energy-var
                 :hess-target-fn hess-target-fn
                 :manual-deriv-spec manual-deriv-spec))

(defmethod stmt-ir:clone-context ((op (eql :inject-gradients))
                                  (ctx grad-inject-context)
                                  block)
  (make-instance 'grad-inject-context
                 :seen (copy-list (inject-ctx-seen ctx))
                 :coord-vars (inject-ctx-coord-vars ctx)
                 :energy-var (inject-ctx-energy-var ctx)
                 :grad-target-fn (grad-inject-ctx-grad-target-fn ctx)))

(defmethod stmt-ir:clone-context ((op (eql :inject-hessians))
                                  (ctx hess-inject-context)
                                  block)
  (make-instance 'hess-inject-context
                 :seen (copy-list (inject-ctx-seen ctx))
                 :coord-vars (inject-ctx-coord-vars ctx)
                 :energy-var (inject-ctx-energy-var ctx)
                 :hess-target-fn (hess-inject-ctx-hess-target-fn ctx)
                 :manual-deriv-spec (hess-inject-ctx-manual-deriv-spec ctx)))

(defmethod stmt-ir:on-statement ((op (eql :inject-gradients))
                                 (ctx grad-inject-context)
                                 st)
  (etypecase st
    (stmt-ir:assignment-statement
     (let* ((seen (append (inject-ctx-seen ctx) (list st)))
            (energy (inject-ctx-energy-var ctx)))
       (setf (inject-ctx-seen ctx) seen)
       (if (eq (stmt-ir:stmt-target-name st) energy)
           (let ((extra '()))
             (dolist (q (inject-ctx-coord-vars ctx))
               (let ((dex (stmt-ir:differentiate-target-in-block seen q energy)))
                 (when dex
                   (push (stmt-ir:make-anchored-assignment-stmt
                          (funcall (grad-inject-ctx-grad-target-fn ctx) q)
                          dex)
                         extra))))
             (values (nconc (list st) (nreverse extra)) ctx))
           (values st ctx))))
    (t
     (values st ctx))))

(defmethod stmt-ir:on-statement ((op (eql :inject-hessians))
                                 (ctx hess-inject-context)
                                 st)
  (etypecase st
    (stmt-ir:assignment-statement
     (let* ((seen (append (inject-ctx-seen ctx) (list st)))
            (energy (inject-ctx-energy-var ctx)))
       (setf (inject-ctx-seen ctx) seen)
       (if (eq (stmt-ir:stmt-target-name st) energy)
           (let* ((blk (stmt-ir:make-block-stmt seen :label :lost-label-in-inject-hessians))
                  (hess-stmts (make-hessian-assignments-from-block
                               blk
                               energy
                               (inject-ctx-coord-vars ctx)
                               (hess-inject-ctx-hess-target-fn ctx)
                               (hess-inject-ctx-manual-deriv-spec ctx))))
             (values (nconc (list st) hess-stmts) ctx))
           (values st ctx))))
    (t
     (values st ctx))))

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

(defun inject-gradients-after-energy (block coord-vars energy-var grad-target-fn)
  "Return a new BLOCK with per-branch gradient assignments inserted
immediately after any assignment to ENERGY-VAR. Gradients are computed
using the assignments visible along that control-flow path."
  (let* ((ctx (make-grad-inject-context
               :coord-vars coord-vars
               :energy-var energy-var
               :grad-target-fn grad-target-fn))
         (new-block nil)
         (new-ctx nil))
    (declare (ignore new-ctx))
    (multiple-value-bind (blk ctx-out)
        (stmt-ir:walk-block-with-context :inject-gradients ctx block)
      (declare (ignore ctx-out))
      blk)))

(defun inject-hessians-after-energy (block coord-vars energy-var hess-target-fn
                                    &optional manual-deriv-spec)
  "Return a new BLOCK with per-branch Hessian assignments inserted
immediately after any assignment to ENERGY-VAR. Hessians are computed
using the assignments visible along that control-flow path."
  (let* ((ctx (make-hess-inject-context
               :coord-vars coord-vars
               :energy-var energy-var
               :hess-target-fn hess-target-fn
               :manual-deriv-spec manual-deriv-spec))
         (new-block nil)
         (new-ctx nil))
    (declare (ignore new-ctx))
    (multiple-value-bind (blk ctx-out)
        (stmt-ir:walk-block-with-context :inject-hessians ctx block)
      (declare (ignore ctx-out))
      blk)))


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

(defstruct annotated-stmt
  stmt
  modes)

(defun normalize-modes (modes)
  (cond
    ((null modes) '(:energy))
    ((symbolp modes) (list modes))
    (t modes)))

(defmacro =. (var expr-string &key modes)
  "Create an assignment statement VAR := parse(EXPR-STRING).
Optional :MODES restricts when the statement is kept:
  :energy (default), :gradient, :hessian. Higher modes include lower ones."
  `(make-annotated-stmt
    :stmt (stmt-ir:make-assignment-stmt
           ',(expr-ir:ev var)
           (expr-ir:parse-expr ,expr-string))
    :modes (normalize-modes ',modes)))

(defmacro stmt-block (label &body forms)
  "Construct a STMT-IR block-statement from FORMS.
Each FORM should evaluate to a statement object (e.g. from =.)."
  `(stmt-ir:make-block-stmt
    (list ,@forms)
    :label ',label
    ))

(defun mode-level (mode)
  (ecase mode
    (:energy 1)
    (:gradient 2)
    (:hessian 3)))

(defun keep-for-mode-p (stmt-modes compute-mode)
  "Return T if a statement tagged with STMT-MODES should be kept for COMPUTE-MODE."
  (let* ((modes (normalize-modes stmt-modes))
         (min-level (reduce #'min modes :key #'mode-level)))
  (<= min-level (mode-level compute-mode))))

(defun filter-block-for-mode (block compute-mode)
  "Strip annotated statements whose modes do not include COMPUTE-MODE.
Bare statements (not annotated-stmt) are treated as :energy."
  (labels ((keep (form)
             (cond
               ((annotated-stmt-p form)
                (if (keep-for-mode-p (annotated-stmt-modes form) compute-mode)
                    (annotated-stmt-stmt form)
                    nil))
               (t
                (if (keep-for-mode-p '(:energy) compute-mode)
                    form
                    nil))))
           (walk (blk)
             (let ((new '()))
               (dolist (st (stmt-ir:block-statements blk))
                 (let ((kept (keep st)))
                   (when kept
                     (typecase kept
                       (stmt-ir:block-statement
                        (push (walk kept) new))
                       (stmt-ir:if-statement
                        (let* ((then-b (stmt-ir:if-then-block kept))
                               (else-b (stmt-ir:if-else-block kept))
                               (new-then (and then-b (walk then-b)))
                               (new-else (and else-b (walk else-b))))
                          (push (stmt-ir:make-if-stmt
                                 (stmt-ir:if-condition kept)
                                 new-then new-else)
                                new)))
                       (t
                        (push kept new))))))
               (stmt-ir:make-block-stmt (nreverse new)
                                        :label (stmt-ir:block-label blk)))))
    (walk block)))

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
       (dotimes (i n)
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
                              (stmt  (stmt-ir:make-anchored-assignment-stmt h-sym sum-simplified)))
                         (push stmt hess-assignments))))))))))
       (let* ((out (reverse hess-assignments))
              (targets (mapcar #'stmt-ir:stmt-target-name out)))
         out)))
    ;; Default / AD path (existing behavior)
    (t
     (let* ((n    (length coord-vars))
            (envs (make-array n)))
       (dotimes (k n)
         (setf (aref envs k)
               (build-deriv-env-for-block base-block (nth k coord-vars))))
       (let ((hess-assignments '()))
         (dotimes (i n)
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
                            (stmt  (stmt-ir:make-anchored-assignment-stmt h-sym d2E)))
                       (push stmt hess-assignments))))))))
         (let* ((out (reverse hess-assignments))
                (targets (mapcar #'stmt-ir:stmt-target-name out)))
           out))))))




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
            (warn "make-hess-macro-call-from-target")
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
    (stmt-ir:make-block-stmt (reverse new-stmts)
                             :label (stmt-ir:block-label block))))







(defun build-deriv-env-for-block-propagate (block base-var)
  "Propagate derivatives d(* )/d(BASE-VAR) through BLOCK and
return the resulting DERIV-ENV (an EXPR-IR derivative environment).

BLOCK is a STMT-IR:BLOCK-STATEMENT whose statements are simple
assignments VAR := EXPR-IR-node.

BASE-VAR is a coordinate symbol like X1, Y1, etc."
  (declare (optimize (debug 3)))
  (labels ((process-block (blk env)
             (dolist (st (stmt-ir:block-statements blk) env)
               (etypecase st
                 (stmt-ir:assignment-statement
                  (let* ((var  (expr-ir:ev (stmt-ir:stmt-target-name st)))
                         (expr (stmt-ir:stmt-expression st))
                         (dvar (expr-ir:differentiate-expr expr base-var env)))
                    (expr-ir:set-var-derivative var dvar env)))
                 (stmt-ir:if-statement
                  ;; Recurse into branches with copies of env; if both branches
                 ;; produce a derivative, prefer THEN; otherwise take what's available.
                  (let* ((then-env (when (stmt-ir:if-then-block st)
                                     (process-block (stmt-ir:if-then-block st)
                                                    (expr-ir:copy-deriv-env env))))
                         (else-env (when (stmt-ir:if-else-block st)
                                     (process-block (stmt-ir:if-else-block st)
                                                    (expr-ir:copy-deriv-env env)))))
                    (when then-env
                      (maphash (lambda (k v)
                                 (expr-ir:set-var-derivative k v env))
                               (expr-ir:deriv-env-table then-env)))
                    (when (and else-env (null then-env))
                      (maphash (lambda (k v)
                                 (expr-ir:set-var-derivative k v env))
                               (expr-ir:deriv-env-table else-env)))))
                 (stmt-ir:block-statement
                  (setf env (process-block st env)))
                 (t env)))))
    (process-block block (expr-ir:make-deriv-env))))



#+(or)
(defun make-gradient-assignments-from-block
    (base-block energy-var coord-vars grad-target-fn &optional manual-deriv-spec)
  "Return a list of assignment statements G_q := ∂(ENERGY-VAR)/∂q for each q.

If MANUAL-DERIV-SPEC is non-NIL:
  - :mode :manual   -> use manual chain rule from the spec only.
  - :mode :hybrid   -> use manual dE/du from the spec, and fill missing du/dq
                       automatically via AD from BASE-BLOCK.

Otherwise, fall back to pure AD on ENERGY-VAR wrt each coordinate."
  (declare (optimize (debug 3)))
  (let ((result
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
                                (stmt    (stmt-ir:make-anchored-assignment-stmt grad-sym sum-simplified)))
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
                            (stmt      (stmt-ir:make-anchored-assignment-stmt grad-sym grad-expr)))
                       (push stmt grad-assignments))))))))))
    result
    ))


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


(defmacro D! (target base &key modes)
  "Insert a derivative request for d(TARGET)/d(BASE) at this point in the kernel body.
TARGET and BASE are symbols naming existing variables in the kernel body.

Example inside a stmt-block:
  (=. e_base \"...\")
  (D e_base r)

This will later be expanded to an assignment computing d(e_base)/d(r)
using the statement-level AD machinery."
  `(make-annotated-stmt
    :stmt (stmt-ir:make-derivative-request-stmt
           ',(expr-ir:ev target)
           ',(expr-ir:ev base))
    :modes (normalize-modes (or ',modes '(:gradient :hessian)))))

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
    (stmt-ir:make-block-stmt (nreverse new-stmts)
                             :label (stmt-ir:block-label block))))

(defun expand-derivative-requests (block)
  "Top-level entry: expand all derivative requests (D TARGET BASE)
inside BLOCK into explicit derivative assignments."
  (let ((new-block (%expand-derivative-requests-in-block block '())))
    new-block))

(defun remove-derivative-requests (block)
  "Return a copy of BLOCK with derivative-request statements removed, and
assignments whose targets are the derivatives requested. Used for
energy-only kernels where D! requests are unnecessary."
  (unless (typep block 'stmt-ir:block-statement)
    (error "remove-derivative-requests: expected BLOCK-STATEMENT, got ~S" block))
  ;; First collect derivative variable names introduced by D! requests (as strings).
  (labels ((collect (blk)
             (let ((names '()))
               (dolist (st (stmt-ir:block-statements blk))
                 (typecase st
                   (stmt-ir:block-statement
                    (setf names (nconc names (collect st))))
                   (stmt-ir:if-statement
                    (when (stmt-ir:if-then-block st)
                      (setf names (nconc names (collect (stmt-ir:if-then-block st)))))
                    (when (stmt-ir:if-else-block st)
                      (setf names (nconc names (collect (stmt-ir:if-else-block st))))))
                   (stmt-ir:derivative-request-statement
                    (let* ((target (stmt-ir:dr-target-var st))
                           (base   (stmt-ir:dr-base-var st))
                           (dname  (stmt-ir:make-derivative-name target base)))
                      (push (symbol-name dname) names)))))
               names))
           (strip (blk drop-names)
             (let ((new-stmts '()))
               (dolist (st (stmt-ir:block-statements blk))
                 (typecase st
                   (stmt-ir:block-statement
                    (push (strip st drop-names) new-stmts))
                   (stmt-ir:if-statement
                    (let ((new-then (and (stmt-ir:if-then-block st)
                                         (strip (stmt-ir:if-then-block st) drop-names)))
                          (new-else (and (stmt-ir:if-else-block st)
                                         (strip (stmt-ir:if-else-block st) drop-names))))
                      (push (stmt-ir:make-if-stmt (stmt-ir:if-condition st) new-then new-else)
                            new-stmts)))
                   (stmt-ir:derivative-request-statement
                    ;; drop in energy-only mode
                    nil)
                   (stmt-ir:assignment-statement
                    (let* ((target (stmt-ir:stmt-target-name st))
                           (rhs    (stmt-ir:stmt-expression st))
                           (uses   (expr-ir:expr-free-vars rhs)))
                      (unless (or (member (symbol-name target) drop-names :test #'string=)
                                  (intersection (mapcar #'symbol-name uses)
                                                drop-names :test #'string=))
                        (push st new-stmts))))
                   (t
                    (push st new-stmts))))
               (stmt-ir:make-block-stmt (nreverse new-stmts)
                                        :label (stmt-ir:block-label blk)))))
    (let ((drop-names (collect block)))
      (strip block drop-names))))



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
                (append coord-load (list core-block))
                :label (stmt-ir:block-label core-block))
               core-block))
         ;; turn energy/grad/hess assignments into KernelGradientAcc/KernelDiagHessAcc/... macros
         (transformed
           (transform-eg-h-block
            block-with-coords
            layout
            coord-vars
            #'general-grad-name
            #'general-hess-name))
         ;; variables known to be defined before derivative/CSE work:
         ;; parameters and coord-load locals
         (initial-defined (nconc (mapcar (lambda (p) (expr-ir:ev (second p))) params)
                                 (mapcar #'expr-ir:ev coord-vars)))
         ;; sanity check ordering after optimization
         (_ (stmt-ir:check-def-before-use-in-block transformed
                                                   :already-defined initial-defined))
         ;; ensure assignments are ordered so definitions precede uses
         (ordered transformed) ;;;(stmt-ir:reorder-block-def-before-use transformed))
         (locals (infer-kernel-locals ordered params coord-vars)))
    (stmt-ir:make-c-function
     (kernel-name kernel)
     ordered
     :return-type (kernel-return-type kernel)
     :parameters  params
     :locals      locals
     :return-expr (kernel-return-expr kernel))))

;; ------------------------------------------------------------
;; Finite-difference wrappers (energy/gradient/hessian)
;; ------------------------------------------------------------

(defun %fd-param-name (param)
  (string-downcase (symbol-name (second param))))

(defun %fd-param-type (param)
  (string-downcase (princ-to-string (first param))))

(defun %fd-param-list-string (params)
  (format nil "~{~a ~a~^, ~}"
          (loop for p in params
                append (list (%fd-param-type p)
                             (%fd-param-name p)))))

(defun %fd-call-args (params &key energy-accum force hessian dvec hdvec)
  "Return a list of argument strings matching PARAMS, with optional overrides."
  (mapcar (lambda (p)
            (let* ((nm (%fd-param-name p)))
              (cond
                ((and energy-accum (string= nm "energy_accumulate")) energy-accum)
                ((and force (string= nm "force")) force)
                ((and hessian (string= nm "hessian")) hessian)
                ((and dvec (string= nm "dvec")) dvec)
                ((and hdvec (string= nm "hdvec")) hdvec)
                (t nm))))
          params))

(defun %fd-coord-specs (kernel)
  "Return plist entries (:ibase :offset) per coord-var using LAYOUT."
  (let ((layout (kernel-layout kernel)))
    (mapcar (lambda (cv)
              (multiple-value-bind (ibase off)
                  (coord-name->ibase+offset (string-upcase (symbol-name cv)) layout)
                (list :ibase (string-downcase (symbol-name ibase))
                      :offset off)))
            (kernel-coord-vars kernel))))

(defun fd-wrapper-c-source (kernel)
  "Return finite-difference wrapper C sources per translation unit:
   - Energy TU: <group>_energy_fd
   - Gradient TU: <group>_gradient_fd
   - Hessian TU: <group>_hessian_fd"
  (when (kernel-compute-energy-p kernel)
    (let* ((params (kernel-params kernel))
           (param-list (%fd-param-list-string params))
           (coord-specs (%fd-coord-specs kernel))
           (ret-type   (or (kernel-return-type kernel) "void"))
           (ret-type-str (if (stringp ret-type) ret-type (princ-to-string ret-type)))
           (return-expr (kernel-return-expr kernel))
           (energy-name (format nil "~a_energy" (string-downcase (kernel-group kernel))))
           (group-name (string-downcase (kernel-group kernel)))
           (energy-fd-name (format nil "~a_energy_fd" group-name))
           (grad-name  (format nil "~a_gradient_fd" group-name))
           (hess-name  (format nil "~a_hessian_fd" group-name))
           (call-args-base (%fd-call-args params
                                          :energy-accum "&e0"
                                          :force "0" :hessian "0" :dvec "0" :hdvec "0"))
           (call-args-base-str (format nil "~{~a~^, ~}" call-args-base))
           (call-args-plus (lambda (var)
                             (%fd-call-args params
                                            :energy-accum (format nil "&~a" var)
                                            :force "0" :hessian "0" :dvec "0" :hdvec "0")))
           (hval-denom "(h*h)")
           (ret-void? (string= (string-downcase ret-type-str) "void")))
      (cond
        ;; Energy TU: emit only energy_fd
        ((and (not (kernel-compute-grad-p kernel))
              (not (kernel-compute-hess-p kernel)))
         (with-output-to-string (s)
           (let ((call-args (format nil "~{~a~^, ~}" (%fd-call-args params))))
             (format s "~a ~a(~a)~%{~%" ret-type-str energy-fd-name param-list)
             (cond
               (return-expr
                (format s "  ~a(~a);~%~a~%" energy-name call-args return-expr))
               (ret-void?
                (format s "  ~a(~a);~%" energy-name call-args))
               (t
                (format s "  return ~a(~a);~%" energy-name call-args)))
             (format s "}~%"))))
        ;; Gradient TU: emit only gradient_fd
        ((and (kernel-compute-grad-p kernel)
              (not (kernel-compute-hess-p kernel)))
         (with-output-to-string (s)
           (format s "extern ~a ~a(~a);~%~%" ret-type-str energy-name param-list)
           (format s "~a ~a(~a)~%{~%" ret-type-str grad-name param-list)
           (format s "  const double h = 1.0e-5;~%  const double inv2h = 1.0/(2.0*h);~%  double e0 = 0.0;~%  ~a(~a);~%  if (energy_accumulate) { *energy_accumulate += e0; }~%"
                   energy-name call-args-base-str)
           (dolist (cs coord-specs)
             (let* ((idx (format nil "~a + ~d" (getf cs :ibase) (getf cs :offset)))
                    (ibase (getf cs :ibase))
                    (off   (getf cs :offset)))
               (format s
                       "  {~%    double saved = position[~a];~%    double e_plus = 0.0;~%    double e_minus = 0.0;~%    position[~a] = saved + h;~%    ~a(~a);~%    position[~a] = saved - h;~%    ~a(~a);~%    position[~a] = saved;~%    double d = (e_plus - e_minus) * inv2h;~%    KernelGradientAcc(~a, ~d, d);~%  }~%"
                       idx idx
                       energy-name (format nil "~{~a~^, ~}" (funcall call-args-plus "e_plus"))
                       idx
                       energy-name (format nil "~{~a~^, ~}" (funcall call-args-plus "e_minus"))
                       idx
                       ibase off)))
           (cond
             (return-expr
              (format s "~a~%}~%" return-expr))
             (ret-void?
              (format s "}~%"))
             (t
              (format s "  return 0;~%}~%")))))
        ;; Hessian TU: emit only hessian_fd
        ((kernel-compute-hess-p kernel)
         (with-output-to-string (s)
           (format s "extern ~a ~a(~a);~%~%" ret-type-str energy-name param-list)
           (format s "~a ~a(~a)~%{~%" ret-type-str hess-name param-list)
           (format s "  const double h = 1.0e-5;~%  const double inv2h = 1.0/(2.0*h);~%  const double invh2 = 1.0/(~a);~%  double e0 = 0.0;~%  ~a(~a);~%  if (energy_accumulate) { *energy_accumulate += e0; }~%"
                   hval-denom energy-name call-args-base-str)
           (dolist (cs coord-specs)
             (let* ((idx (format nil "~a + ~d" (getf cs :ibase) (getf cs :offset)))
                    (ibase (getf cs :ibase))
                    (off   (getf cs :offset)))
               (format s
                       "  {~%    double saved = position[~a];~%    double e_plus = 0.0;~%    double e_minus = 0.0;~%    position[~a] = saved + h;~%    ~a(~a);~%    position[~a] = saved - h;~%    ~a(~a);~%    position[~a] = saved;~%    double d = (e_plus - e_minus) * inv2h;~%    KernelGradientAcc(~a, ~d, d);~%  }~%"
                       idx idx
                       energy-name (format nil "~{~a~^, ~}" (funcall call-args-plus "e_plus"))
                       idx
                       energy-name (format nil "~{~a~^, ~}" (funcall call-args-plus "e_minus"))
                       idx
                       ibase off)))
           (dolist (cs coord-specs)
             (let* ((idx (format nil "~a + ~d" (getf cs :ibase) (getf cs :offset)))
                    (ibase (getf cs :ibase))
                    (off   (getf cs :offset)))
               (format s
                       "  {~%    double saved = position[~a];~%    double e_plus = 0.0;~%    double e_minus = 0.0;~%    position[~a] = saved + h;~%    ~a(~a);~%    position[~a] = saved - h;~%    ~a(~a);~%    position[~a] = saved;~%    double hval = (e_plus + e_minus - (2.0*e0)) * invh2;~%    KernelDiagHessAcc(~a, ~d, ~a, ~d, hval);~%  }~%"
                       idx idx
                       energy-name (format nil "~{~a~^, ~}" (funcall call-args-plus "e_plus"))
                       idx
                       energy-name (format nil "~{~a~^, ~}" (funcall call-args-plus "e_minus"))
                       idx
                       ibase off ibase off)))
           (loop for i from 0 below (length coord-specs) do
                 (loop for j from 0 below i do
                       (let* ((csi (nth i coord-specs))
                              (csj (nth j coord-specs))
                              (idx-i (format nil "~a + ~d" (getf csi :ibase) (getf csi :offset)))
                              (idx-j (format nil "~a + ~d" (getf csj :ibase) (getf csj :offset)))
                              (ibase-i (getf csi :ibase))
                              (off-i   (getf csi :offset))
                              (ibase-j (getf csj :ibase))
                              (off-j   (getf csj :offset)))
                         (format s
                                 "  {~%    double saved_i = position[~a];~%    double saved_j = position[~a];~%    double e_pp = 0.0;~%    double e_pm = 0.0;~%    double e_mp = 0.0;~%    double e_mm = 0.0;~%    position[~a] = saved_i + h; position[~a] = saved_j + h;~%    ~a(~a);~%    position[~a] = saved_j - h;~%    ~a(~a);~%    position[~a] = saved_i - h; position[~a] = saved_j + h;~%    ~a(~a);~%    position[~a] = saved_j - h;~%    ~a(~a);~%    position[~a] = saved_i; position[~a] = saved_j;~%    double hval = (e_pp - e_pm - e_mp + e_mm) * (0.25*invh2);~%    KernelOffDiagHessAcc(~a, ~d, ~a, ~d, hval);~%  }~%"
                                 idx-i idx-j
                                 idx-i idx-j energy-name (format nil "~{~a~^, ~}" (funcall call-args-plus "e_pp"))
                                 idx-j energy-name (format nil "~{~a~^, ~}" (funcall call-args-plus "e_pm"))
                                 idx-i idx-j energy-name (format nil "~{~a~^, ~}" (funcall call-args-plus "e_mp"))
                                 idx-j energy-name (format nil "~{~a~^, ~}" (funcall call-args-plus "e_mm"))
                                 idx-i idx-j
                                 ibase-i off-i ibase-j off-j))))
           (cond
             (return-expr
              (format s "~a~%}~%" return-expr))
             (ret-void?
              (format s "}~%"))
             (t
              (format s "  return 0;~%}~%")))))))))

(defun write-c-code (kernel pathname)
  (ensure-directories-exist pathname)
  (with-open-file (fout pathname :direction :output :if-exists :supersede)
    (let* ((fun (compile-kernel-to-c-function kernel))
           (core-src (stmt-ir:c-function->c-source-string fun))
           (fd-src  (fd-wrapper-c-source kernel)))
      (write-line core-src fout)
      (when fd-src
        (write-line fd-src fout)))))



;;; ------------------------------------------------------------
;;; Top-level DSL: DEFKERNEL
;;; ------------------------------------------------------------


(defclass kernel-ir ()
  ((group :accessor kernel-group :initarg :group)
   (name :accessor kernel-name :initarg :name)
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
    :initform nil)
   ;; C return type and optional auto-return expression
   (return-type
    :accessor kernel-return-type
    :initarg :return-type
    :initform "void")
   (return-expr
    :accessor kernel-return-expr
    :initarg :return-expr
    :initform nil)))



(defun make-kernel-ir (&rest initargs)
  "Convenience constructor mirroring the old DEFSTRUCT MAKE-KERNEL-IR."
  (apply #'make-instance 'kernel-ir initargs))

;;; ------------------------------------------------------------
;;; Top-level make-kernel-from-block
;;;    Implements what DEFKERNEL does
;;; ------------------------------------------------------------


(defun binding-params (params coord-vars)
  (declare (optimize (debug 3)))
  (let ((res (loop for param in params
                   for svar = (symbol-name (second param))
                   for var = (expr-ir:ev svar)
                   until (string= svar "I3X1" :end1 (min (length svar) (length "I3X1")))
                   collect var)))
    (append res coord-vars)))


(defun make-kernel-from-block
    (&key group name pipeline layout coord-vars coord-load-stmts base-block params
       compute-mode compute-energy compute-grad compute-hess derivatives
       extra-equivalence-rules extra-optimization-rules
       post-eg-h-pipeline
       (return-type "void")
       (return-expr nil))
  (declare (optimize (debug 3)))
  (labels ((the-name (nm)
             (format nil "~a ~a" name nm)))
    (format t "[KERNEL ~a] 1. user body provided -> BASE-BLOCK~%" name)
    (let* ((energy-var        (expr-ir:ev 'energy))
           (manual-deriv-spec (normalize-derivatives-spec derivatives))
           (compute-mode      (or compute-mode :energy))
           (binding-params (binding-params params coord-vars))
           (base-block-filtered (filter-block-for-mode base-block compute-mode))
           (base-block*       (progn
                                (format t "[KERNEL ~a] 2. expand-derivative-requests: user base-block -> base-block*~%" name)
                                (if (or compute-grad compute-hess)
                                    (expand-derivative-requests base-block-filtered)
                                    (remove-derivative-requests base-block-filtered))))
           (pass-counter      (stmt-ir:make-pass-id-counter))
           (post-eg-h-pipeline* (or post-eg-h-pipeline *post-eg-h-pipeline*)))
      (expr-ir:with-kernel-rewrite-rules
          (extra-equivalence-rules extra-optimization-rules)
        (when manual-deriv-spec
          (check-intermediate-geometry! base-block* coord-vars manual-deriv-spec)
          (auto-fill-intermediate-geometry-from-ad
           base-block* coord-vars manual-deriv-spec
           :compute-second-derivs t
           :reuse-assignments    t))
        (format t "[KERNEL ~a] 3. start building core E/G/H assignments~%" name)
        ;; Build E/G/H in phases with the walker injectors:
        ;;   1) insert gradients right after energy assignments (path-sensitive)
        ;;   2) optimize E+G
        ;;   3) insert Hessians right after energy assignments (path-sensitive)
        ;;   4) optimize E+G+H
        (let* ((current-stmts (copy-list (stmt-ir:block-statements base-block*)))
               (current-block (stmt-ir:make-block-stmt current-stmts :label (stmt-ir:block-label base-block*)))
               ;; Track the most recent labeled block explicitly.
               (current-label (stmt-ir:block-label current-block))
               ;; Initialize hess-block binding for later use.
               (hess-block nil))
          (when compute-grad
            (setf current-block
                  (inject-gradients-after-energy
                   current-block coord-vars energy-var #'general-grad-name))
            (setf current-stmts (copy-list (stmt-ir:block-statements current-block))))
          (when pipeline
            (let ((grad-block (stmt-ir:make-block-stmt current-stmts :label current-label)))
              (multiple-value-bind (grad-opt results total-before total-after)
                  (stmt-ir:run-optimization-pipeline
                   pass-counter pipeline grad-block binding-params
                   :name (the-name "e-grad")
                   :log-stream *trace-output*)
                (declare (ignore results total-before total-after))
                (setf current-stmts
                      (copy-list (stmt-ir:block-statements grad-opt)))
                (setf current-label (stmt-ir:block-label grad-opt))
                (setf current-block grad-opt))))
          (when compute-hess
            (let* ((local-hess-block (stmt-ir:make-block-stmt current-stmts :label current-label))
                   (with-h (inject-hessians-after-energy
                            local-hess-block coord-vars energy-var #'general-hess-name
                            manual-deriv-spec)))
              (setf current-stmts (copy-list (stmt-ir:block-statements with-h)))
              (setf current-label (stmt-ir:block-label with-h))
              (setf hess-block with-h)))
          (when pipeline
            (let ((local-hess-block (stmt-ir:make-block-stmt current-stmts :label current-label)))
              (multiple-value-bind (hess-opt results total-before total-after)
                  (stmt-ir:run-optimization-pipeline
                   pass-counter pipeline local-hess-block binding-params
                   :name (the-name "e-g-hess")
                   :log-stream *trace-output*)
                (declare (ignore results total-before total-after))
                (setf current-stmts
                      (copy-list (stmt-ir:block-statements hess-opt)))
                (setf current-label (stmt-ir:block-label hess-opt))
                (setf hess-block hess-opt))))
          (let* ((core-block
                   (let ((blk (stmt-ir:make-block-stmt current-stmts :label current-label)))
                     (stmt-ir:check-def-before-use-in-block blk :errorp t)
                     blk))
                 (block-with-coords
                   (if coord-load-stmts
                       (stmt-ir:make-block-stmt
                        (append coord-load-stmts (list core-block))
                        :label (stmt-ir:block-label core-block))
                       core-block))
                 (eg-h-block
                   (transform-eg-h-block
                    block-with-coords layout coord-vars
                    #'general-grad-name #'general-hess-name))
                 (eg-h-block*
                   (if post-eg-h-pipeline*
                       (multiple-value-bind (post-block results total-before total-after)
                           (stmt-ir:run-optimization-pipeline
                            pass-counter post-eg-h-pipeline* eg-h-block binding-params
                            :name (the-name "post-eg-h")
                            :log-stream *trace-output*)
                         (declare (ignore results total-before total-after))
                         post-block)
                       eg-h-block)))
            (make-kernel-ir
             :group group
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
             :extra-optimization-rules extra-optimization-rules
             :return-type return-type
             :return-expr return-expr)))))))


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
           (want-hess      (eq compute-mode :hessian))
           (return-type    (or (clause-value :return-type) "void"))
           (return-expr    (clause-value :return-expr))
           )
      `(push
         (let* ((layout (make-kernel-layout
                         :atom->ibase ',atom->ibase
                         :axis->offset ',axis->offset))
                (coord-vars (vars ,@coord-names))
                (coord-load-stmts ,coord-load)
                (base-block ,body-form))
           (make-kernel-from-block
            :group ,c-function-name
            :name ',(format nil "~a_~a" c-function-name (string-downcase compute-mode))
            :pipeline ,pipeline
            :layout layout
            :coord-vars coord-vars
            :coord-load-stmts coord-load-stmts
            :base-block base-block
            :params ',params
            :compute-energy t
            :compute-mode ',compute-mode
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


(defmacro with-trace-output (filename &body body)
  `(with-open-file (*trace-output* filename :direction :output :if-exists :supersede)
     ,@body))
