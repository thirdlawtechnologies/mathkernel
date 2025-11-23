
(in-package :stmt-ir)

;;; ------------------------------------------------------------
;;; Helpers
;;; ------------------------------------------------------------

(defun make-grad-name (var)
  (intern (format nil "D_E_D_~A" (symbol-name var))
          (symbol-package var)))

(defun make-hess-name (vi vj)
  (intern (format nil "D2_E_D_~A_D_~A"
                  (symbol-name vi) (symbol-name vj))
          (symbol-package vi)))



(defun %maybe-simplify (expr simplify-p)
  (if simplify-p
      (expr-ir:simplify-expr expr)
      expr))

;;; ------------------------------------------------------------
;;; Energy + gradient block builder
;;; ------------------------------------------------------------
;;; API:
;;;   (make-energy-grad-block
;;;     :energy-expr   <expr-ir node for E(x)>
;;;     :coord-vars    '(x1 y1 z1 x2 y2 z2)  ; symbols
;;;     :energy-target 'E                   ; symbol or NIL
;;;     :grad-target-fn (lambda (var) ...)  ; coord-var -> target symbol
;;;     :simplify      t)
;;;
;;; Returns a BLOCK-STATEMENT containing:
;;;   [optional] ENERGY-TARGET = E(x)
;;;   for each v in COORD-VARS:
;;;     (GRAD-TARGET-FN v) = dE/dv
;;;
;;; Targets are simple scalar variables at this stage. Mapping
;;; these to arrays (G[i], etc.) can be done later in codegen or
;;; a separate placement pass.

(defun make-energy-grad-block (&key energy-expr coord-vars
                                 energy-target
                                 (grad-target-fn #'make-grad-name)
                                 (simplify t))
  "Build a BLOCK-STATEMENT that computes energy and gradient
for ENERGY-EXPR with respect to COORD-VARS.

ENERGY-EXPR is an EXPR-IR node.
COORD-VARS is a list of symbols (e.g. (x1 y1 z1 x2 y2 z2)) naming
the base variables.

ENERGY-TARGET, if non-NIL, is a symbol naming the scalar variable that
will receive the energy value E(x).

GRAD-TARGET-FN is a function of one argument (a coord-var symbol)
that returns the symbol naming the scalar variable that will receive
the corresponding gradient component dE/d(var).

If SIMPLIFY is true (default), each expression is passed through
EXPR-IR:SIMPLIFY-EXPR before being wrapped in an assignment."
  (unless (and energy-expr coord-vars grad-target-fn)
    (error "make-energy-grad-block: ENERGY-EXPR, COORD-VARS and GRAD-TARGET-FN are required."))
  (let ((stmts '()))
    ;; Energy assignment, if requested
    (when energy-target
      (let ((e-expr (%maybe-simplify energy-expr simplify)))
        (push (make-assignment-stmt energy-target e-expr) stmts)))
    ;; Gradient components
    (dolist (v coord-vars)
      (let* ((dexpr (expr-ir:differentiate-expr energy-expr v))
             (dexpr-s (%maybe-simplify dexpr simplify))
             (target (funcall grad-target-fn v)))
        (push (make-assignment-stmt target dexpr-s) stmts)))
    (make-block-stmt (nreverse stmts))))

;;; ------------------------------------------------------------
;;; Energy + gradient + (upper-triangular) Hessian block builder
;;; ------------------------------------------------------------
;;; API:
;;;   (make-energy-grad-hess-block
;;;     :energy-expr    <expr-ir node for E(x)>
;;;     :coord-vars     '(x1 y1 z1 x2 y2 z2)
;;;     :energy-target  'E
;;;     :grad-target-fn (lambda (var) ...)
;;;     :hess-target-fn (lambda (var-i var-j) ...)
;;;     :simplify       t)
;;;
;;; Returns a BLOCK-STATEMENT containing:
;;;   [optional] ENERGY-TARGET = E(x)
;;;   for each v in COORD-VARS:
;;;       (GRAD-TARGET-FN v)      = dE/dv
;;;   for each pair i,j with i<=j:
;;;       (HESS-TARGET-FN vi vj)  = d²E/(dvi dvj)
;;;
;;; As above, targets are scalar variables; placement into arrays
;;; is a separate concern.

(defun make-energy-grad-hess-block (&key energy-expr coord-vars
                                      energy-target
                                      (grad-target-fn #'make-grad-name)
                                      (hess-target-fn #'make-hess-name)
                                      (simplify t))
  "Build a BLOCK-STATEMENT that computes energy, gradient, and
upper-triangular Hessian for ENERGY-EXPR with respect to COORD-VARS.

ENERGY-EXPR is an EXPR-IR node.
COORD-VARS is a list of symbols naming the base variables.

ENERGY-TARGET, if non-NIL, is a symbol naming the scalar variable that
will receive the energy value E(x).

GRAD-TARGET-FN is a function (var) -> symbol naming the scalar
variable for dE/d(var).

HESS-TARGET-FN is a function (var-i var-j) -> symbol naming the
scalar variable for d²E/(d var-i d var-j), where var-i and var-j are
symbols from COORD-VARS and the builder only calls it with i <= j.

If SIMPLIFY is true (default), expressions are passed through
EXPR-IR:SIMPLIFY-EXPR before being wrapped in assignments."
  (unless (and energy-expr coord-vars grad-target-fn hess-target-fn)
    (error "make-energy-grad-hess-block: ENERGY-EXPR, COORD-VARS, GRAD-TARGET-FN and HESS-TARGET-FN are required."))
  (let ((stmts '())
        ;; cache gradient expressions so Hessian can reuse them
        (grad-table (make-hash-table :test #'equal)))
    ;; Energy assignment
    (when energy-target
      (let ((e-expr (%maybe-simplify energy-expr simplify)))
        (push (make-assignment-stmt energy-target e-expr) stmts)))
    ;; Gradient assignments, cache exprs
    (dolist (v coord-vars)
      (let* ((dexpr (expr-ir:differentiate-expr energy-expr v))
             (dexpr-s (%maybe-simplify dexpr simplify))
             (g-target (funcall grad-target-fn v)))
        (setf (gethash v grad-table) dexpr-s)
        (push (make-assignment-stmt g-target dexpr-s) stmts)))
    ;; Hessian assignments for i <= j
    (loop
      for i from 0 below (length coord-vars)
      for vi = (nth i coord-vars) do
        (let ((gi (gethash vi grad-table)))
          (unless gi
            ;; Should not happen; defensive
            (setf gi (%maybe-simplify
                      (expr-ir:differentiate-expr energy-expr vi)
                      simplify)))
          (loop
            for j from i below (length coord-vars)
            for vj = (nth j coord-vars) do
              (let* ((hij (expr-ir:differentiate-expr gi vj))
                     (hij-s (%maybe-simplify hij simplify))
                     (h-target (funcall hess-target-fn vi vj)))
                (push (make-assignment-stmt h-target hij-s) stmts)))))
    (make-block-stmt (nreverse stmts))))


