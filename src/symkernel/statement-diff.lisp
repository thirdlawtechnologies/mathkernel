;;;; statement-diff.lisp
;;;; Differentiate sequences of assignment statements using expression-diff.

(in-package :stmt-ir)

(in-package :stmt-ir)

;;; ----------------------------------------------------------------------
;;; Build derivative environment over a block of assignments
;;; ----------------------------------------------------------------------

(defun build-derivative-env-for-block (statements base-var)
  "Given a list of STATEMENTS (typically assignment-statement objects)
and a base variable symbol BASE-VAR, build and return a derivative
environment (hash-table) mapping each intermediate variable name (string)
to its derivative expression d(var)/d(BASE-VAR)."
  (let ((env (expr-ir:make-deriv-env)))
    ;; Optionally seed env[base-var] = 1; lookup-var-derivative will also
    ;; handle this based on name, but this doesn't hurt:
    (setf (gethash (expr-ir:var-key base-var) env)
          (expr-ir:make-expr-const 1))
    (dolist (st statements env)
      (when (typep st 'assignment-statement)
        (let* ((name (stmt-target-name st))
               (rhs  (stmt-expression st))
               (drhs (expr-ir:differentiate-expr rhs base-var env))
               (key  (expr-ir:var-key name)))
          (setf (gethash key env) drhs))))))

;;; ----------------------------------------------------------------------
;;; Get derivative of a specific target variable in the block
;;; ----------------------------------------------------------------------

(defun differentiate-target-in-block (statements base-var target-var)
  "Compute d(TARGET-VAR)/d(BASE-VAR) given a sequence of STATEMENTS.

STATEMENTS is a list of assignment-statement objects that define
intermediate variables (including TARGET-VAR) in order. TARGET-VAR
and BASE-VAR are symbols naming variables in the expression IR.

Returns an expression IR node for the derivative, or NIL if TARGET-VAR
never appears as an assignment target in STATEMENTS."
  (let* ((env (build-derivative-env-for-block statements base-var))
         (key (expr-ir:var-key target-var)))
    (multiple-value-bind (d presentp)
        (gethash key env)
      (if presentp
          d
          nil))))

;;; ----------------------------------------------------------------------
;;; Generate derivative assignment statements for a block
;;; ----------------------------------------------------------------------

(defun make-derivative-name (var base-var)
  "Generate a symbol naming the derivative d(VAR)/d(BASE-VAR).
The naming convention here is: D<var>_D<base>, e.g. DFOO_DX."
  (intern (format nil "D~A_D~A"
                  (symbol-name var)
                  (symbol-name base-var))
          (symbol-package var)))

(defun make-derivative-assignments-for-block (statements base-var)
  "Given a list of STATEMENTS (assignment-statement objects) and BASE-VAR,
return a list of new assignment-statement objects that assign each
intermediate variable's derivative to a fresh derivative variable.

Example:
  foo = f(x)
  bar = g(foo)

yields (for base-var 'x):
  DFOO_DX = d(foo)/dx
  DBAR_DX = d(bar)/dx"
  (let ((env (expr-ir:make-deriv-env))
        (result '()))
    ;; seed base-var
    (setf (gethash (expr-ir:var-key base-var) env)
          (expr-ir:make-expr-const 1))
    (dolist (st statements (nreverse result))
      (when (typep st 'assignment-statement)
        (let* ((var-name (stmt-target-name st))
               (rhs      (stmt-expression st))
               (drhs     (expr-ir:differentiate-expr rhs base-var env))
               (dvar     (make-derivative-name var-name base-var))
               (dst      (make-assignment-stmt dvar drhs))
               (key      (expr-ir:var-key var-name)))
          ;; store derivative in env so later assignments see it
          (setf (gethash key env) drhs)
          (push dst result))))))


