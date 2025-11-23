(defpackage :energy-kernels
  (:use :cl :expr-ir :stmt-ir)
  (:export
   #:make-stretch-energy-kernel))

(in-package :energy-kernels)

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


(in-package :energy-kernels)

(defun expr-var-symbol (name-string)
  "Return the symbol used as the variable name when parsing NAME-STRING
as an expression. Ensures identity with variables in other parsed
expressions."
  (let ((expr (expr-ir:parse-expr name-string)))
    (etypecase expr
      (expr-ir:variable-expression
       (expr-ir:variable-name expr))
      (t
       (error "expr-var-symbol: ~S did not parse to a VARIABLE-EXPRESSION: ~S"
              name-string expr)))))
(defun vars (&rest vars)
  (mapcar #'expr-var-symbol vars))

(defun make-stretch-energy-kernel ()
  "Build a C-FUNCTION object for a simple stretch energy:
     E = kb * (r - r0)^2
   with r the distance between (x1,y1,z1) and (x2,y2,z2).

The body computes E, its gradient, and its upper-triangular Hessian
with respect to the coordinate variables (x1 y1 z1 x2 y2 z2)."
  (let* (;; energy expression E(x1,y1,z1,x2,y2,z2,kb,r0)
         (energy-expr
           (expr-ir:parse-expr
            "kb*(Sqrt((x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2) - r0)^2"))

         ;; Coordinate variables in the SAME package as ENERGY-EXPR vars
         (coord-vars (vars "x1" "y1" "z1" "x2" "y2" "z2"))

         ;; build block: E, grad, upper-triangular Hessian
         (body-block
           (stmt-ir:make-energy-grad-hess-block
            :energy-expr    energy-expr
            :coord-vars     coord-vars
            :energy-target  'E
            :grad-target-fn #'stretch-grad-name
            :hess-target-fn #'stretch-hess-name
            :simplify       t))

         ;; Very simple function signature for now
         (params
           '(("double" kb)
             ("double" r0)
             ("double" x1)
             ("double" y1)
             ("double" z1)
             ("double" x2)
             ("double" y2)
             ("double" z2)
             ("double *" E_out)
             ("double *" G_out)
             ("double *" H_out)))
         (locals nil))

    (stmt-ir:make-c-function
     'stretch_energy_kernel
     body-block
     :return-type "void"
     :parameters  params
     :locals      locals)))


