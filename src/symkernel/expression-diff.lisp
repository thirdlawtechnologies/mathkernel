;;;; expression-diff.lisp
;;;; Symbolic differentiation on expression IR.

(in-package :expr-ir)

;;; ----------------------------------------------------------------------
;;; Derivative environment protocol
;;; ----------------------------------------------------------------------
;;; DERIV-ENV is a hash-table mapping variable symbols to their derivative
;;; expressions with respect to a chosen base variable.

(in-package :expr-ir)

;;; ----------------------------------------------------------------------
;;; Derivative environment keyed by variable name (string)
;;; ----------------------------------------------------------------------

(defun var-key (sym)
  "Canonical key for a variable symbol in the derivative environment."
  (string-upcase (symbol-name sym)))

(defun make-deriv-env ()
  "Create an empty derivative environment (hash-table).
Keys are variable-name strings (UPCASE). Values are expression IR nodes."
  (make-hash-table :test #'equal))

(defun lookup-var-derivative (var base-var deriv-env)
  "Return d(var)/d(base-var) as an expression IR node.

Order of precedence:
  1. If VAR has an explicit entry in DERIV-ENV (keyed by name), return that.
  2. Else if VAR and BASE-VAR have the same name, return 1.
  3. Else return 0."
  (let* ((vkey  (var-key var))
         (bkey  (var-key base-var)))
    (multiple-value-bind (val presentp)
        (gethash vkey deriv-env)
      (cond
        (presentp
         val)
        ((string= vkey bkey)
         (make-expr-const 1))
        (t
         (make-expr-const 0))))))


;;; ----------------------------------------------------------------------
;;; Helper: derivative of unary function-call
;;; ----------------------------------------------------------------------

(defun %differentiate-unary-funcall (f arg d-arg)
  "Return derivative for f(arg) with respect to base variable,
given d-arg = d(arg)/d(base). Uses standard calculus rules for common
functions; unknown functions are treated as black boxes (derivative 0)."
  (ecase f
    ;; sin(x) -> cos(x)*x'
    (sin (make-expr-mul (list (make-expr-funcall 'cos (list arg))
                              d-arg)))
    ;; cos(x) -> -sin(x)*x'
    (cos (make-expr-mul (list (make-expr-neg
                               (make-expr-funcall 'sin (list arg)))
                              d-arg)))
    ;; exp(x) -> exp(x)*x'
    (exp (make-expr-mul (list (make-expr-funcall 'exp (list arg))
                              d-arg)))
    ;; log(x) -> x'/x
    (log (make-expr-mul
          (list d-arg
                (make-expr-pow arg (make-expr-const -1)))))
    ;; sqrt(x) -> (1/(2*sqrt(x)))*x'
    (sqrt (make-expr-mul
           (list d-arg
                 (make-expr-mul
                  (list (make-expr-const 1/2)
                        (make-expr-pow arg (make-expr-const -1/2)))))))))

;;; ----------------------------------------------------------------------
;;; Core differentiation
;;; ----------------------------------------------------------------------

(in-package :expr-ir)

(defun differentiate-expr (expr base-var &optional (deriv-env (make-deriv-env)))
  "Return d(EXPR)/d(BASE-VAR) as a new expression IR node.

DERIV-ENV is a hash-table mapping intermediate variable names to their
derivatives with respect to BASE-VAR. If a variable has an entry in
DERIV-ENV we use that (chain rule). If not, we treat BASE-VAR as the
independent variable: d(BASE-VAR)/d(BASE-VAR) = 1, others 0."
  (labels
      ((d (e)
         (etypecase e
           ;; ----------------------------------------------------------------
           ;; Constants: 0
           ;; ----------------------------------------------------------------
           (constant-expression
            (make-expr-const 0))

           ;; ----------------------------------------------------------------
           ;; Variables: env lookup or 1/0
           ;; ----------------------------------------------------------------
           (variable-expression
            (lookup-var-derivative (variable-name e) base-var deriv-env))

           ;; ----------------------------------------------------------------
           ;; Addition: sum of derivatives
           ;; ----------------------------------------------------------------
           (add-expression
            (make-expr-add (mapcar #'d (expression-arguments e))))

           ;; ----------------------------------------------------------------
           ;; Multiplication: n-ary product rule
           ;; ----------------------------------------------------------------
           (multiply-expression
            (let* ((args (expression-arguments e))
                   (n    (length args)))
              (cond
                ((= n 0)
                 (make-expr-const 0))
                ((= n 1)
                 (d (first args)))
                (t
                 ;; d(prod_i a_i) = Σ_j (a'_j * Π_{i≠j} a_i)
                 (let ((terms '()))
                   (dotimes (j n)
                     (let* ((a-j    (nth j args))
                            (da-j   (d a-j))
                            (others (loop for i from 0 below n
                                          unless (= i j)
                                            collect (nth i args))))
                       (push (make-expr-mul (cons da-j others)) terms)))
                   (make-expr-add (nreverse terms)))))))

           ;; ----------------------------------------------------------------
           ;; Power: special case integer exponent, otherwise general rule
           ;; ----------------------------------------------------------------
           (power-expression
            (let* ((base (power-base-expression e))
                   (expo (power-exponent-expression e))
                   (db   (d base))
                   (de   (d expo)))
              (cond
                ;; Constant integer exponent: x^n -> n*x^(n-1)*x'
                ((and (typep expo 'constant-expression)
                      (integerp (expression-value expo)))
                 (let* ((n      (expression-value expo))
                        (n-expr  (make-expr-const n))
                        (n-1-expr (make-expr-const (1- n))))
                   (make-expr-mul
                    (list n-expr
                          (make-expr-pow base n-1-expr)
                          db))))
                ;; General case: (f(x)^g(x))' = f^g*(g'*log f + g*f'/f)
                (t
                 (let* ((f^g   e)
                        (term1 (make-expr-mul
                                (list de
                                      (make-expr-funcall 'log (list base)))))
                        (term2 (make-expr-mul
                                (list expo
                                      db
                                      (make-expr-pow base (make-expr-const -1)))))
                        (inner (make-expr-add (list term1 term2))))
                   (make-expr-mul (list f^g inner)))))))

           ;; ----------------------------------------------------------------
           ;; Negation: -(f) -> -f'
           ;; ----------------------------------------------------------------
           (negate-expression
            ;; Use the accessor only here, on a guaranteed negate-expression
            (let ((arg (negate-argument-expression e)))
              (make-expr-neg (d arg))))

           ;; ----------------------------------------------------------------
           ;; Function calls
           ;; ----------------------------------------------------------------
           (function-call-expression
            (let* ((fname (function-call-name e))
                   (args  (function-call-arguments e)))
              (cond
                ;; unary built-ins with special rules
                ((and (= (length args) 1)
                      (member fname '(sin cos exp log sqrt)))
                 (%differentiate-unary-funcall fname
                                               (first args)
                                               (d (first args))))
                ;; unknown / multi-arg: treat as black box (derivative 0)
                (t
                 (make-expr-const 0)))))

           ;; ----------------------------------------------------------------
           ;; Comparisons and logical expressions:
           ;; only for conditions, not differentiated.
           ;; ----------------------------------------------------------------
           (comparison-expression
            (make-expr-const 0))
           (logical-nary-expression
            (make-expr-const 0))
           (logical-not-expression
            (make-expr-const 0)))))
    (d expr)))
