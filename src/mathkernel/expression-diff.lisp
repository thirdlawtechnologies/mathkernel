;;;; ----------------------------------------------------------------------
;;;; expression-diff.lisp
;;;; ----------------------------------------------------------------------

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

(defclass deriv-env (stmt-ir:walk-context)
  ((table :initarg :table
          :accessor deriv-env-table
          :initform (make-hash-table :test #'equal)
          :documentation "Derivative environment hash-table keyed by variable name strings."))
  (:documentation "Context wrapper around a derivative environment hash-table."))

(defun make-deriv-env ()
  "Create an empty derivative environment wrapper."
  (make-instance 'deriv-env))

(defun copy-deriv-env-table (table)
  "Return a shallow copy of TABLE (a hash-table)."
  (let ((new (make-hash-table :test #'equal)))
    (maphash (lambda (k v) (setf (gethash k new) v)) table)
    new))

(defun copy-deriv-env (env)
  "Return a shallow copy of ENV (a DERIV-ENV)."
  (make-instance 'deriv-env
                 :table (copy-deriv-env-table (deriv-env-table env))))

(defmethod stmt-ir:clone-context ((operation t) (env deriv-env))
  "Clone DERIV-ENV for branching."
  (make-instance 'deriv-env
                 :table (copy-deriv-env-table (deriv-env-table env))))


(defun set-var-derivative (var deriv-expr deriv-env)
  "Record d(VAR)/d(base-var) = DERIV-EXPR in DERIV-ENV.
Keys are the canonical variable-name strings (see VAR-KEY)."
  (let ((key (if (symbolp var) (var-key var) var)))
    (setf (gethash key (deriv-env-table deriv-env)) deriv-expr)))

(defun lookup-var-derivative (var base-var deriv-env)
  "Return d(var)/d(base-var) as an expression IR node.

Order of precedence:
  1. If VAR has an explicit entry in DERIV-ENV (keyed by name), return that.
  2. Else if VAR and BASE-VAR have the same name, return 1.
  3. Else return 0."
  (let* ((vkey  (var-key var))
         (bkey  (var-key base-var)))
    (multiple-value-bind (val presentp)
        (gethash vkey (deriv-env-table deriv-env))
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

(defun %differentiate-binary-funcall (f arg1 d-arg1 arg2 d-arg2)
  "Return derivative for f(arg1,arg2) with respect to base variable,
given d-arg1 = d(arg1)/d(base) and d-arg2 = d(arg2)/d(base)."
  (ecase f
    (:atan2
     ;; We treat f(y,x) = atan2(y,x), like C's atan2.
     (let* ((y   arg1)
            (dy  d-arg1)
            (x   arg2)
            (dx  d-arg2)
            (two (make-expr-const 2))
            ;; x^2 + y^2
            (x2  (make-expr-pow x two))
            (y2  (make-expr-pow y two))
            (den (make-expr-add (list x2 y2)))
            ;; 1 / (x^2 + y^2)
            (den-inv (make-expr-pow den (make-expr-const -1)))
            ;; partials: ∂z/∂y = x/(x^2+y^2), ∂z/∂x = -y/(x^2+y^2)
            (df-dy (make-expr-mul (list x den-inv)))
            (df-dx (make-expr-mul (list (make-expr-neg y) den-inv)))
            ;; dz/dv = df/dy*dy/dv + df/dx*dx/dv
            (term1 (make-expr-mul (list df-dy dy)))
            (term2 (make-expr-mul (list df-dx dx))))
       (make-expr-add (list term1 term2))))))


(defun %differentiate-unary-funcall (f arg d-arg)
  "Return derivative for f(arg) with respect to base variable,
given d-arg = d(arg)/d(base). Uses standard calculus rules for common
functions; unknown functions are treated as black boxes (derivative 0)."
  (ecase f
    ;; sin(x) -> cos(x)*x'
    (:sin (make-expr-mul (list (make-expr-funcall :cos (list arg))
                              d-arg)))
    ;; cos(x) -> -sin(x)*x'
    (:cos (make-expr-mul (list (make-expr-neg
                               (make-expr-funcall :sin (list arg)))
                              d-arg)))
    ;; exp(x) -> exp(x)*x'
    (:exp (make-expr-mul (list (make-expr-funcall :exp (list arg))
                              d-arg)))
    ;; log(x) -> x'/x
    (:log (make-expr-mul
          (list d-arg
                (make-expr-pow arg (make-expr-const -1)))))
    ;; sqrt(x) -> (1/(2*sqrt(x)))*x'
    (:sqrt (make-expr-mul
           (list d-arg
                 (make-expr-mul
                  (list (make-expr-const 1/2)
                        (make-expr-pow arg (make-expr-const -1/2)))))))
    (:acos
     (progn
       (let* ((u     arg)
              (du    d-arg) ;; chain rule
              (one   (make-constant 1))
              (two   (make-constant 2))
              (minus-half (make-constant -1/2))
              ;; u^2
              (u2    (expr-ir:make-expr-pow u two))
              ;; 1 - u^2  =  1 + (-(u^2))
              (inner (expr-ir:make-expr-add
                      (list one (expr-ir:make-expr-neg u2))))
              ;; (1 - u^2)^(-1/2)
              (den   (expr-ir:make-expr-pow inner minus-half))
              ;; du * (1 - u^2)^(-1/2)
              (core  (expr-ir:make-expr-mul (list du den))))
         ;; - du * (1 - u^2)^(-1/2)
         (expr-ir:make-expr-neg core))))
     ))

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
  (declare (optimize (debug 3)))
  (labels
      ((d (e)
         (let ((dr (etypecase e
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
                                                (make-expr-funcall :log (list base)))))
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
                                (member fname *function-names*))
                           (%differentiate-unary-funcall fname
                                                         (first args)
                                                         (d (first args))))
                          ;; special-case binary atan2(y, x)
                          ((and (= (length args) 2)
                                (member fname *function-names*))
                           (%differentiate-binary-funcall fname
                                                          (first args)  (d (first args))
                                                          (second args) (d (second args))))

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
           dr
           )))
    (let ((dexpr (d expr)))
      dexpr)))


;;; ----------------------------------------------------------------------
;;; Collapse sums
;;; ----------------------------------------------------------------------



(defun expr-sexpr-size (sexpr)
  "Crude size metric (number of nodes) for a sexpr tree, local to expr-ir."
  (if (atom sexpr)
      1
      (1+ (reduce #'+ (mapcar #'expr-sexpr-size sexpr)))))

(defun term->coeff-and-factors (sexpr)
  "Decompose a term S in a sum into (coeff, factors).
COEFF is a numeric scalar (possibly 1), FACTORS is a list of non-numeric
factor sexprs."
  (cond
    ;; Product term: (* a b c ...)
    ((and (consp sexpr) (eq (car sexpr) '*))
     (let ((coeff 1)
           (factors '()))
       (dolist (arg (cdr sexpr))
         (if (numberp arg)
             (setf coeff (* coeff arg))
             (push arg factors)))
       (values coeff (nreverse factors))))
    ;; Pure numeric constant
    ((numberp sexpr)
     (values sexpr nil))
    ;; Anything else: treat as a single non-numeric factor
    (t
     (values 1 (list sexpr)))))

(defun counts-from-factors (factors)
  "Build a multiset (hash-table) of FACTORS keyed by EQUAL."
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (f factors ht)
      (incf (gethash f ht 0)))))

(defun multiset-intersection-sexpr (counts1 counts2)
  "Return the multiset intersection as a list of sexprs (with multiplicities).
Result is sorted by a simple printed representation for stability."
  (let ((result '()))
    (maphash
     (lambda (k v1)
       (let ((v2 (gethash k counts2 0)))
         (when (> (min v1 v2) 0)
           (dotimes (i (min v1 v2))
             (push k result)))))
     counts1)
    (sort result #'string<
          :key (lambda (sx)
                 (format nil "~S" sx)))))

(defun multiset-subset-p-sexpr (cand-counts prod-counts)
  "True if CAND-COUNTS ⊆ PROD-COUNTS (both are factor-count hash tables)."
  (let ((ok t))
    (maphash
     (lambda (k v)
       (unless (<= v (gethash k prod-counts 0))
         (setf ok nil)))
     cand-counts)
    ok))

(defun build-term-sexpr (coeff residual-factors)
  "Rebuild a term sexpr from COEFF and RESIDUAL-FACTORS."
  (cond
    ;; Only coefficient
    ((null residual-factors)
     (cond
       ((eql coeff 1) 1)
       (t coeff)))
    ;; Non-empty residual factors
    (t
     (let ((args (if (eql coeff 1)
                     residual-factors
                     (cons coeff residual-factors))))
       (if (null (cdr args))
           (car args)
           (cons '* args))))))

(defun factor-terms-once (terms min-uses min-factors min-size)
  "Given a list of TERMS (sexprs) being added together, attempt to factor
out a common multiplicative sub-product across them once.

If no beneficial factoring is found, return TERMS unchanged (EQ)."
  (let ((term-infos '()))
    ;; Build per-term info
    (dolist (tt terms)
      (multiple-value-bind (coeff factors)
          (term->coeff-and-factors tt)
        (let ((counts (counts-from-factors factors)))
          (push (list :term    tt
                      :coeff   coeff
                      :factors factors
                      :counts  counts)
                term-infos))))
    (setf term-infos (nreverse term-infos))
    (let* ((n (length term-infos)))
      (if (< n 2)
          ;; Not enough terms to factor
          terms
          (let ((candidates '()))
            ;; Collect candidate factor multisets from pairwise intersections.
            (let ((infos (coerce term-infos 'vector)))
              (loop for i from 0 below (1- n) do
                    (let* ((info-i (aref infos i))
                           (ci     (getf info-i :counts)))
                      (loop for j from (1+ i) below n do
                            (let* ((info-j (aref infos j))
                                   (cj     (getf info-j :counts))
                                   (common (multiset-intersection-sexpr ci cj)))
                              (when (>= (length common) min-factors)
                                (push common candidates)))))))
            (setf candidates (remove-duplicates candidates :test #'equal))
            (if (null candidates)
                ;; No candidate at all
                terms
                ;; Score candidates and pick the best
                (let ((best-cand  nil)
                      (best-score 0)
                      (best-uses  0))
                  (dolist (cand candidates)
                    (let* ((cand-counts (counts-from-factors cand))
                           (prod-sexpr  (cond
                                          ((null cand) 1)
                                          ((null (cdr cand)) (car cand))
                                          (t (cons '* cand))))
                           (size        (expr-sexpr-size prod-sexpr))
                           (uses        0))
                      (when (and (>= (length cand) min-factors)
                                 (>= size min-size))
                        ;; Count how many terms contain this candidate as a sub-multiset
                        (dolist (info term-infos)
                          (when (multiset-subset-p-sexpr cand-counts
                                                         (getf info :counts))
                            (incf uses)))
                        (when (>= uses min-uses)
                          (let ((score (* (- uses 1) size)))
                            (when (> score best-score)
                              (setf best-score score
                                    best-cand  cand
                                    best-uses  uses)))))))
                  ;; No beneficial candidate
                  (if (or (null best-cand)
                          (< best-uses min-uses)
                          (<= best-score 0))
                      terms
                      ;; Rebuild terms using BEST-CAND
                      (let ((cand-counts    (counts-from-factors best-cand))
                            (factored-inner '())
                            (unchanged      '()))
                        (dolist (info term-infos)
                          (let* ((term   (getf info :term))
                                 (coeff  (getf info :coeff))
                                 (counts (getf info :counts)))
                            (if (multiset-subset-p-sexpr cand-counts counts)
                                ;; Term contains the candidate: strip it and
                                ;; keep the residual part inside the inner sum.
                                (let ((residual '()))
                                  (maphash
                                   (lambda (k vterm)
                                     (let* ((vcand (gethash k cand-counts 0))
                                            (vres  (- vterm vcand)))
                                       (when (> vres 0)
                                         (dotimes (_ vres)
                                           (push k residual)))))
                                   counts)
                                  (setf residual (nreverse residual))
                                  (let ((inner-term (build-term-sexpr coeff residual)))
                                    (push inner-term factored-inner)))
                                ;; Term does not contain candidate: leave as-is.
                                (push term unchanged))))
                        (setf factored-inner (nreverse factored-inner))
                        (setf unchanged      (nreverse unchanged))
                        (let* ((factor-sexpr (cond
                                               ((null best-cand) 1)
                                               ((null (cdr best-cand)) (car best-cand))
                                               (t (cons '* best-cand))))
                               (inner-sum    (cond
                                               ((null factored-inner) 0)
                                               ((null (cdr factored-inner))
                                                (car factored-inner))
                                               (t (cons '+ factored-inner))))
                               (factored-term (if (and (numberp factor-sexpr)
                                                       (eql factor-sexpr 1))
                                                  inner-sum
                                                  (list '* factor-sexpr inner-sum)))
                               (new-terms (if unchanged
                                              (cons factored-term unchanged)
                                              (list factored-term))))
                          new-terms))))))))))



(defun factor-sum-sexpr (sexpr min-uses min-factors min-size)
  "Recursively factor sums-of-products in SEXPR."
  (if (consp sexpr)
      (case (car sexpr)
        ((+)
         ;; Recursively factor terms first, then try factoring this sum.
         (let* ((raw-terms (cdr sexpr))
                (subterms  (mapcar (lambda (tt)
                                     (factor-sum-sexpr tt min-uses min-factors min-size))
                                   raw-terms))
                (factored-terms (factor-terms-once subterms
                                                   min-uses
                                                   min-factors
                                                   min-size)))
           (cond
             ((null factored-terms) 0)
             ((null (cdr factored-terms)) (car factored-terms))
             (t (cons '+ factored-terms)))))
        ((*)
         ;; Just recurse into children
         (cons '* (mapcar (lambda (tt)
                            (factor-sum-sexpr tt min-uses min-factors min-size))
                          (cdr sexpr))))
        (t
         ;; Other forms: recurse structurally
         (cons (car sexpr)
               (mapcar (lambda (tt)
                         (factor-sum-sexpr tt min-uses min-factors min-size))
                       (cdr sexpr)))))
      ;; Atom
      sexpr))

(defun factor-sum-of-products (expr &key (min-uses 2) (min-factors 1) (min-size 2))
  "Return a new EXPR-IR expression where obvious common products across
sum terms have been factored out once (greedy heuristic):

  - For (+ t1 ... tn), each term is treated as coeff * product(factors).
  - Pairwise intersections of factor multisets propose common factors.
  - The best candidate (by (uses-1)*size) is factored as
      common * (sum of residual terms)."
  (let* ((sexpr     (expr->sexpr expr))
         (factored  (factor-sum-sexpr sexpr min-uses min-factors min-size)))
    (sexpr->expr-ir factored)))
