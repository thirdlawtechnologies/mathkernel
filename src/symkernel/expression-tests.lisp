;;;; expression-tests.lisp
;;;; Regression tests for expression IR, parsing, and printing.

(in-package :expr-ir.tests)

;;; ----------------------------------------------------------------------
;;; Tiny test harness
;;; ----------------------------------------------------------------------

(defun same-symbol-name-p (a b)
  (and (symbolp a)
       (symbolp b)
       (string= (symbol-name a)
                (symbol-name b))))

(in-package :expr-ir.tests)

(defun sexpr-alpha-equal (a b)
  "Structural equality of S-expressions, ignoring symbol packages."
  (labels ((recur (x y)
             (cond
               ;; both symbols -> compare by name
               ((and (symbolp x) (symbolp y))
                (string= (symbol-name x) (symbol-name y)))
               ;; both numbers -> numeric =
               ((and (numberp x) (numberp y))
                (= x y))
               ;; both conses -> recurse on car + cdr
               ((and (consp x) (consp y))
                (and (recur (car x) (car y))
                     (recur (cdr x) (cdr y))))
               ;; both NIL
               ((and (null x) (null y))
                t)
               ;; otherwise, mismatch
               (t
                nil))))
    (recur a b)))

(defun normalize-commutative (sexpr)
  "Normalize SEXPR so that arguments of commutative + and * are
canonically sorted. Used to ignore operand order in comparisons."
  (labels ((norm (x)
             (cond
               ((atom x) x)
               ((consp x)
                (let ((op   (car x))
                      (args (cdr x)))
                  (cond
                    ;; commutative operators: normalize and sort arguments
                    ((and (symbolp op)
                          (member (symbol-name op) '("+" "*")
                                  :test #'string=))
                     (let* ((nargs  (mapcar #'norm args))
                            (sorted (sort (copy-list nargs) #'string<
                                          :key (lambda (a)
                                                 (with-output-to-string (s)
                                                   (prin1 a s))))))
                       (cons op sorted)))
                    ;; noncommutative: just normalize children in order
                    (t
                     (cons op (mapcar #'norm args))))))
               (t x))))
    (norm sexpr)))

(defun expr-equal? (a b)
  "Compare two expression IR nodes modulo:
   - symbol package,
   - argument order under + and * (commutative operators)."
  (let* ((sa (expr->sexpr a))
         (sb (expr->sexpr b))
         (na (normalize-commutative sa))
         (nb (normalize-commutative sb)))
    (sexpr-alpha-equal na nb)))



(define-condition test-failure (error)
  ((name :initarg :name :reader test-failure-name)
   (detail :initarg :detail :reader test-failure-detail))
  (:report (lambda (c s)
             (format s "~&[TEST FAILED] ~A: ~A"
                     (test-failure-name c)
                     (test-failure-detail c)))))

(defun assert-true (condition name fmt &rest args)
  (unless condition
    (error 'test-failure
           :name name
           :detail (apply #'format nil fmt args))))

(defmacro deftest (name &body body)
  `(defun ,name ()
     (format t "~&[TEST] ~A~%" ',name)
     ,@body
     (format t "  OK~%")))

(defun run-expression-tests ()
  (let ((tests '(test-constants-and-variables
                 test-add-canonicalization
                 test-mul-canonicalization
                 test-pow-canonicalization
                 test-neg-canonicalization
                 test-funcall-basic
                 test-sexpr-arithmetic
                 test-sexpr-comparison-and-logic
                 test-infix-basic-arithmetic
                 test-parse-expr-dispatch
                 test-roundtrip-sexpr
                 test-roundtrip-infix
                 test-simplify-expr-idempotent
                 test-simplify-expr-after-diff
                 test-stretch-energy-variable-consistency
                 test-stretch-diff-x1
                 test-stretch-diff-kb
                 test-stretch-energy-grad-numeric
                 )))
    (dolist (fn tests)
      (handler-case
          (funcall fn)
        (test-failure (c)
          (format *error-output* "~&~A~%" c)
          (return-from run-expression-tests nil))))
    (format t "~&All expression tests passed.~%")
    t))

;;; ----------------------------------------------------------------------
;;; Helpers
;;; ----------------------------------------------------------------------

(defun sexpr->string (form)
  (with-output-to-string (s)
    (prin1 form s)))

;;; ----------------------------------------------------------------------
;;; Tests for basic constructors
;;; ----------------------------------------------------------------------

(deftest test-constants-and-variables
  (let* ((c (make-expr-const 3.5))
         (v (make-expr-var 'x)))
    (assert-true (typep c 'constant-expression)
                 'test-constants-and-variables
                 "c is not constant-expression")
    (assert-true (= (expression-value c) 3.5d0)
                 'test-constants-and-variables
                 "constant value mismatch: ~S" (expression-value c))
    (assert-true (typep v 'variable-expression)
                 'test-constants-and-variables
                 "v is not variable-expression")
    (assert-true (eq (variable-name v) 'x)
                 'test-constants-and-variables
                 "variable name mismatch: ~S" (variable-name v))))

;;; ----------------------------------------------------------------------
;;; Tests for add / mul canonicalization
;;; ----------------------------------------------------------------------

(deftest test-add-canonicalization
  ;; (+ 1 2 3) -> constant 6
  (let* ((a1 (make-expr-add (list (make-expr-const 1)
                                  (make-expr-const 2)
                                  (make-expr-const 3)))))
    (assert-true (typep a1 'constant-expression)
                 'test-add-canonicalization
                 "expected folded constant for sum")
    (assert-true (= (expression-value a1) 6)
                 'test-add-canonicalization
                 "sum constant value mismatch: ~S" (expression-value a1)))
  ;; (+ x y 0) -> (+ x y) (no explicit 0 term)
  (let* ((x (make-expr-var 'x))
         (y (make-expr-var 'y))
         (sum (make-expr-add (list x y (make-expr-const 0)))))
    (assert-true (typep sum 'add-expression)
                 'test-add-canonicalization
                 "expected add-expression")
    (let ((args (expression-arguments sum)))
      (assert-true (= (length args) 2)
                   'test-add-canonicalization
                   "expected 2 arguments in simplified sum, got ~D"
                   (length args))
      (assert-true (null (set-exclusive-or (mapcar #'expr->sexpr args)
                                     (list 'x 'y)
                                     :test #'equal))
                   'test-add-canonicalization
                   "sum arguments not {x,y}: ~S vs ~s"
                   (mapcar #'expr->sexpr args)
                   (list 'x 'y))))
  ;; (+ x (+ 1 y)) -> (+ 1 x y), flattened and sorted
  (let* ((x (make-expr-var 'x))
         (y (make-expr-var 'y))
         (inner (make-expr-add (list (make-expr-const 1) y)))
         (outer (make-expr-add (list x inner))))
    (assert-true (typep outer 'add-expression)
                 'test-add-canonicalization
                 "expected outer add-expression")
    (assert-true (equal (expr->sexpr outer)
                        '(+ 1 X Y))
                 'test-add-canonicalization
                 "unexpected canonical S-expression for add: ~S"
                 (expr->sexpr outer))))

(deftest test-mul-canonicalization
  ;; (* 2 3 4) -> constant 24
  (let* ((m1 (make-expr-mul (list (make-expr-const 2)
                                  (make-expr-const 3)
                                  (make-expr-const 4)))))
    (assert-true (typep m1 'constant-expression)
                 'test-mul-canonicalization
                 "expected folded constant for product")
    (assert-true (= (expression-value m1) 24)
                 'test-mul-canonicalization
                 "product constant value mismatch: ~S" (expression-value m1)))
  ;; (* x y 1) -> (* x y)
  (let* ((x (make-expr-var 'x))
         (y (make-expr-var 'y))
         (prod (make-expr-mul (list x y (make-expr-const 1)))))
    (assert-true (typep prod 'multiply-expression)
                 'test-mul-canonicalization
                 "expected multiply-expression")
    (let ((args (expression-arguments prod)))
      (assert-true (= (length args) 2)
                   'test-mul-canonicalization
                   "expected 2 arguments in simplified product, got ~D"
                   (length args))
      (assert-true (null (set-exclusive-or (mapcar #'expr->sexpr args)
                                     (list 'x 'y)
                                     :test #'equal))
                   'test-mul-canonicalization
                   "product arguments not {x,y}: ~S"
                   (mapcar #'expr->sexpr args))))
  ;; (* x 0 y) -> 0
  (let* ((x (make-expr-var 'x))
         (y (make-expr-var 'y))
         (prod0 (make-expr-mul (list x (make-expr-const 0) y))))
    (assert-true (typep prod0 'constant-expression)
                 'test-mul-canonicalization
                 "expected constant-expression for product with 0")
    (assert-true (= (expression-value prod0) 0)
                 'test-mul-canonicalization
                 "expected 0 product, got ~S" (expression-value prod0))))

;;; ----------------------------------------------------------------------
;;; Tests for pow / neg canonicalization
;;; ----------------------------------------------------------------------

(deftest test-pow-canonicalization
  (let* ((x (make-expr-var 'x))
         (x1 (make-expr-pow x (make-expr-const 1)))
         (x0 (make-expr-pow x (make-expr-const 0))))
    ;; x^1 -> x
    (assert-true (expr-equal? x x1)
                 'test-pow-canonicalization
                 "x^1 did not simplify to x: ~S" (expr->sexpr x1))
    ;; x^0 -> 1
    (assert-true (and (typep x0 'constant-expression)
                      (= (expression-value x0) 1))
                 'test-pow-canonicalization
                 "x^0 did not simplify to 1: ~S" (expr->sexpr x0))))

(deftest test-neg-canonicalization
  ;; -constant -> constant with negated value
  (let* ((c (make-expr-const 5))
         (negc (make-expr-neg c)))
    (assert-true (typep negc 'constant-expression)
                 'test-neg-canonicalization
                 "negation of constant not constant-expression")
    (assert-true (= (expression-value negc) -5)
                 'test-neg-canonicalization
                 "negation of constant wrong: ~S" (expression-value negc)))
  ;; --x -> x
  (let* ((x (make-expr-var 'x))
         (negx (make-expr-neg x))
         (negnegx (make-expr-neg negx)))
    (assert-true (expr-equal? x negnegx)
                 'test-neg-canonicalization
                 "double negation did not simplify to x: ~S"
                 (expr->sexpr negnegx))))

;;; ----------------------------------------------------------------------
;;; Function calls
;;; ----------------------------------------------------------------------

(deftest test-funcall-basic
  (let* ((x (make-expr-var 'x))
         (call (make-expr-funcall 'sin (list x))))
    (assert-true (typep call 'function-call-expression)
                 'test-funcall-basic
                 "sin(x) not function-call-expression")
    (assert-true (eq (function-call-name call) 'sin)
                 'test-funcall-basic
                 "function name mismatch: ~S" (function-call-name call))
    (assert-true (equal (mapcar #'expr->sexpr (function-call-arguments call))
                        '(x))
                 'test-funcall-basic
                 "function arguments mismatch: ~S"
                 (mapcar #'expr->sexpr (function-call-arguments call)))))

;;; ----------------------------------------------------------------------
;;; sexpr->expr-ir tests
;;; ----------------------------------------------------------------------

(deftest test-sexpr-arithmetic
  ;; (+ x 1 (* 2 y))
  (let* ((form '(+ x 1 (* 2 y)))
         (expr (sexpr->expr-ir form)))
    (assert-true (typep expr 'add-expression)
                 'test-sexpr-arithmetic
                 "top-level should be add-expression")
    ;; canonical S-expression form
    (assert-true (equal (expr->sexpr expr)
                        '(+ 1 X (* 2 Y)))
                 'test-sexpr-arithmetic
                 "unexpected canonical sexpr: ~S" (expr->sexpr expr)))
  ;; (- x y) -> (+ x (- y))
  (let* ((form '(- x y))
         (expr (sexpr->expr-ir form)))
    (assert-true (typep expr 'add-expression)
                 'test-sexpr-arithmetic
                 "(- x y) did not become add-expression")
    (assert-true (equal (expr->sexpr expr)
                        '(+ X (- Y)))
                 'test-sexpr-arithmetic
                 "(- x y) canonical sexpr mismatch: ~S" (expr->sexpr expr)))
  ;; (/ x y) -> x * y^-1
  (let* ((form '(/ x y))
         (expr (sexpr->expr-ir form))
         (sexpr (expr->sexpr expr)))
    (assert-true (typep expr 'multiply-expression)
                 'test-sexpr-arithmetic
                 "(/ x y) did not become multiply-expression")
    ;; don't over-specify order, just check general shape
    (assert-true (or (equal sexpr '(* X (EXPT Y -1)))
                     (equal sexpr '(* (EXPT Y -1) X)))
                 'test-sexpr-arithmetic
                 "(/ x y) unexpected representation: ~S" sexpr)))

(deftest test-sexpr-comparison-and-logic
  ;; (< x y)
  (let* ((form '(< x y))
         (expr (sexpr->expr-ir form)))
    (assert-true (typep expr 'comparison-expression)
                 'test-sexpr-comparison-and-logic
                 "(< x y) not comparison-expression")
    (assert-true (equal (expr->sexpr expr) '(< X Y))
                 'test-sexpr-comparison-and-logic
                 "comparison sexpr mismatch: ~S" (expr->sexpr expr)))
  ;; (and (< x y) (> y 0))
  (let* ((form '(and (< x y) (> y 0)))
         (expr (sexpr->expr-ir form)))
    (assert-true (typep expr 'logical-nary-expression)
                 'test-sexpr-comparison-and-logic
                 "and form not logical-nary-expression")
    (assert-true (equal (expr->sexpr expr)
                        '(AND (< X Y) (> Y 0)))
                 'test-sexpr-comparison-and-logic
                 "and canonical sexpr mismatch: ~S" (expr->sexpr expr)))
  ;; (not (= x 0))
  (let* ((form '(not (= x 0)))
         (expr (sexpr->expr-ir form)))
    (assert-true (typep expr 'logical-not-expression)
                 'test-sexpr-comparison-and-logic
                 "not form not logical-not-expression")
    (assert-true (equal (expr->sexpr expr)
                        '(NOT (= X 0)))
                 'test-sexpr-comparison-and-logic
                 "not canonical sexpr mismatch: ~S" (expr->sexpr expr))))

;;; ----------------------------------------------------------------------
;;; infix->expr-ir tests (arithmetic only)
;;; ----------------------------------------------------------------------

(deftest test-infix-basic-arithmetic
  ;; 1 + 2 * 3 -> 1 + (2 * 3)
  (let* ((expr  (infix->expr-ir "1 + 2*3"))
         (sexpr (expr->sexpr expr)))
    (assert-true (typep expr 'constant-expression)
                 'test-infix-basic-arithmetic
                 "1+2*3: expected constant-expression, got ~S" expr)
    (assert-true (= (expression-value expr) 7)
                 'test-infix-basic-arithmetic
                 "1+2*3: constant value not 7, got ~S"
                 (expression-value expr))
    (assert-true (equal sexpr 7)
                 'test-infix-basic-arithmetic
                 "1+2*3: expr->sexpr not 7, got ~S" sexpr))
  ;; (x+1)*sin(y)
  (let* ((expr (infix->expr-ir "(x+1)*sin(y)"))
         (sexpr (expr->sexpr expr))
         (form '(* (+ 1 x) (sin y))))
    (assert-true (sexpr-alpha-equal sexpr form)
                 'test-infix-basic-arithmetic
                 "(x+1)*sin(y) parsed incorrectly: ~S vs ~s" sexpr form))
  ;; -x + 3
  (let* ((expr (infix->expr-ir "-x + 3"))
         (sexpr (expr->sexpr expr)))
    ;; we don't over-specify canonicalization; just check shape
    (assert-true (member sexpr
                         '((+ (- X) 3)
                           (+ 3 (- X)))
                         :test #'sexpr-alpha-equal)
                 'test-infix-basic-arithmetic
                 "-x+3 parsed incorrectly: ~S" sexpr))
  ;; pow: x^2 + y^3
  (let* ((expr (infix->expr-ir "x^2 + y^3"))
         (sexpr (expr->sexpr expr)))
    (assert-true (null (set-exclusive-or sexpr
                                   '(+ (EXPT X 2) (EXPT Y 3))
                                   :test #'sexpr-alpha-equal))
                 'test-infix-basic-arithmetic
                 "x^2 + y^3 parsed incorrectly: ~S" sexpr)))

;;; ----------------------------------------------------------------------
;;; parse-expr dispatch tests
;;; ----------------------------------------------------------------------

(deftest test-parse-expr-dispatch
  ;; already expression
  (let* ((x (make-expr-var 'x))
         (p (parse-expr x)))
    (assert-true (eq p x)
                 'test-parse-expr-dispatch
                 "parse-expr changed expression object unexpectedly"))

  ;; string -> infix, with constant folding: 1+2 -> 3
  (let* ((expr  (parse-expr "1+2"))
         (sexpr (expr->sexpr expr)))
    (assert-true (typep expr 'constant-expression)
                 'test-parse-expr-dispatch
                 "parse-expr \"1+2\": expected constant-expression, got ~S" expr)
    (assert-true (= (expression-value expr) 3)
                 'test-parse-expr-dispatch
                 "parse-expr \"1+2\": constant value not 3, got ~S"
                 (expression-value expr))
    (assert-true (equal sexpr 3)
                 'test-parse-expr-dispatch
                 "parse-expr \"1+2\": expr->sexpr not 3, got ~S" sexpr))

  ;; list -> prefix
  (let* ((expr (parse-expr '(* x (+ 1 y))))
         (sexpr (expr->sexpr expr)))
    (assert-true (sexpr-alpha-equal sexpr '(* X (+ 1 Y)))
                 'test-parse-expr-dispatch
                 "parse-expr list did not parse prefix correctly: ~S"
                 sexpr)))


;;; ----------------------------------------------------------------------
;;; Roundtrip tests
;;; ----------------------------------------------------------------------

(deftest test-roundtrip-sexpr
  ;; Start from sexpr, go to IR, back to sexpr string.
  (let* ((form '(* (+ x 1) (sin y)))
         (expr (sexpr->expr-ir form))
         (roundtrip (expr->sexpr expr))
         (form '(* (+ 1 X) (SIN Y))))
    ;; Their canonical versions should be structurally equal in meaning,
    ;; although variable names may be upcased.
    (assert-true (sexpr-alpha-equal roundtrip form)
                 'test-roundtrip-sexpr
                 "sexpr roundtrip mismatch: ~%~S vs ~%~s" roundtrip form)))

(deftest test-roundtrip-infix
  ;; Infix -> IR -> infix-string should be a readable, equivalent string.
  ;; We don't require exactly the same whitespace.
  (let* ((input " (x+1)*sin(y) ")
         (expr (infix->expr-ir input))
         (out  (expr->infix-string expr)))
    ;; Very loose checks: contains expected substrings
    (assert-true (and (search "1 + X" out)
                      (search "SIN(" out))
                 'test-roundtrip-infix
                 "infix roundtrip missing expected structure: ~S" out))
  ;; 1+2*3 -> constant 7, printed as "7"
  (let* ((expr (infix->expr-ir "1 + 2*3"))
         (out  (expr->infix-string expr)))
    (assert-true (typep expr 'constant-expression)
                 'test-roundtrip-infix
                 "infix roundtrip for 1+2*3: expected constant-expression, got ~S"
                 expr)
    (assert-true (= (expression-value expr) 7)
                 'test-roundtrip-infix
                 "infix roundtrip for 1+2*3: value not 7, got ~S"
                 (expression-value expr))
    (assert-true (string= out "7")
                 'test-roundtrip-infix
                 "infix roundtrip for 1+2*3 unexpected: ~S" out)))

(deftest test-simplify-expr-idempotent
  ;; Parse a nontrivial expression, then simplify twice and check that
  ;; the result is the same up to commutativity of + and *.
  (let* ((expr0 (parse-expr
                 "kb*Power(-r0 + Sqrt(Power(-x1 + x2,2) + Power(-y1 + y2,2) + Power(-z1 + z2,2)),2)"))
         (expr1 (simplify-expr expr0))
         (expr2 (simplify-expr expr1)))
    (assert-true (expr-equal? expr1 expr2)
                 'test-simplify-expr-idempotent
                 "simplify-expr not stable up to commutativity: ~S vs ~S"
                 (expr->sexpr expr1) (expr->sexpr expr2))))

(deftest test-simplify-expr-after-diff
  ;; Differentiate and then simplify; result should still be a valid expression
  ;; and simplification should be stable.
  (let* ((expr   (parse-expr "kb*(r - r0)^2"))
         (dexpr  (differentiate-expr expr 'r))
         (s1     (simplify-expr dexpr))
         (s2     (simplify-expr s1)))
    (assert-true (expr-equal? s1 s2)
                 'test-simplify-expr-after-diff
                 "simplify-expr after differentiation not idempotent: ~S vs ~S"
                 (expr->sexpr s1) (expr->sexpr s2))))

(in-package :expr-ir.tests)

(defun collect-vars (expr)
  "Return a list of the VARIABLE-NAME symbols appearing in EXPR."
  (let ((vars '()))
    (labels ((walk (e)
               (typecase e
                 (variable-expression
                  (push (variable-name e) vars))
                 (add-expression
                  (mapc #'walk (expression-arguments e)))
                 (multiply-expression
                  (mapc #'walk (expression-arguments e)))
                 (power-expression
                  (walk (power-base-expression e))
                  (walk (power-exponent-expression e)))
                 (negate-expression
                  (walk (negate-argument-expression e)))
                 (function-call-expression
                  (mapc #'walk (function-call-arguments e)))
                 (comparison-expression
                  (walk (comparison-left-expression e))
                  (walk (comparison-right-expression e)))
                 (logical-nary-expression
                  (mapc #'walk (logical-nary-arguments e)))
                 (logical-not-expression
                  (walk (logical-not-argument-expression e)))
                 (constant-expression
                  nil)
                 (t
                  nil))))
      (walk expr)
      (nreverse (remove-duplicates vars :test #'eq)))))

(defun expr-var-symbol (name-string)
  "Return the symbol used as the variable name when parsing NAME-STRING."
  (let ((expr (expr-ir:parse-expr name-string)))
    (etypecase expr
      (variable-expression
       (variable-name expr))
      (t
       (error "expr-var-symbol: ~S did not parse to a VARIABLE-EXPRESSION: ~S"
              name-string expr)))))

(deftest test-stretch-energy-variable-consistency
  (let* ((energy-expr
           (expr-ir:parse-expr
            "kb*Power(Sqrt((x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2) - r0, 2)"))
         (vars (collect-vars energy-expr))
         (x1   (expr-var-symbol "x1"))
         (y1   (expr-var-symbol "y1"))
         (z1   (expr-var-symbol "z1")))
    (format t "~&[DEBUG] stretch vars in energy: ~S~%"
            (mapcar #'symbol-name vars))
    (format t "~&[DEBUG] x1 symbol: ~S~%" x1)
    (assert-true (member x1 vars :test #'eq)
                 'test-stretch-energy-variable-consistency
                 "x1 symbol not found in energy vars: ~S" vars)
    (assert-true (member y1 vars :test #'eq)
                 'test-stretch-energy-variable-consistency
                 "y1 symbol not found in energy vars: ~S" vars)
    (assert-true (member z1 vars :test #'eq)
                 'test-stretch-energy-variable-consistency
                 "z1 symbol not found in energy vars: ~S" vars)))

(deftest test-stretch-diff-x1
  (let* ((energy-expr
           (expr-ir:parse-expr
            "kb*Power(Sqrt((x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2) - r0, 2)"))
         (x1 (expr-var-symbol "x1"))
         (dE-dx1 (expr-ir:differentiate-expr energy-expr x1)))
    (format t "~&[DEBUG] dE/dx1 sexpr: ~S~%"
            (expr-ir:expr->sexpr dE-dx1))
    ;; We do NOT expect a constant 0 here.
    (assert-true (not (and (typep dE-dx1 'expr-ir:constant-expression)
                           (zerop (expr-ir:expression-value dE-dx1))))
                 'test-stretch-diff-x1
                 "dE/dx1 is constant zero: ~S"
                 (expr-ir:expr->sexpr dE-dx1))))


(deftest test-stretch-diff-kb
  (let* ((energy-expr
           (expr-ir:parse-expr
            "kb*Power(Sqrt((x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2) - r0, 2)"))
         (kb (expr-var-symbol "kb"))
         (dE-dkb (expr-ir:differentiate-expr energy-expr kb)))
    (format t "~&[DEBUG] dE/dkb sexpr: ~S~%"
            (expr-ir:expr->sexpr dE-dkb))
    ;; dE/dkb should be the rest of the expression (Power(...,2)), not 0.
    (assert-true (not (and (typep dE-dkb 'expr-ir:constant-expression)
                           (zerop (expr-ir:expression-value dE-dkb))))
                 'test-stretch-diff-kb
                 "dE/dkb is constant zero: ~S"
                 (expr-ir:expr->sexpr dE-dkb))))

;; Helper: evaluate an expr at a given environment using expr->sexpr and EVAL.
(defun eval-expr-at (expr env)
  "ENV is an alist (var-symbol . value)."
  (let ((form (expr-ir:expr->sexpr expr)))
    (eval `(let ,(mapcar (lambda (pair)
                           (list (car pair) (cdr pair)))
                  env)
             ,form))))

(defun finite-diff (energy-expr var env &key (h 1.0d-6))
  "Central finite difference dE/dvar at ENV."
  (let* ((val (cdr (assoc var env)))
         (env+ (acons var (+ val h) (remove var env :key #'car :test #'eq)))
         (env- (acons var (- val h) (remove var env :key #'car :test #'eq)))
         (e+  (eval-expr-at energy-expr env+))
         (e-  (eval-expr-at energy-expr env-)))
    (/ (- e+ e-) (* 2.0d0 h))))

(deftest test-stretch-diff-x1
  (let* ((energy-expr
           (expr-ir:parse-expr
            "kb*(Sqrt((x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2) - r0)^2"))
         ;; IMPORTANT: use the *same* symbol that parse-expr uses for x1
         (x1-sym (expr-ir:expr-var-symbol "x1"))
         (dE-dx1 (expr-ir:differentiate-expr energy-expr x1-sym))
         (sexpr  (expr-ir:expr->sexpr dE-dx1)))
    (assert-true (not (and (typep dE-dx1 'expr-ir:constant-expression)
                           (zerop (expr-ir:expression-value dE-dx1))))
                 'test-stretch-diff-x1
                 "dE/dx1 is constant zero: ~S" sexpr)))

(deftest test-stretch-energy-grad-numeric
  (let* ((energy-expr
           (expr-ir:parse-expr
            "kb*(Sqrt((x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2) - r0)^2"))
         ;; Pick a random but fixed configuration and parameters
         (env `((cl-user::kb . 10.0d0)
                (cl-user::r0 . 1.5d0)
                (cl-user::x1 . 0.1d0)
                (cl-user::y1 . -0.2d0)
                (cl-user::z1 . 0.3d0)
                (cl-user::x2 . 1.1d0)
                (cl-user::y2 . 0.8d0)
                (cl-user::z2 . -0.4d0)))
         ;; Analytic dE/dx1
         (dE-dx1-expr (expr-ir:differentiate-expr energy-expr 'cl-user::x1))
         (dE-dx1-val  (eval-expr-at dE-dx1-expr env))
         ;; Numeric dE/dx1 by finite difference
         (dE-dx1-num  (finite-diff energy-expr 'cl-user::x1 env))
         (tol 1.0d-6))
    (assert-true (< (abs (- dE-dx1-val dE-dx1-num)) tol)
                 'test-stretch-energy-grad-numeric
                 "dE/dx1 analytic vs numeric mismatch: ~A vs ~A"
                 dE-dx1-val dE-dx1-num)))
