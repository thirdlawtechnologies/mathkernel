;;;; statement-tests.lisp
;;;; Regression tests for derivative code over statement IR,
;;;; including if-statements and c-functions.
;;;; Uses the same minimal test framework style as expression-tests.lisp.

(in-package :stmt-ir.tests)

;;; ----------------------------------------------------------------------
;;; Tiny test harness (same style as expression-tests.lisp)
;;; ----------------------------------------------------------------------

(define-condition test-failure (error)
  ((name   :initarg :name   :reader test-failure-name)
   (detail :initarg :detail :reader test-failure-detail))
  (:report (lambda (c s)
             (format s "~&[TEST FAILED] ~A: ~A"
                     (test-failure-name c)
                     (test-failure-detail c)))))

(defun assert-true (condition name fmt &rest args)
  (unless condition
    (error 'test-failure
           :name   name
           :detail (apply #'format nil fmt args))))

(defmacro deftest (name &body body)
  `(defun ,name ()
     (format t "~&[TEST] ~A~%" ',name)
     ,@body
     (format t "  OK~%")))

(defun run-statement-tests ()
  (let ((tests '(test-deriv-single-assignment
                 test-deriv-two-step-chain
                 test-deriv-assignments-generation
                 test-deriv-env-ignores-non-assignments
                 test-c-function-container-and-deriv
                 test-simplify-block-basic
                 test-energy-grad-block-quadratic
                 test-energy-grad-hess-block-quadratic
                 test-energy-grad-hess-block-mixed
                 )))
    (dolist (fn tests)
      (handler-case
          (funcall fn)
        (test-failure (c)
          (format *error-output* "~&~A~%" c)
          (return-from run-statement-tests nil))))
    (format t "~&All statement tests passed.~%")
    t))

;;; ----------------------------------------------------------------------
;;; Helpers: structural comparison of S-expressions, ignoring packages
;;; ----------------------------------------------------------------------

(defun same-symbol-name-p (a b)
  (and (symbolp a)
       (symbolp b)
       (string= (symbol-name a)
                (symbol-name b))))

(defun sexpr-alpha-equal (a b)
  "Structural equality of S-expressions, ignoring symbol packages."
  (labels ((recur (x y)
             (cond
               ;; both symbols -> compare by name
               ((and (symbolp x) (symbolp y))
                (same-symbol-name-p x y))
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

(defun expr-equal? (a b)
  "Compare two expression IR nodes by alpha-equal prefix S-expressions."
  (sexpr-alpha-equal (expr->sexpr a)
                     (expr->sexpr b)))

;;; ----------------------------------------------------------------------
;;; 1. Single-assignment differentiation: foo = x^2 + 1
;;; ----------------------------------------------------------------------

(deftest test-deriv-single-assignment
  ;; foo = x^2 + 1  =>  dfoo/dx = 2*x
  (let* ((x      (make-expr-var 'x))
         (expr   (make-expr-add
                  (list (make-expr-pow x (make-expr-const 2))
                        (make-expr-const 1))))
         (stmt   (make-assignment-stmt 'foo expr))
         (stmts  (list stmt))
         ;; use the public API, not raw env access
         (dfoo   (differentiate-target-in-block stmts 'x 'foo)))
    (assert-true dfoo
                 'test-deriv-single-assignment
                 "dfoo/dx is NIL")
    (let ((sexpr (expr->sexpr dfoo)))
      ;; We expect something equivalent to (* 2 X)
      (assert-true (sexpr-alpha-equal sexpr '(* 2 x))
                   'test-deriv-single-assignment
                   "dfoo/dx not 2*x, got: ~S" sexpr))))

;;; ----------------------------------------------------------------------
;;; 2. Two-step chain: foo = x^2+1 ; bar = sin(foo)+foo
;;;    Check d(bar)/dx via chain rule and env propagation.
;;; ----------------------------------------------------------------------

(deftest test-deriv-two-step-chain
  ;; foo = x^2 + 1
  ;; bar = sin(foo) + foo
  ;; dfoo/dx = 2*x
  ;; dbar/dx = cos(foo)*dfoo/dx + dfoo/dx = (cos(foo) + 1)*(2*x)
  (let* ((x        (make-expr-var 'x))
         (foo-expr (parse-expr "x^2 + 1"))
         (bar-expr (parse-expr "sin(foo) + foo"))
         (s1       (make-assignment-stmt 'foo foo-expr))
         (s2       (make-assignment-stmt 'bar bar-expr))
         (stmts    (list s1 s2))
         (dbar-dx  (differentiate-target-in-block stmts 'x 'bar)))
    (assert-true dbar-dx
                 'test-deriv-two-step-chain
                 "dbar/dx is NIL (bar not found?)")
    (let ((sexpr (expr->sexpr dbar-dx)))
      ;; Expected structure: (+ (* 2 X (COS FOO)) (* 2 X))
      ;; (cos(foo)*2*x + 2*x). We don't require factoring.
      (let ((expected1 '(+ (* 2 x (cos foo)) (* 2 x)))
            (expected2 '(+ (* 2 x) (* 2 x (cos foo))))) ; allow swapped terms
        (assert-true (or (sexpr-alpha-equal sexpr expected1)
                         (sexpr-alpha-equal sexpr expected2))
                     'test-deriv-two-step-chain
                     "dbar/dx unexpected shape: ~S" sexpr)))))

;;; ----------------------------------------------------------------------
;;; 3. Generation of derivative assignments for a block
;;; ----------------------------------------------------------------------

(deftest test-deriv-assignments-generation
  ;; Same block as in test-deriv-two-step-chain; generate DFOO_DX, DBAR_DX.
  (let* ((foo-expr (parse-expr "x^2 + 1"))
         (bar-expr (parse-expr "sin(foo) + foo"))
         (s1       (make-assignment-stmt 'foo foo-expr))
         (s2       (make-assignment-stmt 'bar bar-expr))
         (stmts    (list s1 s2))
         (dstmts   (make-derivative-assignments-for-block stmts 'x)))
    ;; We expect two derivative assignment statements
    (assert-true (= (length dstmts) 2)
                 'test-deriv-assignments-generation
                 "expected 2 derivative statements, got ~D" (length dstmts))
    (destructuring-bind (dfoo-stmt dbar-stmt)
        dstmts
      ;; Check target names
      (assert-true (and (typep dfoo-stmt 'assignment-statement)
                        (typep dbar-stmt 'assignment-statement))
                   'test-deriv-assignments-generation
                   "derivative stmts not assignment-statement: ~S" dstmts)
      (let ((name1 (stmt-target-name dfoo-stmt))
            (name2 (stmt-target-name dbar-stmt)))
        (assert-true (string= (symbol-name name1) "DFOO_DX")
                     'test-deriv-assignments-generation
                     "expected DFOO_DX, got ~S" name1)
        (assert-true (string= (symbol-name name2) "DBAR_DX")
                     'test-deriv-assignments-generation
                     "expected DBAR_DX, got ~S" name2))
      ;; Check derivative expressions roughly:
      (let ((dfoo (stmt-expression dfoo-stmt))
            (dbar (stmt-expression dbar-stmt)))
        (assert-true (sexpr-alpha-equal (expr->sexpr dfoo) '(* 2 x))
                     'test-deriv-assignments-generation
                     "dfoo_dx not 2*x: ~S" (expr->sexpr dfoo))
        ;; dbar_dx as before; just reuse same pattern
        (let ((sexpr (expr->sexpr dbar)))
          (let ((expected1 '(+ (* 2 x (cos foo)) (* 2 x)))
                (expected2 '(+ (* 2 x) (* 2 x (cos foo)))))
            (assert-true (or (sexpr-alpha-equal sexpr expected1)
                             (sexpr-alpha-equal sexpr expected2))
                         'test-deriv-assignments-generation
                         "dbar_dx unexpected shape: ~S" sexpr)))))))

;;; ----------------------------------------------------------------------
;;; 4. Derivative logic should ignore non-assignment statements at the
;;;    top level (raw C, if-statement), when building d(var)/dx for
;;;    *intermediates* via build-derivative-env-for-block.
;;; ----------------------------------------------------------------------

(deftest test-deriv-env-ignores-non-assignments
  ;; Block includes:
  ;;   foo = x^2 + 1;
  ;;   (raw C) "/* comment */";
  ;;   if (x > 0) { bar = foo; } else { bar = -foo; }
  ;;
  ;; For purposes of building the intermediate derivative env, only the
  ;; top-level assignment to FOO should matter.
  (let* ((x        (make-expr-var 'x))
         (foo-expr (parse-expr "x^2 + 1"))
         (foo-stmt (make-assignment-stmt 'foo foo-expr))
         (raw-stmt (make-raw-c-statement "/* comment */"))
         ;; condition, then/else blocks
         (cond-expr (parse-expr "x > 0"))
         (bar-then  (make-assignment-stmt 'bar (parse-expr "foo")))
         (bar-else  (make-assignment-stmt 'bar (parse-expr "-foo")))
         (then-blk  (make-block-stmt (list bar-then)))
         (else-blk  (make-block-stmt (list bar-else)))
         (if-stmt   (make-if-stmt cond-expr then-blk else-blk))
         (stmts     (list foo-stmt raw-stmt if-stmt))
         ;; Now use the API to ask for derivatives
         (dfoo      (differentiate-target-in-block stmts 'x 'foo))
         (dbar      (differentiate-target-in-block stmts 'x 'bar)))
    ;; dfoo/dx should be 2*x
    (assert-true dfoo
                 'test-deriv-env-ignores-non-assignments
                 "dfoo/dx is NIL")
    (assert-true (sexpr-alpha-equal (expr->sexpr dfoo) '(* 2 x))
                 'test-deriv-env-ignores-non-assignments
                 "dfoo/dx not 2*x: ~S" (expr->sexpr dfoo))
    ;; bar is only assigned inside the if; our top-level derivative-block
    ;; builder never sees a bar assignment, so dbar/dx should be NIL.
    (assert-true (null dbar)
                 'test-deriv-env-ignores-non-assignments
                 "dbar/dx unexpectedly non-NIL: ~S" (and dbar (expr->sexpr dbar)))))

;;; ----------------------------------------------------------------------
;;; 5. c-function container: structure + derivative use of its body
;;; ----------------------------------------------------------------------

(deftest test-c-function-container-and-deriv
  ;; Define a simple function:
  ;;   double f(double x) {
  ;;       foo = x^2 + 1;
  ;;       bar = sin(foo) + foo;
  ;;   }
  ;;
  ;; We verify:
  ;;   - c-function slots are set correctly.
  ;;   - we can use its body with differentiate-target-in-block.
  (let* ((foo-expr (parse-expr "x^2 + 1"))
         (bar-expr (parse-expr "sin(foo) + foo"))
         (s1       (make-assignment-stmt 'foo foo-expr))
         (s2       (make-assignment-stmt 'bar bar-expr))
         (body-blk (make-block-stmt (list s1 s2)))
         (params   (list (cons "double" 'x)))
         (locals   nil)
         (fun      (make-c-function 'f body-blk
                                    :return-type "double"
                                    :parameters  params
                                    :locals      locals)))
    ;; Check basic structure
    (assert-true (typep fun 'c-function)
                 'test-c-function-container-and-deriv
                 "fun is not a c-function: ~S" fun)
    (assert-true (string= (symbol-name (c-function-name fun)) "F")
                 'test-c-function-container-and-deriv
                 "function name mismatch: ~S" (c-function-name fun))
    (assert-true (string= (c-function-return-type fun) "double")
                 'test-c-function-container-and-deriv
                 "return type mismatch: ~S" (c-function-return-type fun))
    (assert-true (= (length (c-function-parameters fun)) 1)
                 'test-c-function-container-and-deriv
                 "expected 1 parameter, got ~D"
                 (length (c-function-parameters fun)))
    ;; Now use its body for derivatives via the public API
    (let* ((body-stmts (block-statements (c-function-body fun)))
           (dbar       (differentiate-target-in-block body-stmts 'x 'bar)))
      (assert-true dbar
                   'test-c-function-container-and-deriv
                   "bar derivative not present")
      (let ((sexpr (expr->sexpr dbar)))
        (let ((expected1 '(+ (* 2 x (cos foo)) (* 2 x)))
              (expected2 '(+ (* 2 x) (* 2 x (cos foo)))))
          (assert-true (or (sexpr-alpha-equal sexpr expected1)
                           (sexpr-alpha-equal sexpr expected2))
                       'test-c-function-container-and-deriv
                       "dbar/dx in c-function unexpected: ~S" sexpr))))))

(deftest test-simplify-block-basic
  ;; Build a tiny block with an assignment and an if-statement, then simplify.
  (let* ((expr (expr-ir:parse-expr "1 + 2*3"))  ; will be folded to 7 by constructors
         (stmt (make-assignment-stmt 'foo expr))
         (cond-expr (expr-ir:parse-expr "x > 0"))
         (then-stmt (make-assignment-stmt 'bar (expr-ir:parse-expr "foo + 0")))
         (else-stmt (make-assignment-stmt 'bar (expr-ir:parse-expr "foo")))
         (then-blk  (make-block-stmt (list then-stmt)))
         (else-blk  (make-block-stmt (list else-stmt)))
         (if-stmt   (make-if-stmt cond-expr then-blk else-blk))
         (blk       (make-block-stmt (list stmt if-stmt)))
         (s-blk     (simplify-block blk)))
    ;; Just check shape and that expressions are still sane
    (assert-true (typep s-blk 'block-statement)
                 'test-simplify-block-basic
                 "simplify-block did not return a block-statement: ~S" s-blk)
    (let ((s-stmts (block-statements s-blk)))
      (assert-true (= (length s-stmts) 2)
                   'test-simplify-block-basic
                   "simplify-block changed statement count: ~S" s-stmts)
      (assert-true (typep (first s-stmts) 'assignment-statement)
                   'test-simplify-block-basic
                   "first stmt not assignment after simplify: ~S" (first s-stmts))
      (assert-true (typep (second s-stmts) 'if-statement)
                   'test-simplify-block-basic
                   "second stmt not if-statement after simplify: ~S" (second s-stmts)))))


(defun grad-name (var)
  "Return a symbol naming dE/d(var), e.g. G_X, G_Y."
  (intern (format nil "G_~A" (symbol-name var))
          (symbol-package var)))

(defun hess-name (vi vj)
  "Return a symbol naming d²E/(dvi dvj), e.g. H_X_X, H_X_Y, H_Y_Y."
  (intern (format nil "H_~A_~A"
                  (symbol-name vi) (symbol-name vj))
          (symbol-package vi)))

(defun find-assignment (block name)
  "Find the first ASSIGNMENT-STATEMENT in BLOCK whose target-name is NAME."
  (find-if (lambda (st)
             (and (typep st 'assignment-statement)
                  (eql (stmt-target-name st) name)))
           (block-statements block)))

(deftest test-energy-grad-block-quadratic
  ;; E(x,y) = x^2 + y^2
  ;; ∂E/∂x = 2x, ∂E/∂y = 2y
  (let* ((expr (expr-ir:parse-expr "x^2 + y^2"))
         (coord-vars '(x y))
         (block (make-energy-grad-block
                 :energy-expr   expr
                 :coord-vars    coord-vars
                 :energy-target 'E
                 :grad-target-fn #'grad-name
                 :simplify      t)))
    (assert-true (typep block 'block-statement)
                 'test-energy-grad-block-quadratic
                 "builder did not return a block-statement: ~S" block)
    (let* ((stmts (block-statements block))
           ;; energy, G_X, G_Y: expect 3 statements
           (e-stmt   (find-assignment block 'E))
           (gx-stmt  (find-assignment block 'G_X))
           (gy-stmt  (find-assignment block 'G_Y)))
      (assert-true (= (length stmts) 3)
                   'test-energy-grad-block-quadratic
                   "expected 3 statements (E,G_X,G_Y), got ~D" (length stmts))
      ;; Check energy expression is E = x^2 + y^2 (up to our expr-equal?)
      (let ((orig (expr-ir:expr->sexpr expr))
            (in-block (expr-ir:expr->sexpr (stmt-expression e-stmt))))
        (assert-true (sexpr-alpha-equal
                      (expr-ir.tests::normalize-commutative orig)
                      (expr-ir.tests::normalize-commutative in-block))
                     'test-energy-grad-block-quadratic
                     "energy expression in block does not match: ~S"
                     in-block))
      ;; Check gradient components: 2x and 2y
      (let ((gx-expr (stmt-expression gx-stmt))
            (gy-expr (stmt-expression gy-stmt)))
        (assert-true (sexpr-alpha-equal (expr-ir:expr->sexpr gx-expr)
                                        '(* 2 x))
                     'test-energy-grad-block-quadratic
                     "dE/dx is not 2*x: ~S" (expr-ir:expr->sexpr gx-expr))
        (assert-true (sexpr-alpha-equal (expr-ir:expr->sexpr gy-expr)
                                        '(* 2 y))
                     'test-energy-grad-block-quadratic
                     "dE/dy is not 2*y: ~S" (expr-ir:expr->sexpr gy-expr))))))

(deftest test-energy-grad-hess-block-quadratic
  ;; E(x,y) = x^2 + y^2
  ;; ∂E/∂x = 2x, ∂E/∂y = 2y
  ;; Hessian: d2E/dx2 = 2, d2E/dxdy = 0, d2E/dy2 = 2
  (let* ((expr (expr-ir:parse-expr "x^2 + y^2"))
         (coord-vars '(x y))
         (block (make-energy-grad-hess-block
                 :energy-expr    expr
                 :coord-vars     coord-vars
                 :energy-target  'E
                 :grad-target-fn #'grad-name
                 :hess-target-fn #'hess-name
                 :simplify       t)))
    (assert-true (typep block 'block-statement)
                 'test-energy-grad-hess-block-quadratic
                 "builder did not return a block-statement: ~S" block)
    ;; Expected assignments: E, G_X, G_Y, H_X_X, H_X_Y, H_Y_Y => 6
    (let* ((stmts (block-statements block))
           (e-stmt   (find-assignment block 'E))
           (gx-stmt  (find-assignment block 'G_X))
           (gy-stmt  (find-assignment block 'G_Y))
           (hxx-stmt (find-assignment block 'H_X_X))
           (hxy-stmt (find-assignment block 'H_X_Y))
           (hyy-stmt (find-assignment block 'H_Y_Y)))
      (assert-true (= (length stmts) 6)
                   'test-energy-grad-hess-block-quadratic
                   "expected 6 statements (E,G_X,G_Y,H_X_X,H_X_Y,H_Y_Y), got ~D"
                   (length stmts))
      ;; gradient checks
      (assert-true (sexpr-alpha-equal (expr-ir:expr->sexpr (stmt-expression gx-stmt))
                                      '(* 2 x))
                   'test-energy-grad-hess-block-quadratic
                   "dE/dx is not 2*x: ~S"
                   (expr-ir:expr->sexpr (stmt-expression gx-stmt)))
      (assert-true (sexpr-alpha-equal (expr-ir:expr->sexpr (stmt-expression gy-stmt))
                                      '(* 2 y))
                   'test-energy-grad-hess-block-quadratic
                   "dE/dy is not 2*y: ~S"
                   (expr-ir:expr->sexpr (stmt-expression gy-stmt)))
      ;; Hessian checks
      (assert-true (sexpr-alpha-equal (expr-ir:expr->sexpr (stmt-expression hxx-stmt))
                                      2)
                   'test-energy-grad-hess-block-quadratic
                   "d2E/dx2 is not 2: ~S"
                   (expr-ir:expr->sexpr (stmt-expression hxx-stmt)))

      (assert-true (sexpr-alpha-equal (expr-ir:expr->sexpr (stmt-expression hxy-stmt))
                                      0)
                   'test-energy-grad-hess-block-quadratic
                   "d2E/dxdy is not 0: ~S"
                   (expr-ir:expr->sexpr (stmt-expression hxy-stmt)))

      (assert-true (sexpr-alpha-equal (expr-ir:expr->sexpr (stmt-expression hyy-stmt))
                                      2)
                   'test-energy-grad-hess-block-quadratic
                   "d2E/dy2 is not 2: ~S"
                   (expr-ir:expr->sexpr (stmt-expression hyy-stmt)))
      )))


(deftest test-energy-grad-hess-block-mixed
  ;; E(x,y) = x*y
  ;; ∂E/∂x = y, ∂E/∂y = x
  ;; Hessian: d2E/dx2 = 0, d2E/dxdy = 1, d2E/dy2 = 0
  (let* ((expr (expr-ir:parse-expr "x*y"))
         (coord-vars '(x y))
         (block (make-energy-grad-hess-block
                 :energy-expr    expr
                 :coord-vars     coord-vars
                 :energy-target  'E
                 :grad-target-fn #'grad-name
                 :hess-target-fn #'hess-name
                 :simplify       t)))
    (assert-true (typep block 'block-statement)
                 'test-energy-grad-hess-block-mixed
                 "builder did not return a block-statement: ~S" block)
    (let* ((stmts (block-statements block))
           (gx-stmt  (find-assignment block 'G_X))
           (gy-stmt  (find-assignment block 'G_Y))
           (hxx-stmt (find-assignment block 'H_X_X))
           (hxy-stmt (find-assignment block 'H_X_Y))
           (hyy-stmt (find-assignment block 'H_Y_Y)))
      (assert-true (= (length stmts) 6)
                   'test-energy-grad-hess-block-mixed
                   "expected 6 statements, got ~D" (length stmts))
      ;; gradient
      (assert-true (sexpr-alpha-equal (expr-ir:expr->sexpr (stmt-expression gx-stmt))
                                      'y)
                   'test-energy-grad-hess-block-mixed
                   "dE/dx is not y: ~S"
                   (expr-ir:expr->sexpr (stmt-expression gx-stmt)))
      (assert-true (sexpr-alpha-equal (expr-ir:expr->sexpr (stmt-expression gy-stmt))
                                      'x)
                   'test-energy-grad-hess-block-mixed
                   "dE/dy is not x: ~S"
                   (expr-ir:expr->sexpr (stmt-expression gy-stmt)))
      ;; Hessian
      (assert-true (sexpr-alpha-equal (expr-ir:expr->sexpr (stmt-expression hxx-stmt))
                                      '0)
                   'test-energy-grad-hess-block-mixed
                   "d2E/dx2 is not 0: ~S"
                   (expr-ir:expr->sexpr (stmt-expression hxx-stmt)))
      (assert-true (sexpr-alpha-equal (expr-ir:expr->sexpr (stmt-expression hxy-stmt))
                                      '1)
                   'test-energy-grad-hess-block-mixed
                   "d2E/dxdy is not 1: ~S"
                   (expr-ir:expr->sexpr (stmt-expression hxy-stmt)))
      (assert-true (sexpr-alpha-equal (expr-ir:expr->sexpr (stmt-expression hyy-stmt))
                                      '0)
                   'test-energy-grad-hess-block-mixed
                   "d2E/dy2 is not 0: ~S"
                   (expr-ir:expr->sexpr (stmt-expression hyy-stmt))))))
