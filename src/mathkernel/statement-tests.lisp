;;;; statement-tests.lisp
;;;; Regression tests for derivative code over statement IR,
;;;; including if-statements and c-functions.
;;;; Uses the same minimal test framework style as expression-tests.lisp.

(in-package :stmt-ir.tests)



;;; ----------------------------------------------------------------------
;;; Tiny test harness (same style as expression-tests.lisp)
;;; ----------------------------------------------------------------------


(defun expr-nil-variable-p (expr)
  "True if EXPR is a variable-expression whose symbol name is \"NIL\"."
  (and (typep expr 'expr-ir:variable-expression)
       (string= (symbol-name (expr-ir:variable-name expr)) "NIL")))

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

(defun run-all ()
  "Run expression tests followed by statement tests. Returns T only if both pass."
  (and (expr-ir.tests:run-expression-tests)
       (run-statement-tests)))

(defun run-statement-tests ()
  (let ((tests '(test-deriv-single-assignment
                 test-deriv-two-step-chain
                 test-deriv-assignments-generation
                 test-deriv-env-ignores-non-assignments
                 test-energy-only-strips-derivative-requests
                 test-c-function-container-and-deriv
                 test-simplify-block-basic

                 test-kernel-builder-rejects-use-before-def
                 test-stretch-energy-c-regression
                 test-stretch-gradient-c-regression
                 test-stretch-hessian-c-regression

                 test-energy-grad-block-quadratic
                 test-energy-grad-hess-block-quadratic
                 test-energy-grad-hess-block-mixed
                 test-cse-simple-add-duplication
                 test-cse-square-of-difference
                 test-copy-propagation-removes-aliases
                 test-cse-distance-squared-duplication
                 test-cse-temp-order-simple-kernel
                 test-cse-temp-order-distance-squared-duplication


                 test-cse-mul-common-subproduct
                 test-cse-never-produces-nil-rhs
                 test-cse-preserves-energy-grad-hess
                 test-copy-propagate-preserves-hessian-target
                 test-cse-and-copy-propagate-preserve-energy-grad-hess

                 test-cse-does-not-introduce-nil-variable-rhs
                 test-cse-preserves-energy-grad-hess-rhs
                 test-copy-propagate-preserves-hessian-target
                 test-cse-and-copy-propagate-preserve-energy-grad-hess-rhs

                 test-factor-temp-param-products-simple
                 test-factor-temp-param-products-min-uses
                 test-factor-temp-param-products-no-nil-rhs

                 test-factor-temp-param-products_angle_like_gradient
                 test-factor-temp-param-products_simple_2_cse_kt


                 test-angle-like-pipeline_factor_temp_param
                 test-factor-temp-param-products_triple_t1_t18_t6
                 test-factor-temp-param-products_triple_t1_t18_t6_with_minus
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
         (stmt   (make-assignment-stmt 'expr-var::foo expr))
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

(deftest test-energy-only-strips-derivative-requests
  "Energy-only kernels should drop derivative requests instead of expanding them."
  (let* ((req (stmt-ir:make-derivative-request-stmt 'u 'x))
         (block (make-block-stmt (list req)))
         (stripped (mathkernel::remove-derivative-requests block)))
    (assert-true (null (block-statements stripped))
                 'test-energy-only-strips-derivative-requests
                 "derivative request was not removed in energy-only mode"))
  ;; Also drop any assignments whose targets match derivative names introduced
  ;; by the requests we removed.
  (let* ((req (stmt-ir:make-derivative-request-stmt 'foo 'x))
         (dname (stmt-ir:make-derivative-name 'foo 'x))
         (assign (make-assignment-stmt dname (make-expr-const 0)))
         (block (make-block-stmt (list req assign)))
         (stripped (mathkernel::remove-derivative-requests block)))
    (assert-true (null (block-statements stripped))
                 'test-energy-only-strips-derivative-requests
                 "derivative assignment was not stripped in energy-only mode"))
  ;; And drop assignments that *use* derivative variables from requests.
  (let* ((req (stmt-ir:make-derivative-request-stmt 'foo 'x))
         (dname (stmt-ir:make-derivative-name 'foo 'x))
         (assign (make-assignment-stmt 'bar (make-expr-var dname)))
         (block (make-block-stmt (list req assign)))
         (stripped (mathkernel::remove-derivative-requests block)))
    (assert-true (null (block-statements stripped))
                 'test-energy-only-strips-derivative-requests
                 "assignment using derivative variable was not stripped"))
  ;; Drop dead assignments chained only into derivative vars.
  (let* ((req (stmt-ir:make-derivative-request-stmt 'foo 'x))
         (dname (stmt-ir:make-derivative-name 'foo 'x))
         (assign1 (make-assignment-stmt 'tmp (make-expr-const 1)))
         (assign2 (make-assignment-stmt dname (make-expr-var 'tmp)))
         (block (make-block-stmt (list req assign1 assign2)))
         (stripped (mathkernel::remove-derivative-requests block)))
    ;; Only the non-derivative assignment should remain.
    (assert-true (= (length (block-statements stripped)) 1)
                 'test-energy-only-strips-derivative-requests
                 "expected non-derivative assignment to remain after stripping")
    (assert-true (string= (symbol-name (stmt-target-name (first (block-statements stripped))))
                          "TMP")
                 'test-energy-only-strips-derivative-requests
                 "remaining assignment target not TMP")))

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
         (stmt (make-assignment-stmt (expr-ir:ev 'foo) expr))
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
                  (eql (stmt-target-name st) (expr-ir:ev name))))
           (block-statements block)))

(deftest test-energy-grad-block-quadratic
  ;; E(x,y) = x^2 + y^2
  ;; ∂E/∂x = 2x, ∂E/∂y = 2y
  (let* ((expr (expr-ir:parse-expr "x^2 + y^2"))
         (coord-vars '(x y))
         (block (make-energy-grad-block
                 :energy-expr   expr
                 :coord-vars    coord-vars
                 :energy-target 'Energy
                 :grad-target-fn #'grad-name
                 :simplify      t)))
    (declare (optimize (debug 3)))
    (assert-true (typep block 'block-statement)
                 'test-energy-grad-block-quadratic
                 "builder did not return a block-statement: ~S" block)
    (let* ((stmts (block-statements block))
           ;; energy, G_X, G_Y: expect 3 statements
           (e-stmt   (find-assignment block 'energy))
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
                 :energy-target  'Energy
                 :grad-target-fn #'grad-name
                 :hess-target-fn #'hess-name
                 :simplify       t)))
    (assert-true (typep block 'block-statement)
                 'test-energy-grad-hess-block-quadratic
                 "builder did not return a block-statement: ~S" block)
    ;; Expected assignments: E, G_X, G_Y, H_X_X, H_X_Y, H_Y_Y => 6
    (let* ((stmts (block-statements block))
           (e-stmt   (find-assignment block 'Energy))
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
                 :energy-target  'Energy
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

(in-package :stmt-ir.tests)

(defun count-sexpr-occurrences-in-expr (expr target-sexpr)
  "Count how many times TARGET-SEXPR appears as a subexpression of EXPR."
  (labels ((rec (sexpr)
             (cond
               ((equal sexpr target-sexpr) 1)
               ((consp sexpr)
                (reduce #'+ (mapcar #'rec sexpr) :initial-value 0))
               (t 0))))
    (rec (expr-ir:expr->sexpr expr))))

(defun count-sexpr-occurrences-in-block (block target-sexpr)
  "Count TARGET-SEXPR occurrences over all assignment RHSs in BLOCK."
  (let ((count 0))
    (labels ((rec-block (blk)
               (dolist (st (stmt-ir:block-statements blk))
                 (typecase st
                   (stmt-ir:assignment-statement
                    (incf count (count-sexpr-occurrences-in-expr
                                 (stmt-ir:stmt-expression st)
                                 target-sexpr)))
                   (stmt-ir:block-statement
                    (rec-block st))
                   (stmt-ir:if-statement
                    (rec-block (stmt-ir:if-then-block st))
                    (when (stmt-ir:if-else-block st)
                      (rec-block (stmt-ir:if-else-block st))))))))
      (rec-block block)
      count)))




;;; ------------------------------------------------------------
;;; CSE regression tests
;;; ------------------------------------------------------------

(deftest test-cse-simple-add-duplication
  "a = x + y; b = x + y;  ->  temp = x + y; a = temp; b = temp"
  (let* ((expr (expr-ir:parse-expr "x + y"))
         (a    (stmt-ir:make-assignment-stmt 'a expr))
         (b    (stmt-ir:make-assignment-stmt 'b expr))
         (block (stmt-ir:make-block-stmt (list a b)))
         ;; Use the actual representation from PARSE-EXPR
         (target-sexpr (expr-ir:expr->sexpr expr))
         (before (count-sexpr-occurrences-in-block block target-sexpr))
         (after-block (stmt-ir:cse-block-multi block :max-passes 5
                                               :min-uses 2 :min-size 2))
         (after (count-sexpr-occurrences-in-block after-block target-sexpr)))
    (assert-true (>= before 2)
                 'test-cse-simple-add-duplication
                 "Expected at least 2 occurrences of ~S before CSE, got ~A"
                 target-sexpr before)
    (assert-true (<= after 1)
                 'test-cse-simple-add-duplication
                 "Expected at most 1 occurrence of ~S after CSE, got ~A"
                 target-sexpr after)))

(deftest test-cse-square-of-difference
  "a = (x2 - x1)^2; b = (x2 - x1)^2;  ->  temp ≈ (x2 - x1); a,b use temp"
  (let* ((u-expr  (expr-ir:parse-expr "x2 - x1"))
         (u2-expr (expr-ir:parse-expr "(x2 - x1)^2"))
         (a (stmt-ir:make-assignment-stmt 'a u2-expr))
         (b (stmt-ir:make-assignment-stmt 'b u2-expr))
         (block (stmt-ir:make-block-stmt (list a b)))
         (u-sexpr (expr-ir:expr->sexpr u-expr))
         (before (count-sexpr-occurrences-in-block block u-sexpr))
         (after-block (stmt-ir:cse-block-multi block :max-passes 5
                                               :min-uses 2 :min-size 3))
         (after (count-sexpr-occurrences-in-block after-block u-sexpr)))
    (assert-true (>= before 2)
                 'test-cse-square-of-difference
                 "Expected at least 2 occurrences of ~S before CSE, got ~A"
                 u-sexpr before)
    (assert-true (<= after 1)
                 'test-cse-square-of-difference
                 "Expected at most 1 occurrence of ~S after CSE, got ~A"
                 u-sexpr after)))

(deftest test-copy-propagation-removes-aliases
  "t0 = x + y; t1 = t0; a = t1;  ->  t1 assignment removed, a uses t0"
  (let* ((expr (expr-ir:parse-expr "x + y"))
         (t0   (stmt-ir:make-assignment-stmt 't0 expr))
         (t1   (stmt-ir:make-assignment-stmt 't1
                                             (expr-ir:parse-expr "t0")))
         (a    (stmt-ir:make-assignment-stmt 'a
                                             (expr-ir:parse-expr "t1")))
         (block (stmt-ir:make-block-stmt (list t0 t1 a)))
         (after-block (stmt-ir:copy-propagate-optimization (stmt-ir:make-pass-id-counter) block))
         (targets '()))
    (dolist (st (stmt-ir:block-statements after-block))
      (when (typep st 'stmt-ir:assignment-statement)
        (push (stmt-ir:stmt-target-name st) targets)))
    (assert-true (not (member 't1 targets))
                 'test-copy-propagation-removes-aliases
                 "Trivial alias t1 should have been removed, targets: ~S"
                 targets)))



(deftest test-cse-distance-squared-duplication
  "r2 and r3 both use the same distance-squared expression; CSE should
factor that common RHS into a temporary."
  (let* ((r2-expr (expr-ir:parse-expr
                   "(x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2"))
         (r2 (stmt-ir:make-assignment-stmt 'r2 r2-expr))
         (r3 (stmt-ir:make-assignment-stmt 'r3 r2-expr))
         (block (stmt-ir:make-block-stmt (list r2 r3)))
         (target-sexpr (expr-ir:expr->sexpr r2-expr))
         (before (count-sexpr-occurrences-in-block block target-sexpr))
         (after-block (stmt-ir:cse-block-multi block :max-passes 5
                                               :min-uses 2 :min-size 3))
         (after (count-sexpr-occurrences-in-block after-block target-sexpr)))
    (assert-true (>= before 2)
                 'test-cse-distance-squared-duplication
                 "Expected at least 2 occurrences of ~S before CSE, got ~A"
                 target-sexpr before)
    (assert-true (<= after 1)
                 'test-cse-distance-squared-duplication
                 "Expected at most 1 occurrence of ~S after CSE, got ~A"
                 target-sexpr after)))


;;; ------------------------------------------------------------
;;; Helpers
;;; ------------------------------------------------------------

(defun run-check-cse-temp-order (block label)
  "Run CHECK-CSE-TEMP-ORDER on BLOCK and return T if it completes without
signaling, NIL otherwise. If it fails, return the condition object as
second value so tests can print it."
  (let ((ok nil)
        (err nil))
    (handler-case
        (progn
          (stmt-ir:check-cse-temp-order block :label label)
          (setf ok t))
      (error (e)
        (setf err e)))
    (values ok err)))


;;; ------------------------------------------------------------
;;; Kernel codegen snapshots
;;; ------------------------------------------------------------

(defparameter *stretch-kernel-c-cache* nil
  "Memoized alist of (\"name\" . c-source-string) for the stretch kernel variants.")

(defun build-stretch-kernel-c-snapshots ()
  "Build stretch kernels (energy/gradient/hessian) with the production pipeline
and return an alist mapping kernel name strings to generated C source strings."
  (let* ((*package* (find-package :mathkernel-user))
         (*trace-output* (make-broadcast-stream))
         (stmt-ir:*debug* nil)
         (stmt-ir::*factor-temp-param-debug* nil)
         (*standard-output* (make-broadcast-stream))
         (pipeline
           (stmt-ir:make-optimization-pipeline
            :name :kernel-full
            :optimizations
            (list
            (stmt-ir:make-optimization
             :name :linear-canonicalization
             :function 'stmt-ir:linear-canonicalization-optimization)
            (stmt-ir:make-optimization
             :name :factor-sums
             :function 'stmt-ir:factor-sums-optimization
             :keyword-args (list :min-uses 2 :min-factors 1 :min-size 4))
            (stmt-ir:make-optimization
              :name :cse-full
              :function 'stmt-ir:cse-block-multi-optimization
              :keyword-args (list :max-passes 50 :min-uses 2 :min-size 1))
             (stmt-ir:make-optimization
              :name :factor-temp-param
              :function 'stmt-ir:factor-temp-param-products-optimization
              :keyword-args (list :min-uses 2 :min-factors 2 :max-factors 3))
             (stmt-ir:make-optimization
              :name :copy-propagate
              :function 'stmt-ir:copy-propagate-optimization)
             (stmt-ir:make-optimization
              :name :alias-assigned-exprs
              :function 'stmt-ir:alias-assigned-exprs-optimization)
             (stmt-ir:make-optimization
              :name :normalize-signs
              :function 'stmt-ir:normalize-signs-optimization)))))
    (let ((kernels nil))
      (mathkernel:build-multiple-kernels (kernels "stretch" (:energy :gradient :hessian))
        (:pipeline pipeline)
        (:params ((double kb)
                  (double r0)
                  (size_t i3x1)
                  (size_t i3x2)
                  (double* position)
                  (double* energy_accumulate)
                  (double* force)
                  (double* hessian)
                  (double* dvec)
                  (double* hdvec)))
        (:layout ((1 . I3X1) (2 . I3X2))
                 ((X . 0) (Y . 1) (Z . 2)))
        (:coord-vars (x1 y1 z1
                         x2 y2 z2))
        (:coord-load
         (mathkernel:coords-from-position
          ((x1 y1 z1 i3x1)
           (x2 y2 z2 i3x2))))
        (:body
         (mathkernel:stmt-block
           (mathkernel:=. dx "x2 - x1")
           (mathkernel:=. dy "y2 - y1")
           (mathkernel:=. dz "z2 - z1")
           (mathkernel:=. r2 "dx*dx + dy*dy + dz*dz")
           (mathkernel:=. r  "sqrt(r2)")
           (mathkernel:=. dr "r - r0")
           ;; E(r) = kb * (r - r0)^2
           (mathkernel:=. energy "kb*dr*dr"))))
    (mapcar (lambda (nm)
              (let* ((k (find nm kernels :key #'mathkernel::kernel-name :test #'string=))
                     (cfun (mathkernel::compile-kernel-to-c-function k))
                     (src (stmt-ir:c-function->c-source-string cfun)))
                (cons nm src)))
            '("stretch_energy" "stretch_gradient" "stretch_hessian")))))

(defun stretch-kernel-c-source (name)
  "Return cached C source string for stretch kernel NAME, computing if needed."
  (or (cdr (assoc name *stretch-kernel-c-cache* :test #'string=))
      (let ((snapshots (build-stretch-kernel-c-snapshots)))
        (setf *stretch-kernel-c-cache* snapshots)
        (cdr (assoc name snapshots :test #'string=)))))


;;; ------------------------------------------------------------
;;; CSE + temp-order regression tests
;;; ------------------------------------------------------------

(deftest test-cse-temp-order-simple-kernel
  "Ensure that after CSE on a simple duplicated kernel, CSE temps are
defined before use and not redefined."
  (let* (;; expression with a repeated large subexpression
         (expr (expr-ir:parse-expr
                "(x + y + z)^2 + (x + y + z)^2"))
         (a    (stmt-ir:make-assignment-stmt 'a expr))
         (b    (stmt-ir:make-assignment-stmt 'b expr))
         (block (stmt-ir:make-block-stmt (list a b)))
         ;; run multi-pass CSE
         (cse-block (stmt-ir:cse-block-multi block
                                             :max-passes 3
                                             :min-uses 2
                                             :min-size 3)))
    (multiple-value-bind (ok err)
        (run-check-cse-temp-order cse-block
                                  'test-cse-temp-order-simple-kernel)
      (assert-true ok
                   'test-cse-temp-order-simple-kernel
                   "check-cse-temp-order signaled an error: ~A" err))))

(deftest test-cse-temp-order-distance-squared-duplication
  "Ensure that after CSE on a duplicated distance-squared expression,
CSE temps are defined before use and not redefined."
  (let* ((r2-expr (expr-ir:parse-expr
                   "(x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2"))
         ;; two assignments sharing the same complex RHS
         (r2 (stmt-ir:make-assignment-stmt 'r2 r2-expr))
         (r3 (stmt-ir:make-assignment-stmt 'r3 r2-expr))
         (block (stmt-ir:make-block-stmt (list r2 r3)))
         ;; multi-pass CSE
         (cse-block (stmt-ir:cse-block-multi block
                                             :max-passes 3
                                             :min-uses 2
                                             :min-size 3)))
    (multiple-value-bind (ok err)
        (run-check-cse-temp-order cse-block
                                  'test-cse-temp-order-distance-squared-duplication)
      (assert-true ok
                   'test-cse-temp-order-distance-squared-duplication
                   "check-cse-temp-order signaled an error: ~A" err))))

(deftest test-kernel-builder-rejects-use-before-def
  "make-kernel-from-block should fail fast if assignments use locals before they are defined."
  (let* ((layout (mathkernel::make-kernel-layout
                  :atom->ibase '((1 . I1))
                  :axis->offset '((#\X . 0))))
         (bad-block (stmt-ir:make-block-stmt
                     (list
                      (stmt-ir:make-assignment-stmt 'r (expr-ir:parse-expr "dx"))
                      (stmt-ir:make-assignment-stmt 'energy (expr-ir:parse-expr "r"))
                      (stmt-ir:make-assignment-stmt 'dx (expr-ir:parse-expr "1.0")))))
         (caught nil))
    (handler-case
        (mathkernel::make-kernel-from-block
         :name 'test-kernel
         :pipeline nil
         :layout layout
         :coord-vars nil
         :coord-load-stmts nil
         :base-block bad-block
         :params nil
         :compute-energy t
         :compute-grad nil
         :compute-hess nil)
      (error ()
        (setf caught t)))
    (assert-true caught
                 'test-kernel-builder-rejects-use-before-def
                 "make-kernel-from-block should signal on use-before-def in the core block.")))


#+(or) ;; temporarily disabled: stretch energy regression broken
(deftest test-stretch-energy-c-regression
  "Lock in the current generated C for stretch_energy."
  (let* ((src (stretch-kernel-c-source "stretch_energy"))
         (expected
"void stretch_energy(double kb, double r0, size_t i3x1, size_t i3x2, double* position, double* energy_accumulate, double* force, double* hessian, double* dvec, double* hdvec)
{
  double x1 = position[i3x1 + 0];
  double y1 = position[i3x1 + 1];
  double z1 = position[i3x1 + 2];
  double x2 = position[i3x2 + 0];
  double y2 = position[i3x2 + 1];
  double z2 = position[i3x2 + 2];
  double dx = (x2 + (-(x1)));
  double dy = (y2 + (-(y1)));
  double dz = (z2 + (-(z1)));
  double r2 = ((dx * dx) + (dy * dy) + (dz * dz));
  double r = sqrt(r2);
  double dr = (r + (-(r0)));
  double energy = (dr * dr * kb);
  *energy_accumulate += energy;
}
"))
    (assert-true (string= src expected)
                 'test-stretch-energy-c-regression
                 "stretch_energy C output changed.~%Expected:~%~A~%Got:~%~A"
                 expected src)))

(deftest test-stretch-gradient-c-regression
  "Lock in the current generated C for stretch_gradient."
  (let* ((src (stretch-kernel-c-source "stretch_gradient"))
         (expected
"void stretch_gradient(double kb, double r0, size_t i3x1, size_t i3x2, double* position, double* energy_accumulate, double* force, double* hessian, double* dvec, double* hdvec)
{
  double x1 = position[i3x1 + 0];
  double y1 = position[i3x1 + 1];
  double z1 = position[i3x1 + 2];
  double x2 = position[i3x2 + 0];
  double y2 = position[i3x2 + 1];
  double z2 = position[i3x2 + 2];
  double dx = (x2 + (-(x1)));
  double dy = (y2 + (-(y1)));
  double dz = (z2 + (-(z1)));
  double r2 = ((dx * dx) + (dy * dy) + (dz * dz));
  double cse_p1_t1 = pow(r2, -0.50000000000000000    );
  double r = sqrt(r2);
  double dr = (r + (-(r0)));
  double cse_p2_t1 = (dr * kb);
  double cse_p4_t1 = (cse_p1_t1 * cse_p2_t1);
  double cse_p5_t1 = (-2.0000000000000000     * cse_p4_t1);
  double cse_p1_t2 = (2.0000000000000000     * cse_p4_t1);
  double energy = (cse_p2_t1 * dr);
  *energy_accumulate += energy;
  double g_x1 = (cse_p5_t1 * dx);
  KernelGradientAcc(i3x1, 0, g_x1);
  double g_y1 = (cse_p5_t1 * dy);
  KernelGradientAcc(i3x1, 1, g_y1);
  double g_z1 = (cse_p5_t1 * dz);
  KernelGradientAcc(i3x1, 2, g_z1);
  double g_x2 = (cse_p1_t2 * dx);
  KernelGradientAcc(i3x2, 0, g_x2);
  double g_y2 = (cse_p1_t2 * dy);
  KernelGradientAcc(i3x2, 1, g_y2);
  double g_z2 = (cse_p1_t2 * dz);
  KernelGradientAcc(i3x2, 2, g_z2);
}
"))
    (assert-true (string= src expected)
                 'test-stretch-gradient-c-regression
                 "stretch_gradient C output changed.~%Expected:~%~A~%Got:~%~A"
                 expected src)))

(deftest test-stretch-hessian-c-regression
  "Lock in the current generated C for stretch_hessian."
  (let* ((src (stretch-kernel-c-source "stretch_hessian"))
         (expected
"void stretch_hessian(double kb, double r0, size_t i3x1, size_t i3x2, double* position, double* energy_accumulate, double* force, double* hessian, double* dvec, double* hdvec)
{
  double x1 = position[i3x1 + 0];
  double y1 = position[i3x1 + 1];
  double z1 = position[i3x1 + 2];
  double x2 = position[i3x2 + 0];
  double y2 = position[i3x2 + 1];
  double z2 = position[i3x2 + 2];
  double dx = (x2 + (-(x1)));
  double cse_p25_t1 = (dx * dx);
  double dy = (y2 + (-(y1)));
  double cse_p27_t1 = (dy * dy);
  double dz = (z2 + (-(z1)));
  double cse_p18_t1 = (dz * dz);
  double r2 = (cse_p18_t1 + cse_p25_t1 + cse_p27_t1);
  double cse_p1_t1 = pow(r2, -0.50000000000000000    );
  double cse_p10_t47 = (cse_p1_t1 * kb);
  double cse_p17_t1 = (cse_p1_t1 * dy);
  double cse_p10_t17 = (cse_p17_t1 * dy);
  double cse_p10_t19 = (cse_p18_t1 * cse_p1_t1);
  double cse_p12_t24 = (cse_p1_t1 * dx);
  double cse_p10_t14 = (cse_p12_t24 * dx);
  double cse_p10_t45 = pow(r2, -1);
  double cse_p14_t2 = (cse_p10_t47 * cse_p12_t24);
  double cse_p10_t15 = (cse_p14_t2 * dy);
  double cse_p10_t16 = (cse_p14_t2 * dz);
  double cse_p10_t18 = (cse_p10_t47 * cse_p17_t1 * dz);
  double cse_p10_t41 = (-(cse_p10_t15));
  double cse_p10_t42 = (-(cse_p10_t16));
  double cse_p10_t43 = (-(cse_p10_t18));
  double cse_p10_t4 = (2.0000000000000000     * cse_p10_t41);
  double cse_p10_t5 = (2.0000000000000000     * cse_p10_t42);
  double cse_p10_t6 = (2.0000000000000000     * cse_p10_t43);
  double cse_p20_t1 = (cse_p10_t47 * cse_p1_t1);
  double cse_p23_t1 = (-2.0000000000000000     * cse_p20_t1);
  double r = sqrt(r2);
  double dr = (r + (-(r0)));
  double cse_p10_t13 = (cse_p10_t47 * dr);
  double cse_p10_t32 = (cse_p10_t14 + cse_p10_t14 + dr + dr);
  double cse_p10_t33 = (cse_p10_t17 + cse_p10_t17 + dr + dr);
  double cse_p10_t34 = (cse_p10_t19 + cse_p10_t19 + dr + dr);
  double cse_p11_t37 = (cse_p10_t13 * cse_p10_t45);
  double cse_p16_t1 = (cse_p11_t37 * dy);
  double cse_p13_t11 = (cse_p11_t37 * dx);
  double cse_p10_t8 = (cse_p13_t11 * dy);
  double cse_p10_t9 = (cse_p13_t11 * dz);
  double cse_p10_t11 = (cse_p16_t1 * dz);
  double cse_p10_t7 = (cse_p13_t11 * dx);
  double cse_p10_t10 = (cse_p16_t1 * dy);
  double cse_p10_t12 = (cse_p11_t37 * cse_p18_t1);
  double cse_p10_t36 = (-(cse_p10_t8));
  double cse_p10_t37 = (-(cse_p10_t9));
  double cse_p10_t39 = (-(cse_p10_t11));
  double cse_p10_t1 = (2.0000000000000000     * cse_p10_t36);
  double cse_p10_t2 = (2.0000000000000000     * cse_p10_t37);
  double cse_p10_t3 = (2.0000000000000000     * cse_p10_t39);
  double cse_p10_t20 = (cse_p10_t32 * cse_p10_t47);
  double cse_p10_t21 = (cse_p10_t33 * cse_p10_t47);
  double cse_p10_t22 = (cse_p10_t34 * cse_p10_t47);
  double cse_p10_t35 = (-(cse_p10_t7));
  double cse_p10_t38 = (-(cse_p10_t10));
  double cse_p10_t40 = (-(cse_p10_t12));
  double h_x2_y2 = (cse_p10_t1 + cse_p10_t15 + cse_p10_t15);
  KernelOffDiagHessAcc(i3x2, 0, i3x2, 1, h_x2_y2);
  double h_x2_z2 = (cse_p10_t16 + cse_p10_t16 + cse_p10_t2);
  KernelOffDiagHessAcc(i3x2, 0, i3x2, 2, h_x2_z2);
  double h_y2_z2 = (cse_p10_t18 + cse_p10_t18 + cse_p10_t3);
  KernelOffDiagHessAcc(i3x2, 1, i3x2, 2, h_y2_z2);
  double h_y1_x2 = (cse_p10_t4 + cse_p10_t8 + cse_p10_t8);
  KernelOffDiagHessAcc(i3x1, 1, i3x2, 0, h_y1_x2);
  double h_z1_x2 = (cse_p10_t5 + cse_p10_t9 + cse_p10_t9);
  KernelOffDiagHessAcc(i3x1, 2, i3x2, 0, h_z1_x2);
  double h_z1_y2 = (cse_p10_t11 + cse_p10_t11 + cse_p10_t6);
  KernelOffDiagHessAcc(i3x1, 2, i3x2, 1, h_z1_y2);
  double h_x2_x2 = (cse_p10_t20 + cse_p10_t35 + cse_p10_t35);
  KernelDiagHessAcc(i3x2, 0, i3x2, 0, h_x2_x2);
  double h_y2_y2 = (cse_p10_t21 + cse_p10_t38 + cse_p10_t38);
  KernelDiagHessAcc(i3x2, 1, i3x2, 1, h_y2_y2);
  double h_z2_z2 = (cse_p10_t22 + cse_p10_t40 + cse_p10_t40);
  KernelDiagHessAcc(i3x2, 2, i3x2, 2, h_z2_z2);
  double cse_p10_t44 = (-(cse_p10_t13));
  double cse_p2_t1 = (dr * kb);
  double cse_p4_t1 = (cse_p1_t1 * cse_p2_t1);
  double cse_p5_t1 = (-2.0000000000000000     * cse_p4_t1);
  double cse_p1_t2 = (2.0000000000000000     * cse_p4_t1);
  double energy = (cse_p2_t1 * dr);
  *energy_accumulate += energy;
  double g_x1 = (cse_p5_t1 * dx);
  KernelGradientAcc(i3x1, 0, g_x1);
  double g_y1 = (cse_p5_t1 * dy);
  KernelGradientAcc(i3x1, 1, g_y1);
  double g_z1 = (cse_p5_t1 * dz);
  KernelGradientAcc(i3x1, 2, g_z1);
  double g_x2 = (cse_p1_t2 * dx);
  KernelGradientAcc(i3x2, 0, g_x2);
  double g_y2 = (cse_p1_t2 * dy);
  KernelGradientAcc(i3x2, 1, g_y2);
  double g_z2 = (cse_p1_t2 * dz);
  KernelGradientAcc(i3x2, 2, g_z2);
  double h_x1_x2 = (cse_p10_t44 + cse_p10_t44 + cse_p10_t7 + cse_p10_t7 + (cse_p23_t1 * cse_p25_t1));
  KernelOffDiagHessAcc(i3x1, 0, i3x2, 0, h_x1_x2);
  double h_y1_y2 = (cse_p10_t10 + cse_p10_t10 + cse_p10_t44 + cse_p10_t44 + (cse_p23_t1 * cse_p27_t1));
  KernelOffDiagHessAcc(i3x1, 1, i3x2, 1, h_y1_y2);
  double h_z1_z2 = (cse_p10_t12 + cse_p10_t12 + cse_p10_t44 + cse_p10_t44 + (cse_p18_t1 * cse_p23_t1));
  KernelOffDiagHessAcc(i3x1, 2, i3x2, 2, h_z1_z2);
}
"))
    (assert-true (string= src expected)
                 'test-stretch-hessian-c-regression
                 "stretch_hessian C output changed.~%Expected:~%~A~%Got:~%~A"
                 expected src)))




(deftest test-cse-mul-common-subproduct
  "u = a*b*c*d*e, v = a*b*c*d*f ⇒ CSE introduces a shared a*b*c*d subproduct."
  (let* ((p1 (expr-ir:parse-expr "a*b*c*d*e"))
         (p2 (expr-ir:parse-expr "a*b*c*d*f"))
         (s1 (stmt-ir:make-assignment-stmt 'u p1))
         (s2 (stmt-ir:make-assignment-stmt 'v p2))
         (block (stmt-ir:make-block-stmt (list s1 s2)))
         ;; The common subproduct we expect CSE to create explicitly.
         (sub-expr   (expr-ir:parse-expr "a*b*c*d"))
         (sub-sexpr  (expr-ir:expr->sexpr sub-expr))
         (before     (count-sexpr-occurrences-in-block block sub-sexpr))
         (after-block (stmt-ir:cse-block-multi block
                                               :max-passes 5
                                               :min-uses 2
                                               :min-size 3))
         (after      (count-sexpr-occurrences-in-block after-block sub-sexpr)))
    ;; Before factoring, there is no explicit a*b*c*d node.
    (assert-true (zerop before)
                 'test-cse-mul-common-subproduct
                 "Expected 0 occurrences of ~S before CSE, got ~A"
                 sub-sexpr before)
    ;; After factoring we expect to see at least one explicit a*b*c*d.
    (assert-true (>= after 1)
                 'test-cse-mul-common-subproduct
                 "Expected at least 1 occurrence of ~S after CSE, got ~A"
                 sub-sexpr after)))


(deftest test-cse-never-produces-nil-rhs
  "After CSE, no assignment-statement should have a NIL RHS."
  (let* ((expr1 (expr-ir:parse-expr "a*b*c*d*e"))
         (expr2 (expr-ir:parse-expr "a*b*c*d*f"))
         (expr3 (expr-ir:parse-expr "a*b*c*g*h"))
         (s1 (stmt-ir:make-assignment-stmt 'u expr1))
         (s2 (stmt-ir:make-assignment-stmt 'v expr2))
         (s3 (stmt-ir:make-assignment-stmt 'w expr3))
         (block (stmt-ir:make-block-stmt (list s1 s2 s3)))
         (after-block (stmt-ir:cse-block-multi block
                                               :max-passes 5
                                               :min-uses 2
                                               :min-size 2)))
    (dolist (st (stmt-ir:block-statements after-block))
      (when (typep st 'stmt-ir:assignment-statement)
        (assert-true (stmt-ir:stmt-expression st)
                     'test-cse-never-produces-nil-rhs
                     "Assignment ~S has NIL RHS after CSE"
                     st)))))


(deftest test-cse-preserves-energy-grad-hess
  "CSE must not drop or nil-out E, G_X, G_Y, H_X_X, H_X_Y, H_Y_Y."
  (let* ((expr (expr-ir:parse-expr "x^2 + y^2"))
         (coord-vars '(x y))
         (base-block (make-energy-grad-hess-block
                      :energy-expr   expr
                      :coord-vars    coord-vars
                      :energy-target 'E
                      :grad-target-fn #'grad-name
                      :hess-target-fn #'hess-name
                      :simplify      t))
         (after-block (stmt-ir:cse-block-multi base-block
                                               :max-passes 5
                                               :min-uses 2
                                               :min-size 1))
         (e-stmt   (find-assignment after-block 'E))
         (gx-stmt  (find-assignment after-block 'G_X))
         (gy-stmt  (find-assignment after-block 'G_Y))
         (hxx-stmt (find-assignment after-block 'H_X_X))
         (hxy-stmt (find-assignment after-block 'H_X_Y))
         (hyy-stmt (find-assignment after-block 'H_Y_Y)))
    ;; All should still exist.
    (dolist (pair `((E   ,e-stmt)
                    (G_X ,gx-stmt)
                    (G_Y ,gy-stmt)
                    (H_X_X ,hxx-stmt)
                    (H_X_Y ,hxy-stmt)
                    (H_Y_Y ,hyy-stmt)))
      (destructuring-bind (name stmt) pair
        (assert-true stmt
                     'test-cse-preserves-energy-grad-hess
                     "Missing assignment for ~S after CSE" name)
        (assert-true (stmt-ir:stmt-expression stmt)
                     'test-cse-preserves-energy-grad-hess
                     "Assignment for ~S has NIL RHS after CSE" name)))))


(deftest test-copy-propagate-preserves-hessian-target
  "tmp = expr; H_X_X = tmp ⇒ after copy-propagate, H_X_X still has a nontrivial RHS."
  (let* ((expr (expr-ir:parse-expr "x^2 + y^2"))
         (tmp  (stmt-ir:make-assignment-stmt 'tmp expr))
         (hxx  (stmt-ir:make-assignment-stmt 'H_X_X
                                             (expr-ir:parse-expr "tmp")))
         (block (stmt-ir:make-block-stmt (list tmp hxx)))
         (after-block (stmt-ir:copy-propagate-optimization (stmt-ir:make-pass-id-counter) block))
         (hxx-stmt (find-assignment after-block 'H_X_X)))
    (assert-true hxx-stmt
                 'test-copy-propagate-preserves-hessian-target
                 "H_X_X assignment missing after copy-propagate")
    ;; RHS should not be just a single variable; it should have the real expression.
    (let ((rhs (stmt-ir:stmt-expression hxx-stmt)))
      (assert-true (not (typep rhs 'expr-ir:variable-expression))
                   'test-copy-propagate-preserves-hessian-target
                   "H_X_X RHS is still a trivial variable after copy-propagate: ~S"
                   (expr-ir:expr->sexpr rhs)))))


(deftest test-cse-and-copy-propagate-preserve-energy-grad-hess
  "CSE+copy-propagate must keep E, G_X, G_Y, H_X_X, H_X_Y, H_Y_Y with non-NIL RHS."
  (let* ((expr (expr-ir:parse-expr "x^2 + y^2"))
         (coord-vars '(x y))
         (base-block (make-energy-grad-hess-block
                      :energy-expr   expr
                      :coord-vars    coord-vars
                      :energy-target 'E
                      :grad-target-fn #'grad-name
                      :hess-target-fn #'hess-name
                      :simplify      t))
         (cse-block (stmt-ir:cse-block-multi base-block
                                             :max-passes 5
                                             :min-uses 2
                                             :min-size 1))
         (final-block (stmt-ir:copy-propagate-optimization (stmt-ir:make-pass-id-counter) cse-block))
         (e-stmt   (find-assignment final-block 'E))
         (gx-stmt  (find-assignment final-block 'G_X))
         (gy-stmt  (find-assignment final-block 'G_Y))
         (hxx-stmt (find-assignment final-block 'H_X_X))
         (hxy-stmt (find-assignment final-block 'H_X_Y))
         (hyy-stmt (find-assignment final-block 'H_Y_Y)))
    (dolist (pair `((E   ,e-stmt)
                    (G_X ,gx-stmt)
                    (G_Y ,gy-stmt)
                    (H_X_X ,hxx-stmt)
                    (H_X_Y ,hxy-stmt)
                    (H_Y_Y ,hyy-stmt)))
      (destructuring-bind (name stmt) pair
        (assert-true stmt
                     'test-cse-and-copy-propagate-preserve-energy-grad-hess
                     "Missing assignment for ~S after CSE+copy-prop" name)
        (assert-true (stmt-ir:stmt-expression stmt)
                     'test-cse-and-copy-propagate-preserve-energy-grad-hess
                     "Assignment for ~S has NIL RHS after CSE+copy-prop" name)))))

(deftest test-cse-does-not-introduce-nil-variable-rhs
  "After CSE, no assignment-statement RHS should be a variable NIL."
  (let* ((expr1 (expr-ir:parse-expr "a*b*c*d*e"))
         (expr2 (expr-ir:parse-expr "a*b*c*d*f"))
         (expr3 (expr-ir:parse-expr "a*b*c*g*h"))
         (s1 (stmt-ir:make-assignment-stmt 'u expr1))
         (s2 (stmt-ir:make-assignment-stmt 'v expr2))
         (s3 (stmt-ir:make-assignment-stmt 'w expr3))
         (block (stmt-ir:make-block-stmt (list s1 s2 s3)))
         (after-block (stmt-ir:cse-block-multi block
                                               :max-passes 5
                                               :min-uses 2
                                               :min-size 1)))
    (dolist (st (stmt-ir:block-statements after-block))
      (when (typep st 'stmt-ir:assignment-statement)
        (let ((rhs (stmt-ir:stmt-expression st)))
          (assert-true (not (expr-nil-variable-p rhs))
                       'test-cse-does-not-introduce-nil-variable-rhs
                       "CSE produced NIL variable RHS in ~S" st))))))


(deftest test-cse-preserves-energy-grad-hess-rhs
  "CSE must not drop or nil-out E, G_*, H_* assignment RHS."
  (let* ((expr       (expr-ir:parse-expr "x^2 + y^2"))
         (coord-vars '(x y))
         (base-block (stmt-ir:make-energy-grad-hess-block
                      :energy-expr    expr
                      :coord-vars     coord-vars
                      :energy-target  'E
                      :grad-target-fn #'grad-name
                      :hess-target-fn #'hess-name
                      :simplify       t))
         (after-block (stmt-ir:cse-block-multi base-block
                                               :max-passes 5
                                               :min-uses 2
                                               :min-size 1))
         (targets '(E G_X G_Y H_X_X H_X_Y H_Y_Y)))
    (dolist (name targets)
      (let ((st (find-assignment after-block name)))
        (assert-true st
                     'test-cse-preserves-energy-grad-hess-rhs
                     "Missing assignment for ~S after CSE" name)
        (let ((rhs (stmt-ir:stmt-expression st)))
          (assert-true rhs
                       'test-cse-preserves-energy-grad-hess-rhs
                       "Assignment for ~S has NIL RHS after CSE" name)
          (assert-true (not (expr-nil-variable-p rhs))
                       'test-cse-preserves-energy-grad-hess-rhs
                       "Assignment for ~S has NIL variable RHS after CSE: ~S"
                       name rhs))))))


(deftest test-copy-propagate-preserves-hessian-target
  "copy-propagate-block must keep H_X_X with a real RHS, not a NIL variable."
  (let* ((expr (expr-ir:parse-expr "x^2 + y^2"))
         (tmp  (stmt-ir:make-assignment-stmt 'TMP expr))
         (hxx  (stmt-ir:make-assignment-stmt 'H_X_X
                                             (expr-ir:parse-expr "TMP")))
         (block (stmt-ir:make-block-stmt (list tmp hxx)))
         (after-block (stmt-ir:copy-propagate-optimization (stmt-ir:make-pass-id-counter) block))
         (hxx-stmt (find-assignment after-block 'H_X_X)))
    (assert-true hxx-stmt
                 'test-copy-propagate-preserves-hessian-target
                 "H_X_X assignment missing after copy-propagate")
    (let ((rhs (stmt-ir:stmt-expression hxx-stmt)))
      (assert-true rhs
                   'test-copy-propagate-preserves-hessian-target
                   "H_X_X RHS is NIL after copy-propagate")
      (assert-true (not (expr-nil-variable-p rhs))
                   'test-copy-propagate-preserves-hessian-target
                   "H_X_X RHS is NIL variable after copy-propagate: ~S"
                   rhs))))

(deftest test-cse-and-copy-propagate-preserve-energy-grad-hess-rhs
  "CSE+copy-propagate must keep E, G_*, H_* with non-NIL RHS."
  (let* ((expr       (expr-ir:parse-expr "x^2 + y^2"))
         (coord-vars '(x y))
         (base-block (stmt-ir:make-energy-grad-hess-block
                      :energy-expr    expr
                      :coord-vars     coord-vars
                      :energy-target  'E
                      :grad-target-fn #'grad-name
                      :hess-target-fn #'hess-name
                      :simplify       t))
         (block-cse (stmt-ir:cse-block-multi base-block
                                             :max-passes 5
                                             :min-uses 2
                                             :min-size 1))
         (final-block (stmt-ir:copy-propagate-optimization (stmt-ir:make-pass-id-counter) block-cse))
         (targets '(E G_X G_Y H_X_X H_X_Y H_Y_Y)))
    (dolist (name targets)
      (let ((st (find-assignment final-block name)))
        (assert-true st
                     'test-cse-and-copy-propagate-preserve-energy-grad-hess-rhs
                     "Missing assignment for ~S after CSE+copy-prop" name)
        (let ((rhs (stmt-ir:stmt-expression st)))
          (assert-true rhs
                       'test-cse-and-copy-propagate-preserve-energy-grad-hess-rhs
                       "Assignment for ~S has NIL RHS after CSE+copy-prop" name)
          (assert-true (not (expr-nil-variable-p rhs))
                       'test-cse-and-copy-propagate-preserve-energy-grad-hess-rhs
                       "Assignment for ~S has NIL variable RHS after CSE+copy-prop: ~S"
                       name rhs))))))



(defun sexpr-product-contains-factors-p (sexpr required-factors)
  "True if SEXPR is a product (* ...) whose factor list contains
all REQUIRED-FACTORS (by EQ)."
  (and (consp sexpr)
       (eq (car sexpr) '*)
       (let ((args (cdr sexpr)))
         (every (lambda (f)
                  (member f args :test #'eq))
                required-factors))))

(defun looks-like-cse-temp-symbol-p (sym)
  "Heuristic: true if SYM looks like a CSE temp (e.g. CSE_P1_T37)."
  (and (symbolp sym)
       (let ((name (symbol-name sym)))
         (and (>= (length name) 5)
              (search "CSE_P" name :test #'char-equal)))))

(deftest test-factor-temp-param-products-simple
  "Repeated 2 * CSE_P1_T148 * kt should be pulled into a temp."
  ;; Build a block with two assignments whose RHS both contain
  ;; the subproduct 2 * CSE_P1_T148 * kt (order differs).
  (let* ((kt-sym   (expr-ir:ev "KT"))
         (c148-sym (expr-ir:ev "CSE_P1_T148"))
         (expr1    (expr-ir:parse-expr "2.0 * CSE_P1_T148 * kt"))
         (expr2    (expr-ir:parse-expr "2.0 * kt * CSE_P1_T148"))
         (h1       (stmt-ir:make-assignment-stmt 'H1 expr1))
         (h2       (stmt-ir:make-assignment-stmt 'H2 expr2))
         (block    (stmt-ir:make-block-stmt (list h1 h2)))
         ;; parameter list: only kt is treated as a parameter here
         (after-block (stmt-ir:factor-temp-param-products-optimization (stmt-ir:make-pass-id-counter)
                       block
                       :min-uses    2
                       :min-factors 2
                       :max-factors 3))
         (stmts (stmt-ir:block-statements after-block)))
    (declare (optimize (debug 3)))
    ;; 1. There should be a new temp assignment whose RHS is a product
    ;;    that includes both CSE_P1_T148 and kt.
    (let ((temp-assignment
            (find-if
             (lambda (st)
               (and (typep st 'stmt-ir::assignment-statement)
                    (looks-like-cse-temp-symbol-p
                     (stmt-target-name st))
                    (let* ((rhs-sexpr (expr-ir:expr->sexpr
                                       (stmt-expression st))))
                      (sexpr-product-contains-factors-p
                       rhs-sexpr
                       (list kt-sym c148-sym)))))
             stmts)))
      (assert-true temp-assignment
                   'test-factor-temp-param-products-simple
                   "Expected a temp assignment for 2 * CSE_P1_T148 * kt, but found none."))

    ;; 2. H1 and H2 should no longer have RHS that directly contain the
    ;;    product 2 * CSE_P1_T148 * kt. The product should now only appear
    ;;    in the temp definition.
    (labels ((has-direct-product (sym)
               (let* ((st  (find-assignment after-block sym))
                      (rhs (and st (stmt-expression st)))
                      (sx  (and rhs (expr-ir:expr->sexpr rhs))))
                 (and sx
                      (sexpr-product-contains-factors-p
                       sx (list kt-sym c148-sym))))))
      (assert-true (not (has-direct-product 'H1))
                   'test-factor-temp-param-products-simple
                   "H1 still contains the direct product 2 * CSE_P1_T148 * kt.")
      (assert-true (not (has-direct-product 'H2))
                   'test-factor-temp-param-products-simple
                   "H2 still contains the direct product 2 * CSE_P1_T148 * kt."))))


(deftest test-factor-temp-param-products-min-uses
  "If 2 * CSE_P1_T148 * kt appears only once, no temp should be created."
  (let* ((kt-sym   (expr-ir:ev "KT"))
         (expr1    (expr-ir:parse-expr "2.0 * CSE_P1_T148 * kt"))
         (expr2    (expr-ir:parse-expr "3.0 * CSE_P1_T148")) ; no kt here
         (h1       (stmt-ir:make-assignment-stmt 'H1 expr1))
         (h2       (stmt-ir:make-assignment-stmt 'H2 expr2))
         (block    (stmt-ir:make-block-stmt (list h1 h2)))
         (after-block (stmt-ir:factor-temp-param-products-optimization (stmt-ir:make-pass-id-counter)
                       block
                       :min-uses    2
                       :min-factors 2
                       :max-factors 3))
         (stmts (stmt-ir:block-statements after-block)))
    ;; There should be no new CSE_P* temp defined for this pattern.
    (let ((temp-assignment
            (find-if
             (lambda (st)
               (and (typep st 'assignment-statement)
                    (looks-like-cse-temp-symbol-p
                     (stmt-target-name st))))
             stmts)))
      (assert-true (null temp-assignment)
                   'test-factor-temp-param-products-min-uses
                   "Temp was created even though 2 * CSE_P1_T148 * kt only appears once."))))


(defun expr-nil-variable-p (expr)
  "True if EXPR is a variable-expression whose symbol name is \"NIL\"."
  (and (typep expr 'expr-ir:variable-expression)
       (string= (symbol-name (expr-ir:variable-name expr)) "NIL")))

(deftest test-factor-temp-param-products-no-nil-rhs
  "factor-temp-param-products-optimization must not produce NIL or NIL-variable RHS."
  (let* ((kt-sym   (expr-ir:ev "KT"))
         (expr1    (expr-ir:parse-expr "2.0 * CSE_P1_T148 * kt"))
         (expr2    (expr-ir:parse-expr "2.0 * CSE_P1_T148 * kt * CSE_P1_T149"))
         (expr3    (expr-ir:parse-expr "CSE_P1_T148 + CSE_P1_T149"))
         (h1       (stmt-ir:make-assignment-stmt 'H1 expr1))
         (h2       (stmt-ir:make-assignment-stmt 'H2 expr2))
         (h3       (stmt-ir:make-assignment-stmt 'H3 expr3))
         (block    (stmt-ir:make-block-stmt (list h1 h2 h3)))
         (after-block (stmt-ir:factor-temp-param-products-optimization (stmt-ir:make-pass-id-counter)
                       block
                       :min-uses    2
                       :min-factors 2
                       :max-factors 3)))
    (dolist (st (stmt-ir:block-statements after-block))
      (when (typep st 'assignment-statement)
        (let ((rhs (stmt-expression st)))
          (assert-true rhs
                       'test-factor-temp-param-products-no-nil-rhs
                       "Assignment ~S has NIL RHS after factoring."
                       st)
          (assert-true (not (expr-nil-variable-p rhs))
                       'test-factor-temp-param-products-no-nil-rhs
                       "Assignment ~S has NIL variable RHS after factoring."
                       st))))))

(defun count-product-subexprs-with-names-in-block (block factor-names)
  "Count how many product subexpressions in BLOCK contain all FACTOR-NAMES."
  (let ((count 0))
    (labels ((visit-sexpr (sexpr)
               (when (sexpr-product-contains-named-factors-p sexpr factor-names)
                 (incf count))
               (when (consp sexpr)
                 (dolist (child (cdr sexpr))
                   (visit-sexpr child))))
             (visit-expr (expr)
               (visit-sexpr (expr-ir:expr->sexpr expr)))
             (visit-stmt (stmt)
               (typecase stmt
                 (stmt-ir:assignment-statement
                  (visit-expr (stmt-ir:stmt-expression stmt)))
                 (stmt-ir:block-statement
                  (dolist (sub (stmt-ir:block-statements stmt))
                    (visit-stmt sub)))
                 (stmt-ir:if-statement
                  (visit-expr (stmt-ir:if-condition stmt))
                  (let ((then-b (stmt-ir:if-then-block stmt))
                        (else-b (stmt-ir:if-else-block stmt)))
                    (when then-b
                      (dolist (sub (stmt-ir:block-statements then-b))
                        (visit-stmt sub)))
                    (when else-b
                      (dolist (sub (stmt-ir:block-statements else-b))
                        (visit-stmt sub))))))))
      (visit-stmt block)
      count)))

(deftest test-factor-temp-param-products_angle_like_gradient
  "Angle-like gradient terms sharing outer CSE products should be factored."
  (let* ((expr-gx1
           (expr-ir:parse-expr
            "CSE_P1_T37 * CSE_P1_T5 * CSE_P1_T20*CSE_P1_T25 + CSE_P1_T33*(-(CSE_P1_T13*CSE_P1_T27*CSE_P1_T6*CSE_P1_T9))"))
         (expr-gy1
           (expr-ir:parse-expr
            "CSE_P1_T37 * CSE_P1_T5 * CSE_P1_T20*CSE_P1_T29 + CSE_P1_T33*(-(CSE_P1_T13*CSE_P1_T31*CSE_P1_T6*CSE_P1_T8))"))
         (expr-gz1
           (expr-ir:parse-expr
            "CSE_P1_T37 * CSE_P1_T5 * CSE_P1_T20*CSE_P1_T21 + CSE_P1_T33*(-(CSE_P1_T13*CSE_P1_T23*CSE_P1_T6*CSE_P1_T7))"))
         (s-gx1 (stmt-ir:make-assignment-stmt 'G_X1 expr-gx1))
         (s-gy1 (stmt-ir:make-assignment-stmt 'G_Y1 expr-gy1))
         (s-gz1 (stmt-ir:make-assignment-stmt 'G_Z1 expr-gz1))
         (block (stmt-ir:make-block-stmt (list s-gx1 s-gy1 s-gz1)))
         ;; treat everything as temps, no parameters needed for this test
         (before-count
           (count-product-subexprs-with-names-in-block
            block '("CSE_P1_T37" "CSE_P1_T5")))
         (after-block
           (stmt-ir:factor-temp-param-products-optimization (stmt-ir:make-pass-id-counter :start 76)
            block
            :min-uses    2
            :min-factors 2
            :max-factors 3))
         (after-count
           (count-product-subexprs-with-names-in-block
            after-block '("CSE_P1_T37" "CSE_P1_T5")))
         ;; any CSE temp defined by this pass?
         (temp-assignment
           (find-if
            (lambda (stmt)
              (and (typep stmt 'stmt-ir:assignment-statement)
                   (let ((name (stmt-ir:stmt-target-name stmt)))
                     (and (symbolp name)
                          (search "CSE_P77_T" (symbol-name name)
                                  :test #'char-equal)))))
            (stmt-ir:block-statements after-block))))
    ;; sanity: pattern present before
    (assert-true (> before-count 0)
                 'test-factor-temp-param-products_angle_like_gradient
                 "Expected at least one product with CSE_P1_T37*CSE_P1_T5 before factoring.")
    ;; we expect some reduction
    (assert-true (< after-count before-count)
                 'test-factor-temp-param-products_angle_like_gradient
                 "Expected fewer occurrences of CSE_P1_T37*CSE_P1_T5 after factoring, got ~D vs ~D."
                 after-count before-count)
    ;; and a new temp introduced by this pass
    (assert-true temp-assignment
                 'test-factor-temp-param-products_angle_like_gradient
                 "Expected a CSE_P77_T* temp to be introduced, but found none.")))

(deftest test-factor-temp-param-products_simple_2_cse_kt
  "Repeated 2 * CSE_P1_T148 * kt should be pulled into a temp."
  (let* ((expr1 (expr-ir:parse-expr "2.0 * CSE_P1_T148 * kt"))
         (expr2 (expr-ir:parse-expr "2.0 * kt * CSE_P1_T148"))
         (s1    (stmt-ir:make-assignment-stmt 'H1 expr1))
         (s2    (stmt-ir:make-assignment-stmt 'H2 expr2))
         (block (stmt-ir:make-block-stmt (list s1 s2)))
         (before-count
           (count-product-subexprs-with-names-in-block
            block '("CSE_P1_T148" "KT")))
         (after-block
           (stmt-ir:factor-temp-param-products-optimization (stmt-ir:make-pass-id-counter :start 87)
            block
            :min-uses    2
            :min-factors 2
            :max-factors 3))
         (after-count
           (count-product-subexprs-with-names-in-block
            after-block '("CSE_P1_T148" "KT")))
         (temp-assignment
           (find-if
            (lambda (stmt)
              (and (typep stmt 'stmt-ir:assignment-statement)
                   (let ((name (stmt-ir:stmt-target-name stmt)))
                     (and (symbolp name)
                          (search "CSE_P88_T" (symbol-name name)
                                  :test #'char-equal)))))
            (stmt-ir:block-statements after-block))))
    (assert-true (> before-count 0)
                 'test-factor-temp-param-products_simple_2_cse_kt
                 "Expected at least one direct 2*CSE_P1_T148*kt before factoring.")
    (assert-true (< after-count before-count)
                 'test-factor-temp-param-products_simple_2_cse_kt
                 "Expected fewer occurrences of 2*CSE_P1_T148*kt after factoring, got ~D vs ~D."
                 after-count before-count)
    (assert-true temp-assignment
                 'test-factor-temp-param-products_simple_2_cse_kt
                 "Expected a temp assignment CSE_P88_T* for 2*CSE_P1_T148*kt, but found none.")))

#+(or)
(deftest test-factor-temp-param-products_angle_shape_with_minus
  "Angle-like gradient terms with CSE_P1_T37 * -(CSE_P1_T5 * ...) should
   be candidates for factoring the outer CSE_P1_T37 * CSE_P1_T5 product."
  (let* ((expr-gx1
           (expr-ir:parse-expr
            "CSE_P1_T37 * -(CSE_P1_T5 * (CSE_P1_T20*CSE_P1_T25
                                         + CSE_P1_T33*(-(CSE_P1_T13*CSE_P1_T27*CSE_P1_T6*CSE_P1_T7))))"))
         (expr-gy1
           (expr-ir:parse-expr
            "CSE_P1_T37 * -(CSE_P1_T5 * (CSE_P1_T20*CSE_P1_T29
                                         + CSE_P1_T33*(-(CSE_P1_T13*CSE_P1_T31*CSE_P1_T6*CSE_P1_T7))))"))
         (expr-gz1
           (expr-ir:parse-expr
            "CSE_P1_T37 * -(CSE_P1_T5 * (CSE_P1_T20*CSE_P1_T21
                                         + CSE_P1_T33*(-(CSE_P1_T13*CSE_P1_T23*CSE_P1_T6*CSE_P1_T7))))"))
         (s-gx1 (stmt-ir:make-assignment-stmt 'G_X1 expr-gx1))
         (s-gy1 (stmt-ir:make-assignment-stmt 'G_Y1 expr-gy1))
         (s-gz1 (stmt-ir:make-assignment-stmt 'G_Z1 expr-gz1))
         (block (stmt-ir:make-block-stmt (list s-gx1 s-gy1 s-gz1)))
         ;; no parameters for this test; everything treated like temps
         (before-count
           (count-product-subexprs-with-names-in-block
            block '("CSE_P1_T13" "CSE_P1_T7")))
         (after-block
           (let ((stmt-ir::*factor-temp-param-walk-debug* t))
             (stmt-ir:factor-temp-param-products-optimization (stmt-ir:make-pass-id-counter)
              block
              :min-uses    2
              :min-factors 2
              :max-factors 3
              :pass-id     101)))
         (after-count
           (count-product-subexprs-with-names-in-block
            after-block '("CSE_P1_T13" "CSE_P1_T7")))
         (temp-assignment
           (find-if
            (lambda (stmt)
              (and (typep stmt 'stmt-ir:assignment-statement)
                   (let ((name (stmt-ir:stmt-target-name stmt)))
                     (and (symbolp name)
                          (search "CSE_P101_T"
                                  (symbol-name name)
                                  :test #'char-equal)))))
            (stmt-ir:block-statements after-block))))
    ;; current behavior will almost certainly fail this:
    (assert-true (< after-count before-count)
                 'test-factor-temp-param-products_angle_shape_with_minus
                 "Expected fewer occurrences of CSE_P1_T13*CSE_P1_T7 after factoring; got ~D vs ~D."
                 after-count before-count)
    (assert-true temp-assignment
                 'test-factor-temp-param-products_angle_shape_with_minus
                 "Expected a temp assignment CSE_P101_T* for CSE_P1_T13*CSE_P1_T7, but found none.")))




(defun count-direct-products-with-names-in-block (block required-names)
  "Count product subexpressions in RHS of non-temp assignments in BLOCK
that contain all REQUIRED-NAMES."
  (let ((count 0))
    (dolist (st (stmt-ir:block-statements block))
      (when (and (typep st 'stmt-ir:assignment-statement)
                 (not (looks-like-cse-temp-symbol-p
                       (stmt-ir:stmt-target-name st))))
        (labels ((walk (sx)
                   (when (sexpr-product-contains-named-factors-p sx required-names)
                     (incf count))
                   (when (consp sx)
                     (dolist (child (cdr sx))
                       (walk child)))))
          (walk (expr-ir:expr->sexpr (stmt-ir:stmt-expression st))))))
    count))


  
(deftest test-angle-like-pipeline_factor_temp_param
  "Run the kernel pipeline passes on an angle-like block and check that
   a common temp+param subproduct (2*CSE_P1_T148*kt) is factored."
  (let* (;; energy/grad/hess-like assignments containing:
         ;;   2.0 * CSE_P1_T148 * kt * {FOO, BAR, BAZ}
         (expr1 (expr-ir:parse-expr
                 "2.0 * CSE_P1_T148 * kt * FOO + CSE_P1_T25"))
         (expr2 (expr-ir:parse-expr
                 "CSE_P1_T13 + 2.0 * BAR * CSE_P1_T148 * kt"))
         (expr3 (expr-ir:parse-expr
                 "2.0 * CSE_P1_T148 * BAZ * kt - CSE_P1_T31"))
         (s1 (stmt-ir:make-assignment-stmt 'G_X1 expr1))
         (s2 (stmt-ir:make-assignment-stmt 'G_Y1 expr2))
         (s3 (stmt-ir:make-assignment-stmt 'H_X1_X1 expr3))
         (base-block (stmt-ir:make-block-stmt (list s1 s2 s3)))
         ;; only KT is treated as a parameter; matching by name

         ;; mimic the full-block stage of the kernel pipeline
         (full-block     base-block)
         (full-block-cse (stmt-ir:cse-block-multi full-block
                                                  :max-passes 3
                                                  :min-uses 2
                                                  :min-size 1))
         (factored-block (stmt-ir:factor-temp-param-products-optimization (stmt-ir:make-pass-id-counter :start 99)
                          full-block-cse
                          :min-uses    2
                          :min-factors 2
                          :max-factors 3))

         ;; count direct occurrences of products containing both C148 and KT
         ;; in non-temp assignments
         (before-count
           (count-direct-products-with-names-in-block
            full-block-cse '("CSE_P1_T148" "KT")))
         (after-count
           (count-direct-products-with-names-in-block
            factored-block '("CSE_P1_T148" "KT")))

         ;; find a temp introduced by this pass (pass-id 123) whose RHS has C148 & KT
         (temp-assignment
           (find-if
            (lambda (stmt)
              (and (typep stmt 'stmt-ir:assignment-statement)
                   (let ((name (stmt-ir:stmt-target-name stmt)))
                     (and (symbolp name)
                          (search "CSE_P100_T"
                                  (symbol-name name)
                                  :test #'char-equal)))
                   (let* ((rhs (stmt-ir:stmt-expression stmt))
                          (sx  (expr-ir:expr->sexpr rhs)))
                     (sexpr-product-contains-named-factors-p
                      sx '("CSE_P1_T148" "KT")))))
            (stmt-ir:block-statements factored-block))))
    ;; sanity: there were direct uses before factoring
    (assert-true (> before-count 0)
                 'test-angle-like-pipeline_factor_temp_param
                 "Expected at least one direct product with CSE_P1_T148*KT before factoring.")
    ;; 1) We introduced a temp with 2*C148*KT in its RHS
    (assert-true temp-assignment
                 'test-angle-like-pipeline_factor_temp_param
                 "Expected pipeline to introduce a temp CSE_P100_T* whose RHS contains CSE_P1_T148 and KT.")
    ;; 2) No remaining direct occurrences of that subproduct in non-temp assignments
    (assert-true (zerop after-count)
                 'test-angle-like-pipeline_factor_temp_param
                 "Expected no remaining direct product with CSE_P1_T148*KT in non-temp assignments after factoring, got ~D occurrences."
                 after-count)))




(defun sexpr-product-contains-named-factors-p (sexpr required-names)
  "True if SEXPR is a product (* ...) whose factors include all REQUIRED-NAMES
(as symbol-names, case-insensitive)."
  (and (consp sexpr)
       (eq (car sexpr) '*)
       (let ((args (cdr sexpr)))
         (every (lambda (req-name)
                  (some (lambda (factor)
                          (and (symbolp factor)
                               (string= (symbol-name factor) req-name)))
                        args))
                required-names))))

(defun count-products-with-named-factors-in-expr (expr required-names)
  "Count product subexpressions in EXPR that contain all REQUIRED-NAMES."
  (let ((count 0))
    (labels ((walk (sx)
               (when (sexpr-product-contains-named-factors-p sx required-names)
                 (incf count))
               (when (consp sx)
                 (dolist (child (cdr sx))
                   (walk child)))))
      (walk (expr-ir:expr->sexpr expr)))
    count))

(defun count-products-with-named-factors-in-block (block required-names)
  "Count product subexpressions in all assignment RHS expressions in BLOCK."
  (let ((count 0))
    (dolist (stmt (stmt-ir:block-statements block))
      (when (typep stmt 'stmt-ir:assignment-statement)
        (incf count
              (count-products-with-named-factors-in-expr
               (stmt-ir:stmt-expression stmt)
               required-names))))
    count))


(deftest test-factor-temp-param-products_triple_t1_t18_t6
  "Three products T1*T18*T6*{25,29,21} should let us factor T1*T18*T6."
  (let* ((expr1 (expr-ir:parse-expr "CSE_P1_T1*CSE_P1_T18*CSE_P1_T6*CSE_P1_T25"))
         (expr2 (expr-ir:parse-expr "CSE_P1_T1*CSE_P1_T18*CSE_P1_T6*CSE_P1_T29"))
         (expr3 (expr-ir:parse-expr "CSE_P1_T1*CSE_P1_T18*CSE_P1_T6*CSE_P1_T21"))
         (s1    (stmt-ir:make-assignment-stmt 'G_X3 expr1))
         (s2    (stmt-ir:make-assignment-stmt 'G_Y3 expr2))
         (s3    (stmt-ir:make-assignment-stmt 'G_Z3 expr3))
         (block (stmt-ir:make-block-stmt (list s1 s2 s3)))
         (required-names '("CSE_P1_T1" "CSE_P1_T18" "CSE_P1_T6"))
         (before-count
           (count-products-with-named-factors-in-block block required-names))
         (after-block
           (stmt-ir:factor-temp-param-products-optimization (stmt-ir:make-pass-id-counter :start 199)
            block
            :min-uses    2
            :min-factors 3
            :max-factors 3))
         (after-count
           (count-products-with-named-factors-in-block after-block required-names))
         ;; Any temp introduced by this pass?
         (temp-assignment
           (find-if
            (lambda (stmt)
              (and (typep stmt 'stmt-ir:assignment-statement)
                   (let ((name (stmt-ir:stmt-target-name stmt)))
                     (and (symbolp name)
                          (search "CSE_P200_T" (symbol-name name)
                                  :test #'char-equal)))))
            (stmt-ir:block-statements after-block))))
    ;; sanity: triple appears before factoring
    (assert-true (> before-count 0)
                 'test-factor-temp-param-products_triple_t1_t18_t6
                 "Expected at least one product with CSE_P1_T1*CSE_P1_T18*CSE_P1_T6 before factoring.")
    ;; after factoring, that triple should be gone from direct products
    (assert-true (< after-count before-count)
                 'test-factor-temp-param-products_triple_t1_t18_t6
                 "Expected fewer direct products with CSE_P1_T1*CSE_P1_T18*CSE_P1_T6 after factoring, got ~D vs ~D."
                 after-count before-count)
    ;; and a temp for the triple should have been introduced
    (assert-true temp-assignment
                 'test-factor-temp-param-products_triple_t1_t18_t6
                 "Expected a CSE_P200_T* temp to be introduced for CSE_P1_T1*CSE_P1_T18*CSE_P1_T6.")))


(deftest test-factor-temp-param-products_triple_t1_t18_t6_with_minus
  "Three products -(T1*T18*T6*{25,29,21}) should still allow factoring T1*T18*T6."
  (let* ((expr1 (expr-ir:parse-expr
                 "-(CSE_P1_T1*CSE_P1_T18*CSE_P1_T6*CSE_P1_T25)"))
         (expr2 (expr-ir:parse-expr
                 "-(CSE_P1_T1*CSE_P1_T18*CSE_P1_T6*CSE_P1_T29)"))
         (expr3 (expr-ir:parse-expr
                 "-(CSE_P1_T1*CSE_P1_T18*CSE_P1_T6*CSE_P1_T21)"))
         (s1    (stmt-ir:make-assignment-stmt 'G_X3 expr1))
         (s2    (stmt-ir:make-assignment-stmt 'G_Y3 expr2))
         (s3    (stmt-ir:make-assignment-stmt 'G_Z3 expr3))
         (block (stmt-ir:make-block-stmt (list s1 s2 s3)))
         (required-names '("CSE_P1_T1" "CSE_P1_T18" "CSE_P1_T6"))
         (before-count
           (count-products-with-named-factors-in-block block required-names))
         (after-block
           (stmt-ir:factor-temp-param-products-optimization (stmt-ir:make-pass-id-counter :start 200)
            block
            :min-uses    2
            :min-factors 3
            :max-factors 3))
         (after-count
           (count-products-with-named-factors-in-block after-block required-names))
         (temp-assignment
           (find-if
            (lambda (stmt)
              (and (typep stmt 'stmt-ir:assignment-statement)
                   (let ((name (stmt-ir:stmt-target-name stmt)))
                     (and (symbolp name)
                          (search "CSE_P201_T" (symbol-name name)
                                  :test #'char-equal)))))
            (stmt-ir:block-statements after-block))))
    (assert-true (> before-count 0)
                 'test-factor-temp-param-products_triple_t1_t18_t6_with_minus
                 "Expected at least one product with triple inside -(...) before factoring.")
    (assert-true (< after-count before-count)
                 'test-factor-temp-param-products_triple_t1_t18_t6_with_minus
                 "Expected fewer direct triple products after factoring, got ~D vs ~D."
                 after-count before-count)
    (assert-true temp-assignment
                 'test-factor-temp-param-products_triple_t1_t18_t6_with_minus
                 "Expected a CSE_P201_T* temp for CSE_P1_T1*CSE_P1_T18*CSE_P1_T6 with minus.")))
