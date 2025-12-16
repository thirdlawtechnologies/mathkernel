;;; packages.lisp

(defpackage :opt-exp
  (:use :cl)
  (:export
   #:pack-optimize
   #:collect-terms
   #:times-simplify
   #:plus-simplify

   #:deriv
   #:pack
   #:use-opt-exp
   #:start
   #:.=
   #:sv-sub
   #:sv-add
   #:sv-dot
   #:svector-square-components
   #:prefix->infix
   #:prefix->mathematica
   #:make-pack
   #:rule
   #:with-pack
   #:ccode
   #:accumulate
   #:append-ccode
   #:base-variables
   #:append-rule
   #:append-accumulate
   #:append-gradient-force-and-hessian
   #:outputs
   #:sv-cross
   #:svector
   #:sv-len))




;;;; Expression IR for symbolic math / AD / C generation
;;;; ---------------------------------------------------
;;;; This is purely the expression layer (no statements / control flow).

(defpackage :expr-ir
  (:use :cl)
  (:export
   ;; base protocol
   #:expression
   #:numeric-expression
   #:boolean-expression

   ;; core node classes
   #:constant-expression
   #:variable-expression
   #:nary-expression
   #:add-expression
   #:multiply-expression
   #:power-expression
   #:negate-expression
   #:function-call-expression
   #:comparison-expression
   #:logical-nary-expression
   #:logical-not-expression

   ;; accessors (needed by tests and by downstream code)
   #:var-key
   #:expression-value
   #:variable-name
   #:expression-arguments
   #:power-base-expression
   #:power-exponent-expression
   #:negate-argument-expression
   #:function-call-name
   #:function-call-arguments
   #:comparison-operator
   #:comparison-left-expression
   #:comparison-right-expression
   #:logical-operator
   #:logical-arguments
   #:logical-not-argument-expression

   ;; constructors / parsers / printers, if not already exported elsewhere
   #:make-expr-const
   #:make-expr-var
   #:make-expr-add
   #:make-expr-mul
   #:make-expr-pow
   #:make-expr-neg
   #:make-expr-funcall

   #:sexpr->expr-ir
   #:infix->expr-ir
   #:parse-expr

   #:expr->sexpr
   #:expr->infix-string

   #:differentiate-expr
   #:make-deriv-env

   #:simplify-expr
   #:expr->c-expr-string
   #:expr-var-symbol
   #:expr-free-vars
   #:set-var-derivative
   #:lookup-var-derivative
   #:add-expressions
   #:ev

  #:copy-deriv-env
   #:factor-sum-of-products
   #:normalize-signs-expr
   #:debug-sexpr
   #:debug-expr
   #:*rewrite-rules-basic*
   #:*optimization-rules*
   #:draw-sexpr-tree
   #:sexpr-sort-key
   #:side-by-side-sexpr
   #:canonicalize-linear-subexprs
   #:make-template-rule
   #:make-function-rule
   #:apply-rules-to-sexpr-once
   #:wildcard-base-name
   #:lookup-wild
   #:*lequivalence-canonicalization-rules*
   #:*equivalence-canonicalization-rules*
   #:with-kernel-rewrite-rules
   #:deriv-env-table))



(defpackage :expr-ir.tests
  (:use :cl :expr-ir)
  (:export #:run-expression-tests))


(defpackage :stmt-ir
  (:use :cl :expr-ir)
  (:export
   ;; base
   #:statement

   ;; statements
   #:assignment-statement
   #:raw-c-statement
   #:if-statement
   #:block-statement
   #:c-function

   ;; accessors
   #:stmt-target-name
   #:stmt-target-indices
   #:stmt-expression

   #:raw-c-text

   #:if-condition
   #:if-then-block
   #:if-else-block

   #:block-statements

   #:c-function-name
   #:c-function-return-type
   #:c-function-parameters
   #:c-function-locals
   #:c-function-body

   ;; simple helper constructors
   #:make-assignment-stmt
   #:make-raw-c-statement
   #:make-if-stmt
   #:make-block-stmt
   #:make-c-function

  #:build-derivative-env-for-block
   #:differentiate-target-in-block
   #:make-derivative-assignments-for-block
   #:make-local-partial-derivative-assignments-for-block

   #:simplify-statement
   #:simplify-block

  #:make-energy-grad-block
  #:make-energy-grad-hess-block

  #:c-function->c-source-string
  #:cse-block-multi-optimization
   #:cse-block-multi
   #:collect-scalar-targets-in-block
   #:copy-propagate-optimization
   #:*debug*
   #:check-cse-temp-order

  #:factor-sums-optimization
  #:factor-temp-param-products-optimization
  #:normalize-signs-optimization
  #:make-optimization-pipeline
 #:make-optimization
  #:run-optimization-pipeline
  #:make-pass-id-counter
  #:reorder-block-def-before-use
  #:make-derivative-request-stmt
  #:derivative-request-statement
  #:dr-target-var
   #:dr-base-var
   #:make-derivative-name
   #:sexpr-size
  #:linear-canonicalization-optimization
  #:debug-block
  #:raw-c
   #:alias-assigned-exprs-optimization
  #:make-block-stmt-unsafe
  #:check-def-before-use-in-block
  #:walk-context
  #:deriv-env
  #:deriv-env-table
  #:walk-block-with-context
  #:clone-context
  #:on-statement
  ;; comparison harness generator
  #:write-kernel-compare-c
  #:write-kernel-compare-from-kernels
  #:emit-compare-preamble
  #:emit-kernel-test-c
  #:emit-main-for-tests
  #:write-kernel-compare-harness
  #:raw-c-generator
  #:make-anchored-assignment-stmt
  #:anchored-assignment-statement
  #:*verbose-optimization*
  #:block-label
  #:cse-factor-products-in-block
  #:check-block-integrity
  #:rewrite-block))

(defpackage :stmt-ir.tests
  (:use :cl :expr-ir :stmt-ir)
  (:export #:run-statement-tests
           #:run-all))

(defpackage :expr-var
  (:use :cl))

(defpackage :mathkernel
  (:use :cl :opt-exp :expr-ir :stmt-ir)
  (:export
   #:=.
   #:D!
   #:accumulate-here
   #:stmt-block
   #:coords-from-position
   #:rewrite-expr-ir-with-rules
   #:rewrite-sexpr-with-rules
   #:*rewrite-rules-basic*
   #:write-c-code
   #:*post-eg-h-pipeline*
   #:write-all
   #:numeric-gradient!
   #:numeric-hessian!
   #:numeric-force-and-hessian!
   #:define-multiple-kernels
   #:push-kernel
   #:build-multiple-kernels
   #:with-kernels
   #:build-kernel
   #:emit-c-tests
   #:with-trace-output
   #:=!))

;;;; Symbols go in this package
(defpackage :mathkernel-user
  (:use :cl :expr-ir :stmt-ir :opt-exp :mathkernel)
  (:export
   #:D!
   #:raw-c
   #:with-kernels
   #:coords-from-position
   #:stmt-block
   #:=.
   ))
