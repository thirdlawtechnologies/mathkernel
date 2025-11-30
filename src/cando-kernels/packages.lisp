
(defpackage :energy-kernels
  (:use :cl :opt-exp :expr-ir :stmt-ir)
  (:export
   #:rewrite-expr-ir-with-rules
   #:rewrite-sexpr-with-rules
   #:*rewrite-rules-basic*
   #:write-c-code
   #:*post-eg-h-pipeline*
   #:write-all
   #:define-multiple-kernels
   #:push-kernel
   #:build-multiple-kernels
   #:with-kernals))

