(ql:quickload :optimize-expressions)

(opt-exp:start)

(trace opt-exp::deriv opt-exp::deriv-atom opt-exp::simplify-rules
       opt-exp::collect-terms
       opt-exp::collect-terms-for-one
       )

(progn
  (.= stretch-pack
      (make-pack :name "stretch"
                 :base-variables '((x1 x I1 0)
                                   (y1 y I1 1)
                                   (z1 z I1 2)
                                   (x2 x I2 0)
                                   (y2 y I2 1)
                                   (z2 z I2 2))
                 :dependent-variables '(stretch-deviation)
                 :constants '(r0 kb))))
(progn
  (with-pack (stretch-pack)
    (append-ccode (ccode "STRETCH_SET_PARAMETER(kb);"))
    (append-ccode (ccode "STRETCH_SET_PARAMETER(r0);"))
    (append-ccode (ccode "STRETCH_SET_PARAMETER(I1);"))
    (append-ccode (ccode "STRETCH_SET_PARAMETER(I2);"))
    (append-ccode (ccode "STRETCH_APPLY_ATOM_MASK(I1,I2);"))

    (loop for base-var in (base-variables stretch-pack)
          do (destructuring-bind (var-name short-name iname offset)
                 base-var
               (append-ccode (ccode (format nil "STRETCH_SET_POSITION(~a,~a,~d);"
                                     var-name iname offset))))))

  (.= b1 { x1 y1 z1 })
  (.= b2 { x2 y2 z2 })

  (.= delta (sv-sub b1 b2))
  (.= len-squared (sv-dot delta delta))
  (.= len `(sqrt ,len-squared))
  (.= stretch-deviation `(- ,len r0))
  (.= stretch-energy `(* kb (expt stretch-deviation 2)))

  (with-pack (stretch-pack)
    (append-rule (rule stretch-deviation 'stretch-deviation))
    (append-rule (rule stretch-energy 'stretch-energy))
    (append-accumulate (accumulate "STRETCH" 'stretch-energy))
    (push 'stretch-energy (outputs stretch-pack))
    (append-gradient-force-and-hessian stretch-pack nil stretch-energy 'stretch-energy (base-variables stretch-pack))
    ))
#+(or)(break "Check stretch-pack ~s" stretch-pack)
(.= pack (pack-optimize stretch-pack))





(ql:quickload :optimize-expressions)

(opt-exp:start)

;; foo = foo(x)
;; bar = bar(foo)
(let* ((x-expr   (expr-ir:make-expr-var 'x))
       (foo-expr (parse-expr "x^2 + 1"))           ; foo = x^2 + 1
       (bar-expr (parse-expr "sin(foo) + foo"))    ; bar = sin(foo) + foo
       (s1 (make-assignment-stmt 'foo foo-expr))
       (s2 (make-assignment-stmt 'bar bar-expr))
       (stmts (list s1 s2)))
  ;; Compute d(bar)/dx with full chain rule through foo
  (let ((dbar-dx (differentiate-target-in-block stmts 'x 'bar)))
    (expr-ir:expr->sexpr dbar-dx)))


(let* ((E-expr (expr-ir:parse-expr "kb*(r - r0)^2"))
       (coord-vars '(x1 y1 z1 x2 y2 z2))
       (block (stmt-ir:make-energy-grad-hess-block
               :energy-expr   E-expr
               :coord-vars    coord-vars
               :energy-target 'E
               :grad-target-fn #'make-grad-name
               :hess-target-fn #'make-hess-name)))
  ...)
