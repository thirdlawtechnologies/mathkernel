;;;; expression-print.lisp
;;;; Converters from expression IR to:
;;;;   - prefix Lisp S-expressions
;;;;   - infix string form

(in-package :expr-ir)

;;; ----------------------------------------------------------------------
;;; Expression -> prefix Lisp S-expressions
;;; ----------------------------------------------------------------------

(defun expr->sexpr (expr)
  "Convert an expression IR node to a standard Lisp S-expression in prefix form.
This is a structural mapping; / is represented via * and expt, not reconstructed."
  (labels ((rec (e)
             (typecase e
               (constant-expression
                (expression-value e))
               (variable-expression
                (variable-name e))
               (add-expression
                (cons '+ (mapcar #'rec (expression-arguments e))))
               (multiply-expression
                (cons '* (mapcar #'rec (expression-arguments e))))
               (power-expression
                (list 'expt
                      (rec (power-base-expression e))
                      (rec (power-exponent-expression e))))
               (negate-expression
                (list '- (rec (negate-argument-expression e))))
               (function-call-expression
                (cons (function-call-name e)
                      (mapcar #'rec (function-call-arguments e))))
               (comparison-expression
                (let* ((op (comparison-operator e))
                       (sym (ecase op
                              (:<  '<)
                              (:>  '>)
                              (:<= '<=)
                              (:>= '>=)
                              (:=  '=)
                              (:/= '/=))))
                  (list sym
                        (rec (comparison-left-expression e))
                        (rec (comparison-right-expression e)))))
               (logical-nary-expression
                (let* ((op (logical-operator e))
                       (sym (ecase op
                              (:and 'and)
                              (:or  'or))))
                  (cons sym (mapcar #'rec (logical-arguments e)))))
               (logical-not-expression
                (list 'not (rec (logical-not-argument-expression e))))
               (t
                (error "Don't know how to convert ~S to S-expression." e)))))
    (rec expr)))

(defun expr->sexpr-string (expr)
  "Return a printed prefix S-expression string for EXPR."
  (with-output-to-string (s)
    (prin1 (expr->sexpr expr) s)))

;;; ----------------------------------------------------------------------
;;; Expression -> infix string
;;; ----------------------------------------------------------------------
;;; We use a simple precedence-based printer with parentheses when needed.

(defun expression-precedence (expr)
  "Return a numeric precedence for EXPR. Larger means binds tighter."
  (typecase expr
    (logical-nary-expression
     (if (eq (logical-operator expr) :or) 1 2)) ; or < and
    (comparison-expression          3)
    (add-expression                 4)
    (multiply-expression            5)
    ((or negate-expression
         logical-not-expression)    6) ; unary operators
    (power-expression               7)
    ((or function-call-expression
         constant-expression
         variable-expression)       8)
    (t                              0)))

(defun %maybe-paren (string child-prec parent-prec)
  (if (and parent-prec
           (< child-prec parent-prec))
      (format nil "(~a)" string)
      string))

(defun expr->infix-string (expr &optional parent-precedence)
  "Convert an expression IR node to an infix string suitable for C-like output."
  (labels
      ((rec (e &optional (parent-precedence parent-precedence))
         (let* ((prec (expression-precedence e)))
           (flet ((wrap (s child-prec)
                    (%maybe-paren s child-prec parent-precedence)))
             (typecase e
               (constant-expression
                (wrap (princ-to-string (expression-value e)) prec))
               (variable-expression
                (wrap (symbol-name (variable-name e)) prec))
               (add-expression
                (let* ((args (expression-arguments e))
                       (parts (mapcar (lambda (x)
                                        (rec x prec))
                                      args))
                       (joined (format nil "~{~a~^ + ~}" parts)))
                  (wrap joined prec)))
               (multiply-expression
                (let* ((args (expression-arguments e))
                       (parts (mapcar (lambda (x)
                                        (rec x prec))
                                      args))
                       (joined (format nil "~{~a~^ * ~}" parts)))
                  (wrap joined prec)))
               (power-expression
                (let* ((b (rec (power-base-expression e) prec))
                       (p (rec (power-exponent-expression e) prec))
                       (s (format nil "~a ^ ~a" b p)))
                  (wrap s prec)))
               (negate-expression
                (let* ((arg (negate-argument-expression e))
                       (s (format nil "-~a"
                                  (rec arg prec))))
                  (wrap s prec)))
               (function-call-expression
                (let* ((name (symbol-name (function-call-name e)))
                       (args (function-call-arguments e))
                       (parts (mapcar (lambda (x) (rec x 0)) args))
                       (s (format nil "~a(~{~a~^, ~})"
                                  name parts)))
                  (wrap s prec)))
               (comparison-expression
                (let* ((op (comparison-operator e))
                       (op-str (ecase op
                                 (:<  "<")
                                 (:>  ">")
                                 (:<= "<=")
                                 (:>= ">=")
                                 (:=  "==")
                                 (:/= "!=")))
                       (lhs (rec (comparison-left-expression e) prec))
                       (rhs (rec (comparison-right-expression e) prec))
                       (s   (format nil "~a ~a ~a" lhs op-str rhs)))
                  (wrap s prec)))
               (logical-nary-expression
                (let* ((op (logical-operator e))
                       (op-str (ecase op
                                 (:and "&&")
                                 (:or  "||")))
                       (args (logical-arguments e))
                       (parts (mapcar (lambda (x)
                                        (rec x prec))
                                      args))
                       (s (format nil "~{~a~^ ~a ~}"
                                  parts op-str)))
                  (wrap s prec)))
               (logical-not-expression
                (let* ((arg (logical-not-argument-expression e))
                       (s (format nil "!~a" (rec arg prec))))
                  (wrap s prec)))
               (t
                (error "Don't know how to print ~S as infix." e)))))))
    (rec expr nil)))
