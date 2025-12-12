;;;; ----------------------------------------------------------------------
;;;; expression-ir.lisp
;;;; ----------------------------------------------------------------------


(in-package :expr-ir)

(defparameter *function-names* '(:cos :sin :exp :log :sqrt :acos :atan2))

;;; ----------------------------------------------------------------------
;;; Base classes
;;; ----------------------------------------------------------------------

(defclass expression ()
  ()
  (:documentation
   "Abstract base class for all expression nodes.
    Pure, side–effect–free scalar expressions only."))

;; Generic catch-all for any expression node.
;; Assumes you have a common superclass named EXPRESSION; if not, change
;; (expression) to your actual base class or add per-class methods.

(defmethod print-object ((expr expression) stream)
  (if *print-readably*
      ;; When someone asks for READABLE printing (e.g. ~S in some contexts),
      ;; fall back to default so you don't break the reader.
      (call-next-method)
      (print-unreadable-object (expr stream :type t)
        (handler-case
            (let ((*print-pretty* nil)
                  (*package* (find-package :expr-var)))
              (format stream "~s" (expr->sexpr expr)))
          (error ()
            ;; As a fallback, if expr->sexpr blows up, just show the type.
            (princ "<unprintable-expr>" stream))))))


(defclass numeric-expression (expression)
  ()
  (:documentation
   "Marker class for expressions that evaluate to a numeric scalar."))

(defclass boolean-expression (expression)
  ()
  (:documentation
   "Marker class for expressions that evaluate to a boolean value."))

;;; ----------------------------------------------------------------------
;;; Constants and variables
;;; ----------------------------------------------------------------------


(defclass constant-expression (numeric-expression)
  ((value
    :initarg :value
    :accessor expression-value
    :documentation "Literal numeric value (integer, rational, or float)."))
  (:documentation
   "Numeric literal. Invariants:
    - Stored in a canonical numeric representation (e.g. rationals reduced)."))


(defclass variable-expression (numeric-expression)
  ((name
    :initarg :name
    :accessor variable-name
    :type symbol
    :documentation "Symbolic variable name (e.g. 'x, 'y2, 't2_gradient)."))
  (:documentation
   "Reference to a symbolic variable.
    Types, roles, and indices live in a separate environment, not here."))

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

;;; ----------------------------------------------------------------------
;;; N-ary arithmetic operators (associative, possibly commutative)
;;; ----------------------------------------------------------------------

(defclass nary-expression (numeric-expression)
  ((arguments
    :initarg :arguments
    :accessor expression-arguments
    :type list
    :documentation
    "List of child expressions.
     For canonical form, this list should be flattened and sorted where
     the operator is associative / commutative."))
  (:documentation
   "Abstract base class for operators with an arbitrary number of arguments."))

(defclass add-expression (nary-expression)
  ()
  (:documentation
   "Sum of one or more numeric expressions.
    In canonical form:
      - arguments is a flat list (no nested add-expression children),
      - numeric constants combined into at most one constant,
      - arguments sorted according to a global total ordering."))

(defclass multiply-expression (nary-expression)
  ()
  (:documentation
   "Product of one or more numeric expressions.
    In canonical form:
      - arguments is a flat list (no nested multiply-expression children),
      - all numeric constants multiplied into a single leading coefficient,
      - arguments sorted according to a global total ordering."))



;;; ----------------------------------------------------------------------
;;; A few predicates
;;; ----------------------------------------------------------------------
(defun numeric-zero-p (value)
  (and (numberp value)
       (zerop value)))

(defun numeric-one-p (value)
  (and (numberp value)
       (eql value 1)))

(defun numeric-minus-one-p (value)
  (and (numberp value)
       (eql value -1)))

(defun expression-sort-key (expression)
  "Return a simple key used to sort arguments in n-ary expressions.
Order: constants < variables < everything else; ties break by a printable name."
  (labels ((class-rank (expr)
             (cond
               ((typep expr 'constant-expression) 0)
               ((typep expr 'variable-expression) 1)
               (t                                 2)))
           (expr-name (expr)
             (typecase expr
               (constant-expression
                (princ-to-string (expression-value expr)))
               (variable-expression
                (symbol-name (variable-name expr)))
               (function-call-expression
                (symbol-name (function-call-name expr)))
               (nary-expression
                (loop for arg in (expression-arguments expr)
                      collect (multiple-value-list (expression-sort-key arg))))
               (negate-expression
                (multiple-value-list (negate-argument-expression expr)))
               (power-expression
                (append
                 (multiple-value-list (power-base-expression expr))
                 (multiple-value-list (power-exponent-expression expr))))
               (t
                (error "What do we get here ~s" (class-of expr))))))
    (list (class-rank expression)
          (expr-name expression))))

(defun sexpr-sort-key (sexpr)
  (let ((expr (sexpr->expr-ir sexpr)))
    (expression-sort-key expr)))

;;; ----------------------------------------------------------------------
;;; Unary / binary arithmetic operators
;;; ----------------------------------------------------------------------

(defclass power-expression (numeric-expression)
  ((base-expression
    :initarg :base-expression
    :accessor power-base-expression
    :documentation "Base expression.")
   (exponent-expression
    :initarg :exponent-expression
    :accessor power-exponent-expression
    :documentation "Exponent expression. Often an integer or rational."))
  (:documentation
   "Binary exponentiation node representing (base-expression ^ exponent-expression).
    Not associative or commutative; kept as a simple binary node."))

(defclass negate-expression (numeric-expression)
  ((argument-expression
    :initarg :argument-expression
    :accessor negate-argument-expression
    :documentation "Expression being negated."))
  (:documentation
   "Unary numeric negation. Canonicalization may choose to represent
    subtraction either with this node or via multiply-expression with -1."))

;;; ----------------------------------------------------------------------
;;; General function calls (elementary functions, intrinsics, user functions)
;;; ----------------------------------------------------------------------

(defclass function-call-expression (numeric-expression)
  ((function-name
    :initarg :function-name
    :accessor function-call-name
    :type symbol
    :documentation
    "Symbol naming the function (e.g. :sin, :cos, :exp, :log, :sqrt, :fabs :acos :atan2).")
   (argument-list
    :initarg :argument-list
    :accessor function-call-arguments
    :type list
    :documentation
    "List of argument expressions."))
  (:documentation
   "General function application.
    Elementary functions and C intrinsics are represented here.
    Derivative and C-code mappings are provided by an external environment."))

;;; ----------------------------------------------------------------------
;;; Comparisons (for conditions in if-statements)
;;; ----------------------------------------------------------------------

(defclass comparison-expression (boolean-expression)
  ((operator
    :initarg :operator
    :accessor comparison-operator
    :type keyword
    :documentation
    "Comparison operator keyword, e.g. :< :> :<= :>= := :/=.")
   (left-expression
    :initarg :left-expression
    :accessor comparison-left-expression
    :documentation "Left-hand side numeric expression.")
   (right-expression
    :initarg :right-expression
    :accessor comparison-right-expression
    :documentation "Right-hand side numeric expression."))
  (:documentation
   "Binary comparison expression. Evaluates to a boolean value.
    Intended operators: :< :> :<= :>= := :/= ."))

;;; ----------------------------------------------------------------------
;;; Logical operators (for composite conditions)
;;; ----------------------------------------------------------------------

(defclass logical-nary-expression (boolean-expression)
  ((operator
    :initarg :operator
    :accessor logical-operator
    :type keyword
    :documentation
    "Logical operator keyword, e.g. :and or :or.")
   (arguments
    :initarg :arguments
    :accessor logical-arguments
    :type list
    :documentation
    "List of boolean child expressions."))
  (:documentation
   "N-ary logical operator over boolean expressions.
    In canonical form:
      - arguments flattened (no nested logical-nary-expression with same operator),
      - arguments sorted, since :and and :or are commutative."))

(defclass logical-not-expression (boolean-expression)
  ((argument-expression
    :initarg :argument-expression
    :accessor logical-not-argument-expression
    :documentation "Boolean expression being negated."))
  (:documentation
   "Logical negation of a boolean expression."))




;;; ----------------------------------------------------------------------
;;; simplify-expr
;;; ----------------------------------------------------------------------

(defun simplify-expr (expr)
  "Return a simplified, canonical expression IR node built from EXPR.
This is a pure pass that:
  - recursively simplifies children,
  - rebuilds the node via the canonical constructors (make-expr-...),
    so all local simplification and canonicalization rules are re-applied."
  (labels ((simp (e)
             (typecase e
               ;; Leaf nodes: constants and variables
               (constant-expression
                e)
               (variable-expression
                e)

               ;; n-ary arithmetic
               (add-expression
                (let* ((args (expression-arguments e))
                       (sargs (mapcar #'simp args)))
                  (make-expr-add sargs)))

               (multiply-expression
                (let* ((args (expression-arguments e))
                       (sargs (mapcar #'simp args)))
                  (make-expr-mul sargs)))

               ;; powers
               (power-expression
                (let ((b (simp (power-base-expression e)))
                      (p (simp (power-exponent-expression e))))
                  (make-expr-pow b p)))

               ;; negation
               (negate-expression
                (let ((arg (simp (negate-argument-expression e))))
                  (make-expr-neg arg)))

               ;; function calls
               (function-call-expression
                (let* ((fname (function-call-name e))
                       (args  (function-call-arguments e))
                       (sargs (mapcar #'simp args)))
                  (make-expr-funcall (ensure-keyword fname) sargs)))

               ;; comparisons
               (comparison-expression
                (make-instance 'comparison-expression
                               :operator (comparison-operator e)
                               :left-expression (simp (comparison-left-expression e))
                               :right-expression (simp (comparison-right-expression e))))

               ;; logical n-ary (AND/OR/etc.)
               (logical-nary-expression
                (make-instance 'logical-nary-expression
                               :operator (logical-operator e)
                               :arguments (mapcar #'simp (logical-arguments e))))

               ;; logical NOT
               (logical-not-expression
                (make-instance 'logical-not-expression
                               :argument-expression
                               (simp (logical-not-argument-expression e))))

               (t
                (error "simplify-expr: unknown expression node type ~S: ~S"
                       (type-of e) e)))))
    (simp expr)))





;;; ----------------------------------------------------------------------
;;; Assist with debugging of statment ordering
;;; ----------------------------------------------------------------------

(defun expr-free-vars (expr)
  "Return a list of variable symbols appearing in EXPR.
We do this via the sexpr representation: the first element of a list
is treated as an operator/function name, not a variable; only symbols
in argument positions are counted."
  (let ((vars '()))
    (labels ((rec (sexpr)
               (cond
                 ;; variable position
                 ((symbolp sexpr)
                  (pushnew sexpr vars :test #'eq))
                 ;; list: (f arg1 arg2 ...)
                 ((consp sexpr)
                  (let ((head (car sexpr)))
                    ;; do not treat HEAD as a variable; it's an operator
                    (declare (ignore head))
                    (dolist (arg (cdr sexpr))
                      (rec arg))))
                 (t
                  ;; numbers, etc.
                  nil))))
      (rec (expr->sexpr expr))
      vars)))


;;; ----------------------------------------------------------------------
;;; Optional: simple constructor helpers (you can extend / replace these)
;;; ----------------------------------------------------------------------

(defun make-constant (numeric-value)
  "Convenience constructor for a constant-expression."
  (make-instance 'constant-expression :value numeric-value))

(defun make-variable (symbol-name)
  "Convenience constructor for a variable-expression."
  (unless symbol-name
    (error "make-variable with nil"))
  (make-instance 'variable-expression :name symbol-name))


(in-package :expr-ir)

(defun expr->c-expr-string (expr)
  "Render EXPR as a C expression string.
Handles +, unary -, *, power (^), and basic function calls.
Variable and function names are lower-cased."
  (when (null expr)
    (error "expr->c-expr-string: got NIL expr"))
  (labels
      ((emit (e)
         (typecase e
           (constant-expression
            ;; You can refine formatting later (e.g., ensure trailing .0)
            (let ((v (expression-value e)))
              (cond
                ;; rationals: print as double literal
                ((rationalp v)
                 (let* ((num (float (numerator v) 1.0d0))
                        (den (float (denominator v) 1.0d0)))
                   (format nil "~,17g" (/ num den))))
                (t
                 (format nil "~,17g" (coerce v 'double-float))))))

           (variable-expression
            (string-downcase (symbol-name (variable-name e))))

           (add-expression
            (let* ((args (expression-arguments e))
                   (parts (mapcar #'emit args)))
              (format nil "(~{~A~^ + ~})" parts)))

           (multiply-expression
            (let* ((args (expression-arguments e))
                   (parts (mapcar #'emit args)))
              (format nil "(~{~A~^ * ~})" parts)))

           (power-expression
            (let* ((b-str (emit (power-base-expression e)))
                   (p-expr (power-exponent-expression e)))
              (cond
                ;; integer exponents
                ((and (typep p-expr 'constant-expression)
                      (integerp (expression-value p-expr)))
                 (let ((n (expression-value p-expr)))
                   (cond
                     ((= n 2)
                      (format nil "((~A) * (~A))" b-str b-str))
                     ((and (> n 2) (<= n 5))
                      ;; unroll small integer powers
                      (format nil "(~{~A~^ * ~})"
                              (make-list n :initial-element b-str)))
                     (t
                      ;; general integer power
                      (format nil "pow(~A, ~D)" b-str n)))))
                ;; exponent = 1/2 → sqrt
                ((and (typep p-expr 'constant-expression)
                      (let ((v (expression-value p-expr)))
                        (or (eql v 1/2)
                            (eql v 0.5d0)
                            (eql v 0.5))))
                 (format nil "sqrt(~A)" b-str))
                ;; everything else → pow(base, exponent)
                (t
                 (let ((p-str (emit p-expr)))
                   (format nil "pow(~A, ~A)" b-str p-str))))))

           (negate-expression
            (let ((arg (emit (negate-argument-expression e))))
              (format nil "(-(~A))" arg)))

           (function-call-expression
            (let* ((fname-sym (function-call-name e))
                   (fname-str (string-downcase (symbol-name fname-sym)))
                   (args      (mapcar #'emit (function-call-arguments e)))
                   ;; map some common names to standard C math names
                   (c-fname   (case (intern fname-str :keyword)
                                (:sqrt "sqrt")
                                ((:acos :arccos) "acos")
                                (:atan2  "ATAN2")
                                (:sin  "sin")
                                (:cos  "cos")
                                (:tan  "tan")
                                (:exp  "exp")
                                (:log  "log")
                                (:fabs "fabs")
                                (t fname-str))))
              (format nil "~A(~{~A~^, ~})" c-fname args)))

           (comparison-expression
            (let* ((op  (comparison-operator e)) ; assumed string like "<", ">"
                   (lhs (emit (comparison-left-expression e)))
                   (rhs (emit (comparison-right-expression e))))
              (format nil "(~A ~A ~A)" lhs op rhs)))

           (logical-nary-expression
            (let* ((op    (logical-operator e)) ; e.g. "&&" or "||"
                   (args  (logical-arguments e))
                   (parts (mapcar #'emit args))
                   (sep   (format nil " ~A " op)))
              (cond
                ((null parts)
                 ;; No arguments – pick something neutral; adjust if you prefer.
                 "(0)")
                ((null (rest parts))
                 ;; Single argument – just wrap it.
                 (format nil "(~A)" (first parts)))
                (t
                 ;; Fold a1 op a2 op a3 ...
                 (format nil "(~A)"
                         (reduce (lambda (a b)
                                   (format nil "~A~A~A" a sep b))
                                 (rest parts)
                                 :initial-value (first parts)))))))

           (logical-not-expression
            (let ((arg (emit (logical-not-argument-expression e))))
              (format nil "(!(~A))" arg)))

           (t
            (error "expr->c-expr-string: unknown expr node of type ~S: ~S"
                   (type-of e) e)))))
    (emit expr)))

;;; ----------------------------------------------------------------------
;;; Normalization pass
;;; ----------------------------------------------------------------------

(in-package :expr-ir)

(defun unary-minus-sexpr-p (sx)
  "True if SX is a unary minus of the form (- x)."
  (and (consp sx)
       (eq (car sx) '-)
       (= (length sx) 2)))

(defun normalize-signs-sexpr (sx &optional (sign 1))
  "Return a new sexpr equivalent to SIGN * SX, with:
  - unary minus absorbed into numeric factors in products,
  - unary minus at term level in sums (no -(* ...) inside sums),
  - nested + and * flattened where possible.

We only do algebraic rewrites for +, * and unary -. Other heads
(f, sin, cos, ...) are treated as opaque; SIGN is applied as a
unary minus or numeric negation."

  (cond
    ;; Unary minus: flip SIGN and recurse on the inside.
    ((unary-minus-sexpr-p sx)
     (normalize-signs-sexpr (second sx) (- sign)))

    ;; Addition: SIGN * (t1 + t2 + ...) = sum_i (SIGN * ti).
    ((and (consp sx) (eq (car sx) '+))
     (let ((terms '()))
       (dolist (term (cdr sx))
         (let ((norm (normalize-signs-sexpr term sign)))
           ;; flatten nested (+ ...)
           (if (and (consp norm) (eq (car norm) '+))
               (setf terms (nconc terms (cdr norm)))
               (push norm terms))))
       (setf terms (nreverse terms))
       (cond
         ((null terms) 0)
         ((null (cdr terms)) (car terms))
         (t (cons '+ terms)))))

    ;; Multiplication: SIGN * (f1 * f2 * ...) => numeric SCALE * product(Fi).
    ((and (consp sx) (eq (car sx) '*))
     (let ((scale (if (= sign 1) 1 -1))
           (factors '()))
       (dolist (factor (cdr sx))
         (let ((nf (normalize-signs-sexpr factor 1)))
           (cond
             ;; numeric factor: fold into SCALE
             ((numberp nf)
              (setf scale (* scale nf)))
             ;; nested product: flatten
             ((and (consp nf) (eq (car nf) '*))
              (setf factors (nconc factors (cdr nf))))
             ;; ordinary factor
             (t
              (push nf factors)))))
       (setf factors (nreverse factors))
       ;; build product from SCALE and FACTORS
       (cond
         ;; all zero
         ((zerop scale) 0)
         ;; scale = 1
         ((= scale 1)
          (cond
            ((null factors) 1)
            ((null (cdr factors)) (car factors))
            (t (cons '* factors))))
         ;; scale = -1
         ((= scale -1)
          (cond
            ((null factors) -1)
            ;; fold into first numeric factor if present
            ((numberp (car factors))
             (let ((first (car factors))
                   (rest  (cdr factors)))
               (cond
                 ((null rest) (- first))
                 (t (cons '* (cons (- first) rest))))))
            ;; otherwise explicit -1 factor
            (t
             (cons '* (cons -1 factors)))))
         ;; general nontrivial scale
         (t
          (cond
            ((null factors) scale)
            (t (cons '* (cons scale factors))))))))

    ;; Generic function call or other operator: recurse into args,
    ;; then apply SIGN as a unary minus or numeric negation.
    ((consp sx)
     (let* ((head (car sx))
            (args (mapcar (lambda (arg)
                            (normalize-signs-sexpr arg 1))
                          (cdr sx)))
            (inner (cons head args)))
       (if (= sign 1)
           inner
           (if (numberp inner)
               (- inner)
               (list '- inner)))))

    ;; Atom: apply SIGN directly.
    (t
     (if (= sign 1)
         sx
         (if (numberp sx)
             (- sx)
             (list '- sx))))))


(defun normalize-signs-expr (expr)
  "Normalize signs in EXPR-IR expression EXPR by lifting unary minus
out of products/sums and flattening where possible."
  (sexpr->expr-ir
   (normalize-signs-sexpr (expr->sexpr expr) 1)))



;;; ----------------------------------------------------------------------
;;; Expression -> prefix Lisp S-expressions
;;; ----------------------------------------------------------------------

(defun function-name->sexpr-symbol (sym)
  (multiple-value-bind (symbol status)
      (find-symbol (symbol-name sym) :cl)
    (if status
        symbol
        (intern (symbol-name sym) :expr-var))))

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
                (let* ((fn (function-call-name e))
                       (ffn (if (keywordp fn)
                                (function-name->sexpr-symbol fn)
                                fn)))
                  (cons ffn
                        (mapcar #'rec (function-call-arguments e)))))
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
                       (args (logical-arguments e))
                       (parts (mapcar (lambda (x)
                                        (rec x prec))
                                      args))
                       (s (ecase op
                            (:and (format nil "~{~a ~^&&~}" parts))
                            (:or (format nil "~{~a ~^||~}" parts)))))
                  (wrap s prec)))
               (logical-not-expression
                (let* ((arg (logical-not-argument-expression e))
                       (s (format nil "!~a" (rec arg prec))))
                  (wrap s prec)))
               (t
                (error "Don't know how to print ~S as infix." e)))))))
    (rec expr nil)))

(defun draw-sexpr-tree (sexpr &key (stream *standard-output*) (label nil))
  "Draw SEXPR as an ASCII tree.

Treats a list as (OP ARG1 ARG2 ...), with OP as the node label and
each argument as a child subtree.

Example:

  (+ (* x x) (* y y))

prints something like:

  +
  |- *
  |  |- x
  |  `- x
  `- *
     |- y
     `- y"
  (when label
    (format stream "~&~A~%" label))
  (let* ((expr-var-pkg (ignore-errors (find-package :expr-var)))
         (*package*    (or expr-var-pkg *package*)) ; hide expr-var:: prefixes
         (*print-pretty* t)
         (*print-escape* t)
         (*print-case* :downcase)
         (*print-right-margin* 100))

    (labels
        ((node-label (node)
           (if (and (consp node) (not (null node)))
               (car node)
               node))

         (node-children (node)
           (if (and (consp node) (not (null node)))
               (cdr node)
               nil))

         (print-line (prefix connector label)
           (format stream "~&~A~A~A~%" prefix connector label))

         (recur (node prefix is-last)
           (let ((lbl (node-label node))
                 (children (node-children node)))
             (if (null prefix)
                 ;; root: no connector
                 (print-line "" "" lbl)
                 (print-line prefix (if is-last "`- " "|- ") lbl))
             (when children
               (let* ((n (length children))
                      (i 0)
                      (child-prefix (concatenate 'string
                                                 prefix
                                                 (if is-last "   " "|  "))))
                 (dolist (child children)
                   (incf i)
                   (recur child child-prefix (= i n))))))))
      ;; root call: no prefix, treated specially in recur
      (recur sexpr nil t))))

(defun split-string-by-lines (s &key (omit-nulls t))
  (loop for start = 0 then (1+ end)
        for end = (position #\Newline s :start start)
        collect (subseq s start (or end (length s)))
        while end
        finally (when (and (not omit-nulls)
                           (char= (char s (1- (length s))) #\Newline))
                  (collect ""))))

(defun side-by-side-sexpr (s1 s2 &key label1 label2 (stream *standard-output*))
  (let ((t1 (split-string-by-lines
             (with-output-to-string (sout)
              (draw-sexpr-tree s1 :label label1 :stream sout))))
        (t2 (split-string-by-lines
             (with-output-to-string (sout)
               (draw-sexpr-tree s2 :label label2 :stream sout)))))
    (let* ((mlen (max (length t1) (length t2)))
           (max-width 0))
      (loop for l1 in t1
            for l2 in t2
            do (setf max-width (max max-width (length l1) (length l2))))
      (loop for idx from 0 below mlen
            for l1 = (or (nth idx t1) "")
            for l2 = (or (nth idx t2) "")
            do (format stream "~va # ~va #~%" max-width l1 max-width l2)))))

(defun debug-sexpr (sexpr &key (label nil) (stream *standard-output*))
  "Pretty-print S-EXPR with expr-var as the current package so
variable symbols print without package prefixes."
  (when label
    (format stream "~&~A:~%" label))
  (let ((*package* (find-package :expr-var))
        (*print-pretty* t)
        (*print-right-margin* 100)
        (*print-case* :downcase)
        (*print-escape* t))  ;; keep escapes, just drop package noise
    (pprint sexpr stream)))

(defun debug-expr (expr &key (label nil) (stream *standard-output*))
  "Pretty-print S-EXPR with expr-var as the current package so
variable symbols print without package prefixes."
  (when label
    (format stream "~&~A:~%" label))
  (let ((*package* (find-package :expr-var))
        (*print-pretty* t)
        (*print-right-margin* 100)
        (*print-case* :downcase)
        (*print-escape* t)
        (sexpr (expr-ir:expr->sexpr expr)))  ;; keep escapes, just drop package noise
    (pprint sexpr stream)))


(in-package :expr-ir)

;;; ----------------------------------------------------------------------
;;; Linear form representation
;;; ----------------------------------------------------------------------

(defstruct linear-form
  (const 0)
  (terms nil))   ; list of (var-symbol . coeff)

(defun make-linear-zero ()
  (make-linear-form :const 0 :terms nil))

(defun linear-form-add (lf1 lf2)
  "Return LF1 + LF2 as a new LINEAR-FORM."
  (let* ((const (+ (linear-form-const lf1)
                   (linear-form-const lf2)))
         (terms (copy-list (linear-form-terms lf1)))
         (result (make-linear-form :const const
                                   :terms terms)))
    (dolist (term (linear-form-terms lf2) result)
      (destructuring-bind (var . coeff2) term
        (let ((cell (assoc var terms)))
          (if cell
              (incf (cdr cell) coeff2)
              (push (cons var coeff2) terms)))))
    (setf (linear-form-terms result) terms)
    result))

(defun linear-form-scale (lf scalar)
  "Return SCALAR * LF as a new LINEAR-FORM."
  (let ((result (make-linear-form
                 :const (* scalar (linear-form-const lf))
                 :terms nil)))
    (dolist (term (linear-form-terms lf) result)
      (destructuring-bind (var . coeff) term
        (push (cons var (* scalar coeff))
              (linear-form-terms result))))))

(defun linear-form->expr (lf)
  "Convert LINEAR-FORM LF back into an expression IR node in canonical form.

LF represents:
  const + sum_i coeff_i * var_i."
  (let ((const (linear-form-const lf))
        (terms (linear-form-terms lf))
        (add-args '()))
    ;; constant term
    (unless (numeric-zero-p const)
      (push (make-expr-const const) add-args))
    ;; variable terms
    (dolist (term terms)
      (destructuring-bind (var . coeff) term
        (unless (numeric-zero-p coeff)
          (let ((var-expr (make-expr-var var)))
            (cond
              ((numeric-one-p coeff)
               (push var-expr add-args))
              ((numeric-minus-one-p coeff)
               (push (make-expr-neg var-expr) add-args))
              (t
               (push (make-expr-mul
                      (list (make-expr-const coeff)
                            var-expr))
                     add-args)))))))
    ;; build final expression
    (cond
      ;; no terms at all -> 0
      ((null add-args)
       (make-expr-const 0))
      ;; single term
      ((null (cdr add-args))
       (first add-args))
      ;; general sum
      (t
       (make-expr-add add-args)))))

;;; ----------------------------------------------------------------------
;;; Linear canonicalization walker
;;; ----------------------------------------------------------------------

(defun canonicalize-linear-subexprs (expr-or-sexpr)
  "Return a new expression where any linear numeric subexpressions have been
recognized as linear combinations of variables and rewritten into a canonical
form (constant + sum of coeff * var).

EXPR-OR-SEXPR can be either:
  - an expression IR node (subclass of EXPRESSION), or
  - a prefix S-expression understood by SEXPR->EXPR-IR."
  (let ((expr (if (typep expr-or-sexpr 'expression)
                  expr-or-sexpr
                  (sexpr->expr-ir expr-or-sexpr))))
    (multiple-value-bind (expr2 lf linear-p)
        (linearize-expression-internal expr)
      (declare (ignore lf linear-p))
      expr2)))

(defun linearize-expression-internal (expr)
  "Internal worker for CANONICALIZE-LINEAR-SUBEXPRS.

Returns three values:
  1) a possibly rewritten expression,
  2) a LINEAR-FORM object or NIL,
  3) T if the expression is linear in its variables, NIL otherwise."
  (labels
      ((rec (e)
         (typecase e
           ;; constants: c
           (constant-expression
            (let ((v (expression-value e)))
              (values e
                      (make-linear-form :const v :terms nil)
                      t)))

           ;; variables: x
           (variable-expression
            (let* ((name (variable-name e))
                   (lf   (make-linear-form
                          :const 0
                          :terms (list (cons name 1)))))
              (values e lf t)))

           ;; sums: e1 + e2 + ...
           (add-expression
            (let* ((args (expression-arguments e))
                   (new-args '())
                   (lf-acc (make-linear-zero))
                   (all-linear t))
              (dolist (arg args)
                (multiple-value-bind (arg2 lf-a lin-a)
                    (rec arg)
                  (push arg2 new-args)
                  (if lin-a
                      (setf lf-acc (linear-form-add lf-acc lf-a))
                      (setf all-linear nil))))
              (setf new-args (nreverse new-args))
              (if all-linear
                  (let ((lf lf-acc))
                    (values (linear-form->expr lf) lf t))
                  (values (make-expr-add new-args) nil nil))))

           ;; products: e1 * e2 * ...
           (multiply-expression
            (let* ((args      (expression-arguments e))
                   (new-args  '())
                   (lfs       '())
                   (lin-flags '()))
              ;; recurse on children
              (dolist (arg args)
                (multiple-value-bind (arg2 lf-a lin-a)
                    (rec arg)
                  (push arg2 new-args)
                  (push lf-a lfs)
                  (push lin-a lin-flags)))
              (setf new-args  (nreverse new-args))
              (setf lfs       (nreverse lfs))
              (setf lin-flags (nreverse lin-flags))

              ;; classify factors
              (let ((scalar        1) ;; product of pure-constant linear factors
                    (var-lf        nil) ;; the single linear form with variables, if any
                    (var-lf-count  0)
                    (nonlinear?    nil))
                (loop for lf    in lfs
                      for lin?  in lin-flags do
                        (cond
                          ;; non-linear factor present
                          ((not lin?)
                           (setf nonlinear? t))

                          ;; linear factor
                          (t
                           (let ((terms (and lf (linear-form-terms lf)))
                                 (c     (and lf (linear-form-const lf))))
                             (if (null terms)
                                 ;; pure constant linear factor
                                 (setf scalar (* scalar c))
                                 ;; has variable terms: candidate for primary linear factor
                                 (progn
                                   (incf var-lf-count)
                                   (if var-lf
                                       ;; more than one variable-linear factor => treat as non-linear
                                       (setf nonlinear? t)
                                       (setf var-lf lf))))))))

                ;; decide what to do
                (cond
                  ;; Case A: product is entirely linear (no non-linear factors, at most one var-lf)
                  ((and (not nonlinear?)
                        ;; either pure constant, or single linear factor with vars
                        (or (= var-lf-count 0)
                            (= var-lf-count 1)))
                   (let* ((lf (if var-lf
                                  (linear-form-scale var-lf scalar)
                                  (make-linear-form :const scalar :terms nil))))
                     (values (linear-form->expr lf) lf t)))

                  ;; Case B: there are non-linear factors, but exactly one var-lf:
                  ;; treat whole product as non-linear, but canonicalize the linear part
                  ((and nonlinear?
                        (= var-lf-count 1)
                        var-lf)
                   (let* ((scaled-lf (linear-form-scale var-lf scalar))
                          (lin-expr  (linear-form->expr scaled-lf))
                          (other-factors '()))
                     ;; rebuild product: drop linear-constant factors and the old var-lf,
                     ;; then append the canonical linear expr at the end
                     (loop for arg2  in new-args
                           for lf    in lfs
                           for lin?  in lin-flags do
                             (cond
                               ;; pure constant linear factor -> absorbed into scalar
                               ((and lin? lf (null (linear-form-terms lf)))
                                nil)
                               ;; variable linear factor -> replaced by lin-expr later
                               ((and lin? lf (linear-form-terms lf))
                                nil)
                               (t
                                (push arg2 other-factors))))
                     (setf other-factors (nreverse other-factors))
                     (let* ((all-factors (append other-factors (list lin-expr)))
                            (prod-expr  (if (null all-factors)
                                            ;; degenerate, but be defensive
                                            (make-expr-const 0)
                                            (make-expr-mul all-factors))))
                       ;; whole product is NOT linear, but children include a canonical linear factor
                       (values prod-expr nil nil))))

                  ;; Case C: anything else (multiple var-lfs, or weird combination):
                  ;; treat as general non-linear product, but keep recursed children.
                  (t
                   (values (make-expr-mul new-args) nil nil))))))
           #+(or)(multiply-expression
            (let* ((args (expression-arguments e))
                   (new-args '())
                   (lfs '())
                   (lin-flags '()))
              ;; recurse on children
              (dolist (arg args)
                (multiple-value-bind (arg2 lf-a lin-a)
                    (rec arg)
                  (push arg2 new-args)
                  (push lf-a lfs)
                  (push lin-a lin-flags)))
              (setf new-args (nreverse new-args))
              (setf lfs (nreverse lfs))
              (setf lin-flags (nreverse lin-flags))
              ;; determine if whole product is linear
              (let ((nonlinear nil)
                    (primary-lf nil)
                    (scalar 1))
                (loop for lf in lfs
                      for lin? in lin-flags do
                      (cond
                        ((not lin?)
                         (setf nonlinear t))
                        (t
                         (let ((terms (linear-form-terms lf))
                               (c     (linear-form-const lf)))
                           (if (null terms)
                               ;; pure constant factor
                               (setf scalar (* scalar c))
                               ;; has variable terms
                               (if primary-lf
                                   (setf nonlinear t)
                                   (setf primary-lf lf))))))
                      finally
                        (if (or nonlinear (null lfs))
                            ;; not a linear product; keep structure
                            (return (values (make-expr-mul new-args)
                                            nil nil))
                            ;; linear: either scalar * primary-lf, or constant only
                            (let* ((lf (if primary-lf
                                           (linear-form-scale primary-lf scalar)
                                           (make-linear-form
                                            :const scalar :terms nil))))
                              (return (values (linear-form->expr lf)
                                              lf t))))))))

           ;; numeric negation: -e
           (negate-expression
            (multiple-value-bind (arg2 lf-a lin-a)
                (rec (negate-argument-expression e))
              (if lin-a
                  (let ((lf (linear-form-scale lf-a -1)))
                    (values (linear-form->expr lf) lf t))
                  (values (make-expr-neg arg2) nil nil))))

           ;; powers: treat as non-linear (but canonicalize children)
           (power-expression
            (multiple-value-bind (base2 lf-base lin-base)
                (rec (power-base-expression e))
              (declare (ignore lf-base lin-base))
              (multiple-value-bind (exp2 lf-exp lin-exp)
                  (rec (power-exponent-expression e))
                (declare (ignore lf-exp lin-exp))
                (values (make-expr-pow base2 exp2) nil nil))))

           ;; function calls: non-linear, but recurse into args
           (function-call-expression
            (let* ((fname (function-call-name e))
                   (args  (function-call-arguments e))
                   (new-args '()))
              (dolist (arg args)
                (multiple-value-bind (arg2 lf-a lin-a)
                    (rec arg)
                  (declare (ignore lf-a lin-a))
                  (push arg2 new-args)))
              (values (make-expr-funcall fname (nreverse new-args))
                      nil nil)))

           ;; comparisons: recurse into numeric children
           (comparison-expression
            (multiple-value-bind (l lf-l lin-l)
                (rec (comparison-left-expression e))
              (declare (ignore lf-l lin-l))
              (multiple-value-bind (r lf-r lin-r)
                  (rec (comparison-right-expression e))
                (declare (ignore lf-r lin-r))
                (values (make-instance 'comparison-expression
                                       :operator (comparison-operator e)
                                       :left-expression l
                                       :right-expression r)
                        nil nil))))

           ;; logical AND/OR: recurse into arguments
           (logical-nary-expression
            (let ((new-args '()))
              (dolist (arg (logical-nary-arguments e))
                (multiple-value-bind (arg2 lf-a lin-a)
                    (rec arg)
                  (declare (ignore lf-a lin-a))
                  (push arg2 new-args)))
              (values (make-instance 'logical-nary-expression
                                     :operator (logical-nary-operator e)
                                     :arguments (nreverse new-args))
                      nil nil)))

           ;; logical NOT: recurse into argument
           (logical-not-expression
            (multiple-value-bind (arg2 lf-a lin-a)
                (rec (logical-not-argument-expression e))
              (declare (ignore lf-a lin-a))
              (values (make-instance 'logical-not-expression
                                     :argument-expression arg2)
                      nil nil)))

           ;; Fallback: unknown node type; leave as-is.
           (t
            (values e nil nil)))))
    (rec expr)))
