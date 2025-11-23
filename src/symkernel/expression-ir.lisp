(in-package :expr-ir)

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
      (print-unreadable-object (expr stream :type t :identity t)
        (handler-case
            (prin1 (expr->sexpr expr) stream)
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
  "Return a key used to canonically sort arguments in n-ary expressions.
Order:
  1. constants
  2. variables
  3. everything else

Within each class, order by a printable name where possible."
  (labels ((class-rank (expr)
             (cond
               ((typep expr 'constant-expression) 0)
               ((typep expr 'variable-expression) 1)
               (t 2)))
           (expr-name (expr)
             (typecase expr
               (constant-expression
                (princ-to-string (expression-value expr)))
               (variable-expression
                (symbol-name (variable-name expr)))
               (function-call-expression
                (symbol-name (function-call-name expr)))
               (t
                (format nil "~S" (type-of expr))))))
    (list (class-rank expression)
          (expr-name expression))))


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
    "Symbol naming the function (e.g. 'sin, 'cos, 'exp, 'log, 'sqrt, 'fabs).")
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
                  (make-expr-funcall fname sargs)))

               ;; comparisons
               (comparison-expression
                (make-instance 'comparison-expression
                               :operator (comparison-operator e)
                               :left-expression (simp (comparison-left-expression e))
                               :right-expression (simp (comparison-right-expression e))))

               ;; logical n-ary (AND/OR/etc.)
               (logical-nary-expression
                (make-instance 'logical-nary-expression
                               :operator (logical-nary-operator e)
                               :arguments (mapcar #'simp (logical-nary-arguments e))))

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
;;; Optional: simple constructor helpers (you can extend / replace these)
;;; ----------------------------------------------------------------------

(defun make-constant (numeric-value)
  "Convenience constructor for a constant-expression."
  (make-instance 'constant-expression :value numeric-value))

(defun make-variable (symbol-name)
  "Convenience constructor for a variable-expression."
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

