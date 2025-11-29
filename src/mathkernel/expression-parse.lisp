;;;; expression-parse.lisp
;;;; Front-ends for building expression IR:
;;;;   - canonicalizing constructors
;;;;   - S-expression prefix -> IR
;;;;   - basic infix string -> IR (arithmetic only)

(in-package :expr-ir)


;;; ----------------------------------------------------------------------
;;; Utility: expression ordering and helpers
;;; ----------------------------------------------------------------------

(defun expression< (a b)
  "Total but simple ordering on expressions, for canonicalization.
   This is not mathematically deep; it just provides a stable sort key."
  (labels ((rank (expr)
             (typecase expr
               (constant-expression      0)
               (variable-expression      1)
               (add-expression           2)
               (multiply-expression      3)
               (power-expression         4)
               (negate-expression        5)
               (function-call-expression 6)
               (comparison-expression    7)
               (logical-nary-expression  8)
               (logical-not-expression   9)
               (t                        10))))
    (let ((ra (rank a))
          (rb (rank b)))
      (cond
        ((< ra rb) t)
        ((> ra rb) nil)
        (t
         ;; Same rank: compare printed representations as a cheap tie-breaker.
         (string< (with-output-to-string (s) (prin1 a s))
                  (with-output-to-string (s) (prin1 b s))))))))

;;; ----------------------------------------------------------------------
;;; Canonicalizing constructors
;;; ----------------------------------------------------------------------

(defun ev (sym)
  "Ensure that the symbol is in expr-user"
  (cond
    ((null sym)
     (error "We cannot have a NIL variable name"))
    ((stringp sym)
     (intern sym :expr-var))
    ((symbolp sym)
     (if (eq (symbol-package sym) (find-package :expr-var))
         sym
         (intern (symbol-name sym) :expr-var)))
    (t (error "Handle ~s" sym))))

(defun ensure-keyword (sym)
  (cond
    ((stringp sym)
     (intern sym :keyword))
    ((symbolp sym)
  (if (eq (symbol-package sym) (find-package :keyword))
      sym
      (intern (symbol-name sym) :keyword)))
    (t (error "Handle ~s" sym))))


(defun make-expr-const (value)
  "Construct a constant-expression. You can add rational normalization here."
  (make-instance 'constant-expression :value value))

(defun make-expr-var (name)
  "Construct a variable-expression."
  (check-type name symbol)
  (make-variable (ev name)))

(defun make-expr-add (arguments)
  "Canonical constructor for ADD-EXPRESSION.

Simplifications:
  - Flatten nested ADD-EXPRESSION nodes.
  - Fold constant terms into a single constant.
  - Drop 0 terms.
  - If only one nonzero argument remains, return it directly.
  - Sort arguments canonically (constants first, then vars, etc.)."
  (let ((flat-args '())
        (constant-sum 0)
        (has-constant-p nil))
    ;; Flatten and split constants vs non-constants
    (dolist (arg arguments)
      (cond
        ((null arg)
         ;; ignore
         nil)
        ((typep arg 'add-expression)
         (dolist (sub (expression-arguments arg))
           (push sub flat-args)))
        ((typep arg 'constant-expression)
         (let ((value (expression-value arg)))
           (unless (numeric-zero-p value)
             (setf has-constant-p t)
             (setf constant-sum (+ constant-sum value)))))
        (t
         (push arg flat-args))))
    ;; Add combined constant term back in if nonzero
    (when (and has-constant-p
               (not (numeric-zero-p constant-sum)))
      (push (make-expr-const constant-sum) flat-args))
    ;; Remove explicit zeros that might have been left
    (setf flat-args
          (remove-if (lambda (expr)
                       (and (typep expr 'constant-expression)
                            (numeric-zero-p (expression-value expr))))
                     flat-args))
    (cond
      ;; No arguments -> 0
      ((null flat-args)
       (make-expr-const 0))
      ;; Single argument -> just that argument
      ((null (cdr flat-args))
       (first flat-args))
      (t
       ;; Sort canonically
       (let ((sorted (sort flat-args #'string<
                           :key (lambda (expr)
                                  (format nil "~S"
                                          (expression-sort-key expr))))))
         (make-instance 'add-expression
                        :arguments sorted))))))
(defun make-expr-mul (arguments)
  "Canonical constructor for MULTIPLY-EXPRESSION.

Simplifications:
  - Flatten nested MULTIPLY-EXPRESSION nodes.
  - If any factor is 0, return 0.
  - Multiply constant factors into a single constant.
  - Drop 1 factors.
  - Represent an overall -1 factor as a NEGATE-EXPRESSION when possible.
  - If only one factor remains, return it directly.
  - Sort arguments canonically (constants first, then vars, etc.)."
  (let ((flat-args '())
        (constant-prod 1)
        (has-constant-p nil))
    ;; Flatten and accumulate constants
    (dolist (arg arguments)
      (cond
        ((null arg)
         nil)
        ;; nested *
        ((typep arg 'multiply-expression)
         (dolist (sub (expression-arguments arg))
           (push sub flat-args)))
        ;; constant factor
        ((typep arg 'constant-expression)
         (let ((value (expression-value arg)))
           ;; Short-circuit zero
           (when (numeric-zero-p value)
             (return-from make-expr-mul (make-expr-const 0)))
           (setf has-constant-p t)
           (setf constant-prod (* constant-prod value))))
        (t
         (push arg flat-args))))
    ;; Append combined constant if not 1 or if there were no other args.
    (when has-constant-p
      (unless (numeric-one-p constant-prod)
        (push (make-expr-const constant-prod) flat-args)))
    ;; Remove explicit 1 factors
    (setf flat-args
          (remove-if (lambda (expr)
                       (and (typep expr 'constant-expression)
                            (numeric-one-p (expression-value expr))))
                     flat-args))
    ;; Zero- and one-argument cases
    (cond
      ;; Nothing left: product of only constant 1, or all dropped -> 1
      ((null flat-args)
       (make-expr-const 1))
      ;; Single factor: keep as-is (including possible constant -1)
      ((null (cdr flat-args))
       (let ((only (first flat-args)))
         ;; Special case: constant -1 * expr ⇒ -(expr) is handled
         ;; in callers; here we just return ONLY.
         only))
      (t
       ;; Look for an overall -1 factor we can pull out as a negation:
       (let* ((minus-one-idx
                (position-if (lambda (expr)
                               (and (typep expr 'constant-expression)
                                    (numeric-minus-one-p
                                     (expression-value expr))))
                             flat-args))
              (args-without-minus
                (if minus-one-idx
                    (let ((copy (copy-list flat-args)))
                      (setf (nth minus-one-idx copy) nil)
                      (remove nil copy))
                    flat-args)))
         (let ((sorted (sort args-without-minus #'string<
                             :key (lambda (expr)
                                    (format nil "~S"
                                            (expression-sort-key expr))))))
           (if minus-one-idx
               ;; Represent as -(product of remaining args)
               (make-expr-neg
                (if (null (cdr sorted))
                    (first sorted)
                    (make-instance 'multiply-expression
                                   :arguments sorted)))
               ;; No -1 factor; just return n-ary product
               (make-instance 'multiply-expression
                              :arguments sorted))))))))

(defun make-expr-pow (base exponent)
  "Canonical constructor for POWER-EXPRESSION.

Simplifications:
  - x^0 = 1 (for nonzero x; we assume domain OK).
  - x^1 = x.
  - 0^n = 0 for n > 0.
  - 1^n = 1 for any n."
  ;; Normalize exponent if it is a constant-expression
  (when (typep exponent 'constant-expression)
    (let ((n (expression-value exponent)))
      (cond
        ;; x^0 -> 1
        ((numeric-zero-p n)
         (return-from make-expr-pow (make-expr-const 1)))
        ;; x^1 -> x
        ((numeric-one-p n)
         (return-from make-expr-pow base)))))
  ;; Special cases for constant base
  (when (typep base 'constant-expression)
    (let ((b (expression-value base)))
      (cond
        ;; 0^n (n>0) -> 0
        ((and (numeric-zero-p b)
              (typep exponent 'constant-expression)
              (numberp (expression-value exponent))
              (> (expression-value exponent) 0))
         (return-from make-expr-pow (make-expr-const 0)))
        ;; 1^n -> 1
        ((numeric-one-p b)
         (return-from make-expr-pow (make-expr-const 1))))))
  ;; Default: keep as POWER-EXPRESSION
  (make-instance 'power-expression
                 :base-expression base
                 :exponent-expression exponent))

(defun make-expr-neg (argument)
  "Canonical constructor for NEGATE-EXPRESSION.

Simplifications:
  - Negate constants directly: -(c) = (-c).
  - Double negation: -(-f) = f.
  - Push minus into a leading constant in products: -(c*f) = (-c)*f."
  (cond
    ;; -(constant) -> constant
    ((typep argument 'constant-expression)
     (make-expr-const (- (expression-value argument))))
    ;; -(-f) -> f
    ((typep argument 'negate-expression)
     (negate-argument-expression argument))
    ;; -(c * rest) -> ( (-c) * rest )
    ((typep argument 'multiply-expression)
     (let* ((args (expression-arguments argument))
            (first-expr (first args)))
       (if (and (typep first-expr 'constant-expression))
           (let* ((c (expression-value first-expr))
                  (neg-c (make-expr-const (- c)))
                  (new-args (cons neg-c (rest args))))
             (make-expr-mul new-args))
           (make-instance 'negate-expression
                          :argument-expression argument))))
    (t
     (make-instance 'negate-expression
                    :argument-expression argument))))

(defun make-expr-funcall (name args)
  "Constructor for function-call-expression. No canonicalization beyond argument IR."
  (check-type name symbol)
  (unless (keywordp name)
    (error "All function names must be keywords - you have ~s in ~s" name (symbol-package name)))
  (make-instance 'function-call-expression
                 :function-name name
                 :argument-list args))

;;; ----------------------------------------------------------------------
;;; Prefix S-expression front-end
;;; ----------------------------------------------------------------------

(defun sexpr->expr-ir (form &key env)
  "Translate a Lisp S-expression into expression IR.
Supported forms (initial version):
  numbers   -> constant-expression
  symbols   -> variable-expression
  (+ a b c) -> add-expression
  (* a b c) -> multiply-expression
  (- a)     -> negation
  (- a b)   -> a + (-b)
  (/ a b)   -> a * b^-1
  (expt a b) or (^ a b) -> power-expression
  (< a b), etc.        -> comparison-expression
  (and ...), (or ...), (not ...) -> logical expressions
  (f a b c) -> function-call-expression for other symbols.

ENV is reserved for future use (e.g. to distinguish some function names)."
  (declare (ignore env))
  (declare (optimize (debug 3)))
  (cond
    ;; Already an expression node
    ((typep form 'expression)
     form)
    ;; Numeric literal
    ((numberp form)
     (make-expr-const form))
    ;; Symbol -> variable
    ((symbolp form)
     (make-expr-var (ev form)))
    ;; List / compound form
    ((consp form)
     (let* ((op   (car form))
            (args (cdr form)))
       (labels ((rec (x) (sexpr->expr-ir x :env env)))
         (case op
           ;; arithmetic
           ((+)
            (make-expr-add (mapcar #'rec args)))
           ((*)
            (make-expr-mul (mapcar #'rec args)))
           ((-)
            (cond
              ((null args)
               (error "(-) with no arguments is invalid."))
              ((null (cdr args))
               ;; unary minus
               (make-expr-neg (rec (car args))))
              (t
               ;; a - b - c -> a + (-b) + (-c)
               (let* ((first (rec (car args)))
                      (rest  (mapcar (lambda (x)
                                       (make-expr-neg (rec x)))
                                     (cdr args))))
                 (make-expr-add (cons first rest))))))
           ((/)
            (cond
              ((null args)
               (error "(/) with no arguments is invalid."))
              ((null (cdr args))
               ;; 1/x
               (make-expr-mul
                (list (make-expr-const 1)
                      (make-expr-pow (rec (car args))
                                     (make-expr-const -1)))))
              ((null (cddr args))
               ;; a / b
               (make-expr-mul
                (list (rec (car args))
                      (make-expr-pow (rec (cadr args))
                                     (make-expr-const -1)))))
              (t
               ;; a / b / c -> (a / b) / c
               (sexpr->expr-ir (list '/ (list '/ (car args) (cadr args))
                                     (caddr args))
                               :env env))))
           ((expt ^)
            (unless (= (length args) 2)
              (error "~S expects 2 arguments, got ~D" op (length args)))
            (make-expr-pow (rec (first args))
                           (rec (second args))))
           ;; comparisons
           ((< > <= >= = /=)
            (unless (= (length args) 2)
              (error "~S comparison expects exactly 2 arguments." op))
            (let ((left  (rec (first args)))
                  (right (rec (second args)))
                  (kw    (ecase op
                           (<  :<)
                           (>  :>)
                           (<= :<=)
                           (>= :>=)
                           (=  :=)
                           (/= :/=))))
              (make-instance 'comparison-expression
                             :operator kw
                             :left-expression left
                             :right-expression right)))
           ;; logical
           ((and or)
            (let ((children (mapcar #'rec args))
                  (kw (if (eq op 'and) :and :or)))
              (make-instance 'logical-nary-expression
                             :operator kw
                             :arguments children)))
           ((not)
            (unless (= (length args) 1)
              (error "(not ...) expects exactly one argument."))
            (make-instance 'logical-not-expression
                           :argument-expression (rec (first args))))
           ;; default: function call
           (t
            (make-expr-funcall (ensure-keyword op) (mapcar #'rec args)))))))
    (t
     (error "Cannot convert ~S to expression IR." form))))

;;; ----------------------------------------------------------------------
;;; Basic infix parser (arithmetic only)
;;; ----------------------------------------------------------------------
;;; This is a minimal, self-contained recursive descent parser for:
;;;   - numbers
;;;   - identifiers
;;;   - function calls: f(x, y)
;;;   - +, -, *, /, ^, parentheses
;;;
;;; It does NOT yet handle comparisons (<, >, etc.) or logical operators.

;;;; Tokenizer

(defstruct (token (:constructor make-token (type value)))
  type   ; :number, :ident, :symbol, :lparen, :rparen, :comma, :eof
  value)

(defun %digit-char-p (ch)
  (and ch (digit-char-p ch)))

(defun %letter-char-p (ch)
  (and ch (or (alpha-char-p ch)
              (char= ch #\_))))

(defun whitespace-char-p (ch)
  "Return true if CH is one of the standard whitespace characters."
  (and ch
       (member ch '(#\Space #\Tab #\Newline #\Return #\Page)
               :test #'char=)))

(defun tokenize (string)
  "Return a list of tokens from STRING."
  (let ((len (length string))
        (i 0)
        (tokens '()))
    (labels ((peek () (if (< i len) (aref string i) nil))
             (advance () (prog1 (peek) (incf i)))
             (skip-space ()
               (loop while (and (< i len) (whitespace-char-p (peek))) do (incf i)))
             (read-number ()
               (let ((start i)
                     (seen-dot nil))
                 (loop while (< i len) do
                   (let ((ch (peek)))
                     (cond
                       ((%digit-char-p ch)
                        (incf i))
                       ((and (char= ch #\.) (not seen-dot))
                        (setf seen-dot t)
                        (incf i))
                       (t (return)))))
                 (make-token :number
                             (read-from-string (subseq string start i)))))
             (read-ident ()
               (let ((start i))
                 (loop while (< i len) do
                   (let ((ch (peek)))
                     (if (or (%letter-char-p ch)
                             (%digit-char-p ch))
                         (incf i)
                         (return))))
                 (make-token :ident
                             (intern (string-upcase (subseq string start i))
                                     :keyword))))
             (single-symbol-token (ch)
               (make-token :symbol ch)))
      (loop
        (skip-space)
        (if (>= i len)
            (return (nreverse (cons (make-token :eof nil) tokens))))
        (let ((ch (peek)))
          (cond
            ((%digit-char-p ch)
             (push (read-number) tokens))
            ((%letter-char-p ch)
             (push (read-ident) tokens))
            ((char= ch #\()
             (advance)
             (push (make-token :lparen #\() tokens))
            ((char= ch #\))
             (advance)
             (push (make-token :rparen #\)) tokens))
            ((char= ch #\,)
             (advance)
             (push (make-token :comma #\,) tokens))
            ;; comparison operators: <, >, <=, >=, ==, !=
            ((member ch '(#\< #\> #\= #\!))
             (advance)
             (let* ((next (peek))
                    (op-str
                      (cond
                        ;; <= or >=
                        ((and (member ch '(#\< #\>) :test #'char=)
                              next
                              (char= next #\=))
                         (advance)
                         (if (char= ch #\<) "<=" ">="))
                        ;; ==
                        ((and (char= ch #\=)
                              next
                              (char= next #\=))
                         (advance)
                         "==")
                        ;; !=
                        ((and (char= ch #\!)
                              next
                              (char= next #\=))
                         (advance)
                         "!=")
                        ;; single-character < or >
                        (t
                         (string ch)))))
               (push (make-token :cmp op-str) tokens)))
            ((member ch '(#\+ #\- #\* #\/ #\^))
             (advance)
             (push (single-symbol-token ch) tokens))
            (t
             (error "Unexpected character ~C in infix expression." ch))))))))

;;;; Parser state

(defstruct parser-state
  tokens
  (pos 0))

(defun ps-current (ps)
  (let* ((tokens (parser-state-tokens ps))
         (pos    (parser-state-pos ps)))
    (if (< pos (length tokens))
        (aref tokens pos)
        (make-token :eof nil))))

(defun ps-advance (ps)
  (incf (parser-state-pos ps)))

(defun ps-match-symbol (ps ch)
  (let ((tok (ps-current ps)))
    (when (and (eq (token-type tok) :symbol)
               (char= (token-value tok) ch))
      (ps-advance ps)
      t)))

(defun ps-expect (ps type &optional message)
  (let ((tok (ps-current ps)))
    (unless (eq (token-type tok) type)
      (error (or message
                 "Expected token of type ~S but got ~S (~S).")
             type (token-type tok) tok))
    (ps-advance ps)
    tok))

;;;; Recursive descent for precedence:
;;;; expr -> additive
;;;; additive -> multiplicative (('+'|'-') multiplicative)*
;;;; multiplicative -> power (('*'|'/') power)*
;;;; power -> unary ('^' power)?
;;;; unary -> ('+'|'-') unary | primary
;;;; primary -> number | ident '(' args ')' | ident | '(' expr ')'

(defun parse-primary (ps)
  (let ((tok (ps-current ps)))
    (cond
      ;; number
      ((eq (token-type tok) :number)
       (ps-advance ps)
       (make-expr-const (token-value tok)))
      ;; identifier: variable or function call
      ((eq (token-type tok) :ident)
       (ps-advance ps)
       (let* ((name-sym (ev (token-value tok))))
         (if (eq (token-type (ps-current ps)) :lparen)
             ;; function call
             (progn
               (ps-advance ps) ; consume '('
               (let ((args '()))
                 (unless (eq (token-type (ps-current ps)) :rparen)
                   (loop
                     (push (parse-expr-infix ps) args)
                     (if (eq (token-type (ps-current ps)) :comma)
                         (progn (ps-advance ps) (continue))
                         (return))))
                 (ps-expect ps :rparen "Expected ')' after function arguments.")
                 (make-expr-funcall (ensure-keyword name-sym) (nreverse args))))
             ;; simple variable
             (make-expr-var name-sym))))
      ;; '(' expr ')'
      ((eq (token-type tok) :lparen)
       (ps-advance ps)
       (let ((expr (parse-expr-infix ps)))
         (ps-expect ps :rparen "Expected ')' to close parenthesized expression.")
         expr))
      (t
       (error "Unexpected token ~S in primary expression." tok)))))

(defun parse-unary (ps)
  (let ((tok (ps-current ps)))
    (cond
      ;; unary + or -
      ((and (eq (token-type tok) :symbol)
            (member (token-value tok) '(#\+ #\-)))
       (ps-advance ps)
       (let ((sub (parse-unary ps)))
         (if (char= (token-value tok) #\-)
             (make-expr-neg sub)
             sub)))
      (t
       (parse-primary ps)))))

(defun parse-power (ps)
  (let ((base (parse-unary ps)))
    (if (ps-match-symbol ps #\^)
        (let ((exp (parse-power ps))) ; right-associative
          (make-expr-pow base exp))
        base)))

(defun parse-multiplicative (ps)
  (let ((expr (parse-power ps)))
    (loop
      (let ((tok (ps-current ps)))
        (if (and (eq (token-type tok) :symbol)
                 (member (token-value tok) '(#\* #\/)))
            (let ((op (token-value tok)))
              (ps-advance ps)
              (let ((rhs (parse-power ps)))
                (setf expr
                      (if (char= op #\*)
                          (make-expr-mul (list expr rhs))
                          ;; division: expr / rhs -> expr * rhs^-1
                          (make-expr-mul
                           (list expr
                                 (make-expr-pow rhs (make-expr-const -1))))))))
            (return expr))))))

(defun parse-additive (ps)
  (let ((expr (parse-multiplicative ps)))
    (loop
      (let ((tok (ps-current ps)))
        (if (and (eq (token-type tok) :symbol)
                 (member (token-value tok) '(#\+ #\-)))
            (let ((op (token-value tok)))
              (ps-advance ps)
              (let ((rhs (parse-multiplicative ps)))
                (setf expr
                      (if (char= op #\+)
                          (make-expr-add (list expr rhs))
                          ;; expr - rhs -> expr + (-rhs)
                          (make-expr-add
                           (list expr (make-expr-neg rhs)))))))
            (return expr))))))

(defun parse-comparison (ps)
  "Parse comparisons of the form:
     additive [ ( < | > | <= | >= | == | != ) additive ].

If no comparison operator follows, returns the additive expression."
  (let ((lhs (parse-additive ps)))
    (let ((tok (ps-current ps)))
      (if (eq (token-type tok) :cmp)
          (let* ((op-str (token-value tok))
                 (op-kw  (cond
                           ((string= op-str "<")  :<)
                           ((string= op-str ">")  :>)
                           ((string= op-str "<=") :<=)
                           ((string= op-str ">=") :>=)
                           ((string= op-str "==") :=)
                           ((string= op-str "!=") :/=)
                           (t (error "Unknown comparison operator ~S" op-str)))))
            (ps-advance ps) ; consume operator
            (let ((rhs (parse-additive ps)))
              (make-instance 'comparison-expression
                             :operator op-kw
                             :left-expression lhs
                             :right-expression rhs)))
          lhs))))

(defun parse-expr-infix (ps)
  "Top-level entry for the infix recursive-descent parser."
    (parse-comparison ps))

(defun infix->expr-ir (string &key env)
  "Parse a STRING in infix arithmetic syntax into expression IR.
Supported syntax (initial version):
  - numbers (integer / float)
  - identifiers (mapped to variable-expression)
  - function calls: f(x, y)
  - operators: +, -, *, /, ^, and parentheses

Not yet supported: comparison (<, >, ==, etc.) or logical operators (&&, ||)."
  (declare (ignore env))
  (let* ((tokens (coerce (tokenize string) 'vector))
         (ps     (make-parser-state :tokens tokens :pos 0))
         (expr   (parse-expr-infix ps))
         (tok    (ps-current ps)))
    (unless (eq (token-type tok) :eof)
      (error "Unexpected trailing tokens after parsing infix expression: ~S" tok))
    expr))

;;; ----------------------------------------------------------------------
;;; Unified front-end
;;; ----------------------------------------------------------------------

(defun parse-expr (thing &key env)
  "Dispatch to the appropriate front-end:
   - If THING is already an expression node, return it.
   - If THING is a string, parse it as infix syntax.
   - Otherwise treat it as a Lisp S-expression prefix form."
  (cond
    ((typep thing 'expression)
     thing)
    ((stringp thing)
     (infix->expr-ir thing :env env))
    (t
     (sexpr->expr-ir thing :env env))))
