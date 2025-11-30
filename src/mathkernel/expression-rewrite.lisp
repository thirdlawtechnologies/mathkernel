;;;; ----------------------------------------------------------------------
;;;; expression-rewrite.lisp
;;;; ----------------------------------------------------------------------


(in-package :expr-ir)

;;; ----------------------------------------------------------------------
;;; Wildcard utilities
;;; ----------------------------------------------------------------------

(defun wildcard-symbol-p (sym)
  "True if SYM is a pattern wildcard like ?x or ?rest*."
  (and (symbolp sym)
       (let ((name (symbol-name sym)))
         (and (> (length name) 0)
              (char= (char name 0) #\?)))))

(defun sequence-wildcard-symbol-p (sym)
  "True if SYM is a *sequence* wildcard like ?rest* (only supported in lists)."
  (and (wildcard-symbol-p sym)
       (let* ((name (symbol-name sym))
              (last (char name (1- (length name)))))
         (char= last #\*))))

(defun wildcard-base-name (sym)
  "Return a canonical key for wildcard SYM, stripping leading ? and trailing *."
  (let* ((name (symbol-name sym))
         (start 1)
         (end   (length name)))
    (when (and (> end 0)
               (char= (char name (1- end)) #\*))
      (decf end))
    (intern (subseq name start end) :keyword)))


;;; ------------------------------------------------------------
;;; Rule hierarchy
;;; ------------------------------------------------------------

(defclass rewrite-rule ()
  ((name
    :initarg :name
    :accessor rewrite-rule-name
    :initform nil
    :documentation "Symbolic name or keyword for debugging / tracing.")
   (pattern
    :initarg :pattern
    :accessor rewrite-rule-pattern
    :documentation "Pattern sexpr (may contain wildcards).")
   (predicate
    :initarg :predicate
    :accessor rewrite-rule-predicate
    :initform nil
    :documentation "Optional (lambda (env sexpr) ...) -> generalized boolean.
Called after a successful pattern match; if it returns NIL, the rule does not fire."))
  (:documentation "Abstract base class for s-expression rewrite rules."))

(defclass template-rule (rewrite-rule)
  ((template
    :initarg :template
    :accessor template-rule-template
    :documentation "Replacement sexpr template, using the same wildcards
as the pattern."))
  (:documentation "Rewrite rule where replacement is a template sexpr."))

(defclass function-rule (rewrite-rule)
  ((transformer
    :initarg :transformer
    :accessor function-rule-transformer
    :documentation "Function of (env sexpr) -> replacement sexpr."))
  (:documentation "Rewrite rule where replacement is produced by a function."))

;;; ------------------------------------------------------------
;;; Constructors
;;; ------------------------------------------------------------

(defun make-template-rule (name pattern template &key predicate)
  "Create a template rewrite rule:
PATTERN (sexpr with wildcards) -> TEMPLATE (sexpr template).

Example:
  (make-template-rule
    :add-zero-right
    '(+ ?x 0 ?rest*)
    '(+ ?x ?rest*))"
  (make-instance 'template-rule
                 :name name
                 :pattern pattern
                 :template template
                 :predicate predicate))

(defun make-function-rule (name pattern fn &key predicate)
  "Create a function rewrite rule:
PATTERN (sexpr with wildcards) -> (FN ENV SEXPR) => replacement sexpr.

Example:
  (make-function-rule
    :normalize-power
    '(^ ?base ?exp)
    (lambda (env sexpr) ...))"
  (make-instance 'function-rule
                 :name name
                 :pattern pattern
                 :transformer fn
                 :predicate predicate))



(defun lookup-wild (wild env)
  "Helper: get the value bound to wildcard symbol WILD in ENV."
  (let* ((key  (expr-ir:wildcard-base-name wild))
         (cell (assoc key env)))
    (when cell (cdr cell)))
  )


(defun apply-rules-to-sexpr-once (in-sexpr rules &key verbose)
  "Try to apply RULES once somewhere in SEXPR.

Returns two values:
  new-sexpr, changed-p."
  (declare (optimize (debug 3)))
  (labels
      ((try-rules-here (sexpr)
         (dolist (rule rules)
           (let* ((pat (rewrite-rule-pattern rule)))
             (multiple-value-bind (env success-p)
                 (match-pattern pat sexpr nil)
               (if success-p
                 (let ((pred (rewrite-rule-predicate rule)))
                   (when (or (null pred)
                             (funcall pred env sexpr))
                     (return-from try-rules-here
                       (values
                        (let ((sexpr2 (typecase rule
                                        (template-rule
                                         (instantiate-template
                                          (template-rule-template rule) env))
                                        (function-rule
                                         (funcall (function-rule-transformer rule) env sexpr))
                                        (t
                                         ;; Shouldn't happen if you only construct the two subclasses
                                         sexpr))))
                          (when verbose
                            (let ((*package* (find-package :expr-var)))
                              (format t "Applied rule ~s to:~%in  = ~s~%out = ~s~%"
                                      (rewrite-rule-name rule)
                                      sexpr
                                      sexpr2)))
                          sexpr2)
                        t))))
                 (when verbose
                   (let ((*package* (find-package :expr-var)))
                     (format t "No match rule ~s to:~%in  = ~s~%"
                             (rewrite-rule-name rule)
                             sexpr)))
                 ))))
         (values sexpr nil))
       (recur (sexpr)
         (cond
           ((atom sexpr)
            (values sexpr nil))
           (t
            (multiple-value-bind (s2 changed-here)
                (try-rules-here sexpr)
              (when changed-here
                (return-from recur (values s2 t)))
              (let* ((head (car sexpr))
                     (args (cdr sexpr))
                     (new-args '())
                     (changed-any nil))
                (dolist (a args (values (cons head (nreverse new-args))
                                        changed-any))
                  (multiple-value-bind (a2 ch)
                      (recur a)
                    (when ch (setf changed-any t))
                    (push a2 new-args)))))))))
    (recur in-sexpr)))


(defun match-pattern (pattern expr &optional env)
  "Try to match PATTERN against EXPR, returning (values ENV SUCCESS-P).

PATTERN and EXPR are s-expressions.

ENV is an association list mapping wildcard keys (from WILDCARD-BASE-NAME)
to bound values. On success, ENV contains (or extends) the bindings; on
failure, SUCCESS-P is NIL and ENV is NIL.

Supported wildcards:

  ?x       → single subtree
  ?rest*   → sequence wildcard, matches zero or more elements in a list
             position (may appear anywhere in a list pattern, and there
             may be multiple sequence wildcards).

Sequence wildcard bindings are the list of matched elements. Normal
wildcard bindings are the single matched subexpression."
  (labels
      ((bind-wildcard (key value env)
         "Bind KEY to VALUE in ENV, or check consistency with existing binding.
Returns the extended ENV on success, or NIL on conflict."
         (let ((cell (assoc key env)))
           (cond
             ((null cell)
              (cons (cons key value) env))
             ((equal (cdr cell) value)
              env)
             (t
              nil))))                   ; end bind-wildcard

       (match-list (pats exprs env)
         "Match a LIST of patterns PATS against a LIST of exprs EXPRS,
given ENV. Returns (values ENV SUCCESS-P)."
         (cond
           ;; both empty: success
           ((and (null pats) (null exprs))
            (values env t))

           ;; patterns empty, exprs not: fail
           ((null pats)
            (values nil nil))

           ;; patterns remain, exprs empty
           ((null exprs)
            (let ((p1 (car pats))
                  (rest-pats (cdr pats)))
              (if (sequence-wildcard-symbol-p p1)
                  ;; sequence wildcard can match empty tail
                  (let* ((key  (wildcard-base-name p1))
                         (env1 (bind-wildcard key '() env)))
                    (if env1
                        (match-list rest-pats '() env1)
                        (values nil nil)))
                  ;; non-sequence pattern cannot match empty exprs
                  (values nil nil))))

           ;; first pattern is a sequence wildcard: try all splits
           ((sequence-wildcard-symbol-p (car pats))
            (let* ((w-sym (car pats))
                   (key   (wildcard-base-name w-sym))
                   (rest-pats (cdr pats))
                   (len (length exprs)))
              (labels
                  ((try-split (k)
                     (if (> k len)
                         (values nil nil)
                         (let* ((prefix     (subseq exprs 0 k))
                                (rest-exprs (nthcdr k exprs))
                                (env1       (bind-wildcard key prefix env)))
                           (if env1
                               (multiple-value-bind (env2 ok2)
                                   (match-list rest-pats rest-exprs env1)
                                 (if ok2
                                     (values env2 t)
                                     (try-split (1+ k))))
                               (try-split (1+ k)))))))
                (try-split 0))))

           ;; normal element: match head, then tail
           (t
            (let ((p1 (car pats))
                  (e1 (car exprs)))
              (multiple-value-bind (env1 ok1)
                  (match-pattern p1 e1 env)
                (if ok1
                    (match-list (cdr pats) (cdr exprs) env1)
                    (values nil nil))))))) ; end match-list
       )                                   ; end LABELS
    (cond
      ;; wildcard atom (single or sequence)
      ((wildcard-symbol-p pattern)
       (let* ((key  (wildcard-base-name pattern))
              (env1 (bind-wildcard key expr env)))
         (if env1
             (values env1 t)
             (values nil nil))))

      ;; non-wildcard atom: literal match
      ((atom pattern)
       (if (eql pattern expr)
           (values env t)
           (values nil nil)))

      ;; list pattern
      ((consp pattern)
       (if (not (consp expr))
           (values nil nil)
           (multiple-value-bind (env1 ok1)
               (match-pattern (car pattern) (car expr) env)
             (if ok1
                 (match-list (cdr pattern) (cdr expr) env1)
                 (values nil nil)))))

      ;; anything else: fail
      (t
       (values nil nil)))))







(defun instantiate-template (template env)
  "Instantiate TEMPLATE (a sexpr) using ENV (alist of wildcard-key -> sexpr).

Single wildcards (?x) are replaced by their bound sexprs.
Sequence wildcards (?rest*) in list position are spliced in."
  (labels
      ((lookup (sym)
         (let* ((key (wildcard-base-name sym))
                (cell (assoc key env)))
           (when cell
             (cdr cell))))
       (inst (node)
         (cond
           ;; sequence wildcard outside list context: treat like single
           ((sequence-wildcard-symbol-p node)
            (or (lookup node)
                (error "No binding for sequence wildcard ~S" node)))
           ((wildcard-symbol-p node)
            (or (lookup node)
                (error "No binding for wildcard ~S" node)))
           ((atom node) node)
           ((consp node)
            (let* ((head (inst (car node)))
                   (args (cdr node))
                   (new-args '()))
              (dolist (a args (cons head (nreverse new-args)))
                (cond
                  ((sequence-wildcard-symbol-p a)
                   (let* ((val (lookup a)))
                     (dolist (sub val)
                       (push (inst sub) new-args))))
                  (t
                   (push (inst a) new-args))))))
           (t node))))
    (inst template)))



(defparameter *rewrite-rules-basic*
  (list
   ;; (- x x) -> 0
   (make-template-rule
    :minus-same
    '(- ?x ?x)
    '0)

   ;; (+ ?x 0 ?rest*) -> (+ ?x ?rest*)
   (make-template-rule
    :add-zero-right
    '(+ ?x 0 ?rest*)
    '(+ ?x ?rest*))

   ;; (+ 0 ?x ?rest*) -> (+ ?x ?rest*)
   (make-template-rule
    :add-zero-left
    '(+ 0 ?x ?rest*)
    '(+ ?x ?rest*))

   ;; (* ?x 1 ?rest*) -> (* ?x ?rest*)
   (make-template-rule
    :mul-one-right
    '(* ?x 1 ?rest*)
    '(* ?x ?rest*))

   ;; (* 1 ?x ?rest*) -> (* ?x ?rest*)
   (make-template-rule
    :mul-one-left
    '(* 1 ?x ?rest*)
    '(* ?x ?rest*))

   ;; negative of negative
   (make-template-rule
    :neg-neg
    '(- (- ?y))
    '?y)

   (expr-ir:make-template-rule
    :sqrt-expt
    '(sqrt ?x)
    '(expt ?x 1/2))

   (expr-ir:make-template-rule
    :expt-expt
    '(expt (expt ?x ?y) ?z)
    '(expt ?x (* ?y ?z)))

   (expr-ir:make-template-rule
    :expt*expt
    '(* ?prefix* (expt ?x ?a) (expt ?x ?b) ?rest*)
    '(* ?prefix* (expt ?x (+ ?a ?b)) ?rest*))

   (expr-ir:make-template-rule
    :expt-absorb-base-1
    '(* ?prefix* (expt ?x ?a) ?mid* ?x ?rest*)
    '(* ?prefix* (expt ?x (+ ?a 1)) ?mid* ?rest*))

   (expr-ir:make-template-rule
    :expt-absorb-base-2
    '(* ?prefix* ?x ?mid* (expt ?x ?a) ?rest*)
    '(* ?prefix* ?mid* (expt ?x (+ ?a 1)) ?rest*))
   ))




(defparameter *optimization-rules*
  (list*

   (make-template-rule
    :combine-expt^-1
    '(* ?prefix* (expt ?x -1) (expt ?y -1) ?rest*)
    '(* ?prefix* (expt (* ?x ?y) -1) ?rest*))

   (make-template-rule
    :expt-zero
    '(expt ?x 0)
    '1)

   (make-template-rule
    :expt-one
    '(expt ?x 1)
    '?x)
   *rewrite-rules-basic*))



(defun negate-sum-transform (env sexpr)
  "Transform (- (+ a b c ...)) into (+ (- a) (- b) (- c) ...)."
  (declare (ignore sexpr))
  (let* ((terms (lookup-wild '?terms* env))   ;; list of summands, possibly NIL
         (negated-args (mapcar (lambda (tt) (list '- tt)) terms)))
    (cons '+ negated-args)))


(defun expand-expt-of-factors (env sexpr)
  "Transform (- (+ a b c ...)) into (+ (- a) (- b) (- c) ...)."
  (declare (ignore sexpr))
  (let* ((factors (lookup-wild '?factors* env))
         (power (lookup-wild '?p env))
         (expanded-factors (mapcar (lambda (tt) `(expt ,tt ,power)) factors)))
    (cons '* expanded-factors)))

(defparameter *equivalence-canonicalization-rules*
  (list*

   (make-template-rule
    :mul-over-add-2
    '(* ?c (+ ?a ?b))
    '(+ (* ?c ?a)
      (* ?c ?b)))

   (make-template-rule
    :neg-pair-square-1
    '(* ?prefix* ?x ?mid* (- ?x) ?rest*)
    '(* ?prefix* ?mid* (- (* ?x ?x)) ?rest*))

   (make-template-rule
    :neg-pair-square-2
    '(* ?prefix* (- ?x) ?mid* ?x ?rest*)
    '(* ?prefix* ?mid* (- (* ?x ?x)) ?rest*))

   (make-template-rule
    :pull-neg-out-of-mul
    '(* ?prefix* (- ?arg) ?rest*)
    '(- (* ?prefix* ?arg ?rest*)))

   (make-function-rule
    :negate-leading-sum
    '(- (+ ?terms*))
    #'negate-sum-transform)

   (make-function-rule
    :expt-of-factors-factors-of-expts
    '(expt (* ?factors*) ?p)
    #'expand-expt-of-factors)

   (make-template-rule
    :add-2
    '(+ ?x ?x)
    '(* 2 ?x))

   *rewrite-rules-basic*))


(defun rewrite-expr-ir-with-rules (expr &key (rules *rewrite-rules-basic*) (max-iterations 5) verbose)
  "Rewrite a single EXPR-IR expression using RULES (a list of REWRITE-RULE).
Conversion path:
  expr-ir -> sexpr -> rewrite (many steps) -> expr-ir -> simplify-expr."
  (let ((sx (expr-ir:expr->sexpr expr)))
    (dotimes (i max-iterations)
      (multiple-value-bind (new-sx changed)
          (apply-rules-to-sexpr-once sx rules)
        (setf sx new-sx)
        (unless changed
          (return))))
    (expr-ir:simplify-expr
     (expr-ir:sexpr->expr-ir sx))))





(defmacro with-kernel-rewrite-rules ((extra-equivalence extra-optimization)
                                     &body body)
  "Temporarily extend the global rewrite rule sets for BODY.

EXTRA-EQUIVALENCE and EXTRA-OPTIMIZATION are NIL or lists of rewrite-rule
objects (template-rule or function-rule). They are prepended so that
kernel-specific rules have priority over the global ones."
  `(let ((*equivalence-canonicalization-rules*
           (if ,extra-equivalence
               (append ,extra-equivalence
                       *equivalence-canonicalization-rules*)
               *equivalence-canonicalization-rules*))
         (*optimization-rules*
           (if ,extra-optimization
               (append ,extra-optimization
                       *optimization-rules*)
               *optimization-rules*)))
     ,@body))
