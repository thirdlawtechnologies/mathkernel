;;;; ----------------------------------------------------------------------
;;;; statement-ir.lisp
;;;; ----------------------------------------------------------------------
;;;; Statement / control-flow IR for C code generation.
;;;; Uses the expression IR defined in :expr-ir.


(in-package :stmt-ir)

(defparameter *factor-temp-param-debug* nil)

(defclass env-entry ()
  ((index
    :initarg :index
    :reader env-entry-index
    :documentation "Statement index where the symbol was defined.")
   (expr
    :initarg :expr
    :initform nil
    :reader env-entry-expr
    :documentation "Expression associated with the definition, when available."))
  (:documentation "Track where a symbol was defined for CSE scheduling."))

(defclass outer-env-entry (env-entry) ()
  (:documentation "Env entry that originated in an outer block."))

(defclass binding-env ()
  ((table
    :initarg :table
    :reader %binding-env-table
    :documentation "Hash table mapping symbols to env-entry/outer-env-entry.")
   (parent
    :initarg :parent
    :initform nil
    :accessor binding-env-parent
    :documentation "Optional parent environment for chained lookups.")
   (block
    :initarg :block
    :initform nil
    :accessor binding-env-block
    :documentation "Block associated with this environment (for diagnostics/insertion)."))
  (:documentation "Environment that tracks where symbols are defined for CSE.
Assumes SSA: no variable is reassigned, and branch definitions do not join.
Environments can be chained via PARENT; lookups walk the chain."))

(defmethod print-object ((env binding-env) stream)
  (let ((vars '()))
    (maphash (lambda (k _v) (push k vars))
             (%binding-env-table env))
    (setf vars (sort vars #'string< :key #'symbol-name))
    (print-unreadable-object (env stream :type t)
      (format stream "~{~A~^ ~}" vars))))

(defun binding-env-ancestors (env)
  "Return list of ENV and its ancestors, outermost last."
  (let ((out '()))
    (loop for e = env then (binding-env-parent e)
          while e
          do (push e out))
    (nreverse out)))

(defun binding-env-descendant-p (ancestor env)
  "True when ANCESTOR is in the parent chain of ENV (or equal)."
  (loop for e = env then (binding-env-parent e)
        while e
        when (eq e ancestor) do (return t)
        finally (return nil)))

(defun binding-env-lca (env1 env2)
  "Lowest common ancestor of ENV1 and ENV2."
  (when (and env1 env2)
    (let ((seen (make-hash-table :test #'eq)))
      (loop for e = env1 then (binding-env-parent e)
            while e
            do (setf (gethash e seen) t))
      (loop for e = env2 then (binding-env-parent e)
            while e
            when (gethash e seen) do (return e)))))

(defun binding-env-later (env1 env2)
  "Return the youngest environment"
  (if (binding-env-descendant-p env1 env2)
      env2
      env1))

(defun block-dominates-p (ancestor descendant)
  "Return T when DESCENDANT is structurally inside ANCESTOR (or the same object)."
  (labels ((rec (blk)
             (when (eq blk descendant) (return-from block-dominates-p t))
             (dolist (st (block-statements blk))
               (etypecase st
                 (block-statement
                  (when (rec st) (return-from block-dominates-p t)))
                 (if-statement
                  (when (and (if-then-block st) (rec (if-then-block st)))
                    (return-from block-dominates-p t))
                  (when (and (if-else-block st) (rec (if-else-block st)))
                    (return-from block-dominates-p t)))
                 (t nil)))
             nil))
    (rec ancestor)))

(defun block-entry-index-for-descendant (ancestor target)
  "Earliest statement index in ANCESTOR that dominates TARGET."
  (declare (optimize (debug 3)))
  (labels ((rec (blk)
             (loop for st in (block-statements blk)
                   for idx from 0 do
                     (cond
                       ((and (typep st 'block-statement)
                             (eq st target))
                        (return idx))
                       ((typep st 'block-statement)
                        (let ((hit (rec st)))
                          (when hit (return idx))))
                       ((typep st 'if-statement)
                        (let ((hit (or (and (if-then-block st) (rec (if-then-block st)))
                                       (and (if-else-block st) (rec (if-else-block st))))))
                          (when hit (return idx))))))))
    (let ((res (rec ancestor)))
      res)))

(defun make-binding-env (block &key (table (make-hash-table :test #'eq)) parent binding-params)
  "Construct a BINDING-ENV around TABLE (defaults to a fresh hash table), with optional PARENT and BLOCK."
  (when binding-params
    (dolist (sym binding-params)
      (setf (gethash sym table) (make-instance 'outer-env-entry :index -1 :expr nil))))
  (make-instance 'binding-env :table table :parent parent :block block))

(defun make-child-env (block parent)
  "Create a child environment chained to PARENT. BLOCK defaults to the parent's block."
  (make-binding-env block :parent parent))

(defun binding-env-add (env sym entry)
  "Add binding SYM → ENTRY into ENV and return ENTRY."
  (when (binding-env-lookup env sym)
    (error "SSA violation: ~S already defined in env" sym))
  (setf (gethash sym (%binding-env-table env)) entry)
  entry)

(defun binding-env-lookup (env sym)
  "Walk ENV chain to find SYM. Returns (values entry env-found) or NIL."
  (loop for e = env then (binding-env-parent e)
        while e
        for tab = (%binding-env-table e)
        for hit = (gethash sym tab)
        when hit do (return (values hit e))
        finally (return (values nil nil))))

(defun map-binding-env (fn env)
  "Apply FN to each (key value env-found) across ENV and all ancestors."
  (labels ((walk (e)
             (when e
               (walk (binding-env-parent e))
               (maphash (lambda (k v) (funcall fn k v e))
                        (%binding-env-table e)))))
    (walk env)))

(defclass product-descriptor ()
  ((factors
    :initarg :factors
    :reader product-descriptor-factors
    :documentation "List of factors in the original product.")
   (counts
    :initarg :counts
    :reader product-descriptor-counts
    :documentation "Hash table of factor -> multiplicity for the product.")
   (stmt-index
    :initarg :stmt-index
    :reader product-descriptor-stmt-index
    :documentation "Statement index where this product occurs.")
   (binding-env
    :initarg :binding-env
    :reader product-descriptor-binding-env
    :documentation "Binding-env created by the block that contains the product")
   (target
    :initarg :target
    :initform nil
    :reader product-descriptor-target
    :documentation "Assignment target symbol for this product, when applicable."))
  (:documentation "Descriptor for a product expression used in factoring."))

(defmethod print-object ((pd product-descriptor) stream)
  (print-unreadable-object (pd stream :type t)
    (let* ((binding-env (product-descriptor-binding-env pd))
           (block (binding-env-block binding-env))
           (block-desc (block-statement-description block)))
      (format stream "~S @~D/block(~a)~@[ tgt=~A~]"
              (product-descriptor-factors pd)
              (product-descriptor-stmt-index pd)
              block-desc
              (product-descriptor-target pd)))))

(defun counts-from-factors (factors)
  "Build a multiset (hash-table) of FACTORS keyed by EQUAL."
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (f factors ht)
      (incf (gethash f ht 0)))))

(defun copy-binding-env (env block)
  "Create a child ENV chained to ENV, marking existing bindings as outer entries."
  (unless (typep env 'binding-env)
    (error "copy-binding-env expects a BINDING-ENV, got ~S" env))
  (let* ((parent env)
         (child (make-child-env block parent)))
    ;; We don't copy the table because we want it to interogate the parent environments
    child))

(defun make-env-entry (index &optional expr)
  "Construct an ENV-ENTRY instance."
  (make-instance 'env-entry :index index :expr expr))

;;; ----------------------------------------------------------------------
;;; Base class
;;; ----------------------------------------------------------------------

(defclass statement ()
  ()
  (:documentation
   "Abstract base class for all statements in the C-oriented IR.
    Statements define sequencing and control flow and *contain* expression
    IR nodes from the :expr-ir layer where needed (RHS, conditions, indices)."))

(defmethod print-object ((statement statement) stream)
  (let ((*print-pretty* nil))
    (print-unreadable-object (statement stream :type t)
      (handler-case 
          (debug-stmt statement stream 0)
        (error (err)
          (format stream "bad stmt"))))))

;;; ------------------------------------------------------------
;;; Statements that carry a single expression
;;; ------------------------------------------------------------

(defclass expression-statement (statement)
  ((expression
    :initarg :expression
    :accessor stmt-expression
    :documentation "Expression IR node associated with this statement."))
  (:documentation "Base class for statements that carry exactly one expression."))

;; ----------------------------------------------------------------------
;;; Assignment statements
;;; ----------------------------------------------------------------------
;;; We keep the LHS as a simple descriptor:
;;;   - NAME: symbol or string for the base C identifier (e.g. \"f\", \"grad\").
;;;   - INDICES: list of expr-ir expressions for subscripts (e.g. [i], [i][j]).
;;;
;;; Examples:
;;;   NAME = 'f, INDICES = NIL          ->  f
;;;   NAME = 'grad, INDICES = (i-expr)  ->  grad[i]
;;;   NAME = 'hess, INDICES = (i j)     ->  hess[i][j]

(defclass assignment-statement (expression-statement)
  ((target-name
    :initarg :target-name
    :accessor stmt-target-name
    :documentation
    "Base C identifier for the assignment target (symbol or string).")
   (target-indices
    :initarg :target-indices
    :initform nil
    :accessor stmt-target-indices
    :documentation
    "List of index expressions (expression IR nodes) for array subscripts.
     NIL means scalar variable (no indices)."))
  (:documentation
   "Assignment: target = expression;
    The actual C LHS rendering is defined in the code generator using
    TARGET-NAME and TARGET-INDICES."))

(defun anchored-target-name-p (target-name)
  "Return T if TARGET-NAME is a gradient/Hessian variable (G_/H_*)."
  (let* ((target-str (symbol-name target-name))
         (len (length target-str)))
    (and (>= len 2)
         (or (string= target-str "H_" :end1 2)
             (string= target-str "G_" :end1 2)))))

(defun make-assignment-stmt (target-name expression &key target-indices preserve-anchor-of)
  "Convenience constructor for a normal assignment-statement."
  (when (and (typep expression 'variable-expression)
             (eq target-name (expr-ir:variable-name expression)))
    (error "!!!!!!!!! Assigning ~s to itself ~s" expression target-name))
  (let* ((target-name (expr-ir:ev target-name))
         (class (if (or (typep preserve-anchor-of 'anchored-assignment-statement)
                        (anchored-target-name-p target-name))
                    'anchored-assignment-statement
                    'assignment-statement)))
    (make-instance class
                   :target-name target-name
                   :target-indices target-indices
                   :expression expression)))

(defclass anchored-assignment-statement (assignment-statement)
  ()
  (:documentation "Assignment whose target name must not be altered/aliased."))

(defun make-anchored-assignment-stmt (target-name expression &optional target-indices)
  "Constructor for an anchored-assignment-statement (target must not be aliased)."
  (let ((target-name (expr-ir:ev target-name)))
    (make-instance 'anchored-assignment-statement
                   :target-name target-name
                   :target-indices target-indices
                   :expression expression)))

(defmethod print-object ((obj assignment-statement) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~s := ~s" (stmt-target-name obj) (stmt-expression obj))))
;;; ----------------------------------------------------------------------
;;; Raw C statement
;;; ----------------------------------------------------------------------
;;; For things that are easier to just spell in C:
;;;   - declarations
;;;   - pragma
;;;   - #include (if you choose to allow it here)
;;;   - hand-written snippets

(defclass raw-c-statement (expression-statement)
  ((generator
    :initarg :generator
    :accessor raw-c-generator
    :documentation
    "Either a function of one argument (a C-expression string) that returns
     C source text, or a literal string for legacy/expr-less use."))
  (:documentation
   "Raw C statement with an optional associated expression. If EXPRESSION is
    non-NIL, the C emitter formats it and passes the string to GENERATOR to
    obtain the final C text."))

(defmethod print-object ((obj raw-c-statement) stream)
  (print-unreadable-object (obj stream :type t)
    (let* ((gen   (raw-c-generator obj))
           (expr  (stmt-expression obj))
           (c-expr (and expr (expr-ir:expr->c-expr-string expr)))
           (preview (etypecase gen
                      (function (handler-case
                                  (funcall gen c-expr)
                                  (error (ee) "#<raw-c fn>"))
                                )
                      (t gen))))
      (format stream "~a" preview))))

(defun make-raw-c-statement (generator &optional expression)
  "Convenience constructor for raw-c-statement.

GENERATOR may be:
  - a function of one argument (formatted C expr) → returns C text
  - a literal string (legacy)
EXPRESSION, if non-NIL, is an EXPR-IR node that will be rewritten by
optimizations; when emitting C, it is formatted and passed to GENERATOR."
  (when (and expression (stringp generator))
    (error "make-raw-c-statement: provide a function generator when using an expression"))
  (let ((expr (cond
                ((null expression) nil)
                ((symbolp expression) (expr-ir:make-expr-var expression))
                ((typep expression 'expr-ir:numeric-expression) expression)
                (t (error "make-raw-c-statement: unknown expression type ~S" expression)))))
    (make-instance 'raw-c-statement
                   :generator generator
                   :expression expr)))

(defmacro raw-c (text)
  "Insert a RAW-C-STATEMENT into a stmt-block.

TEXT should be a string containing literal C code. It is stored
verbatim in a RAW-C-STATEMENT node and emitted as-is by the C
backend. Use this only for code that cannot (or should not) be
expressed via the structured IR.

Example:
  (stmt-block
    (raw-c \"int i; /* hand-written C */\"))
"
  `(make-raw-c-statement ,text))

;;; ----------------------------------------------------------------------
;;; Block statements (sequence of statements)
;;; ----------------------------------------------------------------------

(defclass block-statement (statement)
  (
   (statements
    :initarg :statements
    :initform nil
    :accessor block-statements
    :documentation
    "Ordered list of STATEMENT instances. Represents a C compound
     statement { ... } or function body.")
   (label
    :initarg :label
    :initform nil
    :accessor block-label
    :documentation
    "A label for debugging - this isn't propagated unless we do it explicitly"))
  (:documentation
   "A sequence of statements. Emitted as a C compound statement {...}."))

(defun make-block-stmt (statements &key label)
  #+(or)(unless label
      (break "Missing label. Check where we are and propagate label"))
  (make-instance 'block-statement :statements statements :label label))

(defun block-statement-description (block)
  (let ((first-statement (first (block-statements block))))
    (typecase first-statement
      (assignment-statement
       (format nil "=.~a" (stmt-target-name first-statement)))
      (if-statement
       (format nil "if(~a)" (if-condition first-statement)))
      (t (format nil "~a" first-statement)))))

(defmethod print-object ((statement block-statement) stream)
  (if t
      (let ((*print-pretty* nil)
            (*package* (find-package :expr-var)))
        (print-unreadable-object (statement stream :type t)
          (handler-case
              (debug-stmt statement stream 0)
            (error (err)
              (format stream "bad stmt")))))
      (print-unreadable-object (statement stream :type t)
        (format stream "~s" (block-label statement)))))

;;; ----------------------------------------------------------------------
;;; If statements
;;; ----------------------------------------------------------------------

(defclass if-statement (statement)
  ((condition
    :initarg :condition
    :accessor if-condition
    :documentation
    "Boolean expression (expression IR node) used as the if condition.")
   (then-block
    :initarg :then-block
    :accessor if-then-block
    :documentation
    "Block-statement executed when CONDITION is true.")
   (else-block
    :initarg :else-block
    :initform nil
    :accessor if-else-block
    :documentation
    "Optional block-statement executed when CONDITION is false.
     NIL means no else branch."))
  (:documentation
   "If / then / else statement. The CONDITION is an expression IR node;
    THEN-BLOCK and ELSE-BLOCK are block-statements."))

(defun make-if-stmt (condition then-block &optional else-block)
  "Convenience constructor for if-statement."
  (make-instance 'if-statement
                 :condition condition
                 :then-block then-block
                 :else-block else-block))

;;; ----------------------------------------------------------------------
;;; C function container
;;; ----------------------------------------------------------------------
;;; This represents a whole C function: return type, name, params, locals, body.
;;; Parameters and locals are kept simple as (TYPE . NAME) pairs for now.

(defclass c-function (statement)
  ((name
    :initarg :name
    :accessor c-function-name
    :documentation
    "Symbol or string naming the C function.")
   (return-type
    :initarg :return-type
    :initform "void"
    :accessor c-function-return-type
    :documentation
    "C return type as a string (e.g. \"void\", \"double\").")
   (parameters
    :initarg :parameters
    :initform nil
    :accessor c-function-parameters
    :documentation
    "List of parameter descriptors.
     For now, each element is a cons (CTYPE . NAME), where:
       CTYPE is a string (e.g. \"double *\"),
       NAME  is a symbol or string (e.g. 'x, \"grad\").")
   (locals
    :initarg :locals
    :initform nil
    :accessor c-function-locals
    :documentation
    "List of local variable declarations.
     Same shape as PARAMETERS: (CTYPE . NAME). These are emitted at
     the top of the function body.")
   (body
    :initarg :body
    :accessor c-function-body
    :documentation
    "Function body as a block-statement.")
   (return-expr
    :initarg :return-expr
    :initform nil
    :accessor c-function-return-expr
    :documentation
    "Optional C expression string to use as an implicit final return.
If non-NIL and RETURN-TYPE is not \"void\", the emitter appends
  return RETURN-EXPR;
just before the closing brace."))
  (:documentation
   "Represents a complete C function ready for emission."))

(defun make-c-function (name body
                        &key
                          (return-type "void")
                          (parameters nil)
                          (locals nil)
                          (return-expr nil))
  "Convenience constructor for c-function."
  (make-instance 'c-function
                 :name name
                 :return-type return-type
                 :parameters parameters
                 :locals locals
                 :body body
                 :return-expr return-expr))



;;; ----------------------------------------------------------------------
;;; simplify
;;; ----------------------------------------------------------------------

(defun simplify-statement (stmt)
  "Return a simplified copy of STMT, with all expression IR children
passed through EXPR-IR:SIMPLIFY-EXPR."
  (etypecase stmt
    ;; Assignment: simplify RHS and indices
    (anchored-assignment-statement
     (let* ((name    (stmt-target-name stmt))
            (indices (stmt-target-indices stmt))
            (rhs     (stmt-expression stmt))
            (s-indices (mapcar #'expr-ir:simplify-expr indices))
            (s-rhs     (expr-ir:simplify-expr rhs)))
       (make-anchored-assignment-stmt name s-rhs s-indices)))
    (assignment-statement
     (let* ((name    (stmt-target-name stmt))
            (indices (stmt-target-indices stmt))
            (rhs     (stmt-expression stmt))
            (s-indices (mapcar #'expr-ir:simplify-expr indices))
            (s-rhs     (expr-ir:simplify-expr rhs)))
       (make-assignment-stmt name s-rhs :target-indices s-indices)))

    ;; Raw C: leave as-is
    (raw-c-statement
     stmt)

    ;; Block: handled by SIMPLIFY-BLOCK below; this is a fallback
    (block-statement
     (simplify-block stmt))

    ;; If: simplify condition and both branches
    (if-statement
     (let* ((cond      (if-condition stmt))
            (then-blk  (if-then-block stmt))
            (else-blk  (if-else-block stmt))
            (s-cond    (expr-ir:simplify-expr cond))
            (s-then    (simplify-block then-blk))
            (s-else    (and else-blk (simplify-block else-blk))))
       (make-if-stmt s-cond s-then s-else)))

    ;; C function: simplify only the body block
    (c-function
     (let ((body (c-function-body stmt)))
       (make-c-function (c-function-name stmt)
                        (simplify-block body)
                        :return-type (c-function-return-type stmt)
                        :parameters  (c-function-parameters stmt)
                        :locals      (c-function-locals stmt))))

    (t
     (error "simplify-statement: unknown statement type ~S: ~S"
            (type-of stmt) stmt))))

(defun simplify-block (block)
  "Return a simplified BLOCK-STATEMENT. Does not modify the original."
  (unless (typep block 'block-statement)
    (error "simplify-block: expected BLOCK-STATEMENT, got ~S" block))
  (let* ((stmts (block-statements block))
         (s-stmts (mapcar #'simplify-statement stmts)))
    (make-block-stmt s-stmts :label (block-label block))))

;;; ------------------------------------------------------------
;;; Debugging assistance
;;; ------------------------------------------------------------

(defparameter *debug* nil
  "When true, extra consistency checks (e.g. assignment ordering) are
performed when building kernels.")

;; use defvar so this doesn't reset when reloading code
(defvar *verbose-optimization* nil
  "Default verbosity level for optimization passes (NIL, T, or integer).")

(defun verbose-level (verbose)
  "Normalize VERBOSE to an integer level.
NIL -> 0, T -> 1, integer -> that integer."
  (cond
    ((null verbose) 0)
    ((integerp verbose) verbose)
    (t 1)))

(defun verbose-log (verbose fmt &rest args)
  "Write formatted trace output when VERBOSE is high enough.

If the first argument after VERBOSE is an integer, it is treated as the
required verbosity LEVEL (higher is more chatty) and the following argument
is the format string. Otherwise LEVEL defaults to 1."
  (let ((*package* (find-package :expr-var))
        (*print-pretty* nil)
        (level 1)
        (format-str fmt)
        (format-args args))
    (when (and (integerp fmt) args)
      (setf level fmt
            format-str (car args)
            format-args (cdr args)))
    (when (>= (verbose-level verbose) level)
      (apply #'format *trace-output* format-str format-args))))



(defun debug-block (block &key label (stream *trace-output*))
  "Pretty-print a STMT-IR:BLOCK-STATEMENT and its nested statements.

LABEL, if non-NIL, is printed as a comment header. STREAM defaults
to *TRACE-OUTPUT* so it plays nicely with your existing tracing."
  (let ((*package* (find-package :expr-var)))
    (when label
      (format stream "~&;;; ~A~%" label))
    (debug-stmt block stream 0))
  (terpri stream)
  block)

(defun debug-stmt (stmt stream indent)
  "Internal helper: print STMT at given INDENT to STREAM."
  (flet ((indent! ()
           (dotimes (i indent)
             (write-char #\Space stream))))
    (etypecase stmt
      (block-statement
       (indent!)
       (if (block-label stmt)
           (format stream "{ ; ~s~%" (block-label stmt))
           (format stream "{ ; LOST-LABEL~%"))
       (dolist (s (block-statements stmt))
         (debug-stmt s stream (+ indent 2)))
       (indent!) (format stream "}~%"))

      (if-statement
       (indent!)
       (format stream "if (~S)~%"
               (expr-ir:expr->sexpr (if-condition stmt)))
       (indent!) (format stream "then~%")
       (debug-stmt (if-then-block stmt) stream (+ indent 2))
      (when (if-else-block stmt)
        (indent!) (format stream "else~%")
        (debug-stmt (if-else-block stmt) stream (+ indent 2))))

      (assignment-statement
       (let ((target  (stmt-target-name stmt))
             (indices (stmt-target-indices stmt))
             (expr    (stmt-expression stmt)))
         (indent!)
         (if indices
             (let ((*package* (find-package :expr-var)))
               (format stream "~A~S := ~s~%" target indices (expr-ir:expr->sexpr expr)))
             (let ((*package* (find-package :expr-var)))
               (format stream "~A := ~s~%" target (expr-ir:expr->sexpr expr))))))

      ;; Fallback: print any other statement with READ-able syntax.
      (t
       (indent!)
       (format stream "~S~%" stmt)))))


  
(defun cse-temp-symbol-p (sym)
  "Return true if SYM looks like a CSE temp (e.g. CSE_P2_T17 or CSE_T17)."
  (and (symbolp sym)
       (let ((name (symbol-name sym)))
         (and (> (length name) 5)
              (or (string= name "CSE_T" :start1 0 :end1 5)
                  (string= name "CSE_P" :start1 0 :end1 5))))))

(defun make-cse-temp-symbol (pass-id index &key suffix)
  "Construct a temp symbol CSE_P<PASS-ID>_T<INDEX>, optionally with a SUFFIX for uniqueness.
SUFFIX can be T (auto-gensym), a symbol, string, or any printable object."
  (let* ((suffix-str (cond
                       ((null suffix) nil)
                       ((eq suffix t) (symbol-name (gensym "G")))
                       ((symbolp suffix) (symbol-name suffix))
                       ((stringp suffix) suffix)
                       (t (princ-to-string suffix))))
         (name (if suffix-str
                   (format nil "CSE_P~D_T~D_~A" pass-id index suffix-str)
                   (format nil "CSE_P~D_T~D" pass-id index))))
    #+(or)(when (string= "CSE_P1_T61_G285" name)
      (break "Caught problem symbol ~s" name))
    (intern name :expr-var)))

(defun factorable-atom-p (factor)
    "Return T if FACTOR is allowed to be part of a common subproduct."
  (or (symbolp factor)
      (numberp factor)))

(defun factorable-atom-present-p (factors)
  "True if FACTORS (a list of sexprs) contains at least one factor
   that is either a CSE temp symbol or a parameter symbol."
  (some #'factorable-atom-p factors))

(defun check-cse-temp-order (block &key (label "cse"))
  "Debug helper: ensure no CSE temp is used before it is defined.

Walk BLOCK in order. For each assignment, check that any variable in
its RHS that looks like a CSE temp (CSE_T...) has already been assigned
earlier in this block. Signal an error if not.

This ignores non-temp variables (E, G_X1, X1, etc.)."
  (let ((defined '()))
    (labels ((check-expr (expr)
               (dolist (v (expr-ir:expr-free-vars expr))
                 (when (and (cse-temp-symbol-p v)
                            (not (member v defined :test #'eq)))
                   (error "stmt-ir: in ~A, CSE temp ~A used before definition"
                          label v))))
             (rec-block (blk)
               (dolist (cycle (block-statements blk))
                 (etypecase cycle
                   (assignment-statement
                    (check-expr (stmt-expression cycle))
                    (let ((tgt (stmt-target-name cycle)))
                      (when (cse-temp-symbol-p tgt)
                        (pushnew tgt defined :test #'eq))))
                   (block-statement
                    (rec-block cycle))
                   (if-statement
                    (check-expr (if-condition cycle))
                    (rec-block (if-then-block cycle))
                    (when (if-else-block cycle)
                      (rec-block (if-else-block cycle))))
                   (t
                    nil)))))
      (rec-block block)
      t)))



(defun collect-scalar-targets-in-block (block)
  "Return a list of all assignment target symbols in BLOCK (recursively)."
  (labels ((rec (blk acc)
             (dolist (cycle (stmt-ir:block-statements blk) acc)
               (setf acc
                     (etypecase cycle
                       (stmt-ir:assignment-statement
                        (let ((name (stmt-ir:stmt-target-name cycle)))
                          (pushnew name acc :test #'eq)))
                       (stmt-ir:block-statement
                        (rec cycle acc))
                       (stmt-ir:if-statement
                        (let ((acc2 (rec (stmt-ir:if-then-block cycle) acc)))
                          (if (stmt-ir:if-else-block cycle)
                              (rec (stmt-ir:if-else-block cycle) acc2)
                              acc2)))
                       (t
                        acc))))))
    (rec block '())))



(defun next-cse-temp-index-for-pass (block pass-id)
  "Return the maximum temp index N such that a symbol named
   CSE_P<pass-id>_T<N> appears as an assignment target in BLOCK.
If none are found, return 0.

We scan BLOCK recursively."
  (let* ((prefix (format nil "CSE_P~D_T" pass-id))
         (plen   (length prefix))
         (max-index 0))
    (labels ((scan-block (blk)
               (dolist (cycle (block-statements blk))
                 (etypecase cycle
                   (assignment-statement
                    (let ((name (stmt-target-name cycle)))
                      (when (symbolp name)
                        (let ((s (symbol-name name)))
                          (when (and (>= (length s) plen)
                                     (string= s prefix :end1 plen))
                            (let ((n (ignore-errors
                                      (parse-integer s :start plen))))
                              (when (and (integerp n) (> n max-index))
                                (setf max-index n))))))))
                   (block-statement
                    (scan-block cycle))
                   (if-statement
                    (scan-block (if-then-block cycle))
                    (when (if-else-block cycle)
                      (scan-block (if-else-block cycle))))
                   (raw-c-statement nil)))))
      (scan-block block))
    max-index))


;;;; Helper: extract used variable symbols from an expression via sexprs

(defun vars-used-in-expr-via-sexpr (expr)
  "Return a list of symbols used in EXPR, based on its s-expression
representation. We treat every symbol we see as a 'variable candidate';
that is fine for dependency ordering, since we only care about those
that are also assignment targets in the same run."
  (let ((vars '()))
    (labels ((walk (sx)
               (cond
                 ((symbolp sx)
                  (pushnew sx vars :test #'eq))
                 ((consp sx)
                  (walk (car sx))
                  (walk (cdr sx))))))
      (walk (expr-ir:expr->sexpr expr)))
    vars))

;;;; Low-level: check a list of assignment statements

(defun check-assignment-run-def-before-use (stmts
                                     &key
                                       (errorp t)
                                       (already-defined nil)
                                       (all-defs nil))
  "Check that within the list of ASSIGNMENT-STATEMENTs STMTS, every
statement only uses variables (that are also assigned in STMTS) after
those variables' own assignments.

ALREADY-DEFINED is a list of symbols that are considered defined before
this run begins.

Signals an ERROR if ERRORP is true and a violation is found.
Returns two values: T (or NIL if ERRORP is NIL and a violation is found)
and the list of symbols defined after processing the run (including
ALREADY-DEFINED and all targets in STMTS)."
  (labels ((collect-defs (sts)
             (loop for st in sts
                   if (typep st 'assignment-statement)
                     collect (stmt-target-name st))))
    (let* ((defs    (collect-defs stmts))
           (all-defs (or all-defs defs))
           (defined (copy-list already-defined)))
      (dolist (st stmts (values t defined))
        (when (typep st 'assignment-statement)
          (let* ((expr (stmt-expression st))
                 (used (vars-used-in-expr-via-sexpr expr)))
            ;; check each used var
            (dolist (v used)
              (when (and (member v all-defs :test #'eq)
                         (not (member v defined :test #'eq)))
                (if errorp
                    (error "Assignment order violation: variable ~S~% used before its definition in statement ~S."
                           v st)
                    (return-from check-assignment-run-def-before-use
                      (values nil defined)))))
            ;; after the check, this target is now defined
            (pushnew (stmt-target-name st) defined :test #'eq)))))))

;;;; Block-level: recursively check blocks and if-statements

(defun check-def-before-use-in-block (block &key (errorp t) (already-defined '()))
  "Check that within BLOCK (a BLOCK-STATEMENT), all assignment statements
are ordered so that any variable whose value is assigned in a given
'run' (between barriers) is never used in an assignment in that run
before its own assignment.

We treat IF-STATEMENT and BLOCK-STATEMENT as barriers (no cross-barrier
checking), and recurse into them. RAW-C-STATEMENT and other non-assignment,
non-barrier statements remain in place and are ignored for ordering.

Signals ERROR on violation if ERRORP is true. Returns T if all is OK,
or NIL if a violation is found and ERRORP is NIL."
  (declare (optimize (debug 3)))
  (let ((all-defs (collect-scalar-targets-in-block block)))
    (labels
        ((assignment-p (st)
           (typep st 'assignment-statement))

         (barrier-p (st)
           (etypecase st
             (if-statement   t)
             (block-statement t)
             (t nil)))

         ;; recurse into nested statements
         (check-stmt (st defined)
           (etypecase st
             (block-statement
              (check-stmt-list (block-statements st) defined))
             (if-statement
              ;; track definitions that are valid after both branches
              (let* ((d1 (if (if-then-block st)
                             (check-stmt-list (block-statements (if-then-block st))
                                              defined)
                             defined))
                     (d2 (if (if-else-block st)
                             (check-stmt-list (block-statements (if-else-block st))
                                              defined)
                             defined)))
                (intersection d1 d2 :test #'eq)))
             ;; non-barriers do not change what is considered defined
             (t
              defined)))

         (check-run (run defined)
           "RUN is a list of statements in program order with no barriers.
We extract the assignment statements from RUN and verify def-before-use
against assignments anywhere in the enclosing BLOCK."
           (let ((assignments (remove-if-not #'assignment-p run)))
             (if assignments
                 (multiple-value-bind (_ok new-defined)
                     (check-assignment-run-def-before-use assignments
                                                          :errorp errorp
                                                          :already-defined defined
                                                          :all-defs all-defs)
                   (declare (ignore _ok))
                   new-defined)
                 defined)))

         (check-stmt-list (stmts defined)
           (labels ((flush-run (run defined)
                      (if run
                          (check-run (nreverse run) defined)
                          defined)))
             (let ((run '())
                   (cur-defined defined))
               (dolist (st stmts cur-defined)
                 (if (barrier-p st)
                     (progn
                       ;; finish current run, then recurse into the barrier node
                       (setf cur-defined (flush-run run cur-defined))
                       (setf run '())
                       (setf cur-defined (check-stmt st cur-defined)))
                     ;; non-barrier, part of the current run
                     (push st run)))
               ;; flush trailing run
               (setf cur-defined (flush-run run cur-defined))
               cur-defined))))
      ;; Only intended for top-level blocks; initial defined set starts from ALREADY-DEFINED.
      (check-stmt-list (block-statements block) already-defined)
      t)))


(defun check-block-integrity (block &key label)
  (check-def-before-use-in-block block :errorp t)
  (check-cse-temp-order block :label label))

(defun vars-used-in-expr (expr)
  "Return a list of symbol names used in EXPR (expression IR), based on its sexpr.
We do not try to distinguish parameters vs locals here; later we intersect
with the set of assignment targets in the block."
  (let ((vars '()))
    (labels ((walk (sx)
               (cond
                 ((symbolp sx)
                  (pushnew sx vars :test #'eq))
                 ((consp sx)
                  (dolist (sub sx)
                    (walk sub))))))
      (walk (expr-ir:expr->sexpr expr))
      vars)))

(defun reorder-assignment-run-def-before-use (stmts)
  "Given a list of ASSIGNMENT-STATEMENTs (no if/block nodes),
return a new list where for any statement S and any variable V
that is both used in S and assigned somewhere in STMTS, the
assignment to V appears earlier than S.

We do a simple dependency-aware scheduling:
  - 'defs' = all symbols that are assignment targets in STMTS
  - 'defined' = symbols whose assignments we have already emitted
  - repeatedly scan 'pending' and move 'ready' statements to 'result'
    until no progress, then append leftovers (cycle would indicate
    a self-dependency bug in the input)."
  (labels ((collect-defs (sts)
             (loop for st in sts
                   collect (stmt-target-name st)))

           (ready-p (st defs defined)
             (let* ((used (vars-used-in-expr (stmt-expression st))))
               (loop for v in used
                     ;; only care about dependencies on vars that are
                     ;; also assigned in this run
                     when (and (member v defs :test #'eq)
                               (not (member v defined :test #'eq)))
                       do (return-from ready-p nil)
                     finally (return t))))
           )
    (let* ((defs     (collect-defs stmts))
           (defined  '())
           (pending  (copy-list stmts))
           (result   '()))
      (loop
        ;; If nothing left, we're done.
        (when (null pending)
          (return (nreverse result)))
        (let ((progress nil)
              (next-pending '()))
          (dolist (st pending)
            (if (ready-p st defs defined)
                (progn
                  (push st result)
                  (pushnew (stmt-target-name st) defined :test #'eq)
                  (setf progress t))
                (push st next-pending)))
          (setf pending (nreverse next-pending))
          ;; No statement was schedulable: break to avoid infinite loop.
          (unless progress
            ;; We just append remaining pending (they may contain a cycle
            ;; but that's an input bug, and we're no worse than before).
            (setf result (nreverse (nconc pending result)))
            (return (nreverse result))))))))

#+(or)
(defun reorder-block-def-before-use (block)
  "Return a new BLOCK-STATEMENT where, within each contiguous run of
ASSIGNMENT-STATEMENTs (possibly interspersed with other, non-barrier
statements like RAW-C-STATEMENT), assignments are reordered to ensure
that any variable used in an assignment and also assigned in that run
is defined earlier in the run.

We treat IF-STATEMENT and BLOCK-STATEMENT as barriers (no reordering
across them). We recurse into nested blocks and if branches.

RAW-C-STATEMENT and other non-assignment, non-barrier statements are
*not* barriers: they remain in place inside the run, but assignments
around them may be reordered as needed to satisfy def-before-use."
  (labels
      ;; recurse into a single statement
      ((reorder-stmt (st)
         (etypecase st
           (assignment-statement
            st)
           (block-statement
            (make-block-stmt
             (reorder-stmt-list (block-statements st))))
           (if-statement
            (let* ((cond   (if-condition st))
                   (then-b (if-then-block st))
                   (else-b (if-else-block st))
                   (new-then (when then-b
                               (make-block-stmt
                                (reorder-stmt-list
                                 (block-statements then-b)))))
                   (new-else (when else-b
                               (make-block-stmt
                                (reorder-stmt-list
                                 (block-statements else-b))))))
              (make-if-stmt cond new-then new-else)))
           (t
            st)))

       ;; a barrier is something we must not reorder across
       (barrier-p (st)
         (etypecase st
           (if-statement t)
           (block-statement t)
           (t nil)))

       ;; reorder a “run” that may contain both assignments and
       ;; non-barrier statements (e.g. RAW-C-STATEMENT).
       ;;
       ;; We only permute the assignment statements; non-barrier
       ;; non-assignments must stay in their original relative positions.
       (reorder-run (run)
         ;; Extract the assignments and their indices within RUN
         run
         (let ((assignments '())
               (indices '()))
           (loop for st in run
                 for i from 0
                 when (typep st 'assignment-statement)
                   do (progn
                        (push st assignments)
                        (push i indices)))
           (setf assignments (nreverse assignments)
                 indices     (nreverse indices))
           (if (null assignments)
               ;; no assignments: nothing to do
               run
               (let* ((reordered-assignments
                        (reorder-assignment-run-def-before-use assignments))
                      (result (copy-list run)))
                 ;; splice the reordered assignments back into the
                 ;; original positions of the assignments
                 (loop for st in reordered-assignments
                       for idx in indices
                       do (setf (nth idx result) st))
                 #+(or)(break "Check result and run")
                 result))))

       ;; reorder list of statements, partitioning into runs between barriers
       (reorder-stmt-list (stmts)
         (labels ((flush-run (run acc)
                    (if run
                        ;; RUN is collected in reverse via PUSH; restore order first.
                        (nconc acc (reorder-run (nreverse run)))
                        acc)))
           (let ((acc '())
                 (run '()))
             (dolist (st stmts)
               (if (barrier-p st)
                   (progn
                     ;; flush the current run, then emit the barrier (recurred)
                     (setf acc (flush-run run acc)
                           run '())
                     (let ((reordered (reorder-stmt st)))
                       (setf acc (nconc acc (list reordered)))))
                   ;; st is not a barrier: include it in the current run
                   (push st run)))
             ;; flush final run if there is one
             (flush-run run acc)))))
    (let ((reordered-stmt-list (reorder-stmt-list (block-statements block))))
      (make-block-stmt reordered-stmt-list))))

;;; ------------------------------------------------------------
;;; Helpers
;;; ------------------------------------------------------------

(defun copy-env-hash-table (env)
  "Portable shallow copy of a hash-table ENV."
  (let ((new (make-hash-table
              :test            (hash-table-test env)
              :size            (hash-table-size env)
              :rehash-size     (hash-table-rehash-size env)
              :rehash-threshold (hash-table-rehash-threshold env))))
    (maphash (lambda (k v)
               (setf (gethash k new) v))
             env)
    new))

(defun %c-ident (sym)
  "Convert a Lisp symbol to a C identifier; default: lowercase name."
  (string-downcase (symbol-name sym)))

(defun %indent (n stream)
  (dotimes (_ (* 2 n)) (write-char #\Space stream)))

(defun %c-type (ctype)
  "Render a C type token in lowercase (e.g. DOUBLE -> double)."
  (string-downcase (princ-to-string ctype)))


;;; ------------------------------------------------------------
;;; Statement / block emitters
;;; ------------------------------------------------------------

(defun emit-assignment-statement-c (stmt indent stream)
  "Emit an assignment. If this is the first time we see this local,
   emit a declaration + initialization; otherwise emit a plain assignment."
  (declare (optimize (debug 3)))
  (let* ((name (stmt-target-name stmt))
         (expr (stmt-expression stmt))
         (ctype (and *c-local-types*
                     (gethash name *c-local-types*)))
         (already-declared
           (and *c-declared-locals*
                (gethash name *c-declared-locals*))))
    (format stream "~&~v@T" (* 2 indent))
    (when (and ctype (not already-declared))
      ;; declare it here
      (setf (gethash name *c-declared-locals*) t)
      (format stream "~a " (%c-type ctype)))
    ;; assignment (either \"x = ...\" or \"DOUBLE x = ...\")
    (format stream "~a = " (string-downcase (symbol-name name)))
    (format stream "~a" (expr->c-expr-string expr))
    (format stream ";~%")))


(defun emit-statement-c (stmt indent stream)
  "Emit one statement to STREAM at INDENT (indent = logical level)."
  (etypecase stmt
    (assignment-statement
     (emit-assignment-statement-c stmt indent stream))

    (raw-c-statement
     (let* ((expr   (stmt-expression stmt))
            (c-expr (and expr (expr-ir:expr->c-expr-string expr)))
            (gen    (raw-c-generator stmt))
            (code   (etypecase gen
                      (function (funcall gen c-expr))
                      (t gen))))
       (%indent indent stream)
       (format stream "~A~%" code)))

    (if-statement
     (let* ((cond-expr (if-condition stmt))
            (cond-str  (expr-ir:expr->c-expr-string cond-expr))
            (then-blk  (if-then-block stmt))
            (else-blk  (if-else-block stmt)))
       (%indent indent stream)
       (format stream "if (~A) {~%" cond-str)
       (emit-block-c then-blk (1+ indent) stream)
       (%indent indent stream)
       (if else-blk
           (progn
             (format stream "} else {~%")
             (emit-block-c else-blk (1+ indent) stream)
             (%indent indent stream)
             (format stream "}~%"))
           (format stream "}~%"))))

    (block-statement
     ;; Generic block outside of if/function: just emit its contents
     (emit-block-c stmt indent stream))


    (t
     (error "emit-statement-c: unknown statement type ~S" stmt))))


(defun emit-block-c (block indent stream)
  "Emit all statements in BLOCK at INDENT."
  (unless (typep block 'block-statement)
    (error "emit-block-c: expected BLOCK-STATEMENT, got ~S" block))
  (dolist (cycle (block-statements block))
    (emit-statement-c cycle indent stream)))


;;; ------------------------------------------------------------
;;; Function emitter
;;; ------------------------------------------------------------
(defparameter *c-local-types* nil
  "Hash table mapping local variable symbols -> C type strings
   for the current function being emitted.")

(defparameter *c-declared-locals* nil
  "Hash table recording which locals have already been declared
   in the current function being emitted.")

(defun build-c-local-type-table (locals)
  "LOCALS is whatever (infer-kernel-locals) returns.
Expected shape: a list of (ctype name) pairs.
Return a hash-table mapping NAME -> CTYPE."
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (loc locals ht)
      (destructuring-bind (ctype name) loc
        (setf (gethash name ht) ctype)))))

(defun emit-c-function (fn stream)
  "Emit a C function from FN to STREAM, declaring locals at first assignment."
  (let* ((name       (c-function-name fn))
         (ret-type   (%c-type (c-function-return-type fn)))  ; e.g. \"void\"
         (params     (c-function-parameters fn))   ; list of (ctype name)
         (locals     (c-function-locals fn))
         (body       (c-function-body fn))
         (return-expr (c-function-return-expr fn))
         (*c-local-types*    (build-c-local-type-table locals))
         (*c-declared-locals* (make-hash-table :test #'eq)))
    ;; function header
    (format stream "~a ~a(" ret-type (string-downcase name))
    (loop for (ptype pname) in params
          for i from 0
          do (progn
               (when (> i 0) (format stream ", "))
               (format stream "~a ~a" (%c-type ptype) (string-downcase pname))))
    (format stream ")~%{~%")
    ;; body
    (emit-statement-c body 1 stream)
    ;; optional implicit return expression
    (when return-expr
      (format stream "  ~a~%" return-expr))
    (format stream "}~%")))

(defun c-function->c-source-string (cfun)
  "Render a STMT-IR:C-FUNCTION as a complete C function definition string."
  (unless (typep cfun 'c-function)
    (error "c-function->c-source-string: expected C-FUNCTION, got ~S" cfun))
  (with-output-to-string (s)
    (emit-c-function cfun s)))



;;; ------------------------------------------------------------
;;; Common subexpression elimination (CSE) on stmt-ir blocks
;;; ------------------------------------------------------------

(defun sexpr-size (sexpr)
  "Crude size metric (number of nodes) for a sexpr tree."
  (if (atom sexpr)
      1
      (1+ (reduce #'+ (mapcar #'sexpr-size sexpr)))))


(defun factor-sums-optimization (pass-counter block binding-params &key (min-uses 2) (min-factors 1) (min-size 4)
                                               (verbose *verbose-optimization*))
  "Return a new BLOCK-STATEMENT where each assignment RHS has had
EXPR-IR:FACTOR-SUM-OF-PRODUCTS applied."
  (declare (ignore pass-counter binding-params))
  (verbose-log verbose "~&[FACTOR-SUMS] rewriting block (min-uses=~D min-factors=~D min-size=~D)~%"
               min-uses min-factors min-size)
  (labels ((rewrite-expr (expr)
             (expr-ir:factor-sum-of-products expr
                                             :min-uses min-uses
                                             :min-factors min-factors
                                             :min-size min-size))
           (rewrite-stmt (cycle)
           (etypecase cycle
             (anchored-assignment-statement
              (make-anchored-assignment-stmt
               (stmt-target-name cycle)
               (rewrite-expr (stmt-expression cycle))
               (stmt-target-indices cycle)))
             (assignment-statement
              (make-assignment-stmt
               (stmt-target-name cycle)
               (rewrite-expr (stmt-expression cycle))
               :target-indices (stmt-target-indices cycle)))
             (raw-c-statement
              (let ((expr (stmt-expression cycle)))
                (if expr
                    (make-raw-c-statement
                     (raw-c-generator cycle)
                     (rewrite-expr expr))
                    cycle)))
             (block-statement
              (make-block-stmt
               (mapcar #'rewrite-stmt (block-statements cycle))
               :label (block-label cycle)))
             (if-statement
              (let* ((new-cond (rewrite-expr (if-condition cycle)))
                     (then-b   (if-then-block cycle))
                     (else-b   (if-else-block cycle))
                     (new-then (when then-b
                                 (make-block-stmt
                                  (mapcar #'rewrite-stmt
                                          (block-statements then-b))
                                  :label (block-label then-b))))
                     (new-else (when else-b
                                 (make-block-stmt
                                  (mapcar #'rewrite-stmt
                                          (block-statements else-b))
                                  :label (block-label else-b)))))
                (make-if-stmt new-cond new-then new-else)))
             (t cycle))))
    (make-block-stmt
     (mapcar #'rewrite-stmt (block-statements block))
     :label (block-label block))))

(defun combinations (items k)
  "Return all k-length combinations of ITEMS (a list), as lists.
The order of combinations follows the order of ITEMS."
  (labels ((recur (rest-items k-left prefix)
             (cond
               ;; we have chosen k items -> one combination
               ((zerop k-left)
                (list (reverse prefix))) ; Changed from nreverse
               ;; no more items but still need to choose -> no combinations
               ((null rest-items)
                nil)
               (t
                (let* ((first (car rest-items))
                       (rest  (cdr rest-items))
                       ;; include FIRST in the combination
                       (with-first
                           (recur rest (1- k-left)
                                  (cons first prefix)))
                       ;; skip FIRST
                       (without-first
                           (recur rest k-left prefix)))
                  ;; concatenate the two lists of combinations
                  (append with-first without-first))))))
    (recur items k nil)))

(defun remove-first-equal (item factors)
  "Remove the first element EQUAL to ITEM from FACTORS.
Returns (values new-factors found-p)."
  (cond
    ((null factors)
     (values nil nil))
    ((equal item (car factors))
     (values (cdr factors) t))
    (t
     (multiple-value-bind (rest found)
         (remove-first-equal item (cdr factors))
       (values (cons (car factors) rest) found)))))

(defun remove-combo-from-factors (combo factors)
  "Try to remove one occurrence of each factor in COMBO from FACTORS.
Returns (values new-factors matched-p)."
  (let ((current factors)
        (matched t))
    (dolist (atom combo)
      (multiple-value-bind (rest found)
          (remove-first-equal atom current)
        (unless found
          (setf matched nil)
          (return))
        (setf current rest)))
    (values current matched)))


(defparameter *factor-temp-param-walk-debug* nil)






(defun factor-temp-param-products-optimization (pass-counter block binding-params &key (min-uses 2)
                                                                     (min-factors 2)
                                                                     (max-factors 3)
                                                                     (verbose *verbose-optimization*))
  (declare (ignore binding-params))
  (unless (typep block 'block-statement)
    (error "factor-temp-param-products-optimization: expected BLOCK-STATEMENT, got ~S"
           block))
  (verbose-log verbose "~&[FTPP] scanning block with ~D statements (min-uses=~D min-factors=~D max-factors=~D)~%"
               (length (block-statements block)) min-uses min-factors max-factors)
  ;; Recurse into child blocks/branches first so factoring happens locally without hoisting.
  (flet ((recur-stmt (st)
           (etypecase st
             (if-statement
                 (let ((cond (if-condition st))
                    (tb   (if-then-block st))
                    (eb   (if-else-block st)))
               (make-if-stmt
                cond
                (and tb (factor-temp-param-products-optimization pass-counter tb binding-params
                                                                 :min-uses min-uses
                                                                 :min-factors min-factors
                                                                 :max-factors max-factors
                                                                 :verbose verbose))
                (and eb (factor-temp-param-products-optimization pass-counter eb binding-params
                                                                 :min-uses min-uses
                                                                 :min-factors min-factors
                                                                 :max-factors max-factors
                                                                 :verbose verbose)))))
             (block-statement
              (factor-temp-param-products-optimization pass-counter st binding-params
                                                       :min-uses min-uses
                                                       :min-factors min-factors
                                                       :max-factors max-factors
                                                       :verbose verbose))
             (t st))))
    (let* ((stmts      (mapcar #'recur-stmt (block-statements block)))
         (n          (length stmts))
         (candidates (make-hash-table :test #'equal))
         (def-index  (make-hash-table :test #'eq))
         (all-defs   (collect-scalar-targets-in-block block)))
    ;; record where each symbol is defined in this block
    (loop for cycle in stmts
          for idx from 0 do
            (when (typep cycle 'assignment-statement)
              (setf (gethash (stmt-target-name cycle) def-index) idx)))
    ;; 1. Collect candidates
    (labels
        ;; This does the recursive walk and calls COLLECT-PRODUCT on every (* ...) node.
        ((walk-sexpr (sexpr stmt-idx depth)
           ;; DEBUG: print every sexpr as we walk it
           (when *factor-temp-param-walk-debug*
             (format *trace-output* "~&[FTPP-WALK stmt=~D depth=~D] ~V@T~S~%"
                     stmt-idx depth (* 2 depth) sexpr))
           (when (and (consp sexpr)
                      (eq (car sexpr) '*))
             (collect-product sexpr stmt-idx depth))
           (when (consp sexpr)
             (dolist (child (cdr sexpr))
               (walk-sexpr child stmt-idx (1+ depth)))))
         ;; This handles one product node: compute atoms and add combos to CANDIDATES.
         (collect-product (sexpr stmt-idx depth)
           (let* ((factors (cdr sexpr))
                  ;; keep only temp/param/number factors
                  (atoms   (remove-if-not
                            (lambda (factor)
                              (factorable-atom-p factor))
                            factors)))
             (when *factor-temp-param-walk-debug*
               (format *trace-output* "~&[FTPP-COLLECT stmt=~D depth=~D] ~V@T~S~%"
                       stmt-idx depth (* 2 depth) atoms))
             (when (>= (length atoms) min-factors)
               (let* ((max-k (min max-factors (length atoms)))
                      (seen  (make-hash-table :test #'equal)))
                 (loop for k from min-factors to max-k do
                   (dolist (combo (combinations atoms k))
                     (when (and (factorable-atom-present-p combo)
                                (not (gethash combo seen)))
                       (setf (gethash combo seen) t)
                       (let ((info (gethash combo candidates)))
                         (if info
                             (progn
                               (incf (getf info :count))
                               (when (< stmt-idx (getf info :first-index))
                                 (setf (getf info :first-index) stmt-idx)))
                             (setf (gethash combo candidates)
                                   (list :count 1
                                         :first-index stmt-idx)))))))))))
         (collect-from-expr (expr stmt-idx)
           (walk-sexpr (expr-ir:expr->sexpr expr) stmt-idx 0))
         (collect-from-stmt (stmt stmt-idx)
           (etypecase stmt
             (assignment-statement
              (collect-from-expr (stmt-expression stmt) stmt-idx))
             (raw-c-statement
              (let ((expr (stmt-expression stmt)))
                (when expr
                  (collect-from-expr expr stmt-idx))))
             (block-statement
              ;; avoid collecting across barrier scopes here
              nil)
             (if-statement
              ;; avoid collecting across barrier scopes here
              (collect-from-expr (if-condition stmt) stmt-idx))
             (t nil))))
      ;; walk each top-level statement
      (loop for idx from 0 below n do
        (collect-from-stmt (nth idx stmts) idx)))
    (when *factor-temp-param-debug*
      (let ((*print-pretty* nil))
        (format *trace-output* "~&[FTPP] candidates:~%")
        (maphash
         (lambda (combo info)
           (format *trace-output* "  combo ~S -> count ~D first-idx ~D~%"
                   combo
                   (getf info :count)
                   (getf info :first-index)))
         candidates)))
    ;; 2. Choose best candidate.
    (let ((best-combo      nil)
          (best-score      0)
          (best-count      0)
          (best-first-idx  nil)
          (candidate-list  '()))
      ;; Sort candidates deterministically before picking best.
      (maphash (lambda (combo info)
                 (push (cons combo info) candidate-list))
               candidates)
      (setf candidate-list
            (sort candidate-list #'string<
                  :key (lambda (entry)
                         (format nil "~S" (car entry)))))
      (verbose-log verbose "~&[FTPP] ~D candidate combos collected~%"
                   (length candidate-list))
      (dolist (entry candidate-list)
        (let* ((combo     (car entry))
               (info      (cdr entry))
               (count     (getf info :count))
               (first-idx (getf info :first-index)))
          (when (>= count min-uses)
            (let* ((len   (length combo))
                   ;; simple score: (uses-1) * length
                   (score (* (- count 1) len)))
              (when (and (>= len min-factors)
                         (or (> score best-score)
                             (and (= score best-score)
                                  (or (null best-combo)
                                      (string< (format nil "~S" combo)
                                               (format nil "~S" best-combo))))))
                (setf best-score     score
                      best-combo     combo
                      best-count     count
                      best-first-idx first-idx))))))
      (when *factor-temp-param-debug*
        (format *trace-output* "~&[FTPP] best-combo=~S count=~D score=~D~%"
                best-combo best-count best-score))
      (verbose-log verbose "~&[FTPP] best combo ~S count=~D score=~D first-idx=~A~%"
                   best-combo best-count best-score best-first-idx)
      (when (or (null best-combo)
                (< best-count min-uses)
                (<= best-score 0))
        ;; No beneficial factoring.
        (verbose-log verbose "~&[FTPP] no beneficial factoring (best combo insufficient)~%")
        (return-from factor-temp-param-products-optimization block))
      ;; 3. Build temp assignment for BEST-COMBO.
      (let* ((pass-id (next-pass-id pass-counter))
             (start-index (next-cse-temp-index-for-pass block pass-id))
             (temp-name   (make-cse-temp-symbol pass-id (1+ start-index) :suffix t))
	     (temp-sexpr  (if (= (length best-combo) 1)
	                      (car best-combo)
	                      (cons '* best-combo)))
	     (temp-expr   (expr-ir:sexpr->expr-ir temp-sexpr))
	     (vars-used   (expr-ir:expr-free-vars temp-expr))
         ;; If any operand is assigned somewhere in this block but not
         ;; defined at this level (e.g., only inside a branch), skip.
         (blocked-p (some (lambda (u)
                            (and (member u all-defs :test #'eq)
                                 (null (gethash u def-index nil))))
                          vars-used))
	     (dep-max
	       (if vars-used
	           (loop with max-di = nil
	                 for u in vars-used
	                 for di = (gethash u def-index nil)
	                 when di do (setf max-di
                                      (if max-di
                                          (max max-di di)
                                          di))
                         finally (return (or max-di -1)))
	           -1))
	     (min-insert (max (1+ dep-max) 0))
	     (max-allowed (or best-first-idx n)))
        (when blocked-p
          (verbose-log verbose "~&[FTPP pass ~D] skip combo ~S (blocked dependencies)~%"
                       pass-id best-combo)
          (return-from factor-temp-param-products-optimization block))
        (when (> min-insert max-allowed)
          ;; Cannot place temp before its first use; skip factoring.
          (verbose-log verbose "~&[FTPP pass ~D] skip combo ~S (min-insert=~D max-allowed=~D)~%"
                       pass-id best-combo min-insert max-allowed)
          (return-from factor-temp-param-products-optimization block))
        (let* ((insert-idx  (min min-insert max-allowed))
               (temp-assign (make-assignment-stmt temp-name temp-expr)))
          (when *factor-temp-param-debug*
            (format *trace-output* "~&[FTPP] introducing temp ~S = ~S at index ~D~%"
                    temp-name temp-sexpr insert-idx))
          (verbose-log verbose "~&[FTPP pass ~D] introducing temp ~S = ~S at index ~D~%"
                       pass-id temp-name temp-sexpr insert-idx)
          ;; 4. Rewrite block: factor BEST-COMBO out of any product that contains it.
          (labels ((rewrite-sexpr (sexpr)
                     (cond
                       ((and (consp sexpr)
                             (eq (car sexpr) '*))
                        (let ((factors (cdr sexpr)))
                          (multiple-value-bind (rest-factors matched)
                              (remove-combo-from-factors best-combo factors)
                            (if matched
                                (let ((rest-rewritten
                                        (mapcar #'rewrite-sexpr rest-factors)))
                                  (cond
                                    ((null rest-rewritten)
                                     temp-name)
                                    (t
                                     (cons '* (cons temp-name rest-rewritten)))))
                                ;; no match here; just rewrite children
                                (cons '* (mapcar #'rewrite-sexpr factors))))))
                       ((consp sexpr)
                        (cons (car sexpr)
                              (mapcar #'rewrite-sexpr (cdr sexpr))))
                       (t
                        sexpr)))
                   (rewrite-expr (expr)
                     (expr-ir:sexpr->expr-ir
                      (rewrite-sexpr (expr-ir:expr->sexpr expr))))
                   (rewrite-stmt (stmt)
                     (etypecase stmt
                       (anchored-assignment-statement
                        (make-anchored-assignment-stmt
                         (stmt-target-name stmt)
                         (rewrite-expr (stmt-expression stmt))
                         (stmt-target-indices stmt)))
                       (assignment-statement
                        (make-assignment-stmt
                         (stmt-target-name stmt)
                         (rewrite-expr (stmt-expression stmt))
                         :target-indices (stmt-target-indices stmt)))
                       (block-statement
                        (make-block-stmt
                         (mapcar #'rewrite-stmt (block-statements stmt))
                         :label (block-label stmt)))
                       (if-statement
                       (let* ((new-cond (rewrite-expr (if-condition stmt)))
                              (then-b  (if-then-block stmt))
                              (else-b  (if-else-block stmt))
                              (new-then (when then-b
                                          (make-block-stmt
                                           (mapcar #'rewrite-stmt
                                                   (block-statements then-b))
                                           :label (block-label then-b))))
                              (new-else (when else-b
                                          (make-block-stmt
                                           (mapcar #'rewrite-stmt
                                                   (block-statements else-b))
                                           :label (block-label else-b)))))
                         (make-if-stmt new-cond new-then new-else)))
                       (t stmt))))
            (let ((new-stmts '()))
              (loop for idx from 0 below n do
                (when (= idx insert-idx)
                  (push temp-assign new-stmts))
                (push (rewrite-stmt (nth idx stmts)) new-stmts))
              (let ((result (make-block-stmt (nreverse new-stmts) :label (block-label block))))
                (verbose-log verbose "~&[FTPP pass ~D] factored combo ~S into temp ~S~%"
                             pass-id best-combo temp-name)
                result)))))))))


(defun normalize-signs-optimization (pass-counter block binding-params &key (verbose *verbose-optimization*))
  "Return a new BLOCK-STATEMENT where each assignment RHS expression
has had EXPR-IR:NORMALIZE-SIGNS-EXPR applied."
  (declare (ignore pass-counter binding-params))
  (verbose-log verbose "~&[NORMALIZE-SIGNS] rewriting block~%")
  (labels ((rewrite-expr (expr)
             (expr-ir:normalize-signs-expr expr))
           (rewrite-stmt (cycle)
             (etypecase cycle
               (anchored-assignment-statement
                (make-anchored-assignment-stmt
                 (stmt-target-name cycle)
                 (rewrite-expr (stmt-expression cycle))
                 (stmt-target-indices cycle)))
               (assignment-statement
                (make-assignment-stmt
                 (stmt-target-name cycle)
                 (rewrite-expr (stmt-expression cycle))
                 :target-indices (stmt-target-indices cycle)))
               (raw-c-statement
                (let ((expr (stmt-expression cycle)))
                  (if expr
                      (make-raw-c-statement
                       (raw-c-generator cycle)
                       (rewrite-expr expr))
                      cycle)))
               (block-statement
                (make-block-stmt
                 (mapcar #'rewrite-stmt
                         (block-statements cycle))
                 :label (block-label cycle)))
               (if-statement
                (let* ((new-cond (rewrite-expr (if-condition cycle)))
                       (then-b  (if-then-block cycle))
                       (else-b  (if-else-block cycle))
                       (new-then (when then-b
                                   (make-block-stmt
                                    (mapcar #'rewrite-stmt
                                            (block-statements then-b))
                                    :label (block-label then-b))))
                       (new-else (when else-b
                                   (make-block-stmt
                                    (mapcar #'rewrite-stmt
                                            (block-statements else-b))
                                    :label (block-label else-b)))))
                  (make-if-stmt new-cond new-then new-else)))
               (t cycle))))
    (make-block-stmt
     (mapcar #'rewrite-stmt (block-statements block))
     :label (block-label block))))

;;; -------------------------------
;;; Measure complexity of a block
;;; -------------------------------


(defun %sexpr-node-count (sx)
  "Count nodes in a sexpr tree SX (1 per cons node or atom)."
  (if (consp sx)
      (1+ (reduce #'+ (mapcar #'%sexpr-node-count (cdr sx)) :initial-value 0))
      1))

(defun expr-complexity (expr)
  "Complexity measure for a single EXPR-IR expression: node count of its sexpr."
  (%sexpr-node-count (expr-ir:expr->sexpr expr)))

(defun block-complexity (block)
  "Complexity measure for a BLOCK-STATEMENT: sum of expression complexities
in RHSs, indices, and conditions. Trivial alias assignments (RHS is a symbol
equal to the target) are ignored."
  (labels
      ((comp-expr (expr)
         (expr-complexity expr))
       (comp-stmt (cycle)
         (etypecase cycle
           (assignment-statement
            (let* ((rhs (stmt-expression cycle))
                   (target (stmt-target-name cycle))
                   (idx (stmt-target-indices cycle))
                   (trivial-alias (and (symbolp rhs)
                                       (eq rhs target))))
                  (+ (comp-expr rhs)
                     (reduce #'+ (mapcar #'comp-expr idx) :initial-value 0))))
           (block-statement
            (reduce #'+ (mapcar #'comp-stmt (block-statements cycle)) :initial-value 0))
           (if-statement
            (+ (comp-expr (if-condition cycle))
               (if (if-then-block cycle)
                   (comp-stmt (if-then-block cycle))
                   0)
           (if (if-else-block cycle)
               (comp-stmt (if-else-block cycle))
               0)))
           (raw-c-statement
            (let ((expr (stmt-expression cycle)))
              (if expr
                  (%sexpr-node-count (expr-ir:expr->sexpr expr))
                  0)))
           (t
            0)))
       (comp-block (blk)
         (unless (typep blk 'block-statement)
           (error "BLOCK-COMPLEXITY: expected BLOCK-STATEMENT, got ~S" blk))
         (reduce #'+ (mapcar #'comp-stmt (block-statements blk)) :initial-value 0)))
    (comp-block block)))




(defun linear-canonicalization-optimization (pass-counter block binding-params &key (verbose *verbose-optimization*))
  "Walk BLOCK and linear-canonicalize every expression, to help factoring and CSE.
Only linear subexpressions are changed; non-linear ones are left as-is."
  (declare (ignore pass-counter binding-params))
  (verbose-log verbose "~&[LIN-CANON] canonicalizing linear subexpressions~%")
  (labels
      ((rewrite-expr (expr)
         ;; You can wrap with SIMPLIFY-EXPR if you like:
         ;; (expr-ir:simplify-expr
         ;;   (expr-ir:canonicalize-linear-subexprs expr))
         (expr-ir:simplify-expr
          (expr-ir:canonicalize-linear-subexprs expr)))

      (rewrite-stmt (st)
        (etypecase st
          (anchored-assignment-statement
           (make-anchored-assignment-stmt
            (stmt-target-name st)
            (rewrite-expr (stmt-expression st))
            (stmt-target-indices st)))
          (assignment-statement
           (make-assignment-stmt
            (stmt-target-name st)
            (rewrite-expr (stmt-expression st))
            :target-indices (stmt-target-indices st)))
          (raw-c-statement
           (let ((expr (stmt-expression st)))
             (if expr
                 (make-raw-c-statement
                  (raw-c-generator st)
                  (rewrite-expr expr))
                 st)))

          (if-statement
           (let* ((new-cond (rewrite-expr (if-condition st)))
                  (then-blk (if-then-block st))
                  (else-blk (if-else-block st))
                  (new-then (rewrite-stmt then-blk))
                  (new-else (and else-blk (rewrite-stmt else-blk))))
             (make-if-stmt new-cond new-then new-else)))

          (block-statement
           (make-block-stmt
            (mapcar #'rewrite-stmt (block-statements st))
            :label (block-label st)))
          (t
           st))))
    (rewrite-stmt block)))



;;; -------------------------------
;;; Optimization pipeline
;;; -------------------------------




(defclass optimization ()
  ((name
    :initarg :name
    :reader optimization-name)
   (function
    :initarg :function
    :reader optimization-function
    :documentation "Function of (block &rest positional-args &key ...).")
   (positional-args
    :initarg :positional-args
    :initform nil
    :accessor optimization-positional-args)
   (keyword-args
    :initarg :keyword-args
    :initform nil
    :accessor optimization-keyword-args
    :documentation "Property list of keyword args passed after positional args.")
   (enabled-p
    :initarg :enabled-p
    :initform t
    :accessor optimization-enabled-p)
   (min-improvement
    :initarg :min-improvement
    :initform 0
    :accessor optimization-min-improvement
    :documentation "Required (old-complexity - new-complexity) to accept result."))
  (:documentation
   "Description of a single block optimization pass."))

(defun make-optimization (&key name function positional-args keyword-args
                               (enabled-p t) (min-improvement 0))
  "Convenience constructor for OPTIMIZATION."
  (make-instance 'optimization
                 :name name
                 :function function
                 :positional-args positional-args
                 :keyword-args keyword-args
                 :enabled-p enabled-p
                 :min-improvement min-improvement))

(defmethod print-object ((obj optimization) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ":name ~s" (optimization-name obj))))

(defclass optimization-pipeline ()
  ((name
    :initarg :name
    :reader optimization-pipeline-name)
   (optimizations
    :initarg :optimizations
    :initform nil
    :accessor optimization-pipeline-optimizations)
   (measure-function
    :initarg :measure-function
    :initform #'block-complexity
    :accessor optimization-pipeline-measure-function))
  (:documentation
   "A sequence of OPTIMIZATIONs applied to a block in order."))

(defun make-optimization-pipeline (&key name optimizations
                                        (measure-function #'block-complexity))
  (make-instance 'optimization-pipeline
                 :name name
                 :optimizations optimizations
                 :measure-function measure-function))


(defclass pass-id-counter ()
  ((value
    :initarg :value
    :initform 0
    :accessor pass-id-counter-value))
  (:documentation "Monotonically increasing pass-id generator."))

(defun make-pass-id-counter (&key (start 0))
  "Create a new pass-id-counter starting at START."
  (make-instance 'pass-id-counter :value start))

(defun next-pass-id (counter)
  "Return the next pass-id from COUNTER."
  (incf (pass-id-counter-value counter)))



(defun run-optimization (pass-counter opt block binding-params &key name (measure #'block-complexity)
                             (log-stream nil) verbose)
  "Apply OPT to BLOCK, compute complexity before/after, and return:
   values (new-block before-complexity after-complexity accepted-p).

If MIN-IMPROVEMENT on OPT is > 0, we only accept the new block if
(before - after) >= MIN-IMPROVEMENT. Otherwise we keep BLOCK."
  (declare (optimize (debug 3)))
  (unless (typep opt 'optimization)
    (error "RUN-OPTIMIZATION: expected OPTIMIZATION, got ~S" opt))
  (unless (typep block 'block-statement)
    (error "RUN-OPTIMIZATION: expected BLOCK-STATEMENT, got ~S" block))
  (let* ((fn        (optimization-function opt))
         (pos-args  (optimization-positional-args opt))
         (kw-args   (optimization-keyword-args opt))
         (before    (funcall measure block))
         (new-block (apply fn pass-counter block binding-params (append pos-args kw-args (list :verbose verbose))))
         (after     (funcall measure new-block))
         (impr      (- before after))
         (threshold (optimization-min-improvement opt))
         (accepted  (>= impr threshold))
         (result-block (if accepted new-block block)))
    ;; Fail fast if an optimization produced a use-before-def ordering bug.
    (check-def-before-use-in-block result-block :errorp t)
    (when log-stream
      (format log-stream "~&~80a complexity: ~6D -> ~6D (Δ=~5D) ~A~%"
              (format nil "[OPT ~a ~A]" name (optimization-name opt))
              before after impr
              (if accepted
                  (if (= impr 0)
                      "useless"
                      "ACCEPT")
                  "REJECT"))
       ;; Extra debug: track Hessian targets during e-g-hess pipelines.
       (when (and name (search "e-g-hess" (string name)))
         (labels ((h-targets (blk)
                    (loop for st in (block-statements blk)
                          for tgt = (and (typep st 'assignment-statement)
                                         (stmt-target-name st))
                          when (and tgt
                                    (symbolp tgt)
                                    (let ((s (symbol-name tgt)))
                                      (and (>= (length s) 2)
                                           (string= (subseq s 0 2) "H_"))))
                            collect tgt)))
           (let* ((before-h (h-targets block))
                  (after-h  (h-targets result-block))
                  (dropped  (set-difference before-h after-h :test #'eq))
                  (added    (set-difference after-h before-h :test #'eq)))
             (when (or dropped added)
               (format log-stream "~&   [HESS] before ~d: ~s~%"
                       (length before-h) before-h)
               (format log-stream "~&   [HESS] after  ~d: ~s~%"
                       (length after-h) after-h)
               (format log-stream "~&   [HESS] delta  -~d +~d dropped: ~s added: ~s~%"
                       (length dropped) (length added) dropped added))))))
    (when (and (eq (optimization-name opt) :cse-full)
               (< impr 0))
      (verbose-log t "~&[CSE-FULL] complexity increased (Δ=~D). Before/after blocks:" impr)
      (debug-block block :label "cse-full BEFORE")
      (debug-block new-block :label "cse-full AFTER")
      ;; Re-run once with verbosity bumped for debugging (discard result, for traces only).
      (ignore-errors
        (apply fn pass-counter block (append pos-args kw-args (list :verbose 2))))
      (verbose-log t "~&[CSE-FULL] re-ran with verbose=2 for debugging~%"))
    (values result-block
            before
            (if accepted after before)
            accepted)))


(defun run-optimization-pipeline (pass-counter pipeline block binding-params
                                  &key name (measure nil) (log-stream nil)
                                    (cycles 10))
  "Run PIPELINE on BLOCK. Returns:
   values (final-block results-list total-before total-after).

RESULTS-LIST is a list of plists:
  (:opt-name name :before b :after a :accepted accepted-p)."
  (unless (typep pipeline 'optimization-pipeline)
    (error "RUN-OPTIMIZATION-PIPELINE: expected OPTIMIZATION-PIPELINE, got ~S"
           pipeline))
  (unless (typep block 'block-statement)
    (error "RUN-OPTIMIZATION-PIPELINE: expected BLOCK-STATEMENT, got ~S" block))
  (let* (results
         (measure (or measure (optimization-pipeline-measure-function pipeline)))
         (current block)
         (total-before (funcall measure block)))
    (loop for cycle below cycles
          for cur-name = (format nil "~a-~d" name cycle)
          do (let ((any-accepted nil))
               (dolist (opt (optimization-pipeline-optimizations pipeline))
                 (when (optimization-enabled-p opt)
                   (multiple-value-bind (new-block before after accepted)
                       (run-optimization pass-counter opt current binding-params
                                         :name cur-name
                                         :measure measure :log-stream log-stream)
                     (push (list :opt-name (format nil "~a ~a" cur-name (optimization-name opt))
                                 :before   before
                                 :after    after
                                 :accepted accepted)
                           results)
                     (setf current new-block)
                     (when accepted
                       (setf any-accepted t)))))
               (unless any-accepted
                 (return))))
    (let ((total-after (funcall measure current)))
      (values current (nreverse results) total-before total-after))))

;;; ----------------------------------------------------------------------
;;; copy alias optimizations
;;; ----------------------------------------------------------------------

(defun copy-expr-alias-env (env)
  (let ((new (make-hash-table :test #'equal)))
    (maphash (lambda (k v)
               (setf (gethash k new) v))
             env)
    new))

(defun rewrite-sexpr-using-aliases (sexpr env)
  "Rewrite SEXPR by replacing any subexpression that exactly matches a key
in ENV (sexpr -> symbol) by that symbol."
  (cond
    ((atom sexpr)
     sexpr)
    (t
     (let* ((op       (car sexpr))
            (args     (cdr sexpr))
            (new-args (mapcar (lambda (sub)
                                (rewrite-sexpr-using-aliases sub env))
                              args))
            (candidate (cons op new-args))
            (alias     (gethash candidate env)))
       (if alias
           alias
           candidate)))))

(defun rewrite-expr-using-aliases (expr env)
  (let* ((sx  (expr-ir:expr->sexpr expr))
         (sx2 (rewrite-sexpr-using-aliases sx env)))
    (expr-ir:sexpr->expr-ir sx2)))

(defun good-alias-rhs-p (sx target)
  "Heuristic: which RHS expressions should we use as aliases?
We generally want non-trivial, non-constant, non-single-variable terms."
  (declare (ignore target))
  (cond
    ;; constants: not helpful
    ((numberp sx) nil)
    ;; single symbol RHS: that's just a copy, let copy-propagation handle it
    ((symbolp sx) nil)
    ;; anything else is fair game
    (t t)))

(defun note-alias-from-assignment (stmt env)
  "If STMT is 'TARGET := EXPR', record EXPR's sexpr as an alias mapping
EXPR -> TARGET, provided it passes GOOD-ALIAS-RHS-P."
  (when (typep stmt 'assignment-statement)
    (let* ((target (stmt-target-name stmt))
           (expr   (stmt-expression stmt))
           (sx     (expr-ir:expr->sexpr expr)))
      (when (good-alias-rhs-p sx target)
        (setf (gethash sx env) target)))))

(defun alias-assigned-exprs-optimization (pass-counter block binding-params &key (verbose *verbose-optimization*))
  "General aliasing optimization.

For every assignment

  T := <EXPR>

we record an alias mapping <EXPR> -> T, and then in all later statements
in the same control-flow region we rewrite any subexpression equal to <EXPR>
to the variable T.

- Aliases flow forward within a block.
- Aliases are visible inside THEN/ELSE blocks if defined before the IF.
- Aliases defined inside a branch do not leak out to the parent block."
  (declare (ignore pass-counter binding-params))
  (verbose-log verbose "~&[ALIAS-ASSIGN] scanning block for reusable RHS aliases~%")
  (labels
      ((process-block (blk env)
         (let ((new-stmts '()))
           (dolist (st (block-statements blk))
             (etypecase st
               (anchored-assignment-statement
               ;; First, rewrite RHS with current aliases
               (let* ((expr      (stmt-expression st))
                      (new-expr  (rewrite-expr-using-aliases expr env))
                      (new-stmt  (make-anchored-assignment-stmt
                                  (stmt-target-name st)
                                  new-expr
                                  (stmt-target-indices st))))
                 ;; Keep the rewritten assignment
                 (push new-stmt new-stmts)
                 ;; Then possibly register a new alias based on its RHS
                 (note-alias-from-assignment new-stmt env)))
               (assignment-statement
               ;; First, rewrite RHS with current aliases
               (let* ((expr      (stmt-expression st))
                      (new-expr  (rewrite-expr-using-aliases expr env))
                      (new-stmt  (make-assignment-stmt
                                  (stmt-target-name st)
                                  new-expr
                                  :target-indices (stmt-target-indices st))))
                 ;; Keep the rewritten assignment
                 (push new-stmt new-stmts)
                 ;; Then possibly register a new alias based on its RHS
                 (note-alias-from-assignment new-stmt env)))

               (raw-c-statement
                (let ((expr (stmt-expression st)))
                  (push (if expr
                            (make-raw-c-statement
                             (raw-c-generator st)
                             (rewrite-expr-using-aliases expr env))
                            st)
                        new-stmts)))

               (block-statement
                (multiple-value-bind (sub-block env-out)
                    (process-block st env)
                  (declare (ignore env-out))
                  (push sub-block new-stmts)))

               (if-statement
                (let* ((new-cond (rewrite-expr-using-aliases
                                  (if-condition st) env))
                       (then-env (copy-expr-alias-env env))
                       (else-env (copy-expr-alias-env env)))
                  (multiple-value-bind (then-block then-env-out)
                      (process-block (if-then-block st) then-env)
                    (declare (ignore then-env-out))
                    (multiple-value-bind (else-block else-env-out)
                        (if (if-else-block st)
                            (process-block (if-else-block st) else-env)
                            (values nil else-env))
                      (declare (ignore else-env-out))
                      (push (make-if-stmt new-cond then-block else-block)
                            new-stmts)))))

               (t
                (push st new-stmts))))
           (values (make-block-stmt (nreverse new-stmts)
                                    :label (block-label blk)) env))))
    (multiple-value-bind (new-block env-out)
        (process-block block (make-hash-table :test #'equal))
      (declare (ignore env-out))
      new-block)))
