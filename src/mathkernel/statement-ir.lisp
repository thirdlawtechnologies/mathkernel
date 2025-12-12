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
    :reader binding-env-table
    :documentation "Hash table mapping symbols to env-entry/outer-env-entry."))
  (:documentation "Environment that tracks where symbols are defined for CSE.
Assumes SSA: no variable is reassigned, and branch definitions do not join."))

(defmethod print-object ((env binding-env) stream)
  (let ((vars '()))
    (maphash (lambda (k _v) (push k vars))
             (binding-env-table env))
    (setf vars (sort vars #'string< :key #'symbol-name))
    (print-unreadable-object (env stream :type t)
      (format stream "~{~A~^ ~}" vars))))

(defun make-binding-env (&optional (table (make-hash-table :test #'eq)))
  "Construct a BINDING-ENV around TABLE (defaults to a fresh hash table)."
  (make-instance 'binding-env :table table))

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
   (target
    :initarg :target
    :initform nil
    :reader product-descriptor-target
    :documentation "Assignment target symbol for this product, when applicable."))
  (:documentation "Descriptor for a product expression used in factoring."))

(defmethod print-object ((pd product-descriptor) stream)
  (print-unreadable-object (pd stream :type t)
    (format stream "~S @~D~@[ tgt=~A~]"
            (product-descriptor-factors pd)
            (product-descriptor-stmt-index pd)
            (product-descriptor-target pd))))

(defun counts-from-factors (factors)
  "Build a multiset (hash-table) of FACTORS keyed by EQUAL."
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (f factors ht)
      (incf (gethash f ht 0)))))

(defun copy-binding-env (env)
  "Return a shallow copy of ENV (a BINDING-ENV), converting existing entries to outer-scope markers.
Any ENV-ENTRY instances are copied as OUTER-ENV-ENTRY instances so consumers can
distinguish values coming from an outer block."
  (unless (typep env 'binding-env)
    (error "copy-binding-env expects a BINDING-ENV, got ~S" env))
  (let* ((table (binding-env-table env))
         (new-table (make-hash-table
                     :test            (hash-table-test table)
                     :size            (hash-table-size table)
                     :rehash-size     (hash-table-rehash-size table)
                     :rehash-threshold (hash-table-rehash-threshold table))))
    (maphash (lambda (k v)
               (let ((entry (cond
                              ((typep v 'outer-env-entry) v)
                              ((typep v 'env-entry)
                               (make-instance 'outer-env-entry
                                              :index (env-entry-index v)
                                              :expr  (env-entry-expr v)))
                              (t v))))
                 (setf (gethash k new-table) entry)))
             table)
    (make-binding-env new-table)))

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

(defun make-assignment-stmt (target-name expression &optional target-indices)
  "Convenience constructor for a normal assignment-statement."
  (let* ((target-name (expr-ir:ev target-name))
         (class (if (anchored-target-name-p target-name)
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
  ((statements
    :initarg :statements
    :initform nil
    :accessor block-statements
    :documentation
    "Ordered list of STATEMENT instances. Represents a C compound
     statement { ... } or function body."))
  (:documentation
   "A sequence of statements. Emitted as a C compound statement {...}."))

(defun make-block-stmt (&optional (statements nil))
  (make-instance 'block-statement :statements statements))

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
       (make-assignment-stmt name s-rhs s-indices)))

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
    (make-block-stmt s-stmts)))

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
       (indent!) (format stream "{~%")
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




(defun debug-cse-temp-trace (block &key (label "cse"))
  "Print a linear trace of BLOCK showing where CSE temps are defined
and where they are used. This is only for debugging.

Each statement is numbered. For each assignment we show:
  - which CSE temps are used in RHS
  - whether they are already defined
  - when a temp is defined for the first time."
  (let ((defined '())
        (idx 0)
        (*print-pretty* nil))
    (labels ((cse-temps-in-expr (expr)
               (remove-if-not #' cse-temp-symbol-p
                                 (expr-ir:expr-free-vars expr)))
             (pp-stmt (cycle)
               (format t "~&[~D] ~A~%" idx cycle))
             (rec-block (blk)
               (dolist (cycle (block-statements blk))
                 (incf idx)
                 (etypecase cycle
                   (assignment-statement
                    (let* ((tgt (stmt-target-name cycle))
                           (expr (stmt-expression cycle))
                           (temps (cse-temps-in-expr expr))
                           (undef (remove-if (lambda (v)
                                               (member v defined :test #'eq))
                                             temps)))
                      (pp-stmt cycle)
                      (when temps
                        (format t "     uses CSE temps: ~S~%" temps))
                      (when undef
                        (format t "     *** UNDEFINED CSE temps here: ~S~%"
                                undef))
                      (when (cse-temp-symbol-p tgt)
                        (unless (member tgt defined :test #'eq)
                          (push tgt defined)
                          (format t "     defines CSE temp: ~A~%" tgt)))))

                   (block-statement
                    (format t "~&[~D] BEGIN SUBBLOCK~%" idx)
                    (rec-block cycle)
                    (format t "~&[~D] END SUBBLOCK~%" idx))

                   (if-statement
                    (format t "~&[~D] IF condition: ~S~%" idx
                            (expr-ir:expr->sexpr (if-condition cycle)))
                    (format t "     THEN:~%")
                    (rec-block (if-then-block cycle))
                    (when (if-else-block cycle)
                      (format t "     ELSE:~%")
                      (rec-block (if-else-block cycle))))

                   (t
                    (pp-stmt cycle))))))
      (format t "~&;;; ---- CSE TEMP TRACE (~A) ----~%" label)
      (rec-block block)
      (format t "~&;;; ---- END CSE TEMP TRACE (~A) ----~%" label))))

;;; ------------------------------------------------------------
;;; Reorder assignments after cse
;;; ------------------------------------------------------------

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


(defun cse-block (block &key (min-uses 2) (min-size 5) (pass-id 1)
                        (defined-env (make-binding-env)) (outer-symbols nil)
                        (verbose *verbose-optimization*))
  "Perform one CSE pass on BLOCK (a BLOCK-STATEMENT).

Temps created in this pass are named CSE_P<pass-id>_T<n>. We ensure
each temp is defined AFTER all CSE temps it uses and BEFORE its first use
in this block. When VERBOSE is non-NIL, progress is reported to *TRACE-OUTPUT*.
If VERBOSE is an integer, higher values (e.g. 2) enable more detailed logging."
  (declare (optimize (debug 3)))
  (unless (typep block 'block-statement)
    (error "cse-block: expected BLOCK-STATEMENT, got ~S" block))

  ;; First, build env for this block (parent env + local defs), then recurse into child blocks/branches.
  (let* ((stmts0 (block-statements block))
         (def-index (copy-binding-env defined-env))
         (def-table (binding-env-table def-index)))
    ;; Seed outer-scope symbols (e.g., parameters/constants) so we don't treat them as missing deps.
    (dolist (sym outer-symbols)
      (unless (gethash sym def-table)
        (setf (gethash sym def-table)
              (make-instance 'outer-env-entry :index -1 :expr nil))))
    (when (>= (verbose-level verbose) 2)
      (let ((env-syms '()))
        (maphash (lambda (k _v) (push k env-syms)) def-table)
        (verbose-log verbose 2 "~&[CSE pass ~D] input block with ~D stmts; env syms: ~S~%"
                     pass-id (length stmts0) (sort env-syms #'string< :key #'symbol-name)))
      (debug-block block :label (format nil "cse-block pass ~D input" pass-id)))
    (verbose-log verbose "~&[CSE pass ~D] scanning block with ~D statements~%"
                 pass-id (length stmts0))
    (loop for cycle in stmts0
          for idx from 0 do
            (when (typep cycle 'assignment-statement)
              ;; store index + expr for reuse
              (let ((tgt (stmt-target-name cycle)))
                (when (gethash tgt def-table)
                  (error "SSA violation: ~S already defined in env" tgt))
                (setf (gethash tgt def-table)
                      (make-env-entry idx (stmt-expression cycle))))))

    (flet ((recur (st env)
             (etypecase st
               (if-statement
                (make-if-stmt
                 (if-condition st)
                 (let ((tb (if-then-block st)))
                   (and tb (cse-block tb :min-uses min-uses :min-size min-size
                                         :pass-id pass-id :defined-env env :verbose verbose)))
                 (let ((eb (if-else-block st)))
                   (and eb (cse-block eb :min-uses min-uses :min-size min-size
                                         :pass-id pass-id :defined-env env :verbose verbose)))))
               (block-statement
                (cse-block st :min-uses min-uses :min-size min-size
                              :pass-id pass-id :defined-env env :verbose verbose))
               (t st))))
      (let* ((stmts (mapcar (lambda (s) (recur s def-index))
                            stmts0))
             (n     (length stmts))
             (current-block (make-block-stmt stmts)))

        ;; ------------------------------------------------------------
        ;; Phase 1: collect subexpr counts + earliest use index per sexpr
        ;; ------------------------------------------------------------
        (let ((counts    (make-hash-table :test #'equal))
              (first-use (make-hash-table :test #'equal)))
          (labels ((accum-sexpr (sexpr idx)
                     (incf (gethash sexpr counts 0))
                     (let ((old (gethash sexpr first-use nil)))
                       (when (or (null old) (< idx old))
                         (setf (gethash sexpr first-use) idx)))
                     (when (consp sexpr)
                       (dolist (arg (cdr sexpr))
                         (accum-sexpr arg idx))))
                   (accum-expr (expr idx)
                     (accum-sexpr (expr-ir:expr->sexpr expr) idx)))
            ;; Seed counts with expressions already available from outer env entries.
            (maphash
             (lambda (_k entry)
               (declare (ignore _k))
               (let ((expr (and (typep entry 'env-entry)
                                (env-entry-expr entry))))
                 (when expr
                   (accum-expr expr 0))))
             (binding-env-table defined-env))
            (loop for cycle in stmts
                  for idx from 0 do
                    (etypecase cycle
                      (assignment-statement
                       (accum-expr (stmt-expression cycle) idx))
                      (if-statement
                       ;; Only count condition at this level; branches are handled by recursion above.
                       (accum-expr (if-condition cycle) idx))
                      (block-statement
                       ;; Already optimized via recursion.
                       nil)
                      (t nil))))

          ;; ------------------------------------------------------------
          ;; Phase 2: collect definition indices (for temps we depend on)
          ;; ------------------------------------------------------------
          (let* ((def-index (copy-binding-env defined-env))
                 (def-table (binding-env-table def-index))
                 (all-defs (collect-scalar-targets-in-block block))
                 (env-sexpr->sym (make-hash-table :test #'equal)))
            ;; Map known expressions to their symbols for reuse.
            (maphash
             (lambda (sym entry)
               (when (and (typep entry 'env-entry)
                          (env-entry-expr entry))
                 (setf (gethash (expr-ir:expr->sexpr (env-entry-expr entry))
                                env-sexpr->sym)
                       sym)))
             def-table)

            ;; ------------------------------------------------------------
            ;; Phase 3: choose CSE candidates
            ;; ------------------------------------------------------------
            (let ((candidates '()))
              (maphash
               (lambda (sexpr count)
                 (when (and (>= count min-uses)
                            (consp sexpr)
                            (>= (sexpr-size sexpr) min-size))
                   (push sexpr candidates)))
               counts)

              (when (null candidates)
                (verbose-log verbose "~&[CSE pass ~D] no candidates (min-uses=~D min-size=~D)~%"
                             pass-id min-uses min-size)
                ;; no CSE change
                (return-from cse-block current-block))
              ;; Establish deterministic ordering of candidates so temp naming
              ;; is stable across runs.
              (setf candidates
                    (sort candidates #'string<
                          :key (lambda (sexpr)
                                 (format nil "~S" sexpr))))

              ;; ------------------------------------------------------------
              ;; Phase 4: build temps, compute insertion indices
              ;; ------------------------------------------------------------
              (let ((sexpr->temp (make-hash-table :test #'equal))
                    (temp-insert (make-hash-table :test #'eq))
                    (temp-counter (next-cse-temp-index-for-pass block pass-id))
                    (existing-temps (make-hash-table :test #'eq)))
                (labels
                    ((temp-name ()
                       (incf temp-counter)
                       (expr-ir:ev (format nil "CSE_P~D_T~D" pass-id temp-counter))))

                  ;; For each candidate sexpr, decide temp symbol & insertion index.
                  (dolist (sexpr candidates)
                    (let* ((existing (gethash sexpr env-sexpr->sym))
                           (temp (or existing (temp-name)))
                           (uses    (expr-ir:expr-free-vars
                                     (expr-ir:sexpr->expr-ir sexpr)))
                           (first   (gethash sexpr first-use)) ; earliest use index of this sexpr
                           ;; If an operand is defined somewhere in this block but has no
                           ;; recorded definition in scope, skip. Unknown symbols (params/constants)
                           ;; are treated as outer-scope and allowed.
                           (blocked-p (some (lambda (u)
                                              (and (member u all-defs :test #'eq)
                                                   (null (gethash u def-table nil))))
                                            uses))
                           (dep-max (if uses
                                        (loop with max-di = -1
                                              for u in uses
                                              for entry = (gethash u def-table nil)
                                              for di = (and entry (env-entry-index entry))
                                              when (and entry
                                                        (not (typep entry 'outer-env-entry))
                                                        (integerp di)) do
                                                          (setf max-di (max max-di di))
                                              finally (return max-di))
                                        -1))
                           (min-insert (max (1+ dep-max) 0))
                           (max-allowed (or first n)))
                      #+(or)(when (and (eq '* (first sexpr))
                                       (<= 2 (loop for sub in (cdr sexpr)
                                                   when (eq sub 'expr-var::invr2)
                                                     sum 1)))
                              (break "New check invr2*invr2"))
                      ;; Skip if blocked by barrier scope or impossible placement.
                      (if (and (not blocked-p)
                               (<= min-insert max-allowed))
                          (let ((insert-idx (min min-insert max-allowed)))
                            (verbose-log verbose "~&[CSE pass ~D] select ~S -> ~A (count=~D size=~D insert=~D existing? ~A deps-max=~D first-use=~A)~%"
                                         pass-id sexpr temp
                                         (gethash sexpr counts) (sexpr-size sexpr)
                                         insert-idx (and existing t) dep-max first)
                            (setf (gethash sexpr sexpr->temp) temp)
                            (unless existing
                              (setf (gethash temp  temp-insert) insert-idx))
                            (when existing
                              (setf (gethash temp existing-temps) t)))
                          (verbose-log verbose "~&[CSE pass ~D] skip ~S (blocked? ~A min-insert=~D max-allowed=~A)~%"
                                       pass-id sexpr blocked-p min-insert max-allowed)))))

              ;; --------------------------------------------------------
              ;; Phase 5: rewrite original statements to use temps
              ;; --------------------------------------------------------
              (labels ((rewrite-sexpr (sexpr target)
                         (let ((maybe-temp (gethash sexpr sexpr->temp)))
                           (cond
                             ((and maybe-temp
                                   (gethash maybe-temp existing-temps)
                                   target
                                   (eq target maybe-temp))
                              ;; Avoid turning a defining assignment into a self-assignment.
                              sexpr)
                             (maybe-temp maybe-temp)
                             ((consp sexpr)
                              (cons (car sexpr)
                                    (mapcar (lambda (s) (rewrite-sexpr s target))
                                            (cdr sexpr))))
                             (t sexpr))))
                       (rewrite-expr (expr target)
                         (expr-ir:sexpr->expr-ir
                          (rewrite-sexpr (expr-ir:expr->sexpr expr) target)))
                       (rewrite-stmt (cycle)
                         (etypecase cycle
                           (anchored-assignment-statement
                            (make-anchored-assignment-stmt
                             (stmt-target-name cycle)
                             (rewrite-expr (stmt-expression cycle)
                                           (stmt-target-name cycle))))
                           (assignment-statement
                            (make-assignment-stmt
                             (stmt-target-name cycle)
                             (rewrite-expr (stmt-expression cycle)
                                           (stmt-target-name cycle))))
                           (if-statement
                            (make-if-stmt
                             (rewrite-expr (if-condition cycle) nil)
                             (cse-block (if-then-block cycle)
                                        :min-uses min-uses
                                        :min-size min-size
                                        :pass-id pass-id
                                        :verbose verbose)
                             (when (if-else-block cycle)
                               (cse-block (if-else-block cycle)
                                          :min-uses min-uses
                                          :min-size min-size
                                          :pass-id pass-id
                                          :verbose verbose))))
                           (block-statement
                            (cse-block cycle
                                       :min-uses min-uses
                                       :min-size min-size
                                       :pass-id pass-id
                                       :verbose verbose))
                           (t cycle))))

                (let* ((rewritten-stmts
                         (loop for cycle in stmts collect (rewrite-stmt cycle)))
                       ;; temp assignments bucketed by insertion index
                       (insert-buckets (make-array (1+ n)
                                                   :initial-element nil)))
                  ;; build temp assignments and bucket them
                  (maphash
                   (lambda (sexpr temp)
                     (let ((idx (gethash temp temp-insert)))
                       (when idx
                         (let* ((expr (expr-ir:sexpr->expr-ir sexpr))
                                (assign (make-assignment-stmt temp expr)))
                           (push assign (aref insert-buckets idx))))))
                   sexpr->temp)
                  ;; now assemble final statement list
                  (let ((new-stmts '()))
                    (loop for idx from 0 below n do
                      ;; temps scheduled *before* statement idx
                      (dolist (a (nreverse (aref insert-buckets idx)))
                        (push a new-stmts))
                      ;; then the original statement idx
                      (push (nth idx rewritten-stmts) new-stmts))
                    ;; temps scheduled after last stmt
                    (dolist (a (nreverse (aref insert-buckets n)))
                      (push a new-stmts))
                    (let ((result (make-block-stmt (nreverse new-stmts))))
                      (verbose-log verbose "~&[CSE pass ~D] inserted ~D new temps~%"
                                   pass-id (hash-table-count temp-insert))
                      result))))))))))))




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
                   (raw-c-statement nil)
                   ))))
      (scan-block block))
    max-index))

(defparameter *in-rewrite* nil)

;;; ----------------------------------------------------------------------
;;; CSE factor walker context/accumulator
;;; ----------------------------------------------------------------------

(defclass cse-factor-accumulator ()
  ((products
    :initarg :products
    :accessor cse-acc-products
    :initform '()
    :documentation "Collected product-descriptor objects for this pass.")
   (env-product->sym
    :initarg :env-product->sym
    :accessor cse-acc-env-product->sym
    :initform (make-hash-table :test #'equal)
    :documentation "Map product sexpr -> existing temp symbol for reuse.")
   (stmts
    :initarg :stmts
    :accessor cse-acc-stmts
    :initform '()
    :documentation "Threaded statements in program order during the walk.")))

(defclass cse-factor-context (walk-context)
  ((env
    :initarg :env
    :accessor cse-ctx-env)
   (idx
    :initarg :idx
    :accessor cse-ctx-idx)
   (pass-id
    :initarg :pass-id
    :reader cse-ctx-pass-id)
   (min-uses
    :initarg :min-uses
    :reader cse-ctx-min-uses)
   (min-factors
    :initarg :min-factors
    :reader cse-ctx-min-factors)
   (min-size
    :initarg :min-size
    :reader cse-ctx-min-size)
   (cse-only
    :initarg :cse-only
    :reader cse-ctx-cse-only)
   (verbose
    :initarg :verbose
    :reader cse-ctx-verbose)
   (outer-symbols
    :initarg :outer-symbols
    :reader cse-ctx-outer-symbols)
   (acc
    :initarg :acc
    :accessor cse-ctx-acc)))

(defun make-cse-factor-context (&key env idx pass-id min-uses min-factors min-size
                                     cse-only verbose outer-symbols acc)
  (make-instance 'cse-factor-context
                 :env env
                 :idx idx
                 :pass-id pass-id
                 :min-uses min-uses
                 :min-factors min-factors
                 :min-size min-size
                 :cse-only cse-only
                 :verbose verbose
                 :outer-symbols outer-symbols
                 :acc acc))

(defmethod clone-context ((operation (eql :cse-factor-products)) (ctx cse-factor-context))
  ;; Branch-local env/idx are copied; accumulator is shared so branches contribute
  ;; to the same analysis.
  (make-cse-factor-context
   :env (copy-binding-env (cse-ctx-env ctx))
   :idx -1
   :pass-id (cse-ctx-pass-id ctx)
   :min-uses (cse-ctx-min-uses ctx)
   :min-factors (cse-ctx-min-factors ctx)
   :min-size (cse-ctx-min-size ctx)
   :cse-only (cse-ctx-cse-only ctx)
   :verbose (cse-ctx-verbose ctx)
   :outer-symbols (cse-ctx-outer-symbols ctx)
   :acc (cse-ctx-acc ctx)))

(defun %counts-from-factors (factors)
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (f factors ht)
      (incf (gethash f ht 0)))))

(defun %record-products-from-expr (expr idx target acc min-factors)
  (labels ((rec (sx)
             (when (and (consp sx) (eq (car sx) '*))
               (let ((factors (cdr sx)))
                 (when (>= (length factors) min-factors)
                   (push (make-instance 'product-descriptor
                                        :factors factors
                                        :counts (%counts-from-factors factors)
                                        :stmt-index idx
                                        :target target)
                         (cse-acc-products acc)))))
             (when (consp sx)
               (dolist (arg (cdr sx))
                 (rec arg)))))
    (rec (expr-ir:expr->sexpr expr))
    acc))

(defmethod on-statement ((operation (eql :cse-factor-products)) (ctx cse-factor-context) st)
  (etypecase st
    (assignment-statement
     (let* ((env (cse-ctx-env ctx))
            (def-table (binding-env-table env))
            (idx (incf (cse-ctx-idx ctx)))
            (tgt (stmt-target-name st)))
       (when (gethash tgt def-table)
         (error "SSA violation: ~S already defined in env" tgt))
       (setf (gethash tgt def-table)
             (make-env-entry idx (stmt-expression st)))
       (%record-products-from-expr (stmt-expression st) idx tgt
                                   (cse-ctx-acc ctx)
                                   (cse-ctx-min-factors ctx))
       (push st (cse-acc-stmts (cse-ctx-acc ctx)))
       (values st ctx)))
    (raw-c-statement
     (push st (cse-acc-stmts (cse-ctx-acc ctx)))
     (values st ctx))
    (if-statement
     ;; Children handled separately in the main function; keep as-is here.
     (push st (cse-acc-stmts (cse-ctx-acc ctx)))
     (values st ctx))
    (block-statement
     (push st (cse-acc-stmts (cse-ctx-acc ctx)))
     (values st ctx))
    (t
     (push st (cse-acc-stmts (cse-ctx-acc ctx)))
     (values st ctx))))

(defun collect-products-in-block (block def-env acc pass-id min-uses min-factors min-size cse-only outer-symbols verbose)
  "Walk BLOCK with a shared accumulator ACC, updating DEF-ENV. Returns ACC."
  (let ((ctx (make-cse-factor-context
              :env def-env
              :idx -1
              :pass-id pass-id
              :min-uses min-uses
              :min-factors min-factors
              :min-size min-size
              :cse-only cse-only
              :verbose verbose
              :outer-symbols outer-symbols
              :acc acc)))
    (walk-block-with-context :cse-factor-products ctx block)
    acc))

(defun choose-best-factor (products min-factors min-size min-uses cse-only env-product->sym pass-id verbose)
  "Score candidates and return (values best-cand best-score best-uses best-first-use)."
  (labels ((multiset-intersection (counts1 counts2)
             (let ((result '()))
               (maphash
                (lambda (k v1)
                  (let ((v2 (gethash k counts2 0)))
                    (when (> (min v1 v2) 0)
                      (dotimes (i (min v1 v2))
                        (push k result)))))
                counts1)
               (sort result #'string< :key (lambda (sexpr) (format nil "~S" sexpr)))))
           (multiset-subset-p (cand-counts prod-counts)
             (let ((ok t))
               (maphash (lambda (k v)
                          (unless (<= v (gethash k prod-counts 0))
                            (setf ok nil)))
                        cand-counts)
               ok))
           (counts-from-factors (factors)
             (let ((ht (make-hash-table :test #'equal)))
               (dolist (f factors ht)
                 (incf (gethash f ht 0))))))
    (let ((candidate-lists '()))
      (let ((plist (coerce products 'vector))
            (m     (length products)))
        (loop for i from 0 below (1- m) do
          (let* ((ppi (aref plist i))
                 (ci  (product-descriptor-counts ppi)))
            (loop for j from (1+ i) below m do
              (let* ((pj (aref plist j))
                     (cj (product-descriptor-counts pj))
                     (common (multiset-intersection ci cj)))
                (when (and (>= (length common) min-factors)
                           (or (not cse-only)
                               (every #'cse-temp-symbol-p common)))
                  (push common candidate-lists)))))))
      (setf candidate-lists (remove-duplicates candidate-lists :test #'equal))
      (setf candidate-lists
            (sort candidate-lists #'string<
                  :key (lambda (cand) (format nil "~S" cand))))
      (when (null candidate-lists)
        (verbose-log verbose "~&[CSE-FACT pass ~D] no factor candidates found~%"
                     pass-id)
        (return-from choose-best-factor (values nil 0 0 nil)))
      (verbose-log verbose "~&[CSE-FACT pass ~D] ~D candidates across ~D products~%"
                   pass-id (length candidate-lists) (length products))
      (verbose-log verbose 2 "~&[CSE-FACT pass ~D] candidates:~%  ~{~A~%  ~}"
                   pass-id
                   (mapcar (lambda (cand) (format nil "~S" cand)) candidate-lists))
      (let ((best-cand nil)
            (best-score 0)
            (best-uses  0)
            (best-first-use nil))
        (dolist (cand candidate-lists)
          (let* ((cand-counts (counts-from-factors cand))
                 (prod-sexpr  (cons '* cand))
                 (size        (sexpr-size prod-sexpr))
                 (uses        0)
                 (first-use   nil))
            (cond
              ((< size min-size)
               (verbose-log verbose 2 "~&[CSE-FACT pass ~D] skip cand ~S (size ~D < min-size ~D)~%"
                            pass-id cand size min-size))
              (t
               (dolist (prod products)
                 (let ((pc (product-descriptor-counts prod)))
                   (when (multiset-subset-p cand-counts pc)
                     (incf uses)
                     (let ((idx (product-descriptor-stmt-index prod)))
                       (setf first-use (if first-use (min first-use idx) idx))))))
               (let ((score (* (- uses 1) size)))
                 (verbose-log verbose 2 "~&[CSE-FACT pass ~D] cand ~S size=~D uses=~D first-use=~A score=~D (min-uses=~D)~%"
                              pass-id cand size uses first-use score min-uses)
                 (when (>= uses min-uses)
                   (cond
                     ((> score best-score)
                      (setf best-score score
                            best-cand cand
                            best-uses uses
                            best-first-use first-use))
                     ((and (= score best-score) best-cand
                           (string< (format nil "~S" cand)
                                    (format nil "~S" best-cand)))
                      (setf best-cand cand
                            best-uses uses
                            best-first-use first-use))))))))))
        (values best-cand best-score best-uses best-first-use)))))

(defun rewrite-with-factor (stmts env-product->sym def-env all-defs best-cand best-first-use pass-id verbose)
  "Insert/reuse temp for BEST-CAND and rewrite STMTs accordingly.
Returns (values new-stmts changed-p)."
  (let* ((n (length stmts))
         (counts-from-factors (lambda (factors)
                                (let ((ht (make-hash-table :test #'equal)))
                                  (dolist (f factors ht)
                                    (incf (gethash f ht 0))))))
         (cand-counts (funcall counts-from-factors best-cand))
         (start-index (next-cse-temp-index-for-pass (make-block-stmt stmts) pass-id))
         (existing (gethash (cons '* best-cand) env-product->sym))
         (temp (or existing
                   (intern (format nil "CSE_P~D_T~D" pass-id (1+ start-index))
                           (symbol-package 'stmt-ir::cse-t))))
         (temp-expr (expr-ir:sexpr->expr-ir (cons '* best-cand)))
         (vars-used (expr-ir:expr-free-vars temp-expr))
         (def-table (binding-env-table def-env))
         (deps-defined-indices
           (loop for u in vars-used
                 for entry = (gethash u def-table nil)
                 for di = (and entry (env-entry-index entry))
                 when (and entry (not (typep entry 'outer-env-entry)) (integerp di))
                   collect di))
         (missing-dep (some (lambda (u)
                              (and (member u all-defs :test #'eq)
                                   (null (gethash u def-table nil))))
                            vars-used))
         (dep-max (if deps-defined-indices (reduce #'max deps-defined-indices) -1))
         (min-insert (max (1+ dep-max) 0))
         (max-allowed (or best-first-use n)))
    (when (and vars-used missing-dep)
      (verbose-log verbose "~&[CSE-FACT pass ~D] skip ~S (missing deps in scope)~%"
                   pass-id best-cand)
      (return-from rewrite-with-factor (values stmts products nil)))
    (when (> min-insert max-allowed)
      (verbose-log verbose "~&[CSE-FACT pass ~D] skip ~S (deps after first use: min-insert=~D max=~D)~%"
                   pass-id best-cand min-insert max-allowed)
      (return-from rewrite-with-factor (values stmts products nil)))
    (let* ((insert-idx (min min-insert max-allowed))
           (temp-assign (and (not existing)
                             (make-assignment-stmt temp temp-expr))))
      (labels ((multiset-subset-p (cand-counts prod-counts)
                 (let ((ok t))
                   (maphash (lambda (k v)
                              (unless (<= v (gethash k prod-counts 0))
                                (setf ok nil)))
                            cand-counts)
                   ok))
             (rewrite-sexpr (sexpr target)
               (cond
                 ((and (consp sexpr) (eq (car sexpr) '*))
                  (let* ((orig-factors (cdr sexpr))
                         (prod-counts (funcall counts-from-factors orig-factors)))
                    (if (multiset-subset-p cand-counts prod-counts)
                        (let ((remaining '()))
                          (maphash
                           (lambda (k vc)
                             (let* ((cc (gethash k cand-counts 0))
                                    (rc (- vc cc)))
                               (when (> rc 0)
                                 (dotimes (i rc) (push k remaining)))))
                           prod-counts)
                          (let ((new-remaining (mapcar (lambda (s) (rewrite-sexpr s target))
                                                       remaining)))
                            (cond
                              ((null new-remaining)
                               (if (and existing target (eq target temp))
                                   sexpr
                                   temp))
                              (t
                               (cons '* (cons (if (and existing target (eq target temp))
                                                  sexpr
                                                  temp)
                                              new-remaining))))))
                        (cons '* (mapcar (lambda (s) (rewrite-sexpr s target))
                                         orig-factors)))))
                 ((consp sexpr)
                  (cons (car sexpr)
                        (mapcar (lambda (s) (rewrite-sexpr s target)) (cdr sexpr))))
                 (t sexpr))))
               (rewrite-expr (expr target)
                 (expr-ir:sexpr->expr-ir (rewrite-sexpr (expr-ir:expr->sexpr expr) target)))
               (rewrite-stmt (cycle)
                 (etypecase cycle
                   (anchored-assignment-statement
                    (make-anchored-assignment-stmt
                     (stmt-target-name cycle)
                     (rewrite-expr (stmt-expression cycle) (stmt-target-name cycle))))
                   (assignment-statement
                    (make-assignment-stmt
                     (stmt-target-name cycle)
                     (rewrite-expr (stmt-expression cycle) (stmt-target-name cycle))))
                   (raw-c-statement
                    (let ((expr (stmt-expression cycle)))
                      (if expr
                          (make-raw-c-statement (raw-c-generator cycle)
                                                (rewrite-expr expr nil))
                          cycle)))
                   (if-statement cycle)
                   (block-statement cycle)
                   (t cycle))))
        (let ((new-stmts '()))
          (loop for idx from 0 below n do
            (when (and temp-assign (= idx insert-idx))
              (push temp-assign new-stmts))
            (push (rewrite-stmt (nth idx stmts)) new-stmts))
          (when (and temp-assign (= insert-idx n))
            (push temp-assign new-stmts))
          ;; record temp for reuse
          (setf (gethash temp def-table)
                (make-env-entry (incf (binding-env-counter def-env)) (candidate-expr nil)))
          (values (nreverse new-stmts) t))))))
(defun cse-factor-products-in-block
    (block &key (min-uses 2) (min-factors 2) (min-size 2) (pass-id 1)
           (cse-only nil) (defined-env (make-binding-env)) (outer-symbols nil)
           (verbose *verbose-optimization*))
  "Try to factor out common multiplicative sub-products across all
product subexpressions in BLOCK (a BLOCK-STATEMENT).

We treat each (* ...) node as a multiset of factors and:

  - For each pair of products, compute the multiset intersection
    of their factor lists to propose a candidate common sub-product.
  - Score each candidate by SCORE = (uses-1) * size, where USES is
    the number of product subexpressions that contain the candidate
    as a sub-multiset, and SIZE is the SEXPR-SIZE of the product
    of the candidate factors.
  - Pick the best-scoring candidate above thresholds and introduce
    a temp CSE_P<pass-id>_T<n> for it.
  - Rewrite each product that contains the candidate factors to use
    the temp times the remaining factors.

If no useful candidate is found, return BLOCK unchanged (EQ).
When VERBOSE is non-NIL (or an integer level), candidate selection decisions
are logged to *TRACE-OUTPUT* (higher levels show more detail)."
  (declare (optimize (debug 3)))
  (unless (typep block 'block-statement)
    (error "cse-factor-products-in-block: expected BLOCK-STATEMENT, got ~S" block))

  (let* ((def-env (copy-binding-env defined-env))
         (def-table (binding-env-table def-env))
         (all-defs (collect-scalar-targets-in-block block))
         (acc (make-instance 'cse-factor-accumulator)))
    ;; Seed outer-scope symbols (e.g., parameters/constants) so we don't treat them as missing deps.
    (dolist (sym outer-symbols)
      (unless (gethash sym def-table)
        (setf (gethash sym def-table)
              (make-instance 'outer-env-entry :index -1 :expr nil))))
    (verbose-log verbose "~&[CSE-FACT pass ~D] scanning block with ~D statements (cse-only? ~A)~%"
                 pass-id (length (block-statements block)) cse-only)
    ;; Walk once to collect products/stmts across this block.
    (let* ((ctx (make-cse-factor-context
                 :env def-env
                 :idx -1
                 :pass-id pass-id
                 :min-uses min-uses
                 :min-factors min-factors
                 :min-size min-size
                 :cse-only cse-only
                 :verbose verbose
                 :outer-symbols outer-symbols
                 :acc acc)))
      (walk-block-with-context :cse-factor-products ctx block))
    (let* ((stmts (nreverse (cse-acc-stmts acc)))
           (n     (length stmts))
           (env-product->sym (cse-acc-env-product->sym acc))
           (products (nreverse (cse-acc-products acc))))

        
      ;; Seed product list and reuse map from env entries.
      (maphash
       (lambda (sym entry)
         (when (and (typep entry 'env-entry)
                    (env-entry-expr entry))
           (let* ((sx (expr-ir:expr->sexpr (env-entry-expr entry))))
             (when (and (consp sx) (eq (car sx) '*))
               (setf (gethash sx env-product->sym) sym)
               (push (make-instance 'product-descriptor
                                    :factors (cdr sx)
                                    :counts  (counts-from-factors (cdr sx))
                                    :stmt-index (env-entry-index entry)
                                    :target sym)
                     products)))))
       def-table)
        ;; Collect all product subexpressions and their factor multisets.
        (labels ((collect-sexpr (sexpr idx)
                   (when (and (consp sexpr)
                              (eq (car sexpr) '*))
                     (let ((factors (cdr sexpr)))
                       ;; Ignore trivial products with too few factors.
                     (when (>= (length factors) min-factors)
                       (let ((counts (make-hash-table :test #'equal)))
                         (dolist (f factors)
                           (incf (gethash f counts 0)))
                         (push (make-instance 'product-descriptor
                                              :factors factors
                                               :counts counts
                                               :stmt-index idx
                                               :target (stmt-target-name (nth idx stmts)))
                                 products))))
                   ;; Recurse into children
                   (when (consp sexpr)
                     (dolist (arg (cdr sexpr))
                       (collect-sexpr arg idx)))))
               (collect-expr (expr idx)
                 (collect-sexpr (expr-ir:expr->sexpr expr) idx))
               (collect-stmt (cycle idx)
                 (etypecase cycle
                   (assignment-statement
                    (collect-expr (stmt-expression cycle) idx))
                   (if-statement
                    ;; Do not collect product candidates from inside branches;
                    ;; avoid hoisting across barriers.
                    (collect-expr (if-condition cycle) idx))
                   (block-statement
                    ;; Skip nested blocks at this level for factoring.
                    nil)
                   (t nil))))
          (loop for cycle in stmts
                for idx from 0 do
                  (collect-stmt cycle idx))

          ;; Make products defined in this block available for reuse in this pass,
          ;; so repeated iterations recognize existing temps.
          (dolist (prod products)
            (let* ((tgt (product-descriptor-target prod))
                   (factors (product-descriptor-factors prod))
                   (sx (cons '* factors)))
              (when tgt
                (setf (gethash sx env-product->sym) tgt))))

          (setf products (nreverse products))

        (when (< (length products) 2)
          (verbose-log verbose "~&[CSE-FACT pass ~D] not enough products (~D)~%"
                       pass-id (length products))
          ;; Nothing to factor.
          (return-from cse-factor-products-in-block current-block))

        ;; Helper: multiset intersection and subset tests on hash tables.
        (flet ((multiset-intersection (counts1 counts2)
                 (let ((result '()))
                   (maphash
                    (lambda (k v1)
                      (let ((v2 (gethash k counts2 0)))
                        (when (> (min v1 v2) 0)
                          (dotimes (i (min v1 v2))
                            (push k result)))))
                    counts1)
                   ;; Canonicalize order for use as hash key / equality.
                   (sort result #'string<
                         :key (lambda (sexpr)
                                (format nil "~S" sexpr)))))
               (multiset-subset-p (cand-counts prod-counts)
                 (let ((ok t))
                   (maphash
                    (lambda (k v)
                      (unless (<= v (gethash k prod-counts 0))
                        (setf ok nil)))
                    cand-counts)
                   ok))
               (counts-from-factors (factors)
                 (let ((ht (make-hash-table :test #'equal)))
                   (dolist (f factors ht)
                     (incf (gethash f ht 0))))))

          ;; Build candidate factor multisets from all pairs of products.
          (let ((candidate-lists '()))
            (let ((plist (coerce products 'vector))
                  (m     (length products)))
              (loop for i from 0 below (1- m) do
                (let* ((ppi (aref plist i))
                       (ci  (product-descriptor-counts ppi)))
                  (loop for j from (1+ i) below m do
                    (let* ((pj (aref plist j))
                           (cj (product-descriptor-counts pj))
                           (common (multiset-intersection ci cj)))
                      (when (and (>= (length common) min-factors)
                                 (or (not cse-only)
                                     (every #'cse-temp-symbol-p common)))
                        (push common candidate-lists)))))))
            (setf candidate-lists
                  (remove-duplicates candidate-lists :test #'equal))
            ;; Deterministic iteration order to stabilize temp naming.
          (setf candidate-lists
                (sort candidate-lists #'string<
                      :key (lambda (cand)
                             (format nil "~S" cand))))

          (when (null candidate-lists)
            (verbose-log verbose "~&[CSE-FACT pass ~D] no factor candidates found~%"
                         pass-id)
            (return-from cse-factor-products-in-block current-block))

          (verbose-log verbose "~&[CSE-FACT pass ~D] ~D candidates across ~D products~%"
                       pass-id (length candidate-lists) (length products))
          (verbose-log verbose 2 "~&[CSE-FACT pass ~D] candidates:~%  ~{~A~%  ~}"
                       pass-id
                       (mapcar (lambda (cand)
                                 (format nil "~S" cand))
                               candidate-lists))

            ;; Score candidates and pick the best.
      (loop
        ;; Pick best candidate
        (multiple-value-bind (best-cand best-score best-uses best-first-use)
            (choose-best-factor products min-factors min-size min-uses cse-only env-product->sym pass-id verbose)
          (when (or (null best-cand)
                    (< best-uses min-uses)
                    (<= best-score 0))
            (return (make-block-stmt stmts)))
          (verbose-log verbose "~&[CSE-FACT pass ~D] best candidate so far: ~S (score=~D uses=~D first-use=~A)~%"
                       pass-id best-cand best-score best-uses best-first-use)
          (multiple-value-bind (new-stmts changed)
              (rewrite-with-factor stmts env-product->sym def-env all-defs best-cand best-first-use pass-id verbose)
            (declare (ignore changed))
            (if (eq new-stmts stmts)
                (return (make-block-stmt stmts))
                (progn
                  (setf stmts new-stmts)
                  ;; Re-walk to collect products again after rewrite.
                  (let* ((acc2 (make-instance 'cse-factor-accumulator)))
                    (collect-products-in-block (make-block-stmt stmts) def-env acc2 pass-id min-uses min-factors min-size cse-only outer-symbols verbose)
                    (setf env-product->sym (cse-acc-env-product->sym acc2))
                    (setf products (nreverse (cse-acc-products acc2))))))))))))


(defun cse-block-multi-optimization (counter block
                                     &key
                                       (max-passes 5)
                                       (min-uses 2)
                                       (min-size 5)
                                       (outer-symbols nil)
                                       (verbose *verbose-optimization*))
  "Apply CSE repeatedly to BLOCK up to MAX-PASSES times, or until
a pass makes no changes.

Each pass uses a distinct temp namespace: CSE_P<pass>_T<n>.
VERBOSE enables trace logging for each pass; use an integer (e.g. 2) for
more detail."
  (declare (optimize (debug 3)))
  (unless (typep block 'block-statement)
    (error "cse-block-multi: expected BLOCK-STATEMENT, got ~S" block))
  (let ((current-block block))
    (loop for ii from 1 upto max-passes do
      (let* ((pass-id (next-pass-id counter))
             (pass pass-id)
             (before-symbol-list
               (collect-scalar-targets-in-block current-block))
             ;; 1. Standard subtree CSE
             (after-cse
               (cse-block current-block
                          :min-uses min-uses
                          :min-size min-size
                          :pass-id  pass-id
                          :outer-symbols outer-symbols
                          :verbose verbose))
             ;; 2. Generic product factoring (disabled via #+(or))
             (after-prod 
               (let ((current after-cse))
                       (loop for iter from 1 to 10 do
                         (let ((next (cse-factor-products-in-block
                                      current
                                      :min-uses   min-uses
                                      :min-factors 2
                                      ;; Allow small products like invr2*invr2 to be reused.
                                      :min-size   1
                                      :pass-id    pass-id
                                      :outer-symbols outer-symbols
                                      :verbose    verbose)))
                           (when verbose
                             (verbose-log verbose "~&[CSE-FACT pass ~D] iter ~D changed? ~A~%"
                                          pass-id iter (not (eq next current))))
                           (when (eq next current)
                             (return current))
                           (setf current next)))
                       current))
             ;; 3. Extra pass: only factor products made entirely of CSE temps,
             ;;    and allow small products like CSE_P*_T* * CSE_P*_T* * CSE_P*_T*. (disabled via #+(or))
             (next-block
               (let ((current after-prod))
                 (loop for iter from 1 to 10 do
                   (let ((next
                           (cse-factor-products-in-block
                            current
                            :min-uses    2
                            :min-factors 3 ; require at least 3 factors, like your example
                            :min-size    1 ; allow smallish expressions
                            :pass-id     pass-id
                            :cse-only    t
                            :outer-symbols outer-symbols
                            :verbose     verbose)))
                     (when verbose
                       (verbose-log verbose "~&[CSE-FACT-TEMP pass ~D] iter ~D changed? ~A~%"
                                    pass-id iter (not (eq next current))))
                     (when (eq next current)
                       (return current))
                     (setf current next)))
                 current))
             ;; 4. Collapse trivial aliases introduced by factoring.
             (after-copy
               (let ((cp (copy-propagate-optimization pass-id next-block :verbose verbose)))
                 (when verbose
                   (verbose-log verbose "~&[CSE pass ~D] copy-propagate after factoring~%" pass-id))
                 cp))
             (next-block after-copy)
             (after-symbol-list
              (collect-scalar-targets-in-block next-block)))
        (when *debug*
          (handler-case
              (check-cse-temp-order next-block
                                    :label (format nil "cse-pass-~D" pass))
            (error (e)
              (format *error-output* "~&[CSE-DEBUG] ~A~%" e)
              (debug-cse-temp-trace next-block
                                    :label (format nil "cse-pass-~D" pass))
              (error e))))
        (verbose-log verbose "~&[CSE pass ~D] changed? ~A (temps ~D -> ~D)~%"
                     pass-id
                     (not (eq next-block current-block))
                     (length before-symbol-list) (length after-symbol-list))
        (when (and (eq next-block current-block)
                   (equal before-symbol-list after-symbol-list))
          (return current-block))
        (setf current-block next-block)))
    ;; Leave ordering as produced; def-before-use is now enforced per pass.
    current-block))


(defun cse-block-multi (block &key (max-passes 5) (min-uses 2) (min-size 5)
                              (outer-symbols nil)
                              (verbose *verbose-optimization*))
  "Compatibility wrapper that runs CSE with a fresh, local pass-id counter."
  (Warn "cse-block-multi is a thin wrapper for debugging")
  (let ((counter (make-pass-id-counter)))
    (cse-block-multi-optimization counter block
                                               :max-passes max-passes
                                               :min-uses   min-uses
                                               :min-size   min-size
                                               :outer-symbols outer-symbols
                                               :verbose    verbose)))



;;; ------------------------------------------------------------
;;; Copy propagation / dead trivial copies on blocks
;;; ------------------------------------------------------------

(defun copy-propagate-optimization (pass-counter block &key (verbose *verbose-optimization*))
  "Eliminate trivial copies in BLOCK of the form:
     t = u;
   where the RHS is just a variable (symbol).

We:
  - maintain an env mapping symbols to their 'root' representative,
  - rewrite all RHS expressions using this env,
  - for trivial copies t = u:
      * in straight-line/top-level code, we may coalesce or alias
        and drop the copy (as before),
      * inside IF branches, we keep the copy (so that definitions
        remain explicit across control-flow joins) and do not
        rely on branch-local aliasing outside the branch."
  (declare (ignore pass-counter))
  (verbose-log verbose "~&[COPY-PROP] scanning block with ~D statements~%"
               (length (block-statements block)))
  (labels
      ((find-root (sym env)
         "Follow env mapping sym -> ... until a fixed point."
         (loop
           for s = sym then (or (gethash s env) s)
           for next = (gethash s env)
           while next
           finally (return s)))

       (rewrite-sexpr (sexpr env)
         (cond
           ((symbolp sexpr)
            (find-root sexpr env))
           ((consp sexpr)
            (mapcar (lambda (sub) (rewrite-sexpr sub env)) sexpr))
           (t
            sexpr)))

       (rewrite-expr (expr env)
         (let* ((sexpr     (expr-ir:expr->sexpr expr))
                (new-sexpr (rewrite-sexpr sexpr env)))
           (expr-ir:sexpr->expr-ir new-sexpr)))

       ;; --- helpers for renaming a symbol in already-processed stmts ---

       (rename-symbol-in-sexpr (sexpr from to)
         (cond
           ((symbolp sexpr)
            (if (eq sexpr from) to sexpr))
           ((consp sexpr)
            (mapcar (lambda (sub)
                      (rename-symbol-in-sexpr sub from to))
                    sexpr))
           (t
            sexpr)))

       (rename-symbol-in-expr (expr from to)
         (expr-ir:sexpr->expr-ir
          (rename-symbol-in-sexpr
           (expr-ir:expr->sexpr expr)
           from to)))

       (rename-symbol-in-stmt (cycle from to)
         (etypecase cycle
           (anchored-assignment-statement
            (let* ((tgt      (stmt-target-name cycle))
                   (idxs     (stmt-target-indices cycle))
                   (new-tgt  (if (eq tgt from) to tgt))
                   (new-idxs (and idxs
                                  (mapcar (lambda (e)
                                            (rename-symbol-in-expr e from to))
                                          idxs)))
                   (new-expr (rename-symbol-in-expr
                              (stmt-expression cycle) from to)))
              (unless (eq new-tgt tgt)
                (error "What do we do here - I don't think we want to change the target from ~s to ~s" tgt new-tgt))
              (make-anchored-assignment-stmt new-tgt new-expr new-idxs)))
           (assignment-statement
           (let* ((tgt      (stmt-target-name cycle))
                  (idxs     (stmt-target-indices cycle))
                  (new-tgt  (if (eq tgt from) to tgt))
                  (new-idxs (and idxs
                                 (mapcar (lambda (e)
                                           (rename-symbol-in-expr e from to))
                                         idxs)))
                  (new-expr (rename-symbol-in-expr
                             (stmt-expression cycle) from to)))
              (make-assignment-stmt new-tgt new-expr new-idxs)))
           (raw-c-statement
            (let ((expr (stmt-expression cycle)))
              (if expr
                  (make-raw-c-statement
                   (raw-c-generator cycle)
                   (rename-symbol-in-expr expr from to))
                  cycle)))
           (block-statement
            (make-block-stmt
             (mapcar (lambda (sub)
                       (rename-symbol-in-stmt sub from to))
                     (block-statements cycle))))
           (if-statement
            (let* ((new-cond (rename-symbol-in-expr
                              (if-condition cycle) from to))
                   (then-b  (if-then-block cycle))
                   (else-b  (if-else-block cycle))
                   (new-then (when then-b
                               (make-block-stmt
                                (mapcar (lambda (sub)
                                          (rename-symbol-in-stmt sub from to))
                                        (block-statements then-b)))))
                   (new-else (when else-b
                               (make-block-stmt
                                (mapcar (lambda (sub)
                                          (rename-symbol-in-stmt sub from to))
                                        (block-statements else-b))))))
              (make-if-stmt new-cond new-then new-else)))
           (t
            cycle)))

       (rename-symbols-in-stmt-list (stmts from to)
         (mapcar (lambda (cycle)
                   (rename-symbol-in-stmt cycle from to))
                 stmts))

       (update-env-for-rename (env from to)
         "Adjust ENV so that FROM is now treated as an alias of TO,
and any entries that mapped to FROM now map to TO."
         (maphash (lambda (k v)
                    (when (eq v from)
                      (setf (gethash k env) to)))
                  env)
         ;; FROM now aliases TO; TO should not be an alias.
         (remhash to env)
         (setf (gethash from env) to))

       (symbol-defined-p (sym stmts)
         "Return true if SYM appears as a target-name of any
assignment-statement in STMTS."
         (loop for cycle in stmts
               thereis (and (typep cycle 'assignment-statement)
                            (eq (stmt-target-name cycle) sym))))

       (process-block (blk env &optional inside-if-p)
         "Return (new-block new-env).

INSIDE-IF-P non-NIL means we are processing the body of an IF branch.
In that case, we must not drop trivial copies that may be the only
definitions visible at the join; we keep those assignments instead of
turning them into pure env aliases."
         (let ((new-stmts '()))
           (dolist (cycle (block-statements blk))
             (etypecase cycle
               (anchored-assignment-statement
                (let* ((target   (stmt-target-name cycle))
                       (expr     (stmt-expression cycle))
                       (new-expr (rewrite-expr expr env)))
                  ;; never alias/downgrade anchored targets
                  (remhash target env)
                  (push (make-anchored-assignment-stmt target new-expr
                                                       (stmt-target-indices cycle))
                        new-stmts)))
               (assignment-statement
                (let* ((target   (stmt-target-name cycle))
                       (expr     (stmt-expression cycle))
                       ;; rewrite RHS through current env
                       (new-expr (rewrite-expr expr env))
                       (sexpr    (expr-ir:expr->sexpr new-expr)))
                  (cond
                    ;; Trivial copy: target = sexpr; sexpr is a symbol
                    ((and (symbolp sexpr)
                          (not (eq target sexpr)))
                     (let ((src sexpr)
                           (dst target))
                       (if inside-if-p
                           ;; Inside IF branches: keep the assignment so the
                           ;; definition is explicit at the join. We may
                           ;; still rewrite RHS via env, but we do not drop
                           ;; the copy nor rely on branch-local aliasing
                           ;; after the IF.
                           (progn
                             (remhash dst env)
                             (push (make-assignment-stmt
                                    dst new-expr (stmt-target-indices cycle))
                                   new-stmts))
                           ;; Straight-line case (old behavior): coalesce
                           ;; or alias and drop the copy.
                           (if (symbol-defined-p src new-stmts)
                               (progn
                                 (setf new-stmts
                                       (rename-symbols-in-stmt-list
                                        new-stmts src dst))
                                 (update-env-for-rename env src dst))
                               (setf (gethash dst env)
                                     (find-root src env))))))

                    (t
                     ;; Keep assignment; target now has its own definition
                     (remhash target env)
                     (push (make-assignment-stmt target new-expr
                                                 (stmt-target-indices cycle))
                           new-stmts)))))

               (block-statement
                (multiple-value-bind (sub-block new-env)
                    (process-block cycle env inside-if-p)
                  (setf env new-env)
                  (push sub-block new-stmts)))

               (if-statement
                ;; Rewrite condition through env, and process branches with
                ;; copies of env. We do not merge branch envs back; this
                ;; treats the IF as a barrier for copy-propagation.
                (let* ((cond-expr (rewrite-expr (if-condition cycle) env))
                       (then-env (copy-env-hash-table env))
                      (else-env (copy-env-hash-table env)))
                  (multiple-value-bind (then-block then-env-out)
                      (process-block (if-then-block cycle) then-env t)
                    (declare (ignore then-env-out))
                    (multiple-value-bind (else-block else-env-out)
                        (if (if-else-block cycle)
                            (process-block (if-else-block cycle) else-env t)
                            (values nil else-env))
                      (declare (ignore else-env-out))
                      (push (make-if-stmt cond-expr then-block else-block)
                            new-stmts)))))

              (raw-c-statement
               (let ((expr (stmt-expression cycle)))
                 (if expr
                     (push (make-raw-c-statement
                            (raw-c-generator cycle)
                            (rewrite-expr expr env))
                           new-stmts)
                     (push cycle new-stmts))))

              (t
               (push cycle new-stmts))))
           (values (make-block-stmt (nreverse new-stmts)) env))))
    (multiple-value-bind (new-block env)
        (process-block block (make-hash-table :test #'eq))
      (declare (ignore env))
      new-block)))


(defun factor-sums-optimization (pass-counter block &key (min-uses 2) (min-factors 1) (min-size 4)
                                               (verbose *verbose-optimization*))
  "Return a new BLOCK-STATEMENT where each assignment RHS has had
EXPR-IR:FACTOR-SUM-OF-PRODUCTS applied."
  (declare (ignore pass-counter))
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
               (stmt-target-indices cycle)))
             (raw-c-statement
              (let ((expr (stmt-expression cycle)))
                (if expr
                    (make-raw-c-statement
                     (raw-c-generator cycle)
                     (rewrite-expr expr))
                    cycle)))
             (block-statement
              (make-block-stmt
               (mapcar #'rewrite-stmt (block-statements cycle))))
             (if-statement
              (let* ((new-cond (rewrite-expr (if-condition cycle)))
                     (then-b   (if-then-block cycle))
                     (else-b   (if-else-block cycle))
                     (new-then (when then-b
                                 (make-block-stmt
                                  (mapcar #'rewrite-stmt
                                          (block-statements then-b)))))
                     (new-else (when else-b
                                 (make-block-stmt
                                  (mapcar #'rewrite-stmt
                                          (block-statements else-b))))))
                (make-if-stmt new-cond new-then new-else)))
             (t cycle))))
    (make-block-stmt
     (mapcar #'rewrite-stmt (block-statements block)))))

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






(defun factor-temp-param-products-optimization (pass-counter block &key (min-uses 2)
                                                                     (min-factors 2)
                                                                     (max-factors 3)
                                                                     (verbose *verbose-optimization*))
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
               (and tb (factor-temp-param-products-optimization pass-counter tb
                                                                :min-uses min-uses
                                                                :min-factors min-factors
                                                                :max-factors max-factors
                                                                :verbose verbose))
               (and eb (factor-temp-param-products-optimization pass-counter eb
                                                                :min-uses min-uses
                                                                :min-factors min-factors
                                                                :max-factors max-factors
                                                                :verbose verbose)))))
             (block-statement
              (factor-temp-param-products-optimization pass-counter st
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
             (temp-name   (intern (format nil "CSE_P~D_T~D"
                                          pass-id (1+ start-index))
                                  (symbol-package 'stmt-ir::make-assignment-stmt)))
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
          (verbose-log verbose "~&[FTPP pass ~D] skip combo ~S (min-insert=~D max=~D)~%"
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
                         (stmt-target-indices stmt)))
                       (block-statement
                        (make-block-stmt
                         (mapcar #'rewrite-stmt (block-statements stmt))))
                       (if-statement
                       (let* ((new-cond (rewrite-expr (if-condition stmt)))
                              (then-b  (if-then-block stmt))
                              (else-b  (if-else-block stmt))
                              (new-then (when then-b
                                          (make-block-stmt
                                           (mapcar #'rewrite-stmt
                                                   (block-statements then-b)))))
                              (new-else (when else-b
                                          (make-block-stmt
                                           (mapcar #'rewrite-stmt
                                                   (block-statements else-b))))))
                         (make-if-stmt new-cond new-then new-else)))
                       (t stmt))))
            (let ((new-stmts '()))
              (loop for idx from 0 below n do
                (when (= idx insert-idx)
                  (push temp-assign new-stmts))
                (push (rewrite-stmt (nth idx stmts)) new-stmts))
              (let ((result (make-block-stmt (nreverse new-stmts))))
                (verbose-log verbose "~&[FTPP pass ~D] factored combo ~S into temp ~S~%"
                             pass-id best-combo temp-name)
                result)))))))))


(defun normalize-signs-optimization (pass-counter block &key (verbose *verbose-optimization*))
  "Return a new BLOCK-STATEMENT where each assignment RHS expression
has had EXPR-IR:NORMALIZE-SIGNS-EXPR applied."
  (declare (ignore pass-counter))
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
               (stmt-target-indices cycle)))
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
                         (block-statements cycle))))
               (if-statement
                (let* ((new-cond (rewrite-expr (if-condition cycle)))
                       (then-b  (if-then-block cycle))
                       (else-b  (if-else-block cycle))
                       (new-then (when then-b
                                   (make-block-stmt
                                    (mapcar #'rewrite-stmt
                                            (block-statements then-b)))))
                       (new-else (when else-b
                                   (make-block-stmt
                                    (mapcar #'rewrite-stmt
                                            (block-statements else-b))))))
                  (make-if-stmt new-cond new-then new-else)))
               (t cycle))))
    (make-block-stmt
     (mapcar #'rewrite-stmt (block-statements block)))))


;;; ----------------------------------------------------------------------
;;; Optimization to apply rewrite expressions
;;; ----------------------------------------------------------------------

#+(or)
(defun rewrite-exprs-in-block-optimization (pass-counter block &key (rules expr-ir:*rewrite-rules-basic*) (max-iterations 5))
  "Return a new BLOCK where every expr-ir expression has been rewritten
via REWRITE-EXPR-IR-WITH-RULES using RULES.

BLOCK is a STMT-IR:BLOCK-STATEMENT."
  (labels
      ((rewrite-expr (e)
         (rewrite-expr-ir-with-rules e rules :max-iterations max-iterations))
       (rewrite-stmt (st)
         (etypecase st
           (stmt-ir:anchored-assignment-statement
            (let* ((rhs (stmt-ir:stmt-expression st))
                   (rhs2 (rewrite-expr rhs)))
              (if (eq rhs rhs2)
                  st
                  (stmt-ir:make-anchored-assignment-stmt
                   (stmt-ir:stmt-target-name st)
                   rhs2
                   (stmt-ir:stmt-target-indices st)))))
           (stmt-ir:assignment-statement
            (let* ((rhs (stmt-ir:stmt-expression st))
                   (rhs2 (rewrite-expr rhs)))
              (if (eq rhs rhs2)
                  st
                  (stmt-ir:make-assignment-stmt
                   (stmt-ir:stmt-target-name st)
                   rhs2
                   (stmt-ir:stmt-target-indices st)))))
           (stmt-ir:if-statement
            (let* ((cond      (stmt-ir:if-condition st))
                   (new-cond  (rewrite-expr cond))
                   (then-blk  (stmt-ir:if-then-block st))
                   (else-blk  (stmt-ir:if-else-block st))
                   (new-then  (rewrite-block then-blk))
                   (new-else  (and else-blk (rewrite-block else-blk))))
              (stmt-ir:make-if-stmt new-cond new-then new-else)))
           (stmt-ir:block-statement
            (rewrite-block st))
           (t st)))
       (rewrite-block (blk)
         (let ((new-stmts '()))
           (dolist (st (stmt-ir:block-statements blk)
                       (stmt-ir:make-block-stmt (nreverse new-stmts)))
             (push (rewrite-stmt st) new-stmts)))))
    (rewrite-block block)))


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
              (if trivial-alias
                  (progn
                    (break "Found a trivial-alias")
                    0)
                  (+ (comp-expr rhs)
                     (reduce #'+ (mapcar #'comp-expr idx) :initial-value 0)))))
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




(defun linear-canonicalization-optimization (pass-counter block &key (verbose *verbose-optimization*))
  "Walk BLOCK and linear-canonicalize every expression, to help factoring and CSE.
Only linear subexpressions are changed; non-linear ones are left as-is."
  (declare (ignore pass-counter))
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
            (stmt-target-indices st)))
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
            (mapcar #'rewrite-stmt (block-statements st))))

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



(defun run-optimization (pass-counter opt block &key name (measure #'block-complexity)
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
         (new-block (apply fn pass-counter block (append pos-args kw-args (list :verbose verbose))))
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


(defun run-optimization-pipeline (pass-counter pipeline block
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
                       (run-optimization pass-counter opt current
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

(defun alias-assigned-exprs-optimization (pass-counter block &key (verbose *verbose-optimization*))
  "General aliasing optimization.

For every assignment

  T := <EXPR>

we record an alias mapping <EXPR> -> T, and then in all later statements
in the same control-flow region we rewrite any subexpression equal to <EXPR>
to the variable T.

- Aliases flow forward within a block.
- Aliases are visible inside THEN/ELSE blocks if defined before the IF.
- Aliases defined inside a branch do not leak out to the parent block."
  (declare (ignore pass-counter))
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
                                  (stmt-target-indices st))))
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
           (values (make-block-stmt (nreverse new-stmts)) env))))
    (multiple-value-bind (new-block env-out)
        (process-block block (make-hash-table :test #'equal))
      (declare (ignore env-out))
      new-block)))
