;;;; statement-ir.lisp
;;;; Statement / control-flow IR for C code generation.
;;;; Uses the expression IR defined in :expr-ir.


(in-package :stmt-ir)

;;; ----------------------------------------------------------------------
;;; Base class
;;; ----------------------------------------------------------------------

(defclass statement ()
  ()
  (:documentation
   "Abstract base class for all statements in the C-oriented IR.
    Statements define sequencing and control flow and *contain* expression
    IR nodes from the :expr-ir layer where needed (RHS, conditions, indices)."))

;;; ----------------------------------------------------------------------
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

(defclass assignment-statement (statement)
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
     NIL means scalar variable (no indices).")
   (expression
    :initarg :expression
    :accessor stmt-expression
    :documentation
    "Right-hand side expression (expression IR node)."))
  (:documentation
   "Assignment: target = expression;
    The actual C LHS rendering is defined in the code generator using
    TARGET-NAME and TARGET-INDICES."))

(defmethod print-object ((obj assignment-statement) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~s := ~s" (stmt-target-name obj) (stmt-expression obj))))

(defun make-assignment-stmt (target-name expression &optional target-indices)
  "Convenience constructor for an assignment-statement."
  (make-instance 'assignment-statement
                 :target-name target-name
                 :target-indices target-indices
                 :expression expression))

;;; ----------------------------------------------------------------------
;;; Raw C statement
;;; ----------------------------------------------------------------------
;;; For things that are easier to just spell in C:
;;;   - declarations
;;;   - pragma
;;;   - #include (if you choose to allow it here)
;;;   - hand-written snippets

(defclass raw-c-statement (statement)
  ((text
    :initarg :text
    :accessor raw-c-text
    :type string
    :documentation
    "Literal C code for this statement. Emitted as-is (with a newline
     or semicolon added by the emitter as appropriate)."))
  (:documentation
   "Opaque raw C statement. Used when code is easier to write directly
    than to express through the structured IR."))

(defmethod print-object ((obj raw-c-statement) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a" (raw-c-text obj))))

(defun make-raw-c-statement (text)
  "Convenience constructor for raw-c-statement."
  (make-instance 'raw-c-statement :text text))

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
  "Convenience constructor for block-statement."
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
    "Function body as a block-statement."))
  (:documentation
   "Represents a complete C function ready for emission."))

(defun make-c-function (name body
                        &key
                          (return-type "void")
                          (parameters nil)
                          (locals nil))
  "Convenience constructor for c-function."
  (make-instance 'c-function
                 :name name
                 :return-type return-type
                 :parameters parameters
                 :locals locals
                 :body body))



;;; ----------------------------------------------------------------------
;;; simplify
;;; ----------------------------------------------------------------------

(defun simplify-statement (stmt)
  "Return a simplified copy of STMT, with all expression IR children
passed through EXPR-IR:SIMPLIFY-EXPR."
  (typecase stmt
    ;; Assignment: simplify RHS and indices
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


;;; ------------------------------------------------------------
;;; Statement / block emitters
;;; ------------------------------------------------------------

(defun emit-statement-c (stmt indent stream)
  "Emit one statement to STREAM at INDENT (indent = logical level)."
  (typecase stmt
    (assignment-statement
     (let* ((target (stmt-target-name stmt))
            (expr   (stmt-expression stmt))
            (tname  (%c-ident target))
            (rhs    (expr-ir:expr->c-expr-string expr)))
       (%indent indent stream)
       (format stream "~A = ~A;~%" tname rhs)))

    (raw-c-statement
     (let ((code (raw-c-text stmt)))
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
  (dolist (st (block-statements block))
    (emit-statement-c st indent stream)))


;;; ------------------------------------------------------------
;;; Function emitter
;;; ------------------------------------------------------------

(defun emit-c-parameter-list (params stream)
  "PARAMS is a list of (ctype name)."
  (loop
    for (ctype pname) in params
    for i from 0
    do (progn
         (when (> i 0) (format stream ", "))
         (format stream "~A ~A"
                 ctype
                 (%c-ident pname)))))


(defun emit-c-locals (locals indent stream)
  "LOCALS is a list of (ctype name)."
  (dolist (local locals)
    (destructuring-bind (ctype lname) local
      (%indent indent stream)
      (format stream "~A ~A;~%" ctype (%c-ident lname)))))


(defun c-function->c-source-string (cfun)
  "Render a STMT-IR:C-FUNCTION as a complete C function definition string."
  (unless (typep cfun 'c-function)
    (error "c-function->c-source-string: expected C-FUNCTION, got ~S" cfun))
  (with-output-to-string (s)
    (let* ((fname   (%c-ident (c-function-name cfun)))
           (rettype (c-function-return-type cfun))
           (params  (c-function-parameters cfun))
           (locals  (c-function-locals cfun))
           (body    (c-function-body cfun)))
      ;; Function header
      (format s "~A ~A(" rettype fname)
      (emit-c-parameter-list params s)
      (format s ")~%{~%")
      ;; Locals
      (when locals
        (emit-c-locals locals 1 s)
        (terpri s))
      ;; Body
      (emit-block-c body 1 s)
      (format s "}~%"))))


;;; ------------------------------------------------------------
;;; Common subexpression elimination (CSE) on stmt-ir blocks
;;; ------------------------------------------------------------

(defun %sexpr-size (sexpr)
  "Crude size metric (number of nodes) for a sexpr tree."
  (if (atom sexpr)
      1
      (1+ (reduce #'+ (mapcar #'%sexpr-size sexpr)))))

(defun %collect-subexpr-counts (sexpr table)
  "Increment counts of all subexpressions of SEXP in TABLE."
  (incf (gethash sexpr table 0))
  (when (consp sexpr)
    (dolist (sub sexpr)
      (%collect-subexpr-counts sub table)))
  table)

(defun %cse-collect-block-subexprs (block table)
  "Populate TABLE (hash-table) with counts of all subexpressions
in the RHS of assignment statements in BLOCK (recursively)."
  (dolist (st (block-statements block))
    (typecase st
      (assignment-statement
       (let* ((expr  (stmt-expression st))
              (sexpr (expr-ir:expr->sexpr expr)))
         (%collect-subexpr-counts sexpr table)))
      (block-statement
       (%cse-collect-block-subexprs st table))
      (if-statement
       (%cse-collect-block-subexprs (if-then-block st) table)
       (when (if-else-block st)
         (%cse-collect-block-subexprs (if-else-block st) table)))))
  table)

(defun cse-block (block &key (min-uses 2) (min-size 5))
  "Perform a simple common subexpression elimination on BLOCK.

We:
  - scan all assignment RHS expressions,
  - count all subexpressions in sexpr form,
  - for any subexpr used at least MIN-USES times and of size >= MIN-SIZE,
    introduce a temporary scalar assigned to that subexpr,
  - rewrite all RHS expressions to use the temporaries.

New temporaries are introduced as assignment statements at the top of
the block; you must ensure they are declared as locals in the C function."
  (unless (typep block 'block-statement)
    (error "cse-block: expected BLOCK-STATEMENT, got ~S" block))
  ;; 1. Count all subexpressions.
  (let ((counts (make-hash-table :test #'equal)))
    (%cse-collect-block-subexprs block counts)
    ;; 2. Choose candidate subexpressions.
    (let ((candidates '()))
      (maphash (lambda (sexpr count)
                 (when (and (consp sexpr)
                            (>= count min-uses)
                            (>= (%sexpr-size sexpr) min-size))
                   (push sexpr candidates)))
               counts)
      (when (null candidates)
        ;; Nothing to do – return block unchanged.
        (return-from cse-block block))
      ;; 3. Assign temporaries to candidates.
      (let* ((cand->temp (make-hash-table :test #'equal))
             (temp-assignments '()))
        (dolist (sexpr candidates)
          (let ((temp (gensym "CSE_T")))
            (setf (gethash sexpr cand->temp) temp)
            ;; temp = sexpr;
            (push (make-assignment-stmt temp (expr-ir:sexpr->expr-ir sexpr))
                  temp-assignments)))
        (setf temp-assignments (nreverse temp-assignments))
        ;; 4. Rewriting sexprs to use temporaries.
        (labels ((rewrite-sexpr (sexpr)
                   (let ((temp (gethash sexpr cand->temp)))
                     (cond
                       (temp
                        ;; Replace the entire subexpr with the temp symbol.
                        temp)
                       ((consp sexpr)
                        (mapcar #'rewrite-sexpr sexpr))
                       (t
                        sexpr))))
                 (rewrite-expr (expr)
                   (let* ((sexpr    (expr-ir:expr->sexpr expr))
                          (new-sexpr (rewrite-sexpr sexpr)))
                     (expr-ir:sexpr->expr-ir new-sexpr)))
                 (rewrite-stmt (st)
                   (typecase st
                     (assignment-statement
                      (make-assignment-stmt (stmt-target-name st) (rewrite-expr (stmt-expression st))))
                     (block-statement
                      (make-block-stmt
                       (mapcar #'rewrite-stmt (block-statements st))))
                     (if-statement
                      (make-if-stmt
                       (rewrite-expr (if-condition st))
                       (rewrite-stmt (if-then-block st))
                       (and (if-else-block st)
                            (rewrite-stmt (if-else-block st)))))
                     (t
                      st))))
          ;; 5. Build new block with temp assignments at the top.
          (make-block-stmt
           (append temp-assignments
                   (mapcar #'rewrite-stmt (block-statements block)))))))))


(defun collect-scalar-targets-in-block (block)
  "Return a list of all assignment target symbols in BLOCK (recursively)."
  (labels ((rec (blk acc)
             (dolist (st (stmt-ir:block-statements blk) acc)
               (setf acc
                     (typecase st
                       (stmt-ir:assignment-statement
                        (let ((name (stmt-ir:stmt-target-name st)))
                          (pushnew name acc :test #'eq)))
                       (stmt-ir:block-statement
                        (rec st acc))
                       (stmt-ir:if-statement
                        (let ((acc2 (rec (stmt-ir:if-then-block st) acc)))
                          (if (stmt-ir:if-else-block st)
                              (rec (stmt-ir:if-else-block st) acc2)
                              acc2)))
                       (t
                        acc))))))
    (rec block '())))

(defun cse-block-multi (block
                        &key
                          (max-passes 5)
                          (min-uses 2)
                          (min-size 5))
  "Apply CSE repeatedly to BLOCK up to MAX-PASSES times, or until
  a pass makes no changes.

Returns the (possibly transformed) BLOCK.

MAX-PASSES: upper limit on the number of CSE passes.
MIN-USES, MIN-SIZE: forwarded to CSE-BLOCK."
  (unless (typep block 'block-statement)
    (error "cse-block-multi: expected BLOCK-STATEMENT, got ~S" block))
  (let ((current block))
    (loop for pass from 1 to max-passes do
          (let* ((before (collect-scalar-targets-in-block current))
                 (next   (cse-block current
                                    :min-uses min-uses
                                    :min-size min-size))
                 (after  (collect-scalar-targets-in-block next)))
            ;; Heuristic: if the set of scalar targets is unchanged and
            ;; the block object is EQ to the previous, assume no new temps.
            (when (and (eq next current)
                       (equal before after))
              (return current))
            (setf current next)))
    current))

(in-package :stmt-ir)

;;; ------------------------------------------------------------
;;; Copy propagation / dead trivial copies on blocks
;;; ------------------------------------------------------------

(defun copy-propagate-block (block)
  "Eliminate trivial copies in BLOCK of the form:
     t = u;
   where the RHS is just a variable (symbol).

We:
  - maintain an env mapping symbols to their 'root' representative,
  - rewrite all RHS expressions using this env,
  - drop copy assignments (t = u) and keep only defining assignments
    with nontrivial RHS."

  (labels ((find-root (sym env)
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
             (let* ((sexpr    (expr-ir:expr->sexpr expr))
                    (new-sexpr (rewrite-sexpr sexpr env)))
               (expr-ir:sexpr->expr-ir new-sexpr)))

           (process-block (blk env)
             "Return (new-block new-env)."
             (let ((new-stmts '()))
               (dolist (st (block-statements blk))
                 (typecase st
                   (assignment-statement
                    (let* ((target (stmt-target-name st))
                           (expr   (stmt-expression st))
                           ;; rewrite RHS through current env
                           (new-expr (rewrite-expr expr env))
                           (sexpr    (expr-ir:expr->sexpr new-expr)))
                      (cond
                        ;; Trivial copy: t = u;  (drop assignment, extend env)
                        ((and (symbolp sexpr)
                              (not (eq target sexpr)))
                         (setf (gethash target env)
                               (find-root sexpr env)))
                        (t
                         ;; Keep assignment; target now has its own definition
                         (remhash target env)
                         (push (make-assignment-stmt target new-expr)
                               new-stmts)))))

                   (block-statement
                    (multiple-value-bind (sub-block new-env)
                        (process-block st env)
                      (setf env new-env)
                      (push sub-block new-stmts)))

                   (if-statement
                    ;; We rewrite condition through env, and process branches with
                    ;; copies of env (we do not try to merge branch envs back).
                    (let* ((cond-expr (rewrite-expr (if-condition st) env))
                           (then-env (copy-env-hash-table env))
                           (else-env (copy-env-hash-table env)))
                      (multiple-value-bind (then-block then-env-out)
                          (process-block (if-then-block st) then-env)
                        (declare (ignore then-env-out))
                        (multiple-value-bind (else-block else-env-out)
                            (if (if-else-block st)
                                (process-block (if-else-block st) else-env)
                                (values nil else-env))
                          (declare (ignore else-env-out))
                          (push (make-if-stmt cond-expr then-block else-block)
                                new-stmts)))))

                   (t
                    (push st new-stmts))))
               (values (make-block-stmt (nreverse new-stmts)) env))))
    (multiple-value-bind (new-block env)
        (process-block block (make-hash-table :test #'eq))
      (declare (ignore env))
      new-block)))
  
