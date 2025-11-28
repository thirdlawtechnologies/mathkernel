;;;; ----------------------------------------------------------------------
;;;; statement-ir.lisp
;;;; ----------------------------------------------------------------------
;;;; Statement / control-flow IR for C code generation.
;;;; Uses the expression IR defined in :expr-ir.


(in-package :stmt-ir)

(defparameter *factor-temp-param-debug* nil)

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
  (let ((target-name (expr-ir:ev target-name)))
    (make-instance 'assignment-statement
                   :target-name target-name
                   :target-indices target-indices
                   :expression expression)))

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
;;; Debugging assistance
;;; ------------------------------------------------------------

(defparameter *debug* nil
  "When true, extra consistency checks (e.g. assignment ordering) are
performed when building kernels.")



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
    (typecase stmt
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

      (accumulation-anchor-statement
       (format stream "ACCUMULATE-HERE;~%"))

      (assignment-statement
       (let ((target  (stmt-target-name stmt))
             (indices (stmt-target-indices stmt))
             (expr    (stmt-expression stmt)))
         (indent!)
         (if indices
             (format stream "~A~S := ~S~%"
                     target indices (expr-ir:expr->sexpr expr))
             (format stream "~A := ~S~%"
                     target (expr-ir:expr->sexpr expr)))))

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
                 (typecase cycle
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
                 (typecase cycle
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
       (let ((*package* (find-package :expr-var))
             (*print-pretty* nil))
         (%indent indent stream)
         (format stream "// ~a = ~a;~%" target (expr-ir:expr->sexpr expr)))
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

    (accumulation-anchor-statement
     ;; Do nothing
     nil)

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

(defun sexpr-size (sexpr)
  "Crude size metric (number of nodes) for a sexpr tree."
  (if (atom sexpr)
      1
      (1+ (reduce #'+ (mapcar #'sexpr-size sexpr)))))


(defun cse-block (block &key (min-uses 2) (min-size 5) (pass-id 1))
  "Perform one CSE pass on BLOCK (a BLOCK-STATEMENT).

Temps created in this pass are named CSE_P<pass-id>_T<n>. We ensure
each temp is defined AFTER all CSE temps it uses and BEFORE its first use
in this block."
  (unless (typep block 'block-statement)
    (error "cse-block: expected BLOCK-STATEMENT, got ~S" block))

  (let* ((stmts (block-statements block))
         (n     (length stmts)))

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
        (loop for cycle in stmts
              for idx from 0 do
                (typecase cycle
                  (assignment-statement
                   (accum-expr (stmt-expression cycle) idx))
                  (if-statement
                   ;; condition only; we don't try to share across branches here
                   (accum-expr (if-condition cycle) idx))
                  (block-statement
                   ;; you can recurse here if you want intra-subblock CSE
                   nil)
                  (t nil))))

      ;; ------------------------------------------------------------
      ;; Phase 2: collect definition indices (for temps we depend on)
      ;; ------------------------------------------------------------
      (let ((def-index (make-hash-table :test #'eq)))
        (loop for cycle in stmts
              for idx from 0 do
                (when (typep cycle 'assignment-statement)
                  (setf (gethash (stmt-target-name cycle) def-index) idx)))

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
            ;; no CSE change
            (return-from cse-block block))

          ;; ------------------------------------------------------------
          ;; Phase 4: build temps, compute insertion indices
          ;; ------------------------------------------------------------
          (let ((sexpr->temp (make-hash-table :test #'equal))
                (temp-insert (make-hash-table :test #'eq))
                (temp-counter (next-cse-temp-index-for-pass block pass-id)))
            (labels
                ((temp-name ()
                   (incf temp-counter)
                   (expr-ir:ev (format nil "CSE_P~D_T~D" pass-id temp-counter)))
                 (temps-used-in-sexpr (sexpr)
                   (remove-if-not #'cse-temp-symbol-p
                                  (expr-ir:expr-free-vars
                                   (expr-ir:sexpr->expr-ir sexpr)))))

              ;; For each candidate sexpr, decide temp symbol & insertion index.
              (dolist (sexpr candidates)
                (let* ((temp (temp-name))
                       (uses    (temps-used-in-sexpr sexpr))
                       (first   (gethash sexpr first-use)) ; earliest use index of this sexpr
                       (dep-max (if uses
                                    (loop for u in uses
                                          for di = (gethash u def-index nil)
                                          when di maximize di)
                                    -1))
                       ;; start by putting it after all CSE temps it depends on
                       (insert-idx (max (1+ dep-max) 0)))
                  ;; defensively clamp to be no later than first use, if we know it
                  (when (and first (> insert-idx first))
                    (setf insert-idx first))
                  (setf (gethash sexpr sexpr->temp) temp)
                  (setf (gethash temp  temp-insert) insert-idx)))

              ;; --------------------------------------------------------
              ;; Phase 5: rewrite original statements to use temps
              ;; --------------------------------------------------------
              (labels ((rewrite-sexpr (sexpr)
                         (or (gethash sexpr sexpr->temp)
                             (if (consp sexpr)
                                 (cons (car sexpr)
                                       (mapcar #'rewrite-sexpr (cdr sexpr)))
                                 sexpr)))
                       (rewrite-expr (expr)
                         (expr-ir:sexpr->expr-ir
                          (rewrite-sexpr (expr-ir:expr->sexpr expr))))
                       (rewrite-stmt (cycle)
                         (typecase cycle
                           (assignment-statement
                            (make-assignment-stmt
                             (stmt-target-name cycle)
                             (rewrite-expr (stmt-expression cycle))))
                           (if-statement
                            (make-if-stmt
                             (rewrite-expr (if-condition cycle))
                             (cse-block (if-then-block cycle)
                                        :min-uses min-uses
                                        :min-size min-size
                                        :pass-id pass-id)
                             (when (if-else-block cycle)
                               (cse-block (if-else-block cycle)
                                          :min-uses min-uses
                                          :min-size min-size
                                          :pass-id pass-id))))
                           (block-statement
                            (cse-block cycle
                                       :min-uses min-uses
                                       :min-size min-size
                                       :pass-id pass-id))
                           (t cycle))))

                (let* ((rewritten-stmts
                         (loop for cycle in stmts collect (rewrite-stmt cycle)))
                       ;; temp assignments bucketed by insertion index
                       (insert-buckets (make-array (1+ n)
                                                   :initial-element nil)))
                  ;; build temp assignments and bucket them
                  (maphash
                   (lambda (sexpr temp)
                     (let* ((expr (expr-ir:sexpr->expr-ir sexpr))
                            (assign (make-assignment-stmt temp expr))
                            (idx (gethash temp temp-insert 0)))
                       (push assign (aref insert-buckets idx))))
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
                    (make-block-stmt (nreverse new-stmts))))))))))))




(defun collect-scalar-targets-in-block (block)
  "Return a list of all assignment target symbols in BLOCK (recursively)."
  (labels ((rec (blk acc)
             (dolist (cycle (stmt-ir:block-statements blk) acc)
               (setf acc
                     (typecase cycle
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
                 (typecase cycle
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
                      (scan-block (if-else-block cycle))))))))
      (scan-block block))
    max-index))

(defun cse-factor-products-in-block
    (block &key (min-uses 2) (min-factors 2) (min-size 5) (pass-id 1) (cse-only nil))
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

If no useful candidate is found, return BLOCK unchanged (EQ)."
  (declare (optimize (debug 3)))
  (unless (typep block 'block-statement)
    (error "cse-factor-products-in-block: expected BLOCK-STATEMENT, got ~S" block))

  (let* ((stmts (block-statements block))
         (n     (length stmts)))
    ;; Collect definition indices for symbols defined in this block,
    ;; so we can place temps after dependencies.
    (let ((def-index (make-hash-table :test #'eq)))
      (loop for cycle in stmts
            for idx from 0 do
              (when (typep cycle 'assignment-statement)
                (setf (gethash (stmt-target-name cycle) def-index) idx)))

      ;; Collect all product subexpressions and their factor multisets.
      (let ((products '()))
        (labels ((collect-sexpr (sexpr idx)
                   (when (and (consp sexpr)
                              (eq (car sexpr) '*))
                     (let ((factors (cdr sexpr)))
                       ;; Ignore trivial products with too few factors.
                       (when (>= (length factors) min-factors)
                         (let ((counts (make-hash-table :test #'equal)))
                           (dolist (f factors)
                             (incf (gethash f counts 0)))
                           (push (list :factors factors
                                       :counts  counts
                                       :stmt-index idx)
                                 products))))
                     ;; Recurse into children
                     (when (consp sexpr)
                       (dolist (arg (cdr sexpr))
                         (collect-sexpr arg idx)))))
                 (collect-expr (expr idx)
                   (collect-sexpr (expr-ir:expr->sexpr expr) idx))
                 (collect-stmt (cycle idx)
                   (typecase cycle
                     (assignment-statement
                      (collect-expr (stmt-expression cycle) idx))
                     (if-statement
                      (collect-expr (if-condition cycle) idx)
                      (let ((tb (if-then-block cycle))
                            (eb (if-else-block cycle)))
                        (when tb
                          (dolist (sub (block-statements tb))
                            (collect-stmt sub idx)))
                        (when eb
                          (dolist (sub (block-statements eb))
                            (collect-stmt sub idx)))))
                     (block-statement
                      (dolist (sub (block-statements cycle))
                        (collect-stmt sub idx)))
                     (t nil))))
          (loop for cycle in stmts
                for idx from 0 do
                  (collect-stmt cycle idx))

          (setf products (nreverse products))

          (when (< (length products) 2)
            ;; Nothing to factor.
            (return-from cse-factor-products-in-block block))

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
                         (ci (getf ppi :counts)))
                    (loop for j from (1+ i) below m do
                      (let* ((pj (aref plist j))
                             (cj (getf pj :counts))
                             (common (multiset-intersection ci cj)))
                        (when (and (>= (length common) min-factors)
                                   (or (not cse-only)
                                       (every #'cse-temp-symbol-p common)))
                          (push common candidate-lists)))))))
              (setf candidate-lists
                    (remove-duplicates candidate-lists :test #'equal))

              (when (null candidate-lists)
                (return-from cse-factor-products-in-block block))

              ;; Score candidates and pick the best.
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
                    ;; Skip very small candidates early.
                    (when (>= size min-size)
                      (dolist (prod products)
                        (let ((pc (getf prod :counts)))
                          (when (multiset-subset-p cand-counts pc)
                            (incf uses)
                            (let ((idx (getf prod :stmt-index)))
                              (setf first-use
                                    (if first-use
                                        (min first-use idx)
                                        idx))))))
                      (when (>= uses min-uses)
                        (let ((score (* (- uses 1) size)))
                          (when (> score best-score)
                            (setf best-score     score
                                  best-cand      cand
                                  best-uses      uses
                                  best-first-use first-use)))))))

                (when (or (null best-cand)
                          (< best-uses min-uses)
                          (<= best-score 0))
                  ;; No beneficial factoring.
                  (return-from cse-factor-products-in-block block))

                ;; We have a chosen candidate BEST-CAND.
                (let* ((start-index (next-cse-temp-index-for-pass block pass-id))
                       (temp
                         (intern (format nil "CSE_P~D_T~D"
                                         pass-id (1+ start-index))
                                 (symbol-package 'stmt-ir::cse-t)))
                       (cand-counts (counts-from-factors best-cand))
                       (temp-expr   (expr-ir:sexpr->expr-ir
                                     (cons '* best-cand)))
                       (temps-used  (remove-if-not #'cse-temp-symbol-p
                                                   (expr-ir:expr-free-vars temp-expr)))
                       (dep-max
                         (if temps-used
                             (loop for u in temps-used
                                   for di = (gethash u def-index nil)
                                   when di maximize di)
                             -1))
                       ;; Start by placing temp after all temps it depends on.
                       (insert-idx (max (1+ dep-max) 0)))
                  ;; Clamp insertion to be no later than the first use.
                  (when (and best-first-use (> insert-idx best-first-use))
                    (setf insert-idx best-first-use))
                  (when (> insert-idx n)
                    (setf insert-idx n))
                  ;; Build temp assignment.
                  (let ((temp-assign (make-assignment-stmt temp temp-expr)))
                    ;; Rewrite expressions to use TEMP for BEST-CAND.
                    (labels
                        ((rewrite-sexpr (sexpr)
                           (let ((result (cond
                                           ;; Multiplication: factor out BEST-CAND if present.
                                           ((and (consp sexpr)
                                                 (eq (car sexpr) '*))
                                            (let* ((orig-factors (cdr sexpr))
                                                   (prod-counts (counts-from-factors
                                                                 orig-factors)))
                                              (if (multiset-subset-p cand-counts prod-counts)
                                                  ;; Build remaining factors = prod-counts - cand-counts.
                                                  (let ((remaining '()))
                                                    (maphash
                                                     (lambda (k vc)
                                                       (let* ((cc (gethash k cand-counts 0))
                                                              (rc (- vc cc)))
                                                         (when (> rc 0)
                                                           (dotimes (i rc)
                                                             (push k remaining)))))
                                                     prod-counts)
                                                    ;; Recurse into remaining factors before
                                                    ;; rebuilding the product.
                                                    (let ((new-remaining
                                                            (mapcar #'rewrite-sexpr remaining)))
                                                      (cond
                                                        ((null new-remaining)
                                                         temp)
                                                        (t
                                                         (cons '* (cons temp new-remaining))))))
                                                    ;; No full candidate here; recurse normally.
                                                  (cons '* (mapcar #'rewrite-sexpr orig-factors)))))
                                           ((consp sexpr)
                                            (cons (car sexpr)
                                                  (mapcar #'rewrite-sexpr (cdr sexpr))))
                                           (t
                                            sexpr))))
                             result))
                         (rewrite-expr (expr)
                           (let* ((sexpr (expr-ir:expr->sexpr expr))
                                  (rsexpr (rewrite-sexpr sexpr)))
                             (expr-ir:sexpr->expr-ir rsexpr)))
                         (rewrite-stmt (cycle)
                           (typecase cycle
                             (assignment-statement
                              (make-assignment-stmt
                               (stmt-target-name cycle)
                               (rewrite-expr (stmt-expression cycle))))
                             (if-statement
                              (make-if-stmt
                               (rewrite-expr (if-condition cycle))
                               (let ((tb (if-then-block cycle)))
                                 (when tb
                                   (make-block-stmt
                                    (mapcar #'rewrite-stmt
                                            (block-statements tb)))))
                               (let ((eb (if-else-block cycle)))
                                 (when eb
                                   (make-block-stmt
                                    (mapcar #'rewrite-stmt
                                            (block-statements eb)))))))
                             (block-statement
                              (make-block-stmt
                               (mapcar #'rewrite-stmt
                                       (block-statements cycle))))
                             (t cycle))))
                      (let ((new-stmts '()))
                        (loop for idx from 0 below n do
                          (when (= idx insert-idx)
                            (push temp-assign new-stmts))
                          (push (rewrite-stmt (nth idx stmts)) new-stmts))
                        ;; If insertion index is at the end, ensure we append.
                        (when (= insert-idx n)
                          (push temp-assign new-stmts))
                        (make-block-stmt (nreverse new-stmts))))))))))))))


(defun cse-block-multi-optimization (counter block
                                     &key
                                       (max-passes 5)
                                       (min-uses 2)
                                       (min-size 5))
  "Apply CSE repeatedly to BLOCK up to MAX-PASSES times, or until
a pass makes no changes.

Each pass uses a distinct temp namespace: CSE_P<pass>_T<n>."
  (unless (typep block 'block-statement)
    (error "cse-block-multi: expected BLOCK-STATEMENT, got ~S" block))
  (let ((current-block block))
    (loop for ii from 1 upto max-passes do
      (let* ((pass-id (next-pass-id counter))
             (before-symbol-list
               (collect-scalar-targets-in-block current-block))
             ;; 1. Standard subtree CSE
             (after-cse
               (cse-block current-block
                          :min-uses min-uses
                          :min-size min-size
                          :pass-id  pass-id))
             ;; 2. Generic product factoring
             (after-prod
               (cse-factor-products-in-block
                after-cse
                :min-uses   min-uses
                :min-factors 2
                :min-size   min-size
                :pass-id    pass-id))
             ;; 3. Extra pass: only factor products made entirely of CSE temps,
             ;;    and allow small products like CSE_P*_T* * CSE_P*_T* * CSE_P*_T*.
             (next-block
               (cse-factor-products-in-block
                after-prod
                :min-uses    2
                :min-factors 3 ; require at least 3 factors, like your example
                :min-size    1 ; allow smallish expressions
                :pass-id     pass-id
                :cse-only    t))
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
        (when (and (eq next-block current-block)
                   (equal before-symbol-list after-symbol-list))
          (return current-block))
        (setf current-block next-block)))
    current-block))


(defun cse-block-multi (block &key (max-passes 5) (min-uses 2) (min-size 5))
  "Compatibility wrapper that runs CSE with a fresh, local pass-id counter."
  (Warn "cse-block-multi is a thin wrapper for debugging")
  (let ((counter (make-pass-id-counter)))
    (cse-block-multi-optimization counter block
                                               :max-passes max-passes
                                               :min-uses   min-uses
                                               :min-size   min-size)))



;;; ------------------------------------------------------------
;;; Copy propagation / dead trivial copies on blocks
;;; ------------------------------------------------------------

(defun copy-propagate-optimization (pass-counter block)
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
         (typecase cycle
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
             (typecase cycle
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

               (t
                (push cycle new-stmts))))
           (values (make-block-stmt (nreverse new-stmts)) env))))
    (multiple-value-bind (new-block env)
        (process-block block (make-hash-table :test #'eq))
      (declare (ignore env))
      new-block)))

#+(or)
(defun copy-propagate-optimization (pass-counter block)
  "Eliminate trivial copies in BLOCK of the form:
     t = u;
   where the RHS is just a variable (symbol).

We:
  - maintain an env mapping symbols to their 'root' representative,
  - rewrite all RHS expressions using this env,
  - for trivial copies t = u:
      * if we have already seen a defining assignment for u, coalesce
        u into t by rewriting previous statements and env, and drop
        the copy;
      * otherwise, drop the copy and treat t as an alias of u
        (old behavior)."
  (declare (ignore pass-counter))
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
         (typecase cycle
           (assignment-statement
            (let* ((tgt        (stmt-target-name cycle))
                   (idxs       (stmt-target-indices cycle))
                   (new-tgt    (if (eq tgt from) to tgt))
                   (new-idxs   (and idxs
                                    (mapcar (lambda (e)
                                              (rename-symbol-in-expr e from to))
                                            idxs)))
                   (new-expr   (rename-symbol-in-expr
                                (stmt-expression cycle) from to)))
              (make-assignment-stmt new-tgt new-expr new-idxs)))
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

       (process-block (blk env)
         "Return (new-block new-env)."
         (let ((new-stmts '()))
           (dolist (cycle (block-statements blk))
             (typecase cycle
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
                       (if (symbol-defined-p src new-stmts)
                           ;; Coalesce src into dst:
                           ;;   - change previous definition(s) and uses of SRC
                           ;;     to use DST,
                           ;;   - treat SRC as alias of DST,
                           ;;   - drop the copy assignment.
                           (progn
                             (setf new-stmts
                                   (rename-symbols-in-stmt-list
                                    new-stmts src dst))
                             (update-env-for-rename env src dst))
                           ;; No prior definition of SRC in this block:
                           ;; fall back to the original behavior and treat
                           ;; DST as an alias of SRC.
                           (setf (gethash dst env)
                                 (find-root src env)))))

                    (t
                     ;; Keep assignment; target now has its own definition
                     (remhash target env)
                     (push (make-assignment-stmt target new-expr
                                                 (stmt-target-indices cycle))
                           new-stmts)))))

               (block-statement
                (multiple-value-bind (sub-block new-env)
                    (process-block cycle env)
                  (setf env new-env)
                  (push sub-block new-stmts)))

               (if-statement
                ;; Rewrite condition through env, and process branches with
                ;; copies of env (we do not try to merge branch envs back).
                (let* ((cond-expr (rewrite-expr (if-condition cycle) env))
                       (then-env (copy-env-hash-table env))
                       (else-env (copy-env-hash-table env)))
                  (multiple-value-bind (then-block then-env-out)
                      (process-block (if-then-block cycle) then-env)
                    (declare (ignore then-env-out))
                    (multiple-value-bind (else-block else-env-out)
                        (if (if-else-block cycle)
                            (process-block (if-else-block cycle) else-env)
                            (values nil else-env))
                      (declare (ignore else-env-out))
                      (push (make-if-stmt cond-expr then-block else-block)
                            new-stmts)))))

               (t
                (push cycle new-stmts))))
           (values (make-block-stmt (nreverse new-stmts)) env))))
    (multiple-value-bind (new-block env)
        (process-block block (make-hash-table :test #'eq))
      (declare (ignore env))
      new-block)))



(defun factor-sums-optimization (pass-counter block &key (min-uses 2) (min-factors 1) (min-size 4))
  "Return a new BLOCK-STATEMENT where each assignment RHS has had
EXPR-IR:FACTOR-SUM-OF-PRODUCTS applied."
  (declare (ignore pass-counter))
  (labels ((rewrite-expr (expr)
             (expr-ir:factor-sum-of-products expr
                                             :min-uses min-uses
                                             :min-factors min-factors
                                             :min-size min-size))
           (rewrite-stmt (cycle)
             (typecase cycle
               (assignment-statement
                (make-assignment-stmt
                 (stmt-target-name cycle)
                 (rewrite-expr (stmt-expression cycle))
                 (stmt-target-indices cycle)))
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
                                                  (max-factors 3))
  (unless (typep block 'block-statement)
    (error "factor-temp-param-products-optimization: expected BLOCK-STATEMENT, got ~S"
           block))
  (let* ((stmts      (block-statements block))
         (n          (length stmts))
         (candidates (make-hash-table :test #'equal)))
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
           (typecase stmt
             (assignment-statement
              (collect-from-expr (stmt-expression stmt) stmt-idx))
             (block-statement
              (dolist (sub (block-statements stmt))
                (collect-from-stmt sub stmt-idx)))
             (if-statement
              (collect-from-expr (if-condition stmt) stmt-idx)
              (let ((then-b (if-then-block stmt))
                    (else-b (if-else-block stmt)))
                (when then-b
                  (dolist (sub (block-statements then-b))
                    (collect-from-stmt sub stmt-idx)))
                (when else-b
                  (dolist (sub (block-statements else-b))
                    (collect-from-stmt sub stmt-idx)))))
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
          (best-first-idx  nil))
      (maphash
       (lambda (combo info)
         (let ((count      (getf info :count))
               (first-idx  (getf info :first-index)))
           (when (>= count min-uses)
             (let* ((len   (length combo))
                    ;; simple score: (uses-1) * length
                    (score (* (- count 1) len)))
               (when (and (>= len min-factors)
                          (> score best-score))
                 (setf best-score     score
                       best-combo     combo
                       best-count     count
                       best-first-idx first-idx))))))
       candidates)
      (when *factor-temp-param-debug*
        (format *trace-output* "~&[FTPP] best-combo=~S count=~D score=~D~%"
                best-combo best-count best-score))
      (when (or (null best-combo)
                (< best-count min-uses)
                (<= best-score 0))
        ;; No beneficial factoring.
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
             (temp-assign (make-assignment-stmt temp-name temp-expr))
             (insert-idx  (or best-first-idx 0)))
        (when *factor-temp-param-debug*
          (format *trace-output* "~&[FTPP] introducing temp ~S = ~S at index ~D~%"
                  temp-name temp-sexpr insert-idx))
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
                   (typecase stmt
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
            (make-block-stmt (nreverse new-stmts))))))))


(defun normalize-signs-optimization (pass-counter block)
  "Return a new BLOCK-STATEMENT where each assignment RHS expression
has had EXPR-IR:NORMALIZE-SIGNS-EXPR applied."
  (labels ((rewrite-expr (expr)
             (expr-ir:normalize-signs-expr expr))
           (rewrite-stmt (cycle)
             (typecase cycle
               (assignment-statement
                (make-assignment-stmt
                 (stmt-target-name cycle)
                 (rewrite-expr (stmt-expression cycle))
                 (stmt-target-indices cycle)))
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

(defun rewrite-exprs-in-block-optimization (pass-counter block &key (rules expr-ir:*rewrite-rules-basic*) (max-iterations 5))
  "Return a new BLOCK where every expr-ir expression has been rewritten
via REWRITE-EXPR-IR-WITH-RULES using RULES.

BLOCK is a STMT-IR:BLOCK-STATEMENT."
  (labels
      ((rewrite-expr (e)
         (rewrite-expr-ir-with-rules e rules :max-iterations max-iterations))
       (rewrite-stmt (st)
         (typecase st
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
in RHSs, indices, and conditions."
  (labels
      ((comp-expr (expr)
         (expr-complexity expr))
       (comp-stmt (cycle)
         (typecase cycle
           (assignment-statement
            (let ((rhs (stmt-expression cycle))
                  (idx (stmt-target-indices cycle)))
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
            0)
           (t
            0)))
       (comp-block (blk)
         (unless (typep blk 'block-statement)
           (error "BLOCK-COMPLEXITY: expected BLOCK-STATEMENT, got ~S" blk))
         (reduce #'+ (mapcar #'comp-stmt (block-statements blk)) :initial-value 0)))
    (comp-block block)))




(defun linear-canonicalization-optimization (pass-counter block &key)
  "Walk BLOCK and linear-canonicalize every expression, to help factoring and CSE.
Only linear subexpressions are changed; non-linear ones are left as-is."
  (declare (ignore pass-counter))
  (labels
      ((rewrite-expr (expr)
         ;; You can wrap with SIMPLIFY-EXPR if you like:
         ;; (expr-ir:simplify-expr
         ;;   (expr-ir:canonicalize-linear-subexprs expr))
         (expr-ir:simplify-expr
          (expr-ir:canonicalize-linear-subexprs expr)))

       (rewrite-stmt (st)
         (typecase st
           (assignment-statement
            (make-assignment-stmt
             (stmt-target-name st)
             (rewrite-expr (stmt-expression st))
             (stmt-target-indices st)))

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
                             (log-stream nil))
  "Apply OPT to BLOCK, compute complexity before/after, and return:
   values (new-block before-complexity after-complexity accepted-p).

If MIN-IMPROVEMENT on OPT is > 0, we only accept the new block if
(before - after) >= MIN-IMPROVEMENT. Otherwise we keep BLOCK."
  (unless (typep opt 'optimization)
    (error "RUN-OPTIMIZATION: expected OPTIMIZATION, got ~S" opt))
  (unless (typep block 'block-statement)
    (error "RUN-OPTIMIZATION: expected BLOCK-STATEMENT, got ~S" block))
  (let* ((fn        (optimization-function opt))
         (pos-args  (optimization-positional-args opt))
         (kw-args   (optimization-keyword-args opt))
         (before    (funcall measure block))
         (new-block (apply fn pass-counter block (append pos-args kw-args)))
         (after     (funcall measure new-block))
         (impr      (- before after))
         (threshold (optimization-min-improvement opt))
         (accepted  (>= impr threshold)))
    (when log-stream
      (format log-stream "~&~60a complexity: ~6D -> ~6D (Δ=~5D) ~A~%"
              (format nil "[OPT ~a ~A]" name (optimization-name opt))
              before after impr
              (if accepted
                  (if (= impr 0)
                      "useless"
                      "ACCEPT")
                  "REJECT")))
    (values (if accepted new-block block)
            before
            (if accepted after before)
            accepted)))


(defun run-optimization-pipeline (pass-counter pipeline block
                                  &key name (measure nil) (log-stream nil)
                                    (cycles 5))
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
          do (dolist (opt (optimization-pipeline-optimizations pipeline))
               (when (optimization-enabled-p opt)
                 (multiple-value-bind (new-block before after accepted)
                     (run-optimization pass-counter opt current
                                       :name cur-name
                                       :measure measure :log-stream log-stream)
                   (stmt-ir:debug-block new-block :label (format nil "block after optimization ~s" cur-name))
                   (push (list :opt-name (format nil "~a ~a" cur-name (optimization-name opt))
                               :before   before
                               :after    after
                               :accepted accepted)
                         results)
                   (setf current new-block)))))
    (let ((total-after (funcall measure current)))
      (values current (nreverse results) total-before total-after))))

;;; ----------------------------------------------------------------------
;;; accumulation control
;;; ----------------------------------------------------------------------


(defclass accumulation-anchor-statement ()
  ()
  (:documentation
   "Marker statement indicating where gradient/Hessian scalar
assignments (G_*/H_*_*) should be inserted before transform-eg-h-block.
Has no runtime effect by itself."))

(defun make-accum-anchor-stmt ()
  (make-instance 'accumulation-anchor-statement))


(defmacro accumulate-here ()
  "Place this in a stmt-block inside the kernel body where you want
gradient/Hessian accumulation code to be spliced.

Example:

  (stmt-block
    ...
    (stmt-ir:make-if-stmt
      (expr-ir:parse-expr \"r < r_cut\")
      (stmt-block
        ... ; compute energy, dE/dr, d2E/dr2
        (accumulate-here))     ; <--- here
      (stmt-block))
    ...)"
  `(stmt-ir:make-accum-anchor-stmt))

(defun block-has-accum-anchor-p (block)
  "Return T if BLOCK (a stmt-ir:block-statement) contains at least
one accumulation-anchor-statement anywhere inside."
  (labels ((scan-stmt (st)
             (typecase st
               (stmt-ir:accumulation-anchor-statement
                t)
               (stmt-ir:block-statement
                (some #'scan-stmt (stmt-ir:block-statements st)))
               (stmt-ir:if-statement
                (or (scan-stmt (stmt-ir:if-then-block st))
                    (and (stmt-ir:if-else-block st)
                         (scan-stmt (stmt-ir:if-else-block st)))))
               (t
                nil))))
    (scan-stmt block)))

(defun splice-derivatives-at-accum-anchor
    (block grad-stmts hess-stmts
           &key compute-grad compute-hess)
  "Return a new BLOCK where any accumulation-anchor-statement has been
replaced by the given gradient/Hessian assignment statements.

GRAD-STMTS and HESS-STMTS are lists of stmt-ir:assignment-statement
(or similar). If COMPUTE-GRAD or COMPUTE-HESS is NIL, the corresponding
statements are not inserted. If both are NIL, anchors are simply removed."
  (labels
      ((rewrite-stmt-list (stmts)
         (loop for st in stmts
               nconc (rewrite-stmt st)))

       (rewrite-stmt (st)
         (typecase st
           (stmt-ir:accumulation-anchor-statement
            ;; Replace anchor by grad/hess, or drop it entirely.
            (let ((inserted '()))
              (when compute-grad
                (setf inserted (nconc inserted (copy-list grad-stmts))))
              (when compute-hess
                (setf inserted (nconc inserted (copy-list hess-stmts))))
              inserted))

           (stmt-ir:block-statement
            (list (stmt-ir:make-block-stmt
                   (rewrite-stmt-list (stmt-ir:block-statements st)))))

           (stmt-ir:if-statement
            (let* ((then-blk (stmt-ir:if-then-block st))
                   (else-blk (stmt-ir:if-else-block st))
                   (new-then (stmt-ir:make-block-stmt
                              (rewrite-stmt-list
                               (stmt-ir:block-statements then-blk))))
                   (new-else (when else-blk
                               (stmt-ir:make-block-stmt
                                (rewrite-stmt-list
                                 (stmt-ir:block-statements else-blk))))))
              (list (stmt-ir:make-if-stmt
                     (stmt-ir:if-condition st)
                     new-then
                     new-else))))

           (t
            (list st)))))
    (stmt-ir:make-block-stmt
     (rewrite-stmt-list (stmt-ir:block-statements block)))))


(defun make-kernel-from-block
    (&key name pipeline layout coord-vars coord-load-stmts base-block params
       compute-energy compute-grad compute-hess derivatives)
  (let* ((energy-var 'energy)
         (manual-deriv-spec (normalize-derivatives-spec derivatives))
         ;; Expand D! requests first
         (base-block* (expand-derivative-requests base-block))
         (pass-counter (stmt-ir:make-pass-id-counter)))
    ;; Optional geometry checking / auto-fill stays the same, but use base-block*
    (when manual-deriv-spec
      (check-intermediate-geometry! base-block* coord-vars manual-deriv-spec))
    (when manual-deriv-spec
      (auto-fill-intermediate-geometry-from-ad
       base-block* coord-vars manual-deriv-spec
       :compute-second-derivs t
       :reuse-assignments t))

    (labels ((the-name (nm)
               (format nil "~a ~a" name nm)))
      ;; 1. Build gradient/Hessian scalar assignments, but do *not* append yet
      (let* ((grad-stmts (and compute-grad
                              (make-gradient-assignments-from-block
                               base-block* energy-var coord-vars
                               #'general-grad-name
                               manual-deriv-spec)))
             (hess-stmts (and compute-hess
                              (make-hessian-assignments-from-block
                               base-block* energy-var coord-vars
                               #'general-hess-name
                               manual-deriv-spec)))
             (has-anchor (block-has-accum-anchor-p base-block*))
             ;; 2. Combine base block + grad/hess, either via anchor or by append
             (combined-block
               (cond
                 (has-anchor
                  (splice-derivatives-at-accum-anchor
                   base-block*
                   (or grad-stmts '())
                   (or hess-stmts '())
                   :compute-grad compute-grad
                   :compute-hess compute-hess))
                 (t
                  ;; old behavior: append at end
                  (let* ((stmts (copy-list
                                 (stmt-ir:block-statements base-block*))))
                    (when grad-stmts
                      (setf stmts (append stmts grad-stmts)))
                    (when hess-stmts
                      (setf stmts (append stmts hess-stmts)))
                    (stmt-ir:make-block-stmt stmts)))))
             ;; 3. Run optimization pipeline on the combined block
             (opt-block combined-block))
        (when pipeline
          (multiple-value-bind (opt-block* results total-before total-after)
              (stmt-ir:run-optimization-pipeline
               pass-counter pipeline opt-block
               :name (the-name "e-g-hess")
               :log-stream *trace-output*)
            (declare (ignore results total-before total-after))
            (setf opt-block opt-block*)))
        ;; 4. Prepend coord load, if any
        (let* ((block-with-coords
                 (if coord-load-stmts
                     (stmt-ir:make-block-stmt
                      (append coord-load-stmts
                              (list opt-block)))
                     opt-block))
               ;; 5. Transform energy/grad/hess scalar assignments into
               ;;    *Energy +=, ForceAcc, DiagHessAcc, OffDiagHessAcc
               (transformed
                 (transform-eg-h-block
                  block-with-coords
                  layout
                  coord-vars
                  #'general-grad-name
                  #'general-hess-name))
               (locals (infer-kernel-locals transformed params coord-vars)))
          ;; 6. Wrap into a C function, as before
          (stmt-ir:make-c-function
           name
           transformed
           :return-type "void"
           :parameters  params
           :locals      locals))))))


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

(defun alias-assigned-exprs-optimization (pass-counter block)
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
  (labels
      ((process-block (blk env)
         (let ((new-stmts '()))
           (dolist (st (block-statements blk))
             (typecase st
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
