;;;; statement-diff.lisp
;;;; Differentiate sequences of assignment statements using expression-diff.

(in-package :stmt-ir)

(in-package :stmt-ir)

;;; ----------------------------------------------------------------------
;;; Build derivative environment over a block of assignments
;;; ----------------------------------------------------------------------


(defun build-derivative-env-for-block (statements base-var)
  "Given a list of STATEMENTS (typically assignment-statement objects)
and a base variable symbol BASE-VAR, build and return a derivative
environment mapping each intermediate variable name (string)
to its derivative expression d(var)/d(BASE-VAR)."
  (let* ((env (expr-ir:make-deriv-env))
         (table (expr-ir:deriv-env-table env)))
    ;; Seed d(base)/d(base) = 1
    (setf (gethash (expr-ir:var-key base-var) table)
          (expr-ir:make-expr-const 1))
    (dolist (st statements env)
      (when (typep st 'assignment-statement)
        (let* ((name (stmt-target-name st)))
          ;; Do not overwrite derivative for the base variable itself
          (unless (eql name base-var)
            (let* ((rhs  (stmt-expression st))
                   (drhs (expr-ir:differentiate-expr rhs base-var env))
                   (key  (expr-ir:var-key name)))
              (setf (gethash key table) drhs))))))
    env))



;;; ----------------------------------------------------------------------
;;; Get derivative of a specific target variable in the block
;;; ----------------------------------------------------------------------

(defun differentiate-target-in-block (statements base-var target-var)
  "Compute d(TARGET-VAR)/d(BASE-VAR) given a sequence of STATEMENTS.

STATEMENTS is a list of assignment-statement objects that define
intermediate variables (including TARGET-VAR) in order. TARGET-VAR
and BASE-VAR are symbols naming variables in the expression IR.

Returns an expression IR node for the derivative, or NIL if TARGET-VAR
never appears as an assignment target in STATEMENTS."
  (declare (optimize (debug 3)))
  (let* ((env (build-derivative-env-for-block statements base-var))
         (key (expr-ir:var-key target-var))
         (table (expr-ir:deriv-env-table env)))
    (multiple-value-bind (d presentp)
        (gethash key table)
      (if presentp
          d
          nil))))


(defun make-local-partial-derivative-assignments-for-block
    (statements &key base-vars)
  "Given a list of STATEMENTS (assignment-statement objects),
return a list of assignment-statement objects for the local partial
derivatives d(target)/d(dep) of each assignment.

For each assignment
    target = RHS(...)
we look at the free variables of RHS. For every such DEP:

  D<TARGET>_D<DEP> = ∂(RHS)/∂DEP

If BASE-VARS is non-NIL, it must be a list of symbols; only
dependencies that are members of BASE-VARS are differentiated."
  (let ((result '()))
    (dolist (st statements (nreverse result))
      (when (typep st 'assignment-statement)
        (let* ((target (stmt-target-name st))
               (rhs    (stmt-expression st))
               (deps   (expr-ir:expr-free-vars rhs)))
          (dolist (dep deps)
            (when (or (null base-vars)
                      (member dep base-vars :test #'eq))
              ;; Local partial: differentiate RHS treating DEP as independent
              (let* ((dexpr (expr-ir:differentiate-expr rhs dep))
                     (dname (make-derivative-name target dep))
                     (dst   (make-assignment-stmt dname dexpr)))
                (push dst result)))))))))


  
;;; ----------------------------------------------------------------------
;;; Generate derivative assignment statements for a block
;;; ----------------------------------------------------------------------

(defun make-derivative-name (var base-var)
  "Generate a symbol naming the derivative d(VAR)/d(BASE-VAR).
The naming convention here is: D<var>_D<base>, e.g. DFOO_DX."
  (intern (format nil "D~A_D~A"
                  (symbol-name var)
                  (symbol-name base-var))
          (symbol-package var)))

(defun make-derivative-assignments-for-block (statements base-var)
  "Given a list of STATEMENTS (assignment-statement objects) and BASE-VAR,
return a list of new assignment-statement objects that assign each
intermediate variable's derivative to a fresh derivative variable.

Example:
  foo = f(x)
  bar = g(foo)

yields (for base-var 'x):
  DFOO_DX = d(foo)/dx
  DBAR_DX = d(bar)/dx"
  (let ((env (expr-ir:make-deriv-env))
        (result '()))
    ;; seed base-var
    (expr-ir:set-var-derivative base-var (expr-ir:make-expr-const 1) env)
    (dolist (st statements (nreverse result))
      (when (typep st 'assignment-statement)
        (let* ((var-name (stmt-target-name st))
               (rhs      (stmt-expression st))
               (drhs     (expr-ir:differentiate-expr rhs base-var env))
               (dvar     (make-derivative-name var-name base-var))
               (dst      (make-assignment-stmt dvar drhs)))
          ;; store derivative in env so later assignments see it
          (expr-ir:set-var-derivative var-name drhs env)
          (push dst result))))))

;;; ----------------------------------------------------------------------
;;; Derivative request statements and the D macro
;;; ----------------------------------------------------------------------

(defclass derivative-request-statement (statement)
  ((target-var
    :initarg :target-var
    :accessor dr-target-var
    :documentation "Variable whose derivative we want, e.g. e_base or energy.")
   (base-var
    :initarg :base-var
    :accessor dr-base-var
    :documentation "Base variable (coordinate or intermediate) we differentiate with respect to."))
  (:documentation
   "Placeholder used in the kernel DSL to request a derivative d(TARGET-VAR)/d(BASE-VAR)
to be computed later and turned into a normal assignment."))

(defun make-derivative-request-stmt (target-var base-var)
  "Create a derivative-request-statement asking for d(TARGET-VAR)/d(BASE-VAR)."
  (make-instance 'derivative-request-statement
                 :target-var target-var
                 :base-var   base-var))



;;; -------------------------------
;;; Expand derivative requests
;;; -------------------------------

#+(or)
(defun expand-derivative-requests-in-block (block)
  "Walk BLOCK (a STMT-IR:BLOCK-STATEMENT) and replace each
DERIVATIVE-REQUEST-STATEMENT with an ASSIGNMENT-STATEMENT that computes
the requested derivative using EXPR-IR's AD machinery.

We assume the derivative request object provides:
  (derivative-request-target stmt)  -> EXPR-IR variable symbol
  (derivative-request-base   stmt)  -> EXPR-IR variable symbol

and we rely on STMT-IR:DIFFERENTIATE-TARGET-IN-BLOCK from
statement-diff.lisp to compute d(target)/d(base) over the assignments
seen so far."
  (labels
      ((rewrite-block (blk)
         (let ((new-stmts      '())
               ;; assignments seen so far in this block, in *program order*
               (assigns-so-far '()))
           (dolist (st (block-statements blk))
             (typecase st
               (assignment-statement
                ;; keep the assignment and remember it
                (push st new-stmts)
                (push st assigns-so-far))

               (derivative-request-statement
                ;; expand D!(target, base) at this point
                (let* ((target (derivative-request-target st))
                       (base   (derivative-request-base st))
                       ;; assignments so far in program order
                       (stmts  (nreverse assigns-so-far))
                       ;; d(target)/d(base) as an expr-ir node
                       (dex    (differentiate-target-in-block stmts base target)))
                  (unless dex
                    (error "No assignment for ~S before derivative request ~S"
                           target st))
                  (let* ((dex-s (expr-ir:simplify-expr dex))
                         ;; choose whatever naming convention you want here:
                         ;; you can use MAKE-DERIVATIVE-NAME, or your own
                         ;; 'dE_base_dr' style function.
                         (dvar  (make-derivative-name target base))
                         (dst   (make-assignment-stmt dvar dex-s)))
                    ;; emit the new derivative assignment *in place of*
                    ;; the request, and also add it to assigns-so-far so
                    ;; later D! calls can differentiate it again (for
                    ;; 2nd derivatives like d2E_base_dr2).
                    (push dst new-stmts)
                    (push dst assigns-so-far))))

               (if-statement
                ;; recursively rewrite then/else blocks
                (let* ((cond     (if-condition st))
                       (then-blk (rewrite-block (if-then-block st)))
                       (else-blk (and (if-else-block st)
                                      (rewrite-block (if-else-block st))))
                       (new-if   (make-if-stmt cond then-blk else-blk)))
                  (push new-if new-stmts)))

               (block-statement
                ;; nested block: rewrite inside, but do not fold its
                ;; assignments into ASSIGNS-SO-FAR (we treat each block
                ;; independently here).
                (let ((sub (rewrite-block st)))
                  (push sub new-stmts)))

               (t
                ;; any other statement: keep as-is
                (push st new-stmts))))
           (make-block-stmt (nreverse new-stmts)))))
    (rewrite-block block)))

#+(or)
(defun expand-derivative-requests-optimization (pass-counter block &key &allow-other-keys)
  "Optimization wrapper: expand all DERIVATIVE-REQUEST-STATEMENTs
into concrete ASSIGNMENT-STATEMENTs."
  (declare (ignore pass-counter))
  (expand-derivative-requests-in-block block))
