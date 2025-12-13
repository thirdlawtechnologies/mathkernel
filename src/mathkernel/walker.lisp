;;;; walker.lisp
;;;; Generic walking context and walker for stmt-ir blocks.

(in-package :stmt-ir)

;;; ------------------------------------------------------------
;;; Generic context class
;;; ------------------------------------------------------------

(defclass walk-context ()
  ()
  (:documentation "Base class for walkers. Subclasses can carry whatever environments they need."))


;;; ------------------------------------------------------------
;;; Generic hooks
;;; ------------------------------------------------------------


(defgeneric clone-context (operation ctx block)
  (:documentation "Return a shallow/deep copy of CTX appropriate for branching, parameterized by OPERATION."))

(defgeneric on-statement (operation ctx stmt)
  (:documentation "Hook called for each statement. May return (values new-stmt new-ctx).
Default behavior: pass STMT through unchanged and return CTX. OPERATION can be used to select behavior."))

(defgeneric rewrite-block (operation ctx statements)
  (:documentation "Rewrite STATEMENTS at one block level. Should return (values new-statements new-ctx).
Default folds ON-STATEMENT over STATEMENTS, threading CTX."))

;;; Default methods
(defmethod clone-context ((operation t) (ctx walk-context) block)
  ctx)

(defmethod on-statement ((operation t) (ctx walk-context) stmt)
  (values stmt ctx))

(defmethod rewrite-block ((operation t) (ctx walk-context) statements)
  (let ((out '())
        (cur ctx))
    (dolist (st statements (values (nreverse out) cur))
      (multiple-value-bind (new-st new-ctx) (on-statement operation cur st)
        (setf cur new-ctx)
        (when new-st
          (if (listp new-st)
              (setf out (nconc (nreverse new-st) out))
              (push new-st out)))))))

;;; ------------------------------------------------------------
;;; Walker
;;; ------------------------------------------------------------

(defun walk-block-with-context (operation ctx block)
  "Walk BLOCK (a block-statement) threading CTX (a WALK-CONTEXT) for OPERATION.
Returns (values new-block new-ctx).

For IF statements, the context is cloned for each branch and not merged."
  (unless (typep block 'block-statement)
    (error "walk-block-with-context: expected BLOCK-STATEMENT, got ~S" block))
  (multiple-value-bind (stmts cur-ctx)
      (rewrite-block operation ctx (block-statements block))
    (let ((new-stmts '()))
      (dolist (st stmts)
        (etypecase st
          (if-statement
           (let* ((cond (if-condition st))
                  (then-blk (if-then-block st))
                  (else-blk (if-else-block st))
                  (then-ctx (clone-context operation cur-ctx then-blk))
                  (else-ctx (clone-context operation cur-ctx else-blk)))
             (multiple-value-bind (new-then then-ctx-out)
                 (and then-blk (walk-block-with-context operation then-ctx then-blk))
               (declare (ignore then-ctx-out))
               (multiple-value-bind (new-else else-ctx-out)
                   (and else-blk (walk-block-with-context operation else-ctx else-blk))
                 (declare (ignore else-ctx-out))
                 (push (make-if-stmt cond new-then new-else) new-stmts)))))
          (block-statement
           (multiple-value-bind (sub-block sub-ctx)
               (walk-block-with-context operation cur-ctx st)
             (setf cur-ctx sub-ctx)
             (push sub-block new-stmts)))
          (t
           (push st new-stmts))))
      (values (make-block-stmt (nreverse new-stmts)) cur-ctx))))
