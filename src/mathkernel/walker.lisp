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
;;; When passing temp-inserts from an analysis phase to writer phase
;;; use this.
;;; ------------------------------------------------------------

(defclass temp-inserts ()
  ((table :initform (make-hash-table :test 'eq)
          :reader %temp-inserts-table
          :documentation "Store the map of block to block-temp-inserts."))
  (:documentation " When passing temp-inserts from an analysis phase to writer phase use this."))

(defclass block-temp-inserts ()
  ((table :initform (make-hash-table :test 'eql)
          :reader %block-temp-inserts-table
          :documentation "Store the map of index to assignment statement."))
  (:documentation " When passing temp-inserts from an analysis phase to writer phase use this."))

(defun temp-inserts-clean-p (temp-inserts)
  "Validate TEMP-INSERTS for dominated-path duplicates of the same temp.
Returns T if clean and NIL if not."
  (let ((entries '()))
    ;; Gather all planned temp assignments: (temp block idx).
      (maphash
       (lambda (blk bucket)
         (maphash
          (lambda (idx stmts)
            (dolist (s (if (listp stmts) stmts (list stmts)))
              (push (list (stmt-target-name s) blk idx) entries)))
          (%block-temp-inserts-table bucket)))
       (%temp-inserts-table temp-inserts))
    ;; Detect dominated-path duplicates of the same temp.
    ;; Compare each pair for dominance and same temp.
    (dolist (a entries)
      (destructuring-bind (temp-a blk-a _idx-a) a
        (declare (ignore _idx-a))
        (when (cse-temp-symbol-p temp-a)
          (dolist (b entries)
            (destructuring-bind (temp-b blk-b _idx-b) b
              (declare (ignore _idx-b))
              (when (and (eq temp-a temp-b)
                         (not (eq blk-a blk-b))
                         (or (block-dominates-p blk-a blk-b)
                             (block-dominates-p blk-b blk-a)))
                (return-from temp-inserts-clean-p nil)
                )))))))
  t)

(defun make-temp-inserts ()
  (make-instance 'temp-inserts))

(defun make-block-temp-inserts ()
  (make-instance 'block-temp-inserts))

(defun temp-insert-add (temp-inserts block block-temp-inserts)
  (setf (gethash block (%temp-inserts-table temp-inserts)) block-temp-inserts)
  (unless (temp-inserts-clean-p temp-inserts)
    (error "Caught temp-insert-add that generated bad temp-inserts")))

(defun block-temp-insert-add (temp-inserts block-temp-inserts index assignment)
  (setf (gethash index (%block-temp-inserts-table block-temp-inserts)) assignment)
  (unless (temp-inserts-clean-p temp-inserts)
    (error "Caught temp-insert-add that generated bad temp-inserts")))

(defun temp-inserts-lookup (temp-inserts block)
  (gethash block (%temp-inserts-table temp-inserts)))

(defun block-temp-inserts-lookup (block-temp-inserts index)
  (gethash index (%block-temp-inserts-table block-temp-inserts)))

(defmethod print-object ((obj temp-inserts) stream)
  (let ((*package* (find-package :expr-var))
        (*print-pretty* nil))
    (print-unreadable-object (obj stream :type t)
      (maphash (lambda (block bti)
                 (maphash (lambda (index asses)
                            (loop for ass in asses
                                  do (format stream "temp-insert: ~s ~d ~s <- ~s~%"
                                             (block-label block) index (stmt-target-name ass) (expr-ir:expr->sexpr (stmt-expression ass)))))
                          (%block-temp-inserts-table bti)))
               (%temp-inserts-table obj)))))


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
      (values (make-block-stmt (nreverse new-stmts) :label (block-label block)) cur-ctx))))
