(in-package :stmt-ir)

;;; Walker-based copy propagation
;;;
;;; Eliminates trivial copies (dst := src) without introducing temps.
;;; - Straight-line: alias and drop the copy.
;;; - Inside IF branches: keep the copy so definitions are explicit at joins.
;;; Preserves block labels via walk-block-with-context.

(defclass cp-context (walk-context)
  ((env        :initarg :env :accessor cp-env)        ; hash-table EQ: sym -> root
   (idx        :initarg :idx :accessor cp-idx)
   (block        :initarg :block :accessor cp-block)
   (inside-if  :initarg :inside-if :accessor cp-inside-if)))

(defun make-cp-context (&key block env idx inside-if)
  (make-instance 'cp-context :block block :env env :idx idx :inside-if inside-if))

(defmethod clone-context ((op (eql :copy-prop)) (ctx cp-context) block)
  ;; Branch-local env clone; flag branches as inside-if.
  (make-cp-context
   :env (copy-env-hash-table (cp-env ctx))
   :block block
   :idx -1
   :inside-if t))

(defun cp-find-root (sym env)
  (loop
    for s = sym then (or (gethash s env) s)
    for next = (gethash s env)
    while next
    finally (return s)))

(defun cp-rewrite-sexpr (sx env)
  (cond
    ((symbolp sx) (cp-find-root sx env))
    ((consp sx)   (mapcar (lambda (sub) (cp-rewrite-sexpr sub env)) sx))
    (t sx)))

(defun cp-rewrite-expr (expr env)
  (expr-ir:sexpr->expr-ir (cp-rewrite-sexpr (expr-ir:expr->sexpr expr) env)))

(defmethod on-statement ((op (eql :copy-prop)) (ctx cp-context) st)
  (etypecase st
    (anchored-assignment-statement
     (let* ((env (cp-env ctx))
            (idx (incf (cp-idx ctx)))
            (tgt (stmt-target-name st))
            (new-expr (cp-rewrite-expr (stmt-expression st) env))
            (new-st (if (eq new-expr (stmt-expression st))
                        st
                        (make-anchored-assignment-stmt tgt new-expr
                                                       (stmt-target-indices st)))))
       (declare (ignore idx))
       (remhash tgt env)
       (values new-st ctx)))
    (assignment-statement
     (let* ((env (cp-env ctx))
            (idx (incf (cp-idx ctx)))
            (tgt (stmt-target-name st))
            (new-expr (cp-rewrite-expr (stmt-expression st) env))
            (sx (expr-ir:expr->sexpr new-expr)))
       (declare (ignore idx))
       (cond
         ((and (symbolp sx) (not (eq sx tgt)))
          (let ((root (cp-find-root sx env)))
            (progn
              (setf (gethash tgt env) root)
              (if (or
                   (eq tgt 'expr-var::energy)
                   (eq tgt 'expr-var::de_dr)
                   (eq tgt 'expr-var::d2e_dr2))
                  (progn
                    (warn "Was about to drop ~s but we special cased it" tgt)
                    (values st ctx))
                  (values nil ctx))
              )))
          (t
           (remhash tgt env)
           (let ((new-st (if (eq new-expr (stmt-expression st))
                             st
                             (make-assignment-stmt tgt new-expr :target-indices (stmt-target-indices st)))))
             (values new-st ctx))))))
     (raw-c-statement
      (let* ((idx (incf (cp-idx ctx)))
             (expr (stmt-expression st)))
        (declare (ignore idx))
        (if expr
            (values (make-raw-c-statement (raw-c-generator st)
                                          (cp-rewrite-expr expr (cp-env ctx)))
                    ctx)
            (values st ctx))))
     (if-statement
      (incf (cp-idx ctx))
      (values st ctx))
     (block-statement
      (incf (cp-idx ctx))
      (values st ctx))
     (t
      (incf (cp-idx ctx))
      (values st ctx))))

(defmethod rewrite-block ((op (eql :copy-prop)) (ctx cp-context) statements)
  (let ((out '())
        (label (block-label (cp-block ctx))))
    (dolist (st statements (values (nreverse out)
                                   ctx))
      (multiple-value-bind (new-st new-ctx)
          (on-statement op ctx st)
        (setf ctx new-ctx)
        (when new-st
          (if (listp new-st)
              (setf out (nconc (nreverse new-st) out))
              (push new-st out)))))))

(defun copy-propagate-optimization (pass-counter block binding-params &key (verbose *verbose-optimization*))
  "Walker-based copy propagation (no temps). Keeps explicit copies in IF branches;
straight-line trivial copies are aliased/dropped."
  (declare (ignore pass-counter binding-params))
  (declare (optimize (debug 3)))
  (unless (typep block 'block-statement)
    (error "copy-propagate-optimization: expected BLOCK-STATEMENT, got ~S" block))
  (verbose-log verbose "~&[COPY-PROP/W] scanning block with ~D statements~%"
               (length (block-statements block)))
  (let* ((env (make-hash-table :test #'eq))
         (ctx (make-cp-context :block block :env env :idx -1 :inside-if nil))
         (new-block (car (multiple-value-list (walk-block-with-context :copy-prop ctx block)))))
    (check-block-integrity new-block)
    new-block))
