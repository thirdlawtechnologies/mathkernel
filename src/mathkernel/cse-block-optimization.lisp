(in-package :stmt-ir)

;;; ----------------------------------------------------------------------
;;; Walker contexts for CSE (collection + rewrite scaffolding)
;;; ----------------------------------------------------------------------

(defclass cse-collect-context (walk-context)
  ((env :initarg :env :accessor cse-col-env)
   (idx :initarg :idx :accessor cse-col-idx)
   (counts :initarg :counts :accessor cse-col-counts)
   (first-use :initarg :first-use :accessor cse-col-first-use)
   (first-use-env :initarg :first-use-env :accessor cse-col-first-use-env)
   (uses-envs :initarg :uses-envs :accessor cse-col-uses-envs)
   (outer-symbols :initarg :outer-symbols :accessor cse-col-outer-symbols)))

(defun make-cse-collect-context (&key env idx counts first-use first-use-env uses-envs outer-symbols)
  (make-instance 'cse-collect-context
                 :env env
                 :idx idx
                 :counts counts
                 :first-use first-use
                 :first-use-env first-use-env
                 :uses-envs uses-envs
                 :outer-symbols outer-symbols))

(defmethod clone-context ((operation (eql :cse-collect)) (ctx cse-collect-context) block)
  (make-cse-collect-context
   :env (copy-binding-env (cse-col-env ctx) block)
   :idx -1
   :counts (cse-col-counts ctx)
   :first-use (cse-col-first-use ctx)
   :first-use-env (cse-col-first-use-env ctx)
   :uses-envs (cse-col-uses-envs ctx)
   :outer-symbols (cse-col-outer-symbols ctx)))

(defun %cse-record-expr (expr idx env counts first-use first-use-env uses-envs)
  (labels ((rec (sx)
             (when (consp sx)
               (incf (gethash sx counts 0))
               (unless (gethash sx first-use)
                 (setf (gethash sx first-use) idx)
                 (setf (gethash sx first-use-env) env))
               (push env (gethash sx uses-envs))
               (dolist (arg (cdr sx))
                 (rec arg)))))
    (rec (expr-ir:expr->sexpr expr))))

(defmethod on-statement ((operation (eql :cse-collect)) (ctx cse-collect-context) st)
  (etypecase st
    (assignment-statement
     (let* ((env (cse-col-env ctx))
            (idx (incf (cse-col-idx ctx)))
            (tgt (stmt-target-name st)))
       (binding-env-add env tgt (make-env-entry idx (stmt-expression st)))
       (%cse-record-expr (stmt-expression st) idx env
                         (cse-col-counts ctx) (cse-col-first-use ctx) (cse-col-first-use-env ctx) (cse-col-uses-envs ctx))
       (values st ctx)))
    (raw-c-statement
     (incf (cse-col-idx ctx))
     (values st ctx))
    (if-statement
     (incf (cse-col-idx ctx))
     (values st ctx))
    (block-statement
     (incf (cse-col-idx ctx))
     (values st ctx))
    (t
     (incf (cse-col-idx ctx))
     (values st ctx))))

(defclass cse-rewrite-context (walk-context)
  ((env :initarg :env :accessor cse-rw-env)
   (idx :initarg :idx :accessor cse-rw-idx)
   (sexpr->temp :initarg :sexpr->temp :accessor cse-rw-sexpr->temp)
   (temp-insert :initarg :temp-insert :accessor cse-rw-temp-insert)
   (counts-from-factors :initarg :counts-from-factors :accessor cse-rw-counts-from-factors)
   (cand-counts :initarg :cand-counts :accessor cse-rw-cand-counts)
   (temps-seen :initarg :temps-seen :accessor cse-rw-temps-seen)
   (changed :initarg :changed :accessor cse-rw-changed)))

(defun make-cse-rewrite-context (&key env idx sexpr->temp temp-insert counts-from-factors cand-counts temps-seen changed)
  (make-instance 'cse-rewrite-context
                 :env env
                 :idx idx
                 :sexpr->temp sexpr->temp
                 :temp-insert temp-insert
                 :counts-from-factors counts-from-factors
                 :cand-counts cand-counts
                 :temps-seen temps-seen
                 :changed changed))

(defmethod clone-context ((operation (eql :cse-rewrite)) (ctx cse-rewrite-context) block)
  (make-cse-rewrite-context
   :env (copy-binding-env (cse-rw-env ctx) block)
   :idx -1
   :sexpr->temp (cse-rw-sexpr->temp ctx)
   :temp-insert (cse-rw-temp-insert ctx)
   :counts-from-factors (cse-rw-counts-from-factors ctx)
   :cand-counts (cse-rw-cand-counts ctx)
   :temps-seen (cse-rw-temps-seen ctx)
   :changed (cse-rw-changed ctx)))

(defun temp-available-p (ctx temp idx)
  (declare (ignorable idx))
  (binding-env-lookup (cse-rw-env ctx) temp)

  #+(or)(or (binding-env-lookup (cse-rw-env ctx) temp)
      (let* ((bucket (temp-inserts-lookup (cse-rw-temp-insert ctx)
                                          (binding-env-block (cse-rw-env ctx)))))
        (when bucket
          (maphash
           (lambda (ins-idx stmts)
             (declare (optimize (debug 3)))
             (break "Check in temp-available-p ins-idx ~d  idx ~d" ins-idx idx)
             (when (and (< ins-idx idx)
                        (let ((lst (if (listp stmts) stmts (list stmts))))
                          (some (lambda (s) (eq (stmt-target-name s) temp)) lst)))
               (return-from temp-available-p t)))
           (%block-temp-inserts-table bucket))
          nil))))

(defun %rewrite-cse-sexpr (sexpr sexpr->temp counts-from-factors avail-fn)
  (declare (optimize (debug 3)))
  (labels ((rec (sx)
             (cond
               ((and (consp sx) (gethash sx sexpr->temp))
                (let ((tt (gethash sx sexpr->temp)))
                  (if (funcall avail-fn tt)
                      tt
                      sx)))
               ((and (consp sx) (eq (car sx) '*))
                (let* ((orig (cdr sx))
                       (prod-counts (funcall counts-from-factors orig))
                       (replacement (gethash sx sexpr->temp)))
                  (declare (ignore prod-counts))
                  (if replacement
                      (if (funcall avail-fn replacement)
                          replacement
                          (cons '* (mapcar #'rec orig)))
                      (cons '* (mapcar #'rec orig)))))
               ((consp sx)
                (cons (car sx) (mapcar #'rec (cdr sx))))
               (t sx))))
    (rec sexpr)))

(defmethod on-statement ((operation (eql :cse-rewrite)) (ctx cse-rewrite-context) st)
  (declare (optimize (debug 3)))
  (etypecase st
    (assignment-statement
     (let* ((env (cse-rw-env ctx))
            (idx (incf (cse-rw-idx ctx)))
            (tgt (stmt-target-name st))
            (expr (stmt-expression st))
            (sexpr (expr-ir:expr->sexpr expr))
            (sexpr->temp (cse-rw-sexpr->temp ctx))
            (counts-from-factors (cse-rw-counts-from-factors ctx))
            (rewritten-sexpr (%rewrite-cse-sexpr sexpr
                                                 sexpr->temp
                                                 counts-from-factors
                                                 (lambda (tt)
                                                   (temp-available-p ctx tt (cse-rw-idx ctx)))))
            (rewritten (expr-ir:sexpr->expr-ir rewritten-sexpr))
            (new-st (if (eq rewritten (stmt-expression st)) st (make-assignment-stmt tgt rewritten
                                                                                     :preserve-anchor-of st
                                                                                     )))
            )
       (binding-env-add env tgt (make-env-entry idx rewritten))
       (when (not (eq new-st st)) (setf (car (cse-rw-changed ctx)) t))
       (values new-st ctx)))
    (raw-c-statement
     (incf (cse-rw-idx ctx))
     (values st ctx))
    (if-statement
     (incf (cse-rw-idx ctx))
     (values st ctx))
    (block-statement
     (incf (cse-rw-idx ctx))
     (values st ctx))
    (t
     (incf (cse-rw-idx ctx))
     (values st ctx))))

(defmethod rewrite-block ((operation (eql :cse-rewrite)) (ctx cse-rewrite-context) statements)
  (declare (optimize (debug 3)))
  (let* ((out '())
         (idx -1)
         (bucket (temp-inserts-lookup (cse-rw-temp-insert ctx)
                                      (binding-env-block (cse-rw-env ctx)))))
    (flet ((handle-stmt (stmt &key changed)
             (multiple-value-bind (new-st new-ctx)
                 (on-statement operation ctx stmt)
               (setf ctx new-ctx)
               (push new-st out)
               (when changed (setf (car (cse-rw-changed ctx)) t)))))
      ;; Handle statements
      (dolist (st statements)
        (incf idx)
        (let* ((temps (and bucket (block-temp-inserts-lookup bucket idx)))
               (temps (if (listp temps) temps (and temps (list temps)))))
          (when temps
            (dolist (tt (reverse temps))
              (handle-stmt tt :changed t))))
        (handle-stmt st :changed nil))
      ;; append temps that need to go on the end
      (let* ((temps (and bucket (block-temp-inserts-lookup bucket (length statements))))
             (temps (if (listp temps) temps (and temps (list temps)))))
        (when temps
          (dolist (tt (reverse temps))
            (handle-stmt tt :changed t))))
      (values (nreverse out) ctx))))


(defun debug-cse-temp-trace (block &key (label "cse"))
  "Print a linear trace of BLOCK showing where CSE temps are defined
and where they are used. This is only for debugging."
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

;;; ----------------------------------------------------------------------
;;; CSE optimizations (walker-based)
;;; ----------------------------------------------------------------------

(defun %collect (blk outer-symbols)
  (let* ((env (make-binding-env blk :binding-params outer-symbols))
         (counts (make-hash-table :test #'equal))
         (first-use (make-hash-table :test #'equal))
         (first-use-env (make-hash-table :test #'equal))
         (uses-envs (make-hash-table :test #'equal))
         (ctx (make-cse-collect-context :env env :idx -1 :counts counts :first-use first-use :first-use-env first-use-env :uses-envs uses-envs :outer-symbols outer-symbols)))
    (walk-block-with-context :cse-collect ctx blk)
    (values env counts first-use first-use-env uses-envs)))


(defun %choose-candidates (counts first-use first-use-env uses-envs &key min-uses min-size)
  (let ((candidates '()))
    (maphash
     (lambda (sx count)
       (when (and (>= count min-uses)
                  (consp sx)
                  (>= (sexpr-size sx) min-size))
         (push sx candidates)))
     counts)
    (setf candidates (sort candidates #'string< :key (lambda (sx) (format nil "~S" sx))))
    (values candidates first-use first-use-env uses-envs)))

(defun %plan-temp (block pass-id candidates counts first-use first-use-env uses-envs env min-uses)
  (declare (optimize (debug 3)))
  (let ((sexpr->temp (make-hash-table :test #'equal))
        (temp-inserts (make-temp-inserts))
        (temp-counter (next-cse-temp-index-for-pass block pass-id))
        (env-sexpr->sym (make-hash-table :test #'equal)))
    (map-binding-env (lambda (sym entry _)
                       (declare (ignore _))
                       (when (env-entry-expr entry)
                         (setf (gethash (expr-ir:expr->sexpr (env-entry-expr entry)) env-sexpr->sym) sym)))
                     env)
    (dolist (sx candidates)
      (let* ((use-idx (gethash sx first-use))
             (use-env (or (gethash sx first-use-env) env))
             (use-envs-list (or (gethash sx uses-envs) (list use-env)))
             (existing (gethash sx env-sexpr->sym)))
        ;; Ignore sx that are already assigned to a var
        (if existing
            (progn
              (setf (gethash sx sexpr->temp) existing))
            (let* ((temp (make-cse-temp-symbol pass-id (incf temp-counter) :suffix t))
                   (uses (expr-ir:expr-free-vars (expr-ir:sexpr->expr-ir sx)))
                   (dep-envs (loop for u in uses
                                   for (_entry env-found) = (multiple-value-list (binding-env-lookup use-env u))
                                   if env-found
                                     collect env-found
                                   else
                                     do (error "Could not find ~s in binding-env ~s or one of its ancestors"
                                               u use-env)))
                   (all-use-envs (or dep-envs (gethash sx uses-envs)))
                   (insert-env (let ((deep-env (reduce #'binding-env-later all-use-envs :initial-value use-env)))
                                 (loop for use in uses
                                       for (_entry env-found) = (multiple-value-list (binding-env-lookup deep-env use))
                                       unless env-found
                                         do (error "For uses ~s  deep-env is ~s and does not dominate ~s" uses deep-env use))
                                 deep-env))
                   (insert-block (or (binding-env-block insert-env) block))
                   (dep-max (if uses
                                (loop with mx = -1
                                      for u in uses
                                      for (entry env-found) = (multiple-value-list (binding-env-lookup insert-env u))
                                      for di = (and entry (env-entry-index entry))
                                      when (and env-found (eq env-found insert-env) (integerp di)) do (setf mx (max mx di))
                                        finally (return mx))
                                -1))
                   (min-insert (max (1+ dep-max) 0))
                   (max-allowed (or (and (eq insert-env use-env) use-idx)
                                    (length (block-statements insert-block))))
                   (bucket (or (temp-inserts-lookup temp-inserts insert-block)
                               (let ((b (make-block-temp-inserts)))
                                 (temp-insert-add temp-inserts insert-block b)
                                 b)))
                   (existing (block-temp-inserts-lookup bucket min-insert)))
              (declare (ignore use-envs-list))
              (when (and (<= min-insert max-allowed) (>= (gethash sx counts) min-uses))
                (block-temp-insert-add temp-inserts bucket min-insert
                                       (cons (make-assignment-stmt temp (expr-ir:sexpr->expr-ir sx))
                                             existing))
                (setf (gethash sx sexpr->temp) temp))))))
    (values sexpr->temp temp-inserts)))

(defun %rewrite (blk env sexpr->temp temp-inserts)
  (declare (optimize (debug 3)))
  (let* ((rw-env (make-binding-env blk)))
    (map-binding-env (lambda (sym entry _)
                       (declare (ignore _))
                       (when (typep entry 'outer-env-entry)
                         (binding-env-add rw-env sym entry)))
                     env)
    (let* ((ctx (make-cse-rewrite-context
                 :env rw-env :idx -1
                 :sexpr->temp sexpr->temp
                 :temp-insert temp-inserts
                 :counts-from-factors #'counts-from-factors
                 :cand-counts nil
                 :temps-seen (make-hash-table :test #'eq)
                 :changed (list nil))))
      (multiple-value-bind (new-block ctx-out)
          (walk-block-with-context :cse-rewrite ctx blk)
        (declare (ignore ctx-out))
        (check-block-integrity new-block)
        (values new-block (car (cse-rw-changed ctx)))))))


(defun cse-block (block binding-params &key (min-uses 2) (min-size 5) (pass-id 1)
                        (outer-symbols nil)
                        (verbose *verbose-optimization*))
  "Walker-based CSE. Collect subexpression counts/first-use with a walker,
then rewrite with temps using a walker."
  (declare (optimize (debug 3)))
  (unless (typep block 'block-statement)
    (error "cse-block: expected BLOCK-STATEMENT, got ~S" block))
  (let ((outer-symbols (or outer-symbols binding-params)))
    (labels ()
      (multiple-value-bind (env counts first-use first-use-env uses-envs) (%collect block outer-symbols)
        (multiple-value-bind (candidates first-use* first-use-env* uses-envs*) (%choose-candidates counts first-use first-use-env uses-envs :min-size min-size :min-uses min-uses)
          (when (null candidates)
            (verbose-log verbose "~&[CSE pass ~D] no candidates (min-uses=~D min-size=~D)~%" pass-id min-uses min-size)
            (return-from cse-block block))
          (multiple-value-bind (sexpr->temp temp-inserts)
              (%plan-temp block pass-id candidates counts first-use* first-use-env* uses-envs* env min-uses)
            (multiple-value-bind (new-block changed)
                (let ((new-env (make-binding-env block :binding-params binding-params)))
                  (%rewrite block new-env sexpr->temp temp-inserts))
              (declare (ignore changed))
              (check-block-integrity new-block)
              new-block)))))))

(defun cse-block-multi-optimization (counter block binding-params
                                     &key
                                       (max-passes 5)
                                       (min-uses 2)
                                       (min-size 5)
                                       (outer-symbols nil)
                                       (verbose *verbose-optimization*))
  "Apply CSE repeatedly to BLOCK up to MAX-PASSES times, or until
a pass makes no changes."
  (declare (optimize (debug 3)))
  (unless (typep block 'block-statement)
    (error "cse-block-multi: expected BLOCK-STATEMENT, got ~S" block))
  (let* ((outer-symbols (or outer-symbols binding-params))
         (current-block block))
    (loop for ii from 1 upto max-passes do
      (let* ((pass-id (next-pass-id counter))
             (before-symbol-list (collect-scalar-targets-in-block current-block))
             (after-cse
               (cse-block current-block binding-params
                          :min-uses min-uses
                          :min-size min-size
                          :pass-id  pass-id
                          :outer-symbols outer-symbols
                          :verbose verbose))
             ;; Catch duplicate CSE temps before factoring
             (dummy (check-block-integrity after-cse :label (format nil "after-cse-~D" pass-id)))
             (after-prod
               #+(or)(progn
                 (warn "Skipping cse-factor-products-in-block")
                 after-cse)
               (let ((current after-cse))
                 (loop for iter from 1 to 10 do
                   (let ((next (cse-factor-products-in-block
                                current binding-params
                                :min-uses   min-uses
                                :min-factors 2
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
                 (check-block-integrity current)
                 current))
             (next-block
               #+(or)(progn
                 (warn "Skipping second cse-factor-products-in-block")
                 after-prod)
               (let ((current after-prod))
                 (loop for iter from 1 to 10 do
                   (let ((next
                           (cse-factor-products-in-block
                            current binding-params
                            :min-uses    2
                            :min-factors 3
                            :min-size    1
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
                 (check-block-integrity current)
                 current))
             (after-copy
               (let ((cp (copy-propagate-optimization pass-id next-block binding-params :verbose verbose)))
                 (when verbose
                   (verbose-log verbose "~&[CSE pass ~D] copy-propagate after factoring~%" pass-id))
                 cp))
             (next-block after-copy)
             (after-symbol-list (collect-scalar-targets-in-block next-block)))
        (declare (ignore dummy))
        (when *debug*
          (handler-case
              (check-block-integrity next-block
                                    :label (format nil "cse-pass-~D" pass-id))
            (error (e)
              (format *error-output* "~&[CSE-DEBUG] ~A~%" e)
              (debug-cse-temp-trace next-block
                                    :label (format nil "cse-pass-~D" pass-id))
              (error e))))
        (verbose-log verbose "~&[CSE pass ~D] changed? ~A (temps ~D -> ~D)~%"
                     pass-id
                     (not (eq next-block current-block))
                     (length before-symbol-list) (length after-symbol-list))
        (when (and (eq next-block current-block)
                   (equal before-symbol-list after-symbol-list))
          (return current-block))
        (setf current-block next-block)))
    current-block))

(defun cse-block-multi (block binding-params &key (max-passes 5) (min-uses 2) (min-size 5)
                              (outer-symbols nil)
                              (verbose *verbose-optimization*))
  "Compatibility wrapper that runs CSE with a fresh, local pass-id counter."
  (Warn "cse-block-multi is a thin wrapper for debugging")
  (let ((counter (make-pass-id-counter)))
    (cse-block-multi-optimization counter block binding-params
                                  :max-passes max-passes
                                  :min-uses   min-uses
                                  :min-size   min-size
                                  :outer-symbols outer-symbols
                                  :verbose    verbose)))
