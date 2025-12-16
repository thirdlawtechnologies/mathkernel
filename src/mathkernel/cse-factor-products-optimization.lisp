(in-package :stmt-ir)


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
  ((env :initarg :env :accessor cse-ctx-env)
   (idx :initarg :idx :accessor cse-ctx-idx)
   (pass-id :initarg :pass-id :reader cse-ctx-pass-id)
   (min-uses :initarg :min-uses :reader cse-ctx-min-uses)
   (min-factors :initarg :min-factors :reader cse-ctx-min-factors)
   (min-size :initarg :min-size :reader cse-ctx-min-size)
   (cse-only :initarg :cse-only :reader cse-ctx-cse-only)
   (verbose :initarg :verbose :reader cse-ctx-verbose)
   (outer-symbols :initarg :outer-symbols :reader cse-ctx-outer-symbols)
   (acc :initarg :acc :accessor cse-ctx-acc)))

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

(defmethod clone-context ((operation (eql :cse-factor-products)) (ctx cse-factor-context) block)
  ;; Branch-local env/idx are copied; accumulator is shared so branches contribute
  ;; to the same analysis.
  (make-cse-factor-context
   :env (copy-binding-env (cse-ctx-env ctx) block)
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

(defun %record-products-from-expr (expr idx target acc min-factors env)
  (labels ((rec (sx)
             (when (and (consp sx) (eq (car sx) '*))
               (let ((factors (cdr sx)))
                 (when (>= (length factors) min-factors)
                   (push (make-instance 'product-descriptor
                                        :factors factors
                                        :counts (%counts-from-factors factors)
                                        :stmt-index idx
                                        :binding-env env
                                        :target target)
                         (cse-acc-products acc)))))
             (when (consp sx)
               (dolist (arg (cdr sx))
                 (rec arg)))))
    (rec (expr-ir:expr->sexpr expr))
    acc))

(defmethod on-statement ((operation (eql :cse-factor-products)) (ctx cse-factor-context) st)
  (let* ((env (cse-ctx-env ctx))
         (idx (incf (cse-ctx-idx ctx))))
    (etypecase st
      (assignment-statement
       (let ((tgt (stmt-target-name st)))
         (binding-env-add env tgt (make-env-entry idx (stmt-expression st)))
         (%record-products-from-expr (stmt-expression st) idx tgt
                                     (cse-ctx-acc ctx)
                                     (cse-ctx-min-factors ctx)
                                     env)
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
       (values st ctx)))))

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


;;; ----------------------------------------------------------------------
;;; Walker-based product factoring (rewrite)
;;; ----------------------------------------------------------------------

(defclass cse-factor-rewrite-context (walk-context)
  ((env :initarg :env :accessor cse-fr-env)
   (idx :initarg :idx :accessor cse-fr-idx)
   (temp :initarg :temp :reader cse-fr-temp)
   (temp-expr :initarg :temp-expr :reader cse-fr-temp-expr)
   (cand-counts :initarg :cand-counts :reader cse-fr-cand-counts)
   (counts-from-factors :initarg :counts-from-factors :reader cse-fr-counts-from-factors)
   (temp-insert :initarg :temp-insert :reader cse-fr-temp-insert)
   (changed :initarg :changed :accessor cse-fr-changed)))

(defmethod clone-context ((operation (eql :cse-factor-rewrite)) (ctx cse-factor-rewrite-context) block)
  (make-instance 'cse-factor-rewrite-context
                 :env (copy-binding-env (cse-fr-env ctx) block)
                 :idx -1
                 :temp (cse-fr-temp ctx)
                 :temp-expr (cse-fr-temp-expr ctx)
                 :cand-counts (cse-fr-cand-counts ctx)
                 :counts-from-factors (cse-fr-counts-from-factors ctx)
                 :temp-insert (cse-fr-temp-insert ctx)
                 :changed (cse-fr-changed ctx)))

(defun factor-temp-available-p (ctx temp idx)
  "Return T if TEMP is already bound in the current env or will be inserted in this block before IDX."
  (binding-env-lookup (cse-fr-env ctx) temp)
  #+(or)(or (binding-env-lookup (cse-fr-env ctx) temp)
      (let ((bucket (temp-inserts-lookup (cse-fr-temp-insert ctx)
                                         (binding-env-block (cse-fr-env ctx)))))
        (when bucket
          (maphash
           (lambda (ins-idx stmts)
             (when (and (< ins-idx idx)
                        (let ((lst (if (listp stmts) stmts (list stmts))))
                          (some (lambda (s) (eq (stmt-target-name s) temp)) lst)))
               (return-from factor-temp-available-p t)))
           (%block-temp-inserts-table bucket))
          nil))))

(defun %rewrite-factor-sexpr (sexpr cand-counts temp counts-from-factors target avail-fn)
  (labels ((multiset-subset-p (cand prod)
             (let ((ok t))
               (maphash (lambda (k v)
                          (unless (<= v (gethash k prod 0))
                            (setf ok nil)))
                        cand)
               ok))
           (rewrite (sx)
             (cond
               ((and (consp sx) (eq (car sx) '*))
                (let* ((factors (cdr sx))
                       (prod-counts (funcall counts-from-factors factors)))
                  (if (multiset-subset-p cand-counts prod-counts)
                      ;; Remove one occurrence of each factor in CAND-COUNTS, preserving order.
                      (let ((remaining '())
                            (needed (let ((ht (make-hash-table :test #'equal)))
                                      (maphash (lambda (k v) (setf (gethash k ht) v))
                                               cand-counts)
                                      ht)))
                        (dolist (f factors)
                          (let ((n (gethash f needed 0)))
                            (if (> n 0)
                                (setf (gethash f needed) (1- n))
                                (push (rewrite f) remaining))))
                        (setf remaining (nreverse remaining))
                        (let ((replacement (if (and target (eq target temp)) sx temp)))
                          (if (and replacement (funcall avail-fn replacement))
                              (cons '* (cons replacement remaining))
                              (cons '* (mapcar #'rewrite factors)))))
                      (cons '* (mapcar #'rewrite factors)))))
               ((consp sx)
                (cons (car sx) (mapcar #'rewrite (cdr sx))))
               (t sx))))
    (if (and target (eq target temp))
        sexpr
        (rewrite sexpr))))

(defmethod on-statement ((operation (eql :cse-factor-rewrite)) (ctx cse-factor-rewrite-context) st)
  (declare (optimize (debug 3)))
  (etypecase st
    (assignment-statement
     (let* ((env (cse-fr-env ctx))
            (idx (incf (cse-fr-idx ctx)))
            (tgt (stmt-target-name st))
            (expr (stmt-expression st))
            (new-expr (if (eq tgt (cse-fr-temp ctx))
                          expr
                          (let* ((sexpr (expr-ir:expr->sexpr expr))
                                 (counts (cse-fr-cand-counts ctx))
                                 (temp (cse-fr-temp ctx))
                                 (factors (cse-fr-counts-from-factors ctx))
                                 (rewrite (%rewrite-factor-sexpr sexpr
                                                                 counts
                                                                 temp
                                                                 factors
                                                                 tgt
                                                                 (lambda (tt)
                                                                   (factor-temp-available-p ctx tt (cse-fr-idx ctx)))))
                                 (nexpr (expr-ir:sexpr->expr-ir rewrite)))
                            nexpr)))
            (new-st (if (eq new-expr expr) st (make-assignment-stmt tgt new-expr :preserve-anchor-of st
                                                                                 ))))
       (binding-env-add env tgt (make-env-entry idx new-expr))
       (when (not (eq new-st st))
         (setf (car (cse-fr-changed ctx)) t))
       (values new-st ctx)))
    (raw-c-statement
     (values st ctx))
    (if-statement
     (values st ctx))
    (block-statement
     (values st ctx))
    (t
     (values st ctx))))

(defmethod rewrite-block ((operation (eql :cse-factor-rewrite)) (ctx cse-factor-rewrite-context) statements)
  (let* ((out '())
         (idx -1)
         (bucket (temp-inserts-lookup (cse-fr-temp-insert ctx)
                                      (binding-env-block (cse-fr-env ctx)))))
    (flet ((handle-stmt (stmt &key changed)
             (multiple-value-bind (new-st new-ctx)
                 (on-statement operation ctx stmt)
               (setf ctx new-ctx)
               (push new-st out)
               (when changed (setf (car (cse-fr-changed ctx)) t)))))
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




(defun %select-factor-candidate (products min-uses min-factors min-size cse-only verbose pass-id)
  (labels ((multiset-subset-p (cand-counts prod-counts)
             (let ((ok t))
               (maphash (lambda (k v)
                          (unless (<= v (gethash k prod-counts 0))
                            (setf ok nil)))
                        cand-counts)
               ok))
           (common-factors (ci cj)
             (let ((result '()))
               (maphash
                (lambda (k v1)
                  (let ((v2 (gethash k cj 0)))
                    (when (> (min v1 v2) 0)
                      (dotimes (_ (min v1 v2))
                        (push k result)))))
                ci)
               (sort result #'string< :key (lambda (sexpr) (format nil "~S" sexpr)))))
           (candidate-list (prods)
             (let ((cand-lists '()))
               (let ((plist (coerce prods 'vector))
                     (m (length prods)))
                 (loop for i from 0 below (1- m) do
                   (let* ((pi1 (aref plist i))
                          (ci (product-descriptor-counts pi1)))
                     (loop for j from (1+ i) below m do
                       (let* ((pj (aref plist j))
                              (cj (product-descriptor-counts pj))
                              (common (common-factors ci cj)))
                         (when (and (>= (length common) min-factors)
                                    (or (not cse-only)
                                        (every #'cse-temp-symbol-p common)))
                           (push common cand-lists)))))))
               (setf cand-lists (remove-duplicates cand-lists :test #'equal))
               (sort cand-lists #'string< :key (lambda (cand) (format nil "~S" cand)))))
           (score-candidate (cand)
             (let* ((cand-counts (counts-from-factors cand))
                    (uses '())
                    (size (sexpr-size (cons '* cand))))
               (dolist (prod products)
                 (when (multiset-subset-p cand-counts (product-descriptor-counts prod))
                   (push prod uses)))
               (values (length uses)
                       (* (- (length uses) 1) size)
                       uses
                       cand-counts
                       size))))
    (let* ((candidates (candidate-list products))
           (best-cand nil)
           (best-uses nil)
           (best-cand-counts nil)
           (best-score 0))
      (when (null candidates)
        (verbose-log verbose "~&[CSE-FACT pass ~D] no factor candidates found~%" pass-id)
        (return-from %select-factor-candidate nil))
      (dolist (cand candidates)
        (multiple-value-bind (use-count score uses cand-counts size)
            (score-candidate cand)
          (when (and (>= use-count min-uses)
                     (>= size min-size))
            (cond
              ((> score best-score)
               (setf best-score score
                     best-cand cand
                     best-uses uses
                     best-cand-counts cand-counts))
              ((and (= score best-score) best-cand
                    (string< (format nil "~S" cand)
                             (format nil "~S" best-cand)))
               (setf best-cand cand
                     best-uses uses
                     best-cand-counts cand-counts))))))
      (when (null best-cand)
        (verbose-log verbose "~&[CSE-FACT pass ~D] no usable factor found~%" pass-id)
        (return-from %select-factor-candidate nil))
      (values best-cand best-uses best-cand-counts))))

(defun %build-insert-map (uses vars block pass-id)
  (let ((block->data (make-hash-table :test #'eq)))
    (dolist (use uses)
      (let* ((use-env (product-descriptor-binding-env use))
             (dep-envs (loop for v in vars
                             for (_entry env-found) = (multiple-value-list (binding-env-lookup use-env v))
                             if env-found
                               collect env-found
                             else
                               do (error "Could not find ~s in binding-env ~s" v use-env)))
             (all-use-envs (or dep-envs (list use-env)))
             (insert-env (reduce #'binding-env-later all-use-envs :initial-value use-env))
             (insert-block (or (binding-env-block insert-env) block))
             (data (gethash insert-block block->data)))
        (dolist (v vars)
          (multiple-value-bind (_ env-found) (binding-env-lookup insert-env v)
            (declare (ignore _))
            (unless env-found
              (error "Could not find dep ~S in insert-env ~S" v insert-env))))
        (if data
            (destructuring-bind (env . uses-list) data
              (setf (gethash insert-block block->data)
                    (cons (binding-env-later env insert-env)
                          (cons use uses-list))))
            (setf (gethash insert-block block->data)
                  (cons insert-env (list use))))))
    ;; Consolidate dominated blocks, leave siblings intact.
    (let ((blocks (loop for b being the hash-keys of block->data collect b)))
      (dolist (b1 blocks)
        (dolist (b2 blocks)
          (when (and b1 b2 (not (eq b1 b2)) (block-dominates-p b1 b2))
            (let* ((data1 (gethash b1 block->data))
                   (data2 (gethash b2 block->data)))
              (when (and data1 data2)
                (destructuring-bind (env1 . uses1) data1
                  (destructuring-bind (_env2 . uses2) data2
                    (declare (ignore _env2))
                    (setf (gethash b1 block->data)
                          (cons env1 (nconc uses1 uses2)))
                    (remhash b2 block->data)))))))))
    block->data))

(defun %emit-temp-inserts (block->data temp temp-expr vars block)
  (let ((temp-inserts (make-temp-inserts))
        (planned nil))
    (maphash
     (lambda (insert-block data)
       (destructuring-bind (insert-env . uses) data
         (let ((limit
                 (or (loop for use in uses
                           for env = (product-descriptor-binding-env use)
                           for idx = (product-descriptor-stmt-index use)
                           for pos = (if (eq env insert-env)
                                         idx
                                         (block-entry-index-for-descendant insert-block
                                                                           (binding-env-block env)))
                           when pos minimize pos)
                     (length (block-statements insert-block))))
               (local-max -1))
           (dolist (v vars)
             (multiple-value-bind (entry env-found) (binding-env-lookup insert-env v)
               (when (and entry env-found (eq env-found insert-env)
                          (integerp (env-entry-index entry)))
                 (setf local-max (max local-max (env-entry-index entry))))))
           (let* ((min-insert (max (1+ local-max) 0))
                  (insert-idx (and (<= min-insert limit) limit)))
             (when insert-idx
               (multiple-value-bind (_ existing-env) (binding-env-lookup insert-env temp)
                 (declare (ignore _))
                 (when existing-env
                   (error "cse-factor plan: temp ~S already defined in insert-env ~S"
                          temp insert-env)))
               (let* ((bucket (or (temp-inserts-lookup temp-inserts insert-block)
                                  (let ((b (make-block-temp-inserts)))
                                    (temp-insert-add temp-inserts insert-block b)
                                    b)))
                      (existing (block-temp-inserts-lookup bucket insert-idx)))
                 (when existing
                   (error "cse-factor plan: duplicate temp slot ~S for ~S in block ~S"
                          insert-idx temp insert-block))
                 (block-temp-insert-add temp-inserts bucket insert-idx
                                        (list (make-assignment-stmt temp temp-expr)))
                 (setf planned t)))))))
     block->data)
    (when planned
      ;; sanity check: ensure no duplicate CSE temp targets already in BLOCK
      (let ((seen (make-hash-table :test #'eq)))
        (dolist (st (block-statements block))
          (when (typep st 'assignment-statement)
            (let ((tgt (stmt-target-name st)))
              (when (cse-temp-symbol-p tgt)
                (if (gethash tgt seen)
                    (error "cse-factor plan: temp ~S already defined in block ~S"
                           tgt block)
                    (setf (gethash tgt seen) t))))))))
    temp-inserts))

(defun plan-factor-temp (products env-product->sym def-env block
                         &key min-uses min-factors min-size pass-id cse-only verbose)
  "Choose a factor candidate and plan temp insertion. Returns
(values make-assign temp temp-expr cand-counts temp-inserts) or NIL if no change."
  (declare (optimize (debug 3)))
  (multiple-value-bind (best-cand best-uses best-cand-counts)
      (%select-factor-candidate products min-uses min-factors min-size cse-only verbose pass-id)
    (unless best-cand
      (return-from plan-factor-temp nil))
    (let* ((cand-sexpr (cons '* best-cand))
           (temp-expr (expr-ir:sexpr->expr-ir cand-sexpr))
           (vars (expr-ir:expr-free-vars temp-expr))
           (start-index (next-cse-temp-index-for-pass block pass-id))
           (existing-temp (gethash cand-sexpr env-product->sym)))
      (if existing-temp
          (progn
            (setf (gethash cand-sexpr env-product->sym) existing-temp)
            nil)
          (let* ((temp (make-cse-temp-symbol pass-id (1+ start-index) :suffix t))
                 (block->data (%build-insert-map best-uses vars block pass-id))
                 (temp-inserts (%emit-temp-inserts block->data temp temp-expr vars block)))
            (setf (gethash cand-sexpr env-product->sym) temp)
            (values t temp temp-expr best-cand-counts temp-inserts))))))

(defun rewrite-factor-block (block def-env temp temp-expr cand-counts temp-inserts)
  (declare (optimize (debug 3)))
  (let ((rw-env (make-binding-env block)))
    (map-binding-env
     (lambda (sym entry _)
       (declare (ignore _))
       (when (typep entry 'outer-env-entry)
         (binding-env-add rw-env sym entry)))
     def-env)
    (let ((ctx (make-instance 'cse-factor-rewrite-context
                              :env rw-env
                              :idx -1
                              :temp temp
                              :temp-expr temp-expr
                              :cand-counts cand-counts
                              :counts-from-factors #'counts-from-factors
                              :temp-insert temp-inserts
                              :changed (list nil))))
      (multiple-value-bind (new-block ctx-out)
          (walk-block-with-context :cse-factor-rewrite ctx block)
        (declare (ignore ctx-out))
        (check-block-integrity new-block)
        (if (car (cse-fr-changed ctx))
            new-block
            block)))))

(defun cse-factor-products-in-block (block binding-params
                                      &key (min-uses 2) (min-factors 2) (min-size 2) (pass-id 1)
                                      (cse-only nil) (defined-env (make-binding-env block))
                                      (outer-symbols nil)
                                      (verbose *verbose-optimization*))
  "Walker-based product factoring. Identifies common sub-products across multiplicative
expressions and factors them into temps CSE_P<pass-id>_T<n> using the walker
infrastructure."
  (declare (optimize (debug 3)))
  (unless (typep block 'block-statement)
    (error "cse-factor-products-in-block: expected BLOCK-STATEMENT, got ~S" block))
  ;; Fail fast on duplicate CSE temp definitions along any executable path.
  ;; Duplicates confined to sibling branches are allowed; redefinitions on the same
  ;; path (including after branches rejoin) are not.
  (labels ((check-stmt-list (stmts defined)
             (dolist (st stmts defined)
               (etypecase st
                 (assignment-statement
                  (let ((tgt (stmt-target-name st)))
                    (when (and (cse-temp-symbol-p tgt)
                               (member tgt defined :test #'eq))
                      (error "cse-factor-products-in-block: temp ~S already defined in input block ~S"
                             tgt block))
                    (when (cse-temp-symbol-p tgt)
                      (push tgt defined))))
                 (block-statement
                  (setf defined (check-stmt-list (block-statements st) defined)))
                 (if-statement
                  (let* ((then-def (check-stmt-list (and (if-then-block st)
                                                        (block-statements (if-then-block st)))
                                                   (copy-list defined)))
                         (else-def (check-stmt-list (and (if-else-block st)
                                                        (block-statements (if-else-block st)))
                                                   (copy-list defined))))
                    ;; After the branch, consider any temp defined in either branch as defined.
                    (setf defined (union then-def else-def :test #'eq))))
                 (t nil)))))
    (check-stmt-list (block-statements block) '()))
  ;; Also run the SSA/ordering check up front to catch any temp reuse before planning.
  (ignore-errors
    (check-block-integrity block :label "pre-cse-factor"))
  (let* ((outer-symbols (or outer-symbols binding-params))
         (def-env (copy-binding-env defined-env block))
         (acc (make-instance 'cse-factor-accumulator)))
    ;; Seed outer symbols so dependency lookups succeed.
    (dolist (sym outer-symbols)
      (unless (binding-env-lookup def-env sym)
        (binding-env-add def-env sym
                         (make-instance 'outer-env-entry :index -1 :expr nil))))
    (collect-products-in-block block def-env acc pass-id min-uses min-factors min-size cse-only outer-symbols verbose)
    ;; Reuse any existing product temps already present in the env.
    (map-binding-env
     (lambda (sym entry _env-found)
       (declare (ignore _env-found))
       (when (and (typep entry 'env-entry)
                  (env-entry-expr entry))
         (let ((sx (expr-ir:expr->sexpr (env-entry-expr entry))))
           (when (and (consp sx) (eq (car sx) '*))
             (setf (gethash sx (cse-acc-env-product->sym acc)) sym)))))
     def-env)
    (let* ((products (nreverse (cse-acc-products acc)))
           (env-product->sym (cse-acc-env-product->sym acc)))
      (when (< (length products) 2)
        (verbose-log verbose "~&[CSE-FACT pass ~D] not enough products (~D)~%"
                     pass-id (length products))
        (return-from cse-factor-products-in-block block))
      (multiple-value-bind (make-assign temp temp-expr cand-counts temp-inserts)
          (plan-factor-temp products env-product->sym def-env block
                            :min-uses min-uses
                            :min-factors min-factors
                            :min-size min-size
                            :pass-id pass-id
                            :cse-only cse-only
                            :verbose verbose)
        (let ((new-block (if (null make-assign)
                             block
                             (rewrite-factor-block block def-env temp temp-expr cand-counts temp-inserts))))
          (check-block-integrity new-block)
          new-block)))))
