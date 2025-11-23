(in-package :opt-exp)

;;;; ============================================================
;;;; Core Classes
;;;; ============================================================

(defparameter *pack* nil)

(defmacro with-pack ((pack) &body body)
  `(let ((*pack* ,pack))
     ,@body))

(defclass node ()
  ((prev :initarg :prev :reader prev)
   (next :initarg :next :accessor next)
   (data :initarg :data :reader data)))

(defmethod print-object ((obj node) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~s" (data obj))))

(defun make-node (&key prev next data)
  (make-instance 'node :prev prev :next next :data data))

(defclass rule ()
  ((lhs :initarg :lhs :reader rule-lhs)
   (rhs :initarg :rhs :reader rule-rhs)))

(defun rule (lhs rhs)
  (make-instance 'rule :lhs lhs :rhs rhs))

(defclass pack ()
  ((name   :initarg :name   :reader name)
   (dependent-variables :initarg :dependent-variables :reader dependent-variables)
   (constants :initarg :constants :reader constants)
   (base-variables :initarg :base-variables :reader base-variables)
   (outputs :initform nil :initarg :outputs :accessor outputs)
   (rules  :initform nil :initarg :rules :accessor rules)
   (rule-table :initform (make-hash-table) :reader rule-table))
   )

(defun make-pack (&rest args &key name outputs rules base-variables constants dependent-variables)
    (apply 'make-instance 'pack args))

(defun append-rule (rule &key (pack *pack*))
  (setf (rules pack) (append (rules pack) (list rule))
        (gethash (rule-rhs rule) (rule-table pack)) rule)
  rule)

(defun lookup-rule (pack rhs &key errorp)
  (let ((rule (gethash rhs (rule-table pack))))
    (if (null rule)
        (if errorp
            (error "Could not find rule")
            nil)
        rule)))

(defmethod print-object ((obj rule) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~s -> ~s" (rule-lhs obj) (rule-rhs obj))))

(defclass ccode ()
  ((code :initarg :code :reader code)))

(defun ccode (&rest strs)
  (make-instance 'ccode :code (format nil "~{~a~}" strs)))

(defmethod print-object ((obj ccode) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~s" (code obj))))

(defun append-ccode (ccode &key (pack *pack*))
  (setf (rules pack) (append (rules pack) (list ccode))))

(defclass accumulate ()
  ((code :initarg :code :reader code)
   (var :initarg :var :reader var)))

(defun accumulate (str var)
  (make-instance 'accumulate :code str :var var))

(defmethod print-object ((obj accumulate) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~s ~s" (code obj) (var obj))))

(defun append-accumulate (accumulate &key (pack *pack*))
  (setf (rules pack) (append (rules pack) (list accumulate))))

;;;; ============================================================
;;;; Expression Utilities (plain sexp expressions)
;;;; ============================================================

(defun expr-atom-p (x) (or (symbolp x) (numberp x)))
(defun expr-call-p (x) (and (consp x) (symbolp (car x))))
(defun expr-op   (x) (if (expr-call-p x) (car x) nil))
(defun expr-args (x) (if (expr-call-p x) (cdr x) nil))

(defun expr-equal (a b) (equal a b))

(defun tree-depth (expr)
  (if (expr-atom-p expr)
      1
      (1+ (reduce #'max (mapcar #'tree-depth (expr-args expr)) :initial-value 0))))

(defun collect-subexprs (expr)
  (labels ((walk (e)
             (if (expr-atom-p e) (list e)
                 (cons e (mapcan #'walk (expr-args e))))))
    (walk expr)))


;;;; ============================================================
;;;; Rule substitution
;;;; ============================================================

(defun replace-once (old new expr)
  (cond
    ((expr-equal expr old) new)
    ((expr-atom-p expr) expr)
    (t (let* ((op (expr-op expr))
              (args (expr-args expr))
              (new-args (mapcar (lambda (x) (replace-once old new x)) args)))
         (if (equal args new-args) expr (cons op new-args))))))

(defgeneric apply-rule-once (rule expr))

(defmethod apply-rule-once ((rule ccode) expr)
  nil)

(defmethod apply-rule-once ((rule accumulate) expr)
  nil)

(defmethod apply-rule-once ((rule rule) expr)
  (replace-once (rule-lhs rule) (rule-rhs rule) expr))

(defun apply-rules (rules expr)
  (labels ((sstep (e)
             (let ((out (reduce (lambda (acc r) (apply-rule-once r acc))
                                rules :initial-value e)))
               (values out (not (expr-equal out e))))))
    (loop
      with changed = nil
      for (res ch?) = (multiple-value-list (sstep expr))
      do (setf expr res)
      (if ch? (setf changed t) (return (values res changed))))))


;;;; Debug view
(defun expr->sexp (x) x)
(defun rule->sexp (r) (list (rule-lhs r) :-> (rule-rhs r)))


;;;; ============================================================
;;;; Simplifier (local algebra)
;;;; ============================================================

(defun simplify-expr (expr)
  (cond
    ((expr-atom-p expr) expr)

    ;; expt simplifications
    ((and (expr-call-p expr) (eq (expr-op expr) 'expt))
     (destructuring-bind (base pow) (expr-args expr)
       (let ((base (simplify-expr base))
             (pow  (simplify-expr pow)))
         (cond ((and (numberp pow) (= pow 0)) 1)
               ((and (numberp pow) (= pow 1)) base)
               ((and (numberp pow) (= pow 2)) (list '* base base))
               ((and (numberp pow) (= pow 3)) (list '* base base base))
               (t (list 'expt base pow))))))

    ;; multiplication
    ((and (expr-call-p expr) (eq (expr-op expr) '*))
     (let* ((args (mapcar #'simplify-expr (expr-args expr)))
            (nums (remove-if-not #'numberp args))
            (others (remove-if #'numberp args))
            (prod (reduce #'* nums :initial-value 1)))
       (when (member 0 nums) 0)
       (cond ((and (= prod 1) (= (length others) 1)) (first others))
             ((= prod 1) (cons '* others))
             ((null others) prod)
             (t (cons '* (cons prod others))))))

    ;; addition
    ((and (expr-call-p expr) (eq (expr-op expr) '+))
     (let* ((args (mapcar #'simplify-expr (expr-args expr)))
            (nums (remove-if-not #'numberp args))
            (others (remove-if #'numberp args))
            (sum (reduce #'+ nums :initial-value 0)))
       (cond ((and (= sum 0) (= (length others) 1)) (first others))
             ((= sum 0) (cons '+ others))
             ((null others) sum)
             (t (append (cons '+ others) (list sum))))))

    ;; generic
    (t (cons (expr-op expr) (mapcar #'simplify-expr (expr-args expr))))))

(defun simplify-rules (rules)
  (mapcar (lambda (r) (rule (simplify-expr (rule-lhs r)) (rule-rhs r)))
          rules))


;;;; ============================================================
;;;; Common Subexpression Elimination (CSE)
;;;; ============================================================

(defun frequency-table (items)
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (i items) (incf (gethash i ht 0)))
    ht))

(defun cse-expr (expr rules next-index)
  (let* ((subs (collect-subexprs expr))
         (freq (frequency-table subs))
         (cand (remove-if (lambda (x)
                            (or (expr-atom-p x) (= (gethash x freq) 1)))
                          (remove-duplicates subs :test #'equal)))
         (new-rules '()))
    (setf cand (sort cand #'> :key #'tree-depth))
    (dolist (sub cand)
      (let ((temp (intern (format nil "TZZ~A" next-index) :keyword)))
        (incf next-index)
        (push (rule sub temp) new-rules)
        (setf expr (apply-rule-once (rule sub temp) expr))))
    (values expr (append rules (nreverse new-rules)) next-index)))

(defun collect-terms-for-one (name expr rules next-index)
  (multiple-value-bind (expr1 ignore) (apply-rules rules expr)
    (declare (ignore ignore))
    (multiple-value-bind (expr2 rules2 next2) (cse-expr expr1 rules next-index)
      (values (append rules2 (list (rule expr2 name))) next2))))


(defun eliminate-trivial-rules (rules outputs)
  (let ((kept '())
        (subs '()))
    (dolist (r rules)
      (cond
        ((typep r 'ccode) nil)
        ((typep r 'accumulate) nil)
        (t (let ((lhs (rule-lhs r)))
             (if (and (symbolp lhs) (not (member lhs outputs :test #'eq)))
                 (push (rule lhs (rule-rhs r)) subs)
                 (push r kept))))))
    (if subs
        (values
         (mapcar (lambda (r)
                   (multiple-value-bind (new ignore)
                       (apply-rules subs (rule-lhs r))
                     (declare (ignore ignore))
                     (rule new (rule-rhs r))))
                 (nreverse kept))
         t)
        (values (nreverse kept) nil))))


(defun collect-terms (rule-list outputs)
  (let ((acc '())
        (changed nil)
        (idx 1))
    (dolist (r rule-list)
      (cond
        ((typep r 'ccode)
         (setf acc (append acc (list r)))
         (incf idx)
         )
        ((typep r 'accumulate)
         (setf acc (append acc (list r)))
         (incf idx)
         )
        (t (multiple-value-bind (new idx2)
               (collect-terms-for-one (rule-rhs r) (rule-lhs r) acc idx)
             (when (> (- (length new) (length acc) 1) 0) (setf changed t))
             (setf acc new idx idx2)))))
    (multiple-value-bind (nt ch2) (eliminate-trivial-rules acc outputs)
      (values nt (or changed ch2)))))


;;;; ============================================================
;;;; Pair factoring for associative ops (* and +)
;;;; ============================================================

(defun my-subsetp (xs ys)
  (every (lambda (x) (member x ys :test #'equal)) xs))

(defun assoc-terms>2 (rules op)
  (remove-if-not
   (lambda (r)
     (and (expr-call-p (rule-lhs r))
          (eq (expr-op (rule-lhs r)) op)
          (> (length (expr-args (rule-lhs r))) 2)))
   rules))

(defun extract-factor-lists (rules op)
  (mapcar #'expr-args (mapcar #'rule-lhs (assoc-terms>2 rules op))))

(defun possible-pairs (list)
  (let ((pairs '()))
    (dotimes (i (1- (length list)))
      (dotimes (j (- (length list) (1+ i)))
        (push (list (nth i list) (nth (+ i j 1) list)) pairs)))
    (remove-duplicates pairs :test #'equal)))

(defun most-common-pair (rules op)
  (let* ((factors (extract-factor-lists rules op)))
    (when factors
      (let* ((pairs (remove-duplicates (mapcan #'possible-pairs factors) :test #'equal))
             (counts (mapcar (lambda (p)
                               (list (count-if (lambda (fl)
                                                 (and (member (first p) fl :test #'equal)
                                                      (member (second p) fl :test #'equal)))
                                               factors)
                                     p))
                             pairs))
             (best (car (last (sort counts #'< :key #'first)))))
        (and best (> (first best) 1) best)))))

(defun assoc-substitute (lhs vars new op)
  (let* ((keep (remove-if (lambda (x) (member x vars :test #'equal)) (expr-args lhs)))
         (newargs (append keep (list new))))
    (cons op newargs)))

(defun assoc-simplify-pair (rules vars op idx)
  (let* ((temp (intern (format nil "TZZ~A" idx) :keyword))
         (idx2 (1+ idx))
         (out '())
         (pos -1)
         (n 0))
    ;; find first occurrence
    (setf pos (loop for r in rules for k from 0
                    when (and (expr-call-p (rule-lhs r))
                              (eq (expr-op (rule-lhs r)) op)
                              (my-subsetp vars (expr-args (rule-lhs r))))
                      do (return k)
                    finally (return -1)))
    (when (< pos 0) (error "not found"))
    (dolist (r rules)
      (if (and (expr-call-p (rule-lhs r))
               (eq (expr-op (rule-lhs r)) op)
               (my-subsetp vars (expr-args (rule-lhs r))))
          (progn
            (incf n)
            (push (rule (assoc-substitute (rule-lhs r) vars temp op) (rule-rhs r)) out))
          (push r out)))
    (let* ((ins (rule (cons op vars) temp))
           (rev (nreverse out)))
      (values (append (subseq rev 0 pos) (list ins) (subseq rev pos)) idx2 n))))

(defun associatives-simplify (rules op outputs)
  (let ((cur rules) (idx (1+ (length rules))) (ch nil))
    (loop
      (let ((best (most-common-pair cur op)))
        (unless best (return))
        (destructuring-bind (count vars) best
          (unless (> count 1) (return))
          (multiple-value-bind (n2 idx2 subs)
              (assoc-simplify-pair cur vars op idx)
            (declare (ignore subs))
            (setf cur n2 idx idx2 ch t)))))
    (multiple-value-bind (nt ch2) (eliminate-trivial-rules cur outputs)
      (values nt (or ch ch2)))))

(defun times-simplify (rules outputs) (associatives-simplify rules '* outputs))
(defun plus-simplify  (rules outputs) (associatives-simplify rules '+ outputs))


;;;; ============================================================
;;;; Topological ordering
;;;; ============================================================

(defun collect-symbols (expr)
  (cond
    ((symbolp expr) (list expr))
    ((numberp expr) '())
    ((consp expr) (mapcan #'collect-symbols (cdr expr)))
    (t '())))

(defun topo-order-rules (rules)
  (let* ((rhs-list (mapcar #'rule-rhs rules))
         (defs (lambda (s) (member s rhs-list :test #'eq)))
         (deps (make-hash-table :test #'eq))
         (rev  (make-hash-table :test #'eq))
         (ind (make-hash-table :test #'eq))
         (by  (make-hash-table :test #'eq)))

    ;; index nodes
    (dolist (r rules)
      (setf (gethash (rule-rhs r) by) r
            (gethash (rule-rhs r) ind) 0))

    ;; edges
    (dolist (r rules)
      (let* ((v (rule-rhs r))
             (syms (remove-duplicates (collect-symbols (rule-lhs r)) :test #'eq))
             (us (remove-if-not defs syms)))
        (setf (gethash v deps) us)
        (dolist (u us)
          (push v (gethash u rev))
          (incf (gethash v ind)))))

    ;; Kahn stable
    (let ((queue (remove-if-not (lambda (s) (= (gethash s ind) 0)) rhs-list))
          (out '()))
      (loop while queue do
        (let* ((v (pop queue))
               (r (gethash v by)))
          (when r (push r out))
          (dolist (w (gethash v rev))
            (decf (gethash w ind))
            (when (= (gethash w ind) 0)
              (setf queue (append queue (list w)))))))
      (nreverse out))))

;;;; ============================================================
;;;; Gradient
;;;; ============================================================

(defun append-derivative (pack expression exp-var var)
  (declare (optimize (debug 3)))
  (multiple-value-bind (deriv partials dependent-variables)
      (deriv pack expression var)
    (when partials
      (loop for partial in partials
            for dependent-variable in dependent-variables
            for sub-expr = (rule-lhs (lookup-rule pack dependent-variable))
            do (append-derivative pack sub-expr dependent-variable var)))
    (let ((rule (append-rule (rule deriv (make-partial-symbol exp-var var)))))
      (cons (rule-rhs rule) var))))

(defun append-gradient-and-force (pack raw-energy-fn energy-name raw-var-names)
  (loop for raw-var in raw-var-names
        collect (destructuring-bind (var kk ii of)
               raw-var
             (append-derivative pack raw-energy-fn energy-name var))))


(defun append-gradient-force-and-hessian (pack hessian-structure raw-energy-fn raw-energy-name raw-var-names)
  (with-pack (pack)
    (append-ccode (ccode "#ifdef " (name pack) "_CALC_FORCE //["))
    (append-ccode (ccode "if ( calcForce ) {"))
    (let ((partial-derivatives (append-gradient-and-force pack raw-energy-fn raw-energy-name raw-var-names)))
      (warn "Check partial-derivatives ~s" partial-derivatives))
    (append-ccode (ccode "#ifdef " (name pack) "_CALC_DIAGONAL_HESSIAN //["))
    #|
    AppendTo[be,CCode["if ( calcDiagonalHessian ) {"]]; ;
    AppendChainHessianDiagonalElements[macroPrefix,be,outputs,hessianStructure,energyFn,chainVar,chainFn,varNames]; ;
    AppendTo[be,CCode["#ifdef "<>macroPrefix<>"_CALC_OFF_DIAGONAL_HESSIAN //["]]; ;
    AppendTo[be, CCode["if ( calcOffDiagonalHessian ) {" ]]; ;
    AppendChainHessianOffDiagonalElements[macroPrefix,be,outputs,hessianStructure,energyFn,chainVar,chainFn,varNames]; ;
    AppendTo[be, CCode["} /*if calcOffDiagonalHessian */ "]]; ;
    AppendTo[be, CCode["#endif /* "<>macroPrefix<>"_CALC_OFF_DIAGONAL_HESSIAN ]*/"]]; ;
    AppendTo[be,CCode["} /*calcDiagonalHessian */"]]; ;
    AppendTo[be, CCode["#endif /* "<>macroPrefix<>"_CALC_DIAGONAL_HESSIAN ]*/"]]; ;
    AppendTo[be,CCode["} /*calcForce */"]]; ;
    AppendTo[be, CCode["#endif /* "<>macroPrefix<>"_CALC_FORCE ]*/"]]; ;
    |#))

  

;;;; ============================================================
;;;; Orchestrator (returns new pack)
;;;; ============================================================

(defun pack-optimize (input-pack &key (short-cycles t) skip-times skip-plus)
  (declare (optimize (debug 3)))
  (let* ((outputs (outputs input-pack))
         (r0 (rules input-pack)))

    (labels ((sstep (r)
               (multiple-value-bind (r2 ch) (collect-terms r outputs)
                 (declare (ignore ch))
                 (break "Check r2 ch")
                 (simplify-rules r2))))

      (let* ((r1 (sstep r0))
             (r2 (if short-cycles r1 (sstep r1)))
             (r3 (if short-cycles r2 (sstep r2)))
             (r4 (if short-cycles r3 (sstep r3)))
             (r5 (if skip-times r4 (simplify-rules (car (multiple-value-list (times-simplify r4 outputs))))))
             (r6 (if short-cycles r5 (sstep r5)))
             (r7 (if skip-plus r6 (simplify-rules (car (multiple-value-list (plus-simplify r6 outputs))))))
             (r8 (simplify-rules r7))
             (rt (topo-order-rules r8)))
        (break "Check vars")
        (make-pack :name (name input-pack)
                   :base-variables (base-variables input-pack)
                   :constants (constants input-pack)
                   :dependent-variables (dependent-variables input-pack)
                   :outputs outputs :rules rt)))))



