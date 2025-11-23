(in-package :opt-exp)


(defun make-partial-symbol (sym var)
  "Make a symbol representing ∂SYM/∂VAR, e.g. R_X."
  (let* ((pkg (symbol-package sym))
         (name (format nil "~a_~a" sym var)))
    (intern name (or pkg *package*)))
  )


(defun deriv-atom (pack expr var)
  "Derivative of an atomic EXPR with respect to VAR.

Rules:
- inner-deriv(var)/dvar = 1
- if EXPR is declared dependent on a list including VAR:
    inner-deriv(EXPR)/dvar = partial symbol (e.g., R_X)
- else: constant ⇒ 0."
  (cond
    ;; same variable
    ((eq expr var) (values 1 nil))

    ;; declared dependent on multiple variables
    ((let ((deps (dependent-variables pack)))
       (and deps (member expr deps :test #'eq)))
     (values (make-partial-symbol expr var) t))

    ;; otherwise treated as constant
    (t (values 0 nil))))




;;; Minimal symbolic differentiator with simplification

(defun constp (x var)
  (or (numberp x) (and (symbolp x) (not (eql x var)))))

(defparameter *expression-heads* '(+ * - / expt sin cos tan exp log sqrt))

(defun ee (xx)
  (cond
    ((numberp xx) xx)
    ((symbolp xx) xx)
    ((and (consp xx) (member (car xx) *expression-heads*)) xx)
    (t (error "Is this an expression: ~s" xx))))


(defun make-sum (&rest terms)
  (let ((flat '()) (num 0))
    (dolist (tt terms)
      (cond
        ((null tt))
        ((and (consp tt) (eq (car tt) '+))
         (dolist (u (cdr tt)) (push u flat)))
        ((numberp tt) (setf num (+ num tt)))
        (t (push (ee tt) flat))))
    (when (/= num 0) (push num flat))
    (cond ((null flat) 0)
          ((null (cdr flat)) (ee (car flat)))
          (t (cons '+ (nreverse flat))))))

(defun make-prod (&rest factors)
  (let ((flat '()) (num 1))
    (dolist (ff factors)
      (cond
        ((null ff))
        ((and (numberp ff) (zerop ff)) (setf flat '(0) num 0) (return))
        ((and (consp ff) (eq (car ff) '*))
         (dolist (u (cdr ff)) (push u flat)))
        ((numberp ff) (setf num (* num ff)))
        (t (push (ee ff) flat))))
    (cond
      ((zerop num) 0)
      (t
       (when (/= num 1) (push num flat))
       (cond ((null flat) 1)
             ((null (cdr flat)) (ee (car flat)))
             (t (cons '* (nreverse flat))))))))

(defun make-expt (b ee)
  (cond
    ((numberp ee)
     (cond ((= ee 0) 1)
           ((= ee 1) b)
           ((and (numberp b)) (expt b ee))
           (t (list 'expt (ee b) (ee ee)))))
    (t (list 'expt (ee b) (ee ee)))))

(defun simp (expr)
  (declare (optimize (debug 3)))
  (cond
    ((atom expr) expr)
    (t
     (let* ((op (car expr))
            (args (mapcar #'simp (cdr expr))))
       (ecase op
         (+ (apply #'make-sum args))
         (* (apply #'make-prod args))
         (- (cond
               ((null args) 0)
               ((null (cdr args)) (make-prod -1 (car args)))
               (t (apply #'make-sum (car args)
                         (mapcar (lambda (x) (make-prod -1 x)) (cdr args))))))
         (/ (cond
               ((null (cdr args)) (list '/ (car args))) ; leave as-is
               (t (simp (make-prod (car args) (make-expt (cadr args) -1))))))
         (expt (make-expt (first args) (second args)))
         (sin (list 'sin (ee (first args))))
         (cos (list 'cos (ee (first args))))
         (tan (list 'tan (ee (first args))))
         (exp (list 'exp (ee (first args))))
         (log (list 'log (ee (first args))))
         (sqrt (list 'sqrt (ee (first args))))
         (t (cons op args)))))))

(defun deriv (pack expr var)
  (declare (optimize (debug 3)))
  (let ((all-partials nil)
        (all-dependent-variables nil))
    (labels ((inner-deriv (ee)
               (cond
                 ((numberp ee) 0)
                 ((symbolp ee) (multiple-value-bind (dderiv partials)
                                   (deriv-atom pack ee var)
                                 (when partials
                                   (pushnew dderiv all-partials)
                                   (pushnew ee all-dependent-variables))
                                 dderiv))
                 ((consp ee)
                  (let ((op (car ee)) (args (cdr ee)))
                    (case op
                      (+ (apply #'make-sum (mapcar #'inner-deriv args)))
                      (- (cond
                           ((null (cdr args)) (make-prod -1 (inner-deriv (car args))))
                           (t (apply #'make-sum (inner-deriv (car args))
                                     (mapcar (lambda (x) (make-prod -1 (inner-deriv x)))
                                             (cdr args))))))

                      (* ;; product rule for n factors
                       (apply 'make-sum
                              (mapcar
                               (lambda (i)
                                 (make-prod (inner-deriv (nth i args))
                                            (apply #'make-prod
                                                   (loop for j from 0 below (length args)
                                                         unless (= j i) collect (nth j args)))))
                               (loop for k from 0 below (length args) collect k))))

                      (/ ;; treat as ff * g^-1
                       (inner-deriv (make-prod (first args) (make-expt (second args) -1))))

                      (expt
                       (let* ((ff (first args)) (g (second args))
                              (df (inner-deriv ff)) (dg (inner-deriv g)))
                         (cond
                           ((numberp g)
                            (make-prod g (make-expt ff (1- g)) df))
                           ((constp ff var)
                            (make-prod (make-expt ff g) (log ff) dg))
                           (t
                            (make-prod (make-expt ff g)
                                       (make-sum (make-prod dg (list 'log ff))
                                                 (make-prod g (make-prod df (make-expt ff -1)))))))))

                      (sqrt (let* ((ff (first args))
                                   (df (inner-deriv ff)))
                              `(/ ,df (* 2 (sqrt ,ff)))))
                      (sin (make-prod (list 'cos (first args)) (inner-deriv (first args))))
                      (cos (make-prod -1 (list 'sin (first args)) (inner-deriv (first args))))
                      (tan (make-prod (inner-deriv (first args))
                                      (make-expt (list 'cos (first args)) -2)))
                      (exp (make-prod (list 'exp (first args)) (inner-deriv (first args))))
                      (log (make-prod (inner-deriv (first args)) (make-expt (first args) -1)))
                      (t 0))))
                 (t 0))))
      (values (simp (inner-deriv expr)) all-partials all-dependent-variables))))
