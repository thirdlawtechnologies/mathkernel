;;; math.lisp


(in-package :opt-exp)


;;;; Symbolic vector class

(defclass svector ()
  ((components
     :initarg :components
     :accessor svector-components)))

(defun make-svector (&rest components)
  "Create a symbolic vector from COMPONENTS (a list of expressions)."
  (make-instance 'svector :components components))

(defmethod print-object ((v svector) stream)
  (format stream "{ ~{~a~^ ~}}" (svector-components v))
  )

(defmacro svector (xx yy zz)
  `(make-svector ',xx ',yy ',zz))

(defun read-svector (stream char)
  (declare (ignore char))
  ;; Read forms up to the closing '}' and make an svector.
  (let ((components (read-delimited-list #\} stream t)))
    (apply #'make-svector components)))

(defparameter *opt-exp-readtable*
  (let ((rt (copy-readtable nil)))
    ;; install { ... } -> SVECTOR on rt
    (set-macro-character #\{ #'read-svector nil rt)
    (set-macro-character #\} (get-macro-character #\)) nil rt)
    rt))


;;;; Helpers

(defmacro .= (var value &optional docstring)
  `(defparameter ,var ,value ,@(when docstring (list docstring))))

(defun expr-sum (terms)
  "Build a symbolic sum (+ a b c ...)."
  (cond
    ((null terms) 0)
    ((null (rest terms)) (first terms))
    (t `(+ ,(first terms)
           ,(expr-sum (rest terms))))))

(defun svector-binary-op (op sv1 sv2)
  "Elementwise combination of two svectors with symbolic OP."
  (let ((c1 (svector-components sv1))
        (c2 (svector-components sv2)))
    (unless (= (length c1) (length c2))
      (error "Mismatched svector lengths: ~D and ~D"
             (length c1) (length c2)))
    (let ((args (mapcar (lambda (a b) `(,op ,a ,b)) c1 c2)))
      (apply 'make-svector
             ;; e.g. `(- a b)` or `(+ a b)`
             args))))

;;;; Vector operations

(defun sv-sub (sv1 sv2)
  "Symbolic sv1 - sv2 (elementwise)."
  (svector-binary-op '- sv1 sv2))

(defun sv-add (sv1 sv2)
  "Symbolic sv1 - sv2 (elementwise)."
  (svector-binary-op '+ sv1 sv2))

(defun sv-dot (sv1 sv2)
  "Symbolic dot product of two svectors."
  (let* ((c1 (svector-components sv1))
         (c2 (svector-components sv2))
         (products (mapcar (lambda (a b) `(* ,a ,b)) c1 c2)))
    (expr-sum products)))

(defun sv-len (sv)
  "Symbolic dot product of two svectors."
    `(sqrt ,(sv-dot sv sv)))

(defun sv-cross (sv1 sv2)
  "Symbolic dot product of two svectors."
  (let* ((c1 (svector-components sv1))
         (c2 (svector-components sv2))
         (x1 (elt c1 0))
         (y1 (elt c1 1))
         (z1 (elt c1 2))
         (x2 (elt c2 0))
         (y2 (elt c2 1))
         (z2 (elt c2 2))
         (cross (make-svector `(- (* ,y1 ,z2) (* ,y2 ,z1))
                              `(- (* ,x2 ,z1) (* ,x1 ,z2))
                              `(- (* ,x1 ,y2) (* ,x2 ,y1)))))
    cross))

(defun sv-square-components (sv)
  "Return list of component^2 expressions for an svector."
  (mapcar (lambda (x) `(* ,x ,x))
          (svector-components sv)))

(defun infix-nary (op args top-level)
  (let ((sym (case op
               (+ "+")
               (- "-")
               (* "*")
               (/ "/")
               (t (prin1-to-string op)))))
    (cond
      ;; no args → empty string
      ((null args) "")

      ;; single arg: just print it, no operator, no extra parens
      ((null (cdr args))
       (prefix->infix (car args) top-level))

      ;; real n-ary: a op b op c ...
      (t
       (let* ((parts (mapcar (lambda (a) (prefix->infix a nil)) args))
              (body  (reduce (lambda (a b)
                               (format nil "~a ~a ~a" a sym b))
                             parts)))
         (if top-level body (format nil "(~a)" body)))))))

(defun prefix->infix (expr &optional (top-level t))
  "Convert a symbolic prefix expression into an infix string.
TOP-LEVEL = T means: don't wrap the whole thing in parens."
  (cond
    ((atom expr)
     (prin1-to-string expr))

    (t
     (let ((op   (first expr))
           (args (rest expr)))
       (case op
         ;; n-ary infix operators
         ((+ - * /)
          (infix-nary op args top-level))

         ;; exponentiation
         (expt
          (let* ((base (prefix->infix (first args) nil))
                 (pow  (prefix->infix (second args) nil))
                 (body (format nil "~a^~a" base pow)))
            (if top-level body (format nil "(~a)" body))))

         ;; sqrt
         (sqrt
          (let* ((inside (prefix->infix (first args) nil))
                 (body   (format nil "Sqrt[~a]" inside)))
            (if top-level body (format nil "(~a)" body))))

         ;; generic function call f(a,b,...)
         (otherwise
           (let* ((parts (mapcar (lambda (a) (prefix->infix a nil)) args))
                  (body  (format nil "~a(~{~a~^, ~})" op parts)))
             (if top-level body (format nil "(~a)" body)))))))))


(defun mathematica-infix-nary (op args top-level)
  (let ((sym (case op
               (+ "+")
               (- "-")
               (* "*")
               (/ "/")
               (t (prin1-to-string op)))))
    (cond
      ;; no args
      ((null args) "")

      ;; single arg: no operator, no extra parens
      ((null (cdr args))
       (prefix->mathematica (car args) top-level))

      ;; real n-ary: a op b op c ...
      (t
       (let* ((parts (mapcar (lambda (a) (prefix->mathematica a nil)) args))
              (body  (reduce (lambda (a b)
                               (format nil "~a ~a ~a" a sym b))
                             parts)))
         (if top-level body (format nil "(~a)" body)))))))


(defun prefix->mathematica (expr &optional (top-level t))
  "Convert a symbolic prefix expression into an infix Mathematica string."
  (cond
    ((atom expr)
     (prin1-to-string expr))

    (t
     (let ((op   (first expr))
           (args (rest expr)))
       (case op
         ;; n-ary infix operators
         ((+ - * /)
          (mathematica-infix-nary op args top-level))

         ;; exponentiation: base^pow
         (expt
          (let* ((base (prefix->mathematica (first args) nil))
                 (pow  (prefix->mathematica (second args) nil))
                 (body (format nil "~a^~a" base pow)))
            (if top-level body (format nil "(~a)" body))))

         ;; sqrt → Sqrt[expr]
         (sqrt
          (let* ((inside (prefix->mathematica (first args) nil))
                 (body   (format nil "Sqrt[~a]" inside)))
            (if top-level body (format nil "(~a)" body))))

         ;; generic function call → f[a, b, ...]
         (otherwise
          (let* ((parts (mapcar (lambda (a) (prefix->mathematica a nil)) args))
                 (body  (format nil "~a[~{~a~^, ~}]" op parts)))
            (if top-level body (format nil "(~a)" body)))))))))


(defun enable-opt-exp-syntax ()
  (setf *readtable* *opt-exp-readtable*))


(defun start () 
  (enable-opt-exp-syntax)
  (in-package :expr-user))


;;;; Example usage
#|
(defparameter *va* (make-svector 'xa 'ya 'za))
(defparameter *vb* (make-svector 'xb 'yb 'zb))

(defparameter *vdelta* (svector-sub *va* *vb*))
;; components → ((- xa xb) (- ya yb) (- za zb))

(defparameter *dist2* (svector-dot *vdelta* *vdelta*))
;; (+ (* (- xa xb) (- xa xb))
;;    (* (- ya yb) (- ya yb))
;;    (* (- za zb) (- za zb)))

;; If you prefer: sum_i (va_i - vb_i)^2 via square-components
(defparameter *dist2-alt*
  (expr-sum (svector-square-components *vdelta*)))
|#
