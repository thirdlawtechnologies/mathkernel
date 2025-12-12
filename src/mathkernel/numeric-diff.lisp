;;; numeric-diff.lisp
;;; Numeric gradient/force and Hessian via finite differences over an energy function.

(in-package :mathkernel)

(defun numeric-gradient! (energy-fn position gradient-out &key (h 1.0d-6) (energy-args '()))
  "Accumulate central-difference dE/dx into GRADIENT-OUT.

ENERGY-FN is called as (ENERGY-FN POSITION &rest ENERGY-ARGS) and must
return a scalar energy. POSITION is mutated during evaluation but restored
before returning. Results are added (via INCF) into GRADIENT-OUT."
  (let* ((n (length position))
         (scale (/ 1.0d0 (* 2.0d0 h))))
    (when (and gradient-out (/= (length gradient-out) n))
      (error "GRADIENT-OUT length ~A does not match POSITION length ~A" (length gradient-out) n))
    (dotimes (i n gradient-out)
      (let ((orig (aref position i)))
        (setf (aref position i) (+ orig h))
        (let ((e+ (apply energy-fn position energy-args)))
          (setf (aref position i) (- orig h))
          (let ((e- (apply energy-fn position energy-args)))
            (setf (aref position i) orig)
            (incf (aref gradient-out i) (* scale (- e+ e-)))))))))

(defun numeric-hessian! (energy-fn position hessian-out &key (h 1.0d-5) (energy-args '()))
  "Accumulate central-difference Hessian entries into HESSIAN-OUT (row-major).

ENERGY-FN is called as (ENERGY-FN POSITION &rest ENERGY-ARGS) and must return
a scalar energy. POSITION is mutated during evaluation but restored. HESSIAN-OUT
must be a flat array of length N*N, where N = (length POSITION). Results are
added (via INCF) into HESSIAN-OUT and symmetry is enforced for off-diagonal terms."
  (let* ((n (length position))
         (expected (* n n))
         (h2 (* h h))
         (e0 (apply energy-fn position energy-args)))
    (when (/= (length hessian-out) expected)
      (error "HESSIAN-OUT length ~A does not match POSITION^2 (~A)" (length hessian-out) expected))
    ;; Diagonal second derivatives
    (dotimes (i n)
      (let ((orig (aref position i)))
        (setf (aref position i) (+ orig h))
        (let ((e+ (apply energy-fn position energy-args)))
          (setf (aref position i) (- orig h))
          (let ((e- (apply energy-fn position energy-args)))
            (setf (aref position i) orig)
            (let ((hii (/ (+ e+ e- (- (* 2.0d0 e0))) h2)))
              (incf (aref hessian-out (+ (* n i) i)) hii))))))
    ;; Off-diagonal mixed second derivatives
    (dotimes (i n)
      (dotimes (j i)
        (let ((orig-i (aref position i))
              (orig-j (aref position j)))
          ;; ++
          (setf (aref position i) (+ orig-i h))
          (setf (aref position j) (+ orig-j h))
          (let ((e++ (apply energy-fn position energy-args)))
            ;; +- (i+h, j-h)
            (setf (aref position j) (- orig-j h))
            (let ((e+- (apply energy-fn position energy-args)))
              ;; -+ (i-h, j+h)
              (setf (aref position i) (- orig-i h))
              (setf (aref position j) (+ orig-j h))
              (let ((e-+ (apply energy-fn position energy-args)))
                ;; -- (i-h, j-h)
                (setf (aref position j) (- orig-j h))
                (let ((e-- (apply energy-fn position energy-args)))
                  ;; restore
                  (setf (aref position i) orig-i)
                  (setf (aref position j) orig-j)
                  (let* ((hij (/ (+ e++ e-- (- e+-) (- e-+)) (* 4.0d0 h2)))
                         (idx (+ (* n i) j))
                         (jdx (+ (* n j) i)))
                    (incf (aref hessian-out idx) hij)
                    (incf (aref hessian-out jdx) hij)))))))))))

(defun numeric-force-and-hessian! (energy-fn position force-out hessian-out
                                     &key (h 1.0d-5) (energy-args '()))
  "Accumulate force (= -∇E) and Hessian via central differences of ENERGY-FN.

ENERGY-FN is called as (ENERGY-FN POSITION &rest ENERGY-ARGS) and must return
a scalar energy. POSITION is mutated during evaluation but restored. FORCE-OUT
and HESSIAN-OUT are incremented in place; either may be NIL to skip that part.

HESSIAN-OUT is a flat row-major array of length N*N where N = (length POSITION)."
  (let ((n (length position)))
    (when (and force-out (/= (length force-out) n))
      (error "FORCE-OUT length ~A does not match POSITION length ~A" (length force-out) n))
    (when hessian-out
      (numeric-hessian! energy-fn position hessian-out :h h :energy-args energy-args))
    (when force-out
      (let ((grad (make-array n :element-type 'double-float :initial-element 0d0)))
        (numeric-gradient! energy-fn position grad :h h :energy-args energy-args)
        ;; force = -grad; accumulate into caller-provided array.
        (dotimes (i n)
          (decf (aref force-out i) (aref grad i)))))
    (values force-out hessian-out)))
