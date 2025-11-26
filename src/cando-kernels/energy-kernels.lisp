;;;; -------------------------------
;;;; energy-kernels.lisp
;;;; -------------------------------

(in-package :energy-kernels)

(defparameter *pipeline*
  (stmt-ir:make-optimization-pipeline
   :name :kernel-full
   :optimizations
   (list
    ;; factor sums
    (stmt-ir:make-optimization
     :name :factor-sums
     :function #'stmt-ir:factor-sums-optimization
     :keyword-args (list :min-uses 2
                         :min-factors 1
                         :min-size 4))
    ;; CSE over full block
    (stmt-ir:make-optimization
     :name :cse-full
     :function #'stmt-ir:cse-block-multi-optimization
     :keyword-args (list :max-passes 50
                         :min-uses 2
                         :min-size 1))
    ;; temp+param factoring
    (stmt-ir:make-optimization
     :name :factor-temp-param
     :function #'stmt-ir:factor-temp-param-products-optimization
     :keyword-args (list :min-uses    2
                         :min-factors 2
                         :max-factors 3 ))
    ;; copy-propagation
    (stmt-ir:make-optimization
     :name :copy-propagate
     :function #'stmt-ir:copy-propagate-optimization)
    ;; sign normalization
    (stmt-ir:make-optimization
     :name :normalize-signs
     :function #'stmt-ir:normalize-signs-optimization))))





(defkernel *stretch-energy-kernel*
  (:c-function-name stretch_energy_kernel)
  (:compute (energy grad hess))
  (:pipeline *pipeline*)
  ;; C function signature
  (:params ((double kb)
            (double r0)
            (size_t i3x1)
            (size_t i3x2)
            (double* position)
            (double* energy_accumulate)
            (double* force)
            (double* hessian)
            (double* dvec)
            (double* hdvec)))

  ;; mapping atom index -> base index, axis -> offset
  (:layout ((1 . I3X1) (2 . I3X2))
           ((X . 0) (Y . 1) (Z . 2)))

  ;; coordinate vars for AD
  (:coord-vars (x1 y1 z1 x2 y2 z2))

  ;; how to load coords from position[]
  (:coord-load
   (coords-from-position
    ((x1 y1 z1 i3x1)
     (x2 y2 z2 i3x2))))

  ;; symbolic kernel (base block)
  (:body
   (stmt-block
     ;; final energy scalar
     (=. energy "kb*(-r0 + Sqrt((-x1 + x2)^2 + (-y1 + y2)^2 + (-z1 + z2)^2))^2"))))


(generate-kernel-code "~/tmp/stretch.c" :kernel *stretch-energy-kernel*)


#+(or)
(defkernel *angle-energy-kernel*
  ;; what to generate
  (:c-function-name angle_energy_kernel)
  (:compute (energy grad hess))    ; or (energy) or (energy grad) etc.
  (:pipeline *pipeline*)

  ;; C function signature
  (:params ((double kt)
            (double t0)
            (size_t i3x1)
            (size_t i3x2)
            (size_t i3x3)
            (double* position)
            (double* energy_accumulate)
            (double* force)
            (double* hessian)
            (double* dvec)
            (double* hdvec)))


  ;; mapping atom index -> base index, axis -> offset
  (:layout ((1 . I3X1) (2 . I3X2) (3 . I3X3))
           ((X . 0) (Y . 1) (Z . 2)))

  ;; coordinate vars for AD
  (:coord-vars (x1 y1 z1 x2 y2 z2 x3 y3 z3))

  ;; how to load coords from position[]
  (:coord-load
   (coords-from-position
    ((x1 y1 z1 i3x1)
     (x2 y2 z2 i3x2)
     (x3 y3 z3 i3x3))))

  ;; symbolic kernel (base block)
  (:body
   (stmt-block
     (=. energy "kt*(-t0 + acos(((x1 - x2)*(-x2 + x3) + (y1 - y2)*(-y2 + y3) + (z1 - z2)*(-z2 + z3))/(Sqrt((x1 - x2)^2 + (y1 - y2)^2 + (z1 - z2)^2)*Sqrt((-x2 + x3)^2 + (-y2 + y3)^2 + (-z2 + z3)^2))))^2"))))

(defkernel *angle-energy-kernel*
  ;; what to generate
  (:c-function-name angle_energy_kernel)
  (:compute (energy grad hess))    ; or (energy) or (energy grad) etc.
  (:pipeline *pipeline*)

  ;; C function signature
  (:params ((double kt)
            (double t0)
            (size_t i3x1)
            (size_t i3x2)
            (size_t i3x3)
            (double* position)
            (double* energy_accumulate)
            (double* force)
            (double* hessian)
            (double* dvec)
            (double* hdvec)))


  ;; mapping atom index -> base index, axis -> offset
  (:layout ((1 . I3X1) (2 . I3X2) (3 . I3X3))
           ((X . 0) (Y . 1) (Z . 2)))

  ;; coordinate vars for AD
  (:coord-vars (x1 y1 z1 x2 y2 z2 x3 y3 z3))

  ;; how to load coords from position[]
  (:coord-load
   (coords-from-position
    ((x1 y1 z1 i3x1)
     (x2 y2 z2 i3x2)
     (x3 y3 z3 i3x3))))

  ;; symbolic kernel (base block)
  (:body
   (stmt-block
     ;; vectors
     (=. ax "x1 - x2")
     (=. ay "y1 - y2")
     (=. az "z1 - z2")
     (=. bx "x3 - x2")
     (=. by "y3 - y2")
     (=. bz "z3 - z2")

     ;; norms and dot
     (=. a2 "ax*ax + ay*ay + az*az")
     (=. b2 "bx*bx + by*by + bz*bz")
     (=. ab "ax*bx + ay*by + az*bz")
     (=. ra "Sqrt(a2)")
     (=. rb "Sqrt(b2)")
     (=. cos_th "ab/(ra*rb)")
     (=. theta "Acos(cos_th)")
     (=. angle_dev "theta - t0")

     ;; energy
     (=. energy "kt*angle_dev*angle_dev"))))

(generate-kernel-code "~/tmp/angle.c" :kernel *angle-energy-kernel*)





(let* ((ri (svector x1 y1 z1 ))
       (rj (svector x2 y2 z2 ))
       (rk (svector x3 y3 z3 ))
       (rl (svector x4 y4 z4 ))
       (ff (sv-sub ri rj))
       (gg (sv-sub rj rk))
       (hh (sv-sub rl rk))
       (aa (sv-cross ff gg))
       (bb (sv-cross hh gg))
       (lena (sv-len aa))
       (lenb (sv-len bb))
       (reclenA `(receprocal ,lena))
       )
  (format t "lena = ~s~%" lena))

#+(or)
(defkernel *dihedral-energy-kernel*
  ;; what to generate
  (:c-function-name angle_energy_kernel)
  (:compute (energy grad hess))    ; or (energy) or (energy grad) etc.
  (:pipeline *pipeline*)

  ;; C function signature
  (:params ((double V)
            (double DN)
            (int  IN)
            (double cosPhase)
            (double sinPhase)
            (size_t i3x1)
            (size_t i3x2)
            (size_t i3x3)
            (size_t i3x4)
            (double* position)
            (double* energy_accumulate)
            (double* force)
            (double* hessian)
            (double* dvec)
            (double* hdvec)))


  ;; mapping atom index -> base index, axis -> offset
  (:layout ((1 . I3X1) (2 . I3X2) (3 . I3X3) (4 . I3X4))
           ((X . 0) (Y . 1) (Z . 2)))

  ;; coordinate vars for AD
  (:coord-vars (x1 y1 z1 x2 y2 z2 x3 y3 z3 x4 y4 z4))

  ;; how to load coords from position[]
  (:coord-load
   (coords-from-position
    ((x1 y1 z1 i3x1)
     (x2 y2 z2 i3x2)
     (x3 y3 z3 i3x3)
     (x4 y4 z4 i3x4)
     )))

  ;; symbolic kernel (base block)
  (:body
   (stmt-block
     ;; vectors
     (=. ri { x1 y1 z1 })
     (=. rj { x2 y2 z2 })
     (=. rk { x3 y3 z3 })
     (=. rl { x4 y4 z4 })

     (=. ff (sv-sub ri rj))
     (=. gg (sv-sub rj rk))
     (=. hh (sv-sub rl rk))

     (=. aa (sv-cross ff gg))
     (=. bb (sv-cross hh gg))

     (=. lena `(sqrt

     (=. ax "x1 - x2")
     (=. ay "y1 - y2")
     (=. az "z1 - z2")
     (=. bx "x3 - x2")
     (=. by "y3 - y2")
     (=. bz "z3 - z2")

     ;; norms and dot
     (=. a2 "ax*ax + ay*ay + az*az")
     (=. b2 "bx*bx + by*by + bz*bz")
     (=. ab "ax*bx + ay*by + az*bz")
     (=. ra "Sqrt(a2)")
     (=. rb "Sqrt(b2)")
     (=. cos_th "ab/(ra*rb)")
     (=. theta "Acos(cos_th)")
     (=. angle_dev "theta - t0")

     ;; energy
     (=. energy "kt*angle_dev*angle_dev"))))

   ))




