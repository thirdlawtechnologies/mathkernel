;;;; -------------------------------
;;;; energy-kernels.lisp
;;;; -------------------------------

(in-package :energy-kernels)

(defparameter *pipeline*
  (stmt-ir:make-optimization-pipeline
   :name :kernel-full
   :optimizations
   (list

    ;; linear canonicalization to unify linear forms before factoring + CSE
    (stmt-ir:make-optimization
     :name :linear-canonicalization
     :function #'stmt-ir:linear-canonicalization-optimization)

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

     ;; expt aliasing
    (stmt-ir:make-optimization
     :name :alias-assigned-exprs
     :function #'stmt-ir:alias-assigned-exprs-optimization)

    ;; sign normalization
    (stmt-ir:make-optimization
     :name :normalize-signs
     :function #'stmt-ir:normalize-signs-optimization))))



(energy-kernels:with-kernels (kernels)
 
  (build-multiple-kernels (*kernels* "stretch" (:energy :gradient :hessian))
    (:pipeline *pipeline*)
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

    (:layout ((1 . I3X1) (2 . I3X2))
             ((X . 0) (Y . 1) (Z . 2)))

    (:coord-vars (x1 y1 z1
                     x2 y2 z2))

    (:coord-load
     (coords-from-position
      ((x1 y1 z1 i3x1)
       (x2 y2 z2 i3x2))))

    (:body
     (stmt-block
       (=. dx "x1 - x2")
       (=. dy "y1 - y2")
       (=. dz "z1 - z2")
       (=. r2 "dx*dx + dy*dy + dz*dz")
       (=. r  "sqrt(r2)")
       (=. dr "r - r0")
       ;; E(r) = 0.5 * kb * (r - r0)^2
       (=. energy "0.5*kb*dr*dr")))

    (:derivatives
     (:mode :manual
      :intermediates (r)

      ;; dr/dq
      :intermediate->coord
      ((r ((x1 "dx / r")
           (y1 "dy / r")
           (z1 "dz / r")
           (x2 "-dx / r")
           (y2 "-dy / r")
           (z2 "-dz / r"))))

      ;; d²r/(dqi dqj)
      :intermediate->coord2
      ((r (
           ((x1 x1) "(r*r - dx*dx)/(r^3)")
           ((x1 y1) "(-dx*dy)/(r^3)")
           ((x1 z1) "(-dx*dz)/(r^3)")
           ((x1 x2) "(dx*dx - r*r)/(r^3)")
           ((x1 y2) "dx*dy/(r^3)")
           ((x1 z2) "dx*dz/(r^3)")

           ((y1 y1) "(r*r - dy*dy)/(r^3)")
           ((y1 z1) "(-dy*dz)/(r^3)")
           ((y1 x2) "dx*dy/(r^3)")
           ((y1 y2) "(dy*dy - r*r)/(r^3)")
           ((y1 z2) "dy*dz/(r^3)")

           ((z1 z1) "(r*r - dz*dz)/(r^3)")
           ((z1 x2) "dx*dz/(r^3)")
           ((z1 y2) "dy*dz/(r^3)")
           ((z1 z2) "(dz*dz - r*r)/(r^3)")

           ((x2 x2) "(r*r - dx*dx)/(r^3)")
           ((x2 y2) "(-dx*dy)/(r^3)")
           ((x2 z2) "(-dx*dz)/(r^3)")

           ((y2 y2) "(r*r - dy*dy)/(r^3)")
           ((y2 z2) "(-dy*dz)/(r^3)")

           ((z2 z2) "(r*r - dz*dz)/(r^3)"))))

      ;; E(r) = 0.5*kb*(r - r0)^2
      ;; dE/dr = kb*(r - r0), d²E/dr² = kb
      :energy->intermediate
      (:gradient ((r "kb*(r - r0)"))
       :hessian  (((r r) "kb")))

      :hessian-modes ((r :full))
      :geometry-check :warn)))


  (build-multiple-kernels (*kernels* "angle" (:energy :gradient :hessian))
    (:pipeline *pipeline*)
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

    (:layout ((1 . I3X1) (2 . I3X2) (3 . I3X3))
             ((X . 0) (Y . 1) (Z . 2)))

    (:coord-vars (x1 y1 z1
                     x2 y2 z2
                     x3 y3 z3))

    (:coord-load
     (coords-from-position
      ((x1 y1 z1 i3x1)
       (x2 y2 z2 i3x2)
       (x3 y3 z3 i3x3))))

    (:body
     (stmt-block
       ;; bond vectors about atom 2
       (=. vx1 "x1 - x2")
       (=. vy1 "y1 - y2")
       (=. vz1 "z1 - z2")

       (=. vx2 "x3 - x2")
       (=. vy2 "y3 - y2")
       (=. vz2 "z3 - z2")

       ;; dot products and norms
       (=. dot   "vx1*vx2 + vy1*vy2 + vz1*vz2")
       (=. n1_sq "vx1*vx1 + vy1*vy1 + vz1*vz1")
       (=. n2_sq "vx2*vx2 + vy2*vy2 + vz2*vz2")
       (=. n1    "sqrt(n1_sq)")
       (=. n2    "sqrt(n2_sq)")

       (=. cos_theta "dot / (n1*n2)")
       (=. sin_theta "sqrt(1 - cos_theta^2)")
       (=. theta     "acos(cos_theta)")
       (=. dtheta    "theta - t0")

       ;; E(theta) = kt * (theta - t0)^2  (kt includes the 1/2 if desired)
       (=. energy "kt*dtheta*dtheta")))

    (:derivatives
     (:mode :manual
      :intermediates (theta)

      ;; dtheta/dcoord (manual, checked against AD)
      :intermediate->coord
      ((theta
        ((x1 "(-vx2/(n1*n2*sin_theta) + dot*vx1/(n1^3*n2*sin_theta))")
         (y1 "(-vy2/(n1*n2*sin_theta) + dot*vy1/(n1^3*n2*sin_theta))")
         (z1 "(-vz2/(n1*n2*sin_theta) + dot*vz1/(n1^3*n2*sin_theta))")

         (x3 "(-vx1/(n1*n2*sin_theta) + dot*vx2/(n1*n2^3*sin_theta))")
         (y3 "(-vy1/(n1*n2*sin_theta) + dot*vy2/(n1*n2^3*sin_theta))")
         (z3 "(-vz1/(n1*n2*sin_theta) + dot*vz2/(n1*n2^3*sin_theta))")

         (x2 "(vx2/(n1*n2*sin_theta)
               - dot*vx1/(n1^3*n2*sin_theta)
               + vx1/(n1*n2*sin_theta)
               - dot*vx2/(n1*n2^3*sin_theta))")
         (y2 "(vy2/(n1*n2*sin_theta)
               - dot*vy1/(n1^3*n2*sin_theta)
               + vy1/(n1*n2*sin_theta)
               - dot*vy2/(n1*n2^3*sin_theta))")
         (z2 "(vz2/(n1*n2*sin_theta)
               - dot*vz1/(n1^3*n2*sin_theta)
               + vz1/(n1*n2*sin_theta)
               - dot*vz2/(n1*n2^3*sin_theta))"))))

      ;; no :intermediate->coord2 needed for :outer-product-only

      ;; E(theta) = kt * (theta - t0)^2
      ;; dE/dtheta  = 2*kt*(theta - t0)
      ;; d²E/dtheta² = 2*kt
      :energy->intermediate
      (:gradient ((theta "2*kt*(theta - t0)"))
       :hessian  (((theta theta) "2*kt")))

      :hessian-modes ((theta :outer-product-only))
      :geometry-check :warn)))


  (build-multiple-kernels (*kernels* "dihedral" (:energy :gradient :hessian))
    (:pipeline *pipeline*)
    (:params ((double V)   ;; amplitude
              (double n)   ;; multiplicity
              (double phase) ;; phase offset δ
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

    (:layout ((1 . I3X1) (2 . I3X2) (3 . I3X3) (4 . I3X4))
             ((X . 0) (Y . 1) (Z . 2)))

    (:coord-vars (x1 y1 z1
                     x2 y2 z2
                     x3 y3 z3
                     x4 y4 z4))

    (:coord-load
     (coords-from-position
      ((x1 y1 z1 i3x1)
       (x2 y2 z2 i3x2)
       (x3 y3 z3 i3x3)
       (x4 y4 z4 i3x4))))

    (:body
     (stmt-block
       ;; bond vectors
       (=. v1x "x2 - x1")
       (=. v1y "y2 - y1")
       (=. v1z "z2 - z1")

       (=. v2x "x3 - x2")
       (=. v2y "y3 - y2")
       (=. v2z "z3 - z2")

       (=. v3x "x4 - x3")
       (=. v3y "y4 - y3")
       (=. v3z "z4 - z3")

       ;; plane normals
       (=. c1x "v1y*v2z - v1z*v2y")
       (=. c1y "v1z*v2x - v1x*v2z")
       (=. c1z "v1x*v2y - v1y*v2x")

       (=. c2x "v2y*v3z - v2z*v3y")
       (=. c2y "v2z*v3x - v2x*v3z")
       (=. c2z "v2x*v3y - v2y*v3x")

       ;; norms and dot products
       (=. c1_sq "c1x*c1x + c1y*c1y + c1z*c1z")
       (=. c2_sq "c2x*c2x + c2y*c2y + c2z*c2z")
       (=. v2_sq "v2x*v2x + v2y*v2y + v2z*v2z")
       (=. v2_len "sqrt(v2_sq)")

       (=. dot12 "v1x*v2x + v1y*v2y + v1z*v2z")
       (=. dot23 "v2x*v3x + v2y*v3y + v2z*v3z")

       ;; torsion via atan2
       (=. t1 "v2_len*(v1x*c2x + v1y*c2y + v1z*c2z)")
       (=. t2 "c1x*c2x + c1y*c2y + c1z*c2z")
       (=. phi "atan2(t1, t2)")

       ;; Amber-style torsion:
       ;; E(phi) = V * (1 + cos(n*phi - phase))
       (=. nphi  "n*phi")
       (=. angle "nphi - phase")
       (=. energy "V*(1.0 + cos(angle))")))

    (:derivatives
     (:mode :manual
      :intermediates (phi)

      ;; no manual geometry; AD provides dphi/dcoord

      ;; E(phi) = V*(1 + cos(angle)), angle = n*phi - phase
      ;; dE/dphi  = -V*n*sin(angle)
      ;; d²E/dphi² = -V*n^2*cos(angle)
      :energy->intermediate
      (:gradient ((phi "-V*n*sin(angle)"))
       :hessian  (((phi phi) "-V*n*n*cos(angle)")))

      :hessian-modes ((phi :outer-product-only))
      :geometry-check :warn)))




  (build-multiple-kernels (*kernels* "nonbond" (:energy :gradient :hessian))
    (:pipeline *pipeline*)
    (:params ((double A) ;; LJ A coefficient  (A / r^12)
              (double B) ;; LJ B coefficient  (B / r^6)
              (double qq) ;; Coulomb prefactor (qq / r)
              (size_t i3x1)
              (size_t i3x2)
              (double* position)
              (double* energy_accumulate)
              (double* force)
              (double* hessian)
              (double* dvec)
              (double* hdvec)))

    (:layout ((1 . I3X1) (2 . I3X2))
             ((X . 0) (Y . 1) (Z . 2)))

    (:coord-vars (x1 y1 z1
                     x2 y2 z2))

    (:coord-load
     (coords-from-position
      ((x1 y1 z1 i3x1)
       (x2 y2 z2 i3x2))))

    (:body
     (stmt-block
       (=. dx "x1 - x2")
       (=. dy "y1 - y2")
       (=. dz "z1 - z2")
       (=. r2 "dx*dx + dy*dy + dz*dz")
       (=. r  "sqrt(r2)")
       (=. invr  "1.0 / r")
       (=. invr2 "invr*invr")
       (=. invr6 "invr2*invr2*invr2")

       ;; E(r) = A/r^12 - B/r^6 + qq/r
       (=. e_lj   "A*invr6*invr6 - B*invr6")
       (=. e_coul "qq*invr")
       (=. energy "e_lj + e_coul")))

    (:derivatives
     (:mode :manual
      :intermediates (r)

      ;; radial geometry as in stretch
      :intermediate->coord
      ((r ((x1 "dx / r")
           (y1 "dy / r")
           (z1 "dz / r")
           (x2 "-dx / r")
           (y2 "-dy / r")
           (z2 "-dz / r"))))

      :intermediate->coord2
      ((r (
           ((x1 x1) "(r*r - dx*dx)/(r^3)")
           ((x1 y1) "(-dx*dy)/(r^3)")
           ((x1 z1) "(-dx*dz)/(r^3)")
           ((x1 x2) "(dx*dx - r*r)/(r^3)")
           ((x1 y2) "dx*dy/(r^3)")
           ((x1 z2) "dx*dz/(r^3)")

           ((y1 y1) "(r*r - dy*dy)/(r^3)")
           ((y1 z1) "(-dy*dz)/(r^3)")
           ((y1 x2) "dx*dy/(r^3)")
           ((y1 y2) "(dy*dy - r*r)/(r^3)")
           ((y1 z2) "dy*dz/(r^3)")

           ((z1 z1) "(r*r - dz*dz)/(r^3)")
           ((z1 x2) "dx*dz/(r^3)")
           ((z1 y2) "dy*dz/(r^3)")
           ((z1 z2) "(dz*dz - r*r)/(r^3)")

           ((x2 x2) "(r*r - dx*dx)/(r^3)")
           ((x2 y2) "(-dx*dy)/(r^3)")
           ((x2 z2) "(-dx*dz)/(r^3)")

           ((y2 y2) "(r*r - dy*dy)/(r^3)")
           ((y2 z2) "(-dy*dz)/(r^3)")

           ((z2 z2) "(r*r - dz*dz)/(r^3)"))))

      ;; E(r) = A/r^12 - B/r^6 + qq/r
      ;; dE/dr   = -12*A/r^13 + 6*B/r^7 - qq/r^2
      ;; d²E/dr² = 156*A/r^14 - 42*B/r^8 + 2*qq/r^3
      :energy->intermediate
      (:gradient ((r "-12*A/(r^13) + 6*B/(r^7) - qq/(r^2)"))
       :hessian  (((r r) "156*A/(r^14) - 42*B/(r^8) + 2*qq/(r^3)")))

      :hessian-modes ((r :full))
      :geometry-check :warn)))



  (build-multiple-kernels (*kernels* "nonbond_dd_cutoff" (:energy :gradient :hessian))
    (:pipeline *pipeline*)

    (:params ((double A)      ;; LJ A coefficient  (A / r^12)
              (double B)      ;; LJ B coefficient  (B / r^6)
              (double qq)     ;; Coulomb prefactor
              (double dd)     ;; epsilon(r) = dd*r
              (double r_switch) ;; switching start
              (double r_cut)    ;; cutoff
              (size_t i3x1)
              (size_t i3x2)
              (double* position)
              (double* energy_accumulate)
              (double* force)
              (double* hessian)
              (double* dvec)
              (double* hdvec)))

    (:layout ((1 . I3X1) (2 . I3X2))
             ((X . 0) (Y . 1) (Z . 2)))

    (:coord-vars (x1 y1 z1
                     x2 y2 z2))

    (:coord-load
     (coords-from-position
      ((x1 y1 z1 i3x1)
       (x2 y2 z2 i3x2))))

    (:body
     (stmt-block
       ;; geometry
       (=. dx "x1 - x2")
       (=. dy "y1 - y2")
       (=. dz "z1 - z2")
       (=. r2 "dx*dx + dy*dy + dz*dz")
       (=. r  "sqrt(r2)")

       ;; outer cutoff: only do work if r < r_cut
       (stmt-ir:make-if-stmt
        (expr-ir:parse-expr "r < r_cut")
        (stmt-block
          ;; channel work (invr, invr2, invr6, e_lj, e_coul, e_base)
          (=. invr  "r^-1")
          (=. invr2 "invr*invr")
          (=. invr3 "invr*invr2")
          (=. invr6 "invr2*invr2*invr2")
          (=. e_lj   "A*invr6*invr6 - B*invr6")
          (=. e_coul "qq*invr2/dd")
          (=. e_base "e_lj + e_coul")

          ;; base radial derivatives via AD
          (D! e_base r)
          (D! de_base_dr r)

          ;; inner piecewise structure as before:
          (stmt-ir:make-if-stmt
           (expr-ir:parse-expr "r < r_switch")
           (stmt-block
             (=. energy  "e_base")
             (=. dE_dr   "de_base_dr")
             (=. d2E_dr2 "dde_base_dr_dr"))
           (stmt-block
             ;; smoothing region
             (=. drs       "r - r_switch")
             (=. inv_range "1.0 / (r_cut - r_switch)")
             (=. t  "drs*inv_range")
             (=. t2 "t*t")
             (=. t3 "t2*t")
             (=. t4 "t2*t2")
             (=. t5 "t3*t2")
             ;; S(t) = 1 - 10 t^3 + 15 t^4 - 6 t^5
             (=. s "1.0 - 10.0*t3 + 15.0*t4 - 6.0*t5")
             ;; dS/dt, d²S/dt²
             (=. ds_dt   "-30.0*t2 + 60.0*t3 - 30.0*t4")
             (=. d2s_dt2 "-60.0*t + 180.0*t2 - 120.0*t3")
             ;; dS/dr, d²S/dr²
             (=. ds_dr   "ds_dt*inv_range")
             (=. d2s_dr2 "d2s_dt2*inv_range*inv_range")

             ;; E(r) = S(r)*E_base(r)
             (=. energy "s*e_base")

             ;; dE/dr   = S'*E_base + S*dE_base/dr
             ;; d²E/dr² = S''*E_base + 2*S'*dE_base/dr + S*d2E_base/dr2
             (=. dE_dr   "ds_dr*e_base + s*dE_base_dr")
             (=. d2E_dr2 "d2s_dr2*e_base + 2.0*ds_dr*dE_base_dr + s*ddE_base_dr_dr"))
           )
          (ACCUMULATE-HERE)
          )
        ;; else-branch: optionally nothing
        )
       ))

    (:derivatives
     (:mode :manual
      :intermediates (r)

      ;; radial geometry
      :intermediate->coord
      ((r ((x1 "dx / r")
           (y1 "dy / r")
           (z1 "dz / r")
           (x2 "-dx / r")
           (y2 "-dy / r")
           (z2 "-dz / r"))))

      :intermediate->coord2
      ((r (
           ((x1 x1) "(r*r - dx*dx)/(r^3)")
           ((x1 y1) "(-dx*dy)/(r^3)")
           ((x1 z1) "(-dx*dz)/(r^3)")
           ((x1 x2) "(dx*dx - r*r)/(r^3)")
           ((x1 y2) "dx*dy/(r^3)")
           ((x1 z2) "dx*dz/(r^3)")

           ((y1 y1) "(r*r - dy*dy)/(r^3)")
           ((y1 z1) "(-dy*dz)/(r^3)")
           ((y1 x2) "dx*dy/(r^3)")
           ((y1 y2) "(dy*dy - r*r)/(r^3)")
           ((y1 z2) "dy*dz/(r^3)")

           ((z1 z1) "(r*r - dz*dz)/(r^3)")
           ((z1 x2) "dx*dz/(r^3)")
           ((z1 y2) "dy*dz/(r^3)")
           ((z1 z2) "(dz*dz - r*r)/(r^3)")

           ((x2 x2) "(r*r - dx*dx)/(r^3)")
           ((x2 y2) "(-dx*dy)/(r^3)")
           ((x2 z2) "(-dx*dz)/(r^3)")

           ((y2 y2) "(r*r - dy*dy)/(r^3)")
           ((y2 z2) "(-dy*dz)/(r^3)")

           ((z2 z2) "(r*r - dz*dz)/(r^3)"))))

      ;; radial link energy ↔ r via branchwise dE_dr, d2E_dr2
      :energy->intermediate
      (:gradient ((r "dE_dr"))
       :hessian  (((r r) "d2E_dr2")))

      :hessian-modes ((r :full))
      :geometry-check :warn)))


  (build-multiple-kernels (*kernels* "chiral_restraint" (:energy :gradient :hessian))
    ;; Optimization pipeline
    (:pipeline *pipeline*)

    ;; Parameters: (ctype name)
    ;;   K   – force constant
    ;;   CO  – target chiral value
    (:params ((double K)
              (double CO)
              (size_t i3x1)
              (size_t i3x2)
              (double* position)
              (double* energy_accumulate)
              (double* force)
              (double* hessian)
              (double* dvec)
              (double* hdvec)
              ))

    ;; Layout: 4 atoms with 3D coords
    (:layout ((1 . I3X1) (2 . I3X2) (3 . I3X3) (4 . I3X4))
             ((#\X . 0) (#\Y . 1) (#\Z . 2)))

    ;; Coordinate vars in expr-var package
    (:coord-vars (x1 y1 z1
                     x2 y2 z2
                     x3 y3 z3
                     x4 y4 z4))

    ;; How to load coords from position[]
    (:coord-load
     (coords-from-position
      ((x1 y1 z1 I3X1)
       (x2 y2 z2 I3X2)
       (x3 y3 z3 I3X3)
       (x4 y4 z4 I3X4))))

    ;; Manual derivative spec: we introduce one intermediate Q
    ;; and give dE/dQ. We deliberately DROP d²E/dQ² so that this
    ;; channel contributes only the outer‑product term to the Hessian.
    (:derivatives
     (:mode :hybrid
      :intermediates (Q)

      ;; No manual geometry; let AD fill:
      ;;   du/dq = dQ/dq, d²u/dqidqj = d²Q/dqidqj
      :intermediate->coord ()
      :intermediate->coord2 ()

      ;; Energy derivatives w.r.t Q:
      ;;   E = K*(CO + Q)^3
      ;;   dE/dQ   = 3*K*(CO + Q)^2
      ;;   We omit d²E/dQ² so that only the
      ;;   outer-product term (dE/dQ * dQ/dqi dQ/dqj) is kept.
      :energy->intermediate
      (:gradient ((Q "3*K*(CO + Q)^2"))
       :hessian  ())

      ;; Only outer‑product Hessian contribution from this channel
      :hessian-modes ((Q :outer-product-only))

      ;; Check geometry (AD vs auto-filled) and warn on mismatch
      :geometry-check :warn))

    ;; Kernel body
    (:body
     (stmt-block
       ;; r13 = r1 - r3
       (=. dx13 "x1 - x3")
       (=. dy13 "y1 - y3")
       (=. dz13 "z1 - z3")

       ;; r23 = r2 - r3
       (=. dx23 "x2 - x3")
       (=. dy23 "y2 - y3")
       (=. dz23 "z2 - z3")

       ;; r43 = r4 - r3
       (=. dx43 "x4 - x3")
       (=. dy43 "y4 - y3")
       (=. dz43 "z4 - z3")

       ;; norms
       (=. r13_2 "dx13*dx13 + dy13*dy13 + dz13*dz13")
       (=. r23_2 "dx23*dx23 + dy23*dy23 + dz23*dz23")
       (=. r43_2 "dx43*dx43 + dy43*dy43 + dz43*dz43")

       (=. r13 "sqrt(r13_2)")
       (=. r23 "sqrt(r23_2)")
       (=. r43 "sqrt(r43_2)")

       ;; cross = r13 × r23
       (=. cx "dy13*dz23 - dz13*dy23")
       (=. cy "dz13*dx23 - dx13*dz23")
       (=. cz "dx13*dy23 - dy13*dx23")

       ;; triple product V = (r13 × r23) · r43
       (=. V "cx*dx43 + cy*dy43 + cz*dz43")

       ;; normalized chiral measure Q = V / (|r13| |r23| |r43|)
       (=. denom "r13*r23*r43")
       (=. Q "V/denom")

       ;; Energy: E = K * (CO + Q)^3
       (=. ENERGY "K * (CO + Q)^3")

       ;; Accumulate E/G/H at this point
       (accumulate-here))))


  (build-multiple-kernels (*kernels* "anchor" (:energy :gradient :hessian))
    (:pipeline *pipeline*)
    (:layout ((1 . I3X1))
             ((X . 0) (Y . 1) (Z . 2)))
    (:coord-vars (x1 y1 z1))
    (:coord-load
     (coords-from-position
      ((x1 y1 z1 i3x1))))
    (:body
     (stmt-block
       (=. dx "x1 - xa")
       (=. dy "y1 - ya")
       (=. dz "z1 - za")
       (=. r2 "dx*dx + dy*dy + dz*dz")
       (=. energy "ka * r2")
       (accumulate-here)))
    (:params ((double ka)
              (double xa)
              (double ya)
              (double za)
              (double* position)
              (double* energy_accumulate)
              (double* force_accumulate)
              (double* hess_accumulate)))
    (:derivatives
     (:mode :manual
      :intermediates (dx dy dz r2)
      :intermediate->coord
      ((dx ((x1 "1")))
       (dy ((y1 "1")))
       (dz ((z1 "1")))
       (r2 ((x1 "2*dx")
            (y1 "2*dy")
            (z1 "2*dz"))))
      :intermediate->coord2
      ((dx (((x1 x1) "0")))
       (dy (((y1 y1) "0")))
       (dz (((z1 z1) "0")))
       (r2 (((x1 x1) "2")
            ((y1 y1) "2")
            ((z1 z1) "2"))))
      :energy->intermediate
      (:gradient
       ((r2 "ka"))
       :hessian
       (((r2 r2) "0")))
      :hessian-modes
      ((dx :none)
       (dy :none)
       (dz :none)
       (r2 :outer-product-only))
      :geometry-check :warn)))



  (build-multiple-kernels (*kernels* "dihedral_restraint" (:energy :gradient :hessian))
    (:pipeline *pipeline*)

    ;; Parameters:
    ;;   kdh  – force constant
    ;;   phi0 – target dihedral angle (radians)
    (:params ((double kdh)
              (double phi0)))

    ;; 4-atom dihedral 1–2–3–4
    (:layout ((1 . I3X1) (2 . I3X2) (3 . I3X3) (4 . I3X4))
             ((#\X . 0) (#\Y . 1) (#\Z . 2)))

    (:coord-vars (x1 y1 z1
                     x2 y2 z2
                     x3 y3 z3
                     x4 y4 z4))

    (:coord-load
     (coords-from-position
      ((x1 y1 z1 I3X1)
       (x2 y2 z2 I3X2)
       (x3 y3 z3 I3X3)
       (x4 y4 z4 I3X4))))

    ;; Manual derivative spec:
    ;;   intermediates: phi, deltaPhi, deltaPhiModFn
    ;;   E = kdh * deltaPhiModFn^2
    ;;   dE/d(deltaPhiModFn)   = 2*kdh*deltaPhiModFn
    ;;   d²E/d(deltaPhiModFn)² = 2*kdh
    ;; Geometry of phi/deltaPhi/deltaPhiModFn is left to AD+auto-fill.
    ;; For truncated Newton, we keep only the dominant outer‑product
    ;; Hessian terms from deltaPhiModFn and drop curvature terms from
    ;; phi and deltaPhi.
    (:derivatives
     (:mode :hybrid
      :intermediates (phi deltaPhi deltaPhiModFn)

      :intermediate->coord ()
      :intermediate->coord2 ()

      :energy->intermediate
      (:gradient ((deltaPhiModFn "2*kdh*deltaPhiModFn"))
       :hessian  (((deltaPhiModFn deltaPhiModFn) "2*kdh")))

      ;; Only deltaPhiModFn contributes to the Hessian; phi and deltaPhi
      ;; are treated as having no second‑derivative contribution.
      :hessian-modes ((phi :none)
                      (deltaPhi :none)
                      (deltaPhiModFn :outer-product-only))

      :geometry-check :warn))

    (:body
     (stmt-block
       ;; Bond vectors
       (=. dx12 "x2 - x1")
       (=. dy12 "y2 - y1")
       (=. dz12 "z2 - z1")

       (=. dx23 "x3 - x2")
       (=. dy23 "y3 - y2")
       (=. dz23 "z3 - z2")

       (=. dx34 "x4 - x3")
       (=. dy34 "y4 - y3")
       (=. dz34 "z4 - z3")

       ;; Norms
       (=. r12_2 "dx12*dx12 + dy12*dy12 + dz12*dz12")
       (=. r23_2 "dx23*dx23 + dy23*dy23 + dz23*dz23")
       (=. r34_2 "dx34*dx34 + dy34*dy34 + dz34*dz34")

       (=. r12 "sqrt(r12_2)")
       (=. r23 "sqrt(r23_2)")
       (=. r34 "sqrt(r34_2)")

       ;; Planes normals: n1 = (r12 × r23), n2 = (r23 × r34)
       (=. n1x "dy12*dz23 - dz12*dy23")
       (=. n1y "dz12*dx23 - dx12*dz23")
       (=. n1z "dx12*dy23 - dy12*dx23")

       (=. n2x "dy23*dz34 - dz23*dy34")
       (=. n2y "dz23*dx34 - dx23*dz34")
       (=. n2z "dx23*dy34 - dy23*dx34")

       ;; Norms of normals
       (=. n1_2 "n1x*n1x + n1y*n1y + n1z*n1z")
       (=. n2_2 "n2x*n2x + n2y*n2y + n2z*n2z")
       (=. n1   "sqrt(n1_2)")
       (=. n2   "sqrt(n2_2)")

       ;; Cos(phi) = (n1 · n2) / (|n1||n2|)
       (=. dot_n1n2 "n1x*n2x + n1y*n2y + n1z*n2z")
       (=. inv_n1n2 "1/(n1*n2)")
       (=. cosphi "dot_n1n2 * inv_n1n2")

       ;; phi in [0,pi]
       (=. phi "acos(cosphi)")

       ;; raw delta = phi - phi0
       (=. deltaPhi "phi - phi0")

       ;; For now, deltaPhiModFn == deltaPhi; wrapping is done externally
       (=. deltaPhiModFn "deltaPhi")

       ;; Harmonic restraint energy
       (=. ENERGY "kdh*deltaPhiModFn*deltaPhiModFn")

       (accumulate-here))))




  (write-all "~/tmp/code/kernels/")
  )

