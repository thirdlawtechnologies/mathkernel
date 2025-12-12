(in-package :mathkernel)
;;; ------------------------------------------------------------
;;; General harness from kernel objects
;;; ------------------------------------------------------------

(defun %i3-index-from-name (sym)
  (let* ((s (string-downcase (string sym)))
         (prefix "i3x"))
    (when (and (>= (length s) (length prefix))
               (string= s prefix :end1 (length prefix)))
      (let ((rest (subseq s (length prefix))))
        (ignore-errors (* 3 (1- (parse-integer rest))))))))


;;; ------------------------------------------------------------
;;; Single-kernel wrapper emitter (legacy)
;;; ------------------------------------------------------------



(defun write-kernel-compare-harness (kernels pathname include-new include-old
                                      &key (atol 1d-9) (rtol 1d-6))
  "Write a full C harness comparing each KERNEL in KERNELS against
ground-truth (finite-difference) implementations. INCLUDE-NEW/INCLUDE-OLD
are header filenames to include for new/reference implementations."
  (with-open-file (out pathname :direction :output :if-exists :supersede
                       :if-does-not-exist :create)
    (emit-compare-preamble out include-new include-old)
    (let ((tests '()))
      (dolist (k kernels)
        (push (emit-kernel-test-c k out :atol atol :rtol rtol) tests))
      (emit-main-for-tests (nreverse tests) out))
    pathname))
;;; ------------------------------------------------------------
;;; Convenience: stretch kernel comparison harness
;;; ------------------------------------------------------------




;;; ------------------------------------------------------------
;;; New API: emit common preamble, per-kernel test, and main
;;; ------------------------------------------------------------

(defun emit-compare-preamble (stream &key dumpp)
  "Emit standard includes, plus INCLUDE-NEW and INCLUDE-OLD, and helper
functions almost_equal/compare_array to STREAM."
  (flet ((p (fmt &rest args) (apply #'format stream fmt args)))
    (p "/* Auto-generated kernel comparison harness */~%")
    (p "#include <stdio.h>~%#include <math.h>~%#include <string.h>~%#include <stdlib.h>~%~%")
    (p "typedef double DOUBLE; typedef size_t SIZE_T;~%")
    (p "int global_debug = 0;~%")
    (p "size_t global_position_size = 0;~%")
    (p "#define VERYSMALL 1e-6~%")
    (p "#define true 1~%")
    (p "#define false 0~%")
    (p "#define DUMP12(j1,p1,j2,p2,gv) if(global_debug){printf(\"%s hessian12[%zu,%zu] = %f\\n\",TERM_NAME,(j1+p1),(j2+p2),gv);}~%")
    (p "#define DUMP21(j1,p1,j2,p2,gv) if(global_debug){printf(\"%s hessian21[%zu,%zu] = %f\\n\",TERM_NAME,(j1+p1),(j2+p2),gv);}~%")
    (p "#define DUMP11(j1,p1,j2,p2,gv) if(global_debug){printf(\"%s hessian11[%zu,%zu] = %f\\n\",TERM_NAME,(j1+p1),(j2+p2),gv);}~%")
    (p "#define GDUMP(ii,oo,gg) if(global_debug){printf(\"%s grad[%zu] = %f\\n\",TERM_NAME,(ii+oo),gg);}~%")
    (p "#define KernelGradientAcc(ii,oo,gg) {GDUMP(ii,oo,gg);force[ii+oo] += gg;};~%")
    (p "#define KernelOffDiagHessAcc(ii1,oo1,ii2,oo2,gg) {\\
~a;\\
hessian[(ii1+oo1)*global_position_size+(ii2+oo2)]+=gg;\\
hessian[(ii2+oo2)*global_position_size+(ii1+oo1)]+=gg;\\
double v22=gg*dvec[ii2+oo2];double v11=gg*dvec[ii1+oo1];\\
hdvec[ii1+oo1]+=v22;hdvec[ii2+oo2]+=v11;}~%"
       (if dumpp
           "DUMP12(ii1,oo1,ii2,oo2,gg);DUMP21(ii2,oo2,ii1,oo1,gg)"
           ""))
    (p "#define KernelDiagHessAcc(ii1,oo1,ii2,oo2,gg) {\\
~a;\\
hessian[(ii1+oo1)*global_position_size+(ii2+oo2)]+=gg;\\
double vd=gg*dvec[ii1+oo1];\\
hdvec[ii1+oo1]+=vd;\\
}~%"
       (if dumpp
           "DUMP11(ii1,oo1,ii2,oo2,gg)"
           "")
       )
    (p "static int almost_equal(double a, double b, double atol, double rtol) { double diff=fabs(a-b); double tol=atol+rtol*fmax(fabs(a),fabs(b)); return diff<=tol; }~%")
    (p "static int compare_array(const char* label,const double* nn,const double* oo,size_t n,double atol,double rtol){int errs=0;for(size_t i=0;i<n;++i){if(!almost_equal(nn[i],oo[i],atol,rtol)){if(errs<10)fprintf(stderr,\"MISMATCH %s[%zu] %g(new) vs %g(ground)\\n\",label,i,nn[i],oo[i]);++errs;}}return errs;}~%~%")
    (p "static int compare_hessian(const char* label,const double* nn,const double* oo,size_t n,double atol,double rtol){int errs=0;for(size_t i=0;i<n;++i){if(!almost_equal(nn[i],oo[i],atol,rtol)){if(errs<10)fprintf(stderr,\"MISMATCH %s[%zu aka %zu,%zu] %g(new) vs %g(ground)\\n\",label,i,i/global_position_size,i%global_position_size,nn[i],oo[i]);++errs;}}return errs;}~%~%")
    (p "static void linear_angle_error() { printf(\"linear angle error\"); abort(); }~%" )))

(defun emit-old-call-args (kernel)
  (let ((tparams (append
                  (loop for param in (kernel-params kernel)
                        until (string= "POSITION" (second param))
                        collect (format nil "~a ~a"
                                        (string-downcase (first param))
                                        (string-downcase (second param))))
                 (list
                  "double* position"
                  "double* energy_accumulate"
                   "double* force_ground"
                   "double* hessian_ground"
                   "double* dvec_ground"
                   "double* hdvec_ground"
                   )
                 ))
        (targs (append
                (loop for param in (kernel-params kernel)
                        until (string= "POSITION" (second param))
                      collect
                                        (string-downcase (second param)))
                 (list
                  "position_size"
                  "position"
                   "force_ground"
                   "hessian_ground"
                   "dvec_ground"
                   "hdvec_ground"
                   )
                 )))
    ;; build call args
    (let ((call-args-new '())
          (call-args-old '()))
      (dolist (param (kernel-params kernel))
        (destructuring-bind (ctype pname) param
          (declare (ignore ctype))
          (let* ((pstr (string-downcase (string pname)))
                 (actual
                   (cond
                     ((string= pstr "position") "position")
                     ((string= pstr "energy_accumulate") "&e_new")
                     ((string= pstr "force") "force_new")
                     ((string= pstr "hessian") "hessian_new")
                     ((string= pstr "dvec") "dvec_new")
                     ((string= pstr "hdvec") "hdvec_new")
                     (t pstr)))
                 (actual-old
                   (cond
                     ((string= pstr "position") "position")
                     ((string= pstr "energy_accumulate") "&e_ground")
                     ((string= pstr "force") "force_ground")
                     ((string= pstr "hessian") "hessian_ground")
                     ((string= pstr "dvec") "dvec_ground")
                     ((string= pstr "hdvec") "hdvec_ground")
                     (t pstr))))
            (push actual call-args-new)
            (push actual-old call-args-old))))
      (setf call-args-new (nreverse call-args-new)
            call-args-old (nreverse call-args-old))
      (values tparams targs call-args-new call-args-old))))

(defun emit-call-args (kernel)
  (let ((tparams (append
                  (loop for param in (kernel-params kernel)
                        until (string= "POSITION" (second param))
                        collect (format nil "~a ~a"
                                        (string-downcase (first param))
                                        (string-downcase (second param))))
                  (list
                   "size_t position_size"
                   "double* position"
                   "double* force_new"
                   "double* force_ground"
                   "double* hessian_new"
                   "double* hessian_ground"
                   "double* dvec_new"
                   "double* dvec_ground"
                   "double* hdvec_new"
                   "double* hdvec_ground"
                   )
                  ))
        (targs (append
                (loop for param in (kernel-params kernel)
                        until (string= "POSITION" (second param))
                      collect
                                        (string-downcase (second param)))
                  (list
                   "position_size"
                   "position"
                   "force_new"
                   "force_ground"
                   "hessian_new"
                   "hessian_ground"
                   "dvec_new"
                   "dvec_ground"
                   "hdvec_new"
                   "hdvec_ground"
                   )
                  )))
    ;; build call args
    (let ((call-args-new '())
          (call-args-old '()))
      (dolist (param (kernel-params kernel))
        (destructuring-bind (ctype pname) param
          (declare (ignore ctype))
          (let* ((pstr (string-downcase (string pname)))
                 (actual
                   (cond
                     ((string= pstr "position") "position")
                     ((string= pstr "energy_accumulate") "&e_new")
                     ((string= pstr "force") "force_new")
                     ((string= pstr "hessian") "hessian_new")
                     ((string= pstr "dvec") "dvec_new")
                     ((string= pstr "hdvec") "hdvec_new")
                     (t pstr)))
                 (actual-old
                   (cond
                     ((string= pstr "position") "position")
                     ((string= pstr "energy_accumulate") "&e_ground")
                     ((string= pstr "force") "force_ground")
                     ((string= pstr "hessian") "hessian_ground")
                     ((string= pstr "dvec") "dvec_ground")
                     ((string= pstr "hdvec") "hdvec_ground")
                     (t pstr))))
            (push actual call-args-new)
            (push actual-old call-args-old))))
      (setf call-args-new (nreverse call-args-new)
            call-args-old (nreverse call-args-old))
      (values tparams targs call-args-new call-args-old))))


(defun emit-kernel-wrapper-c (stream kernel &key (prefix "ground") include-pathname)
  "Emit a C wrapper function for KERNEL to STREAM.

Wrapper looks like:
  void <prefix>_<kernel-name>(<params>) {
  #include \"<prefix>_<kernel-name>_function.c\"
  }

Parameters are derived via EMIT-CALL-ARGS so the signature matches the
generated kernel implementations."
  (declare (optimize (debug 3)))
  (multiple-value-bind (tparams)
      (emit-old-call-args kernel)
    (let* ((name (string-downcase (kernel-name kernel)))
           (wrapper (format nil "~a_~a" prefix name))
           (fd-stub (format nil "~a_energy_fd_function.c" (kernel-group kernel)))
           (include-file (make-pathname :name fd-stub :defaults include-pathname)))
      (format stream "#define TERM_NAME \"hessian_ground\"~%")
      (format stream "void ~a(~{~a~^, ~})~%{~%#include ~s~%}~%"
              wrapper tparams (namestring include-file))
      (format stream "#undef TERM_NAME~%"))))


(defun emit-sin-nphi-c (stream)
  "Emit a standalone helper C function:
     void sinNPhiCosNPhi(double N, double* sinNPhi, double* cosNPhi,
                         double sinPhi, double cosPhi)
   that computes sin(n*phi) and cos(n*phi) iteratively, without
   recovering phi from sin/cos."
  (format stream
"void sinNPhiCosNPhi(double N, double* sinNPhi, double* cosNPhi, double sinPhi, double cosPhi) {
  int n = (int)N;
  int sign = 1;
  if (n < 0) { n = -n; sign = -1; }
  if (n == 0) { *sinNPhi = 0.0; *cosNPhi = 1.0; return; }
  if (n == 1) { *sinNPhi = sign * sinPhi; *cosNPhi = cosPhi; return; }
  double sk = sinPhi;
  double ck = cosPhi;
  for (int k = 2; k <= n; ++k) {
    double skm1 = sk;
    double ckm1 = ck;
    sk = sinPhi * ckm1 + cosPhi * skm1;
    ck = cosPhi * ckm1 - sinPhi * skm1;
  }
  *sinNPhi = sign * sk;
  *cosNPhi = ck;
}
"))


(defun emit-kernel-test-c (stream kernel include-directory &key (atol 1d-9) (rtol 1d-6))
  "Emit a single C test function for KERNEL to STREAM. Reference impl is
finite-difference for grad/hess kernels, or the same energy kernel otherwise.
Returns the test function name as a string."
  (declare (optimize (debug 3)))
  (let* ((name (string-downcase (kernel-name kernel)))
         (group (string-downcase (kernel-group kernel)))
         (old-name (cond
                     ((kernel-compute-hess-p kernel) (format nil "~a_hessian_fd" group))
                     ((kernel-compute-grad-p kernel) (format nil "~a_gradient_fd" group))
                     (t name)))
         (params  (kernel-params kernel))
         (coord-vars (kernel-coord-vars kernel))
         (compute-grad (kernel-compute-grad-p kernel))
         (compute-hess (kernel-compute-hess-p kernel))
         (pos-size (length coord-vars))
         )
    (flet ((p (fmt &rest args) (apply #'format stream fmt args)))
      (p "#define TERM_NAME \"hessian_new\"~%")
      (p "#include \"~a/~a.c\"~%" include-directory name)
      (p "#undef TERM_NAME~%")
      (multiple-value-bind (tparams targs call-args-new call-args-old)
          (emit-call-args kernel)
        (p "static int test_~a( size_t index, ~{ ~a~^,~}) {~%" name tparams)
        (p "  double e_new=0.0, e_ground=0.0;~%")
        (p "  for (size_t ii=0; ii<position_size; ii++ ) {~%   force_new[ii] = 0.0;~%    force_ground[ii] = 0.0;~%    dvec_new[ii] = 1.0;~%    dvec_ground[ii] = 1.0;~%    hdvec_new[ii] = 0.0;~%    hdvec_ground[ii] = 0.0;~%  }~%")
        (p "  for (size_t ii=0; ii<position_size*position_size; ii++ ) {~%   hessian_new[ii] = 0.0;~%    hessian_ground[ii] = 0.0;~%  }~%")
        ;; Hessian/hdvec comparisons need looser tolerances than energy/force.
        ;; Use an absolute floor high enough to tolerate FD noise near zero.
        (p "  const double h_atol = fmax(~f, 1.0e-3);~%" atol)
        (p "  const double h_rtol = fmax(~f, 1.0e-5);~%" rtol)
        ;; build call args
        (p "  if(global_debug) {~%")
        (p "    printf(\" kernel(~a)  index: %zu\\n\", index );~%" (kernel-name kernel))
        (loop for arg in targs
              until (string= arg "position" :start1 0 :end1 (min (length arg) (length "position")))
              if (string= arg "i3x" :start1 0 :end1 (min (length arg) (length "i3x")))
                do (progn
                     (p "   printf(\" position[%zu] = %f\\n\", ~a, position[~a]);~%" arg arg)
                     (p "   printf(\" position[%zu] = %f\\n\", ~a+1, position[~a+1]);~%" arg arg)
                     (p "   printf(\" position[%zu] = %f\\n\", ~a+2, position[~a+2]);~%" arg arg)
                     )
              else
                do (p "   printf(\" ~a = %f\\n\", ~a );~%" arg arg))
        (p "  }~%")
        (p "  ~a(~a);~%" old-name (format nil "~{~a~^, ~}" call-args-old))
        (p "  ~a(~a);~%" name (format nil "~{~a~^, ~}" call-args-new))
        (p "  int errs=0;~%")
        (p "  if(!almost_equal(e_new,e_ground,~f,~f)){fprintf(stderr,\"~a: energy %g(new) vs %g(ground)\\n\",e_new,e_ground);errs++;}~%" atol rtol (kernel-name kernel))
        (when compute-grad
          (p "  errs+=compare_array(\"~a/force\",force_new,force_ground,position_size,~f,~f);~%" (kernel-name kernel) atol rtol))
        (when compute-hess
          (p "  errs+=compare_array(\"~a/hdvec\",hdvec_new,hdvec_ground,position_size,h_atol,h_rtol);~%" (kernel-name kernel))
          (p "  errs+=compare_hessian(\"~a/hessian\",hessian_new,hessian_ground,position_size*position_size,h_atol,h_rtol);~%" (kernel-name kernel))
          )
        (p "  if (errs) {~%")
        (p "   printf(\" MISMATCH in kernel(~a)  index = %zu\\n\", index);~%" (kernel-name kernel))
        (loop for arg in targs
              until (string= arg "position" :start1 0 :end1 (min (length arg) (length "position")))
              if (string= arg "i3x" :start1 0 :end1 (min (length arg) (length "i3x")))
                do (progn
                     (p "   printf(\" position[%zu] = %f\\n\", ~a, position[~a]);~%" arg arg)
                     (p "   printf(\" position[%zu] = %f\\n\", ~a+1, position[~a+1]);~%" arg arg)
                     (p "   printf(\" position[%zu] = %f\\n\", ~a+2, position[~a+2]);~%" arg arg)
                     )
              else
                do (p "   printf(\" ~a = %f\\n\", ~a );~%" arg arg))
        (p "  printf(\"----------------------------------------\\n\");")
        (p "  }~%")
        (p "  return errs;~%}~%~%")))))

(defun emit-run-all (stream position-include calls-include)
  (declare (optimize (debug 3)))
  (flet ((p (fmt &rest args) (apply #'format stream fmt args)))
    (p "int run_all_tests()~%")
    (p "{~%  size_t totals=0;~%")
    (p "  size_t* errs=&totals;~%")
    (let ((count 64))
      (p "  double position[~d];~%" count)
      (p "  size_t position_size = ~d;~%" count)
      (p "  double* force_new = (double*)malloc(sizeof(double)*position_size);~%")
      (p "  double* force_ground = (double*)malloc(sizeof(double)*position_size);~%")
      (p "  double* dvec_new = (double*)malloc(sizeof(double)*position_size);~%")
      (p "  double* dvec_ground = (double*)malloc(sizeof(double)*position_size);~%")
      (p "  double* hdvec_new = (double*)malloc(sizeof(double)*position_size);~%")
      (p "  double* hdvec_ground = (double*)malloc(sizeof(double)*position_size);~%")
      (p "  double* hessian_new = (double*)malloc(sizeof(double)*position_size*position_size);~%")
      (p "  double* hessian_ground = (double*)malloc(sizeof(double)*position_size*position_size);~%")
      (labels ((split-on-space (line)
                 (loop for start = 0 then (1+ end)
                       for end = (position #\Space line :start start)
                       collect (subseq line start end)
                       while end))
               (process-line (line)
                 (let* ((splits (split-on-space line))
                        (cmd (pop splits)))
                   (cond
                     ((string= cmd "POSITION")
                      (let* ((test (pop splits))
                             (num (parse-integer (pop splits)))
                             (positions splits))
                        (declare (ignore test))
                        (format stream "  global_position_size = ~a;~%" num)
                        (loop for idx below num
                              for pos in positions
                              do (format stream "  position[~d] = ~a;~%" idx pos))))
                     ((string= cmd "TEST")
                      (let* ((test (pop splits))
                             (num (parse-integer (pop splits)))
                             (args splits))
                        (format stream "   test_~a(~{~a~^,~});~%"
                                test
                                (append args
                                        (list num
                                              "position"
                                              "force_new" "force_ground"
                                              "hessian_new" "hessian_ground"
                                              "dvec_new" "dvec_ground"
                                              "hdvec_new" "hdvec_ground")))))
                     (t (error "Handle command ~s" cmd))))))
        (with-open-file (fin calls-include :direction :input)
          (loop for line = (read-line fin nil :eof)
                until (eq line :eof)
                do (process-line line)
                )
          (p "   return *errs;~%")
          (p "}~%"))))))

(defun emit-main-for-tests (stream)
  "Emit a main() that runs TEST-NAMES (strings) and returns non-zero on
any failure."
  (flet ((p (fmt &rest args) (apply #'format stream fmt args)))
    (p "int main(int argc, char* argv[]){~%  int total=0;~%")
    (p "  global_debug = (argc>1);~%")
    (p "  printf(\" global_debug = %d\\n\", global_debug );~%")
    (p "  total = run_all_tests();~%")
    (p "  if (total) {~%    fprintf(stderr,\"FAILED with %d mismatches\\n\", total);~%    return 1;~%  }~%")
    (p "  printf(\"OK\\n\");~%  return 0;~%}~%")))


(defun emit-group-test (stream group-name kernels)
  (declare (optimize (debug 3)))
  (multiple-value-bind (tparams targs call-args-new call-args-old)
      (emit-call-args (first kernels))
    (flet ((p (fmt &rest args) (apply #'format stream fmt args)))
      (p "void test_~a(size_t index,~{ ~a~^,~} ) {~%" group-name tparams)
      (loop for kernel in kernels
            do (p "  test_~a( index, ~{ ~a~^,~});~%" (kernel-name kernel) targs))
      (p "}~%")
      )))


(defun emit-c-tests (kernel-list include-pathname position-include call-include c-tests-pathname)
  (ensure-directories-exist c-tests-pathname)
  (let ((mathkernel-pathname (make-pathname :name "mathkernel" :type "c" :defaults c-tests-pathname)))
    (with-open-file (stream mathkernel-pathname :direction :output :if-exists :supersede)
      (emit-compare-preamble stream :dumpp t)
      (emit-sin-nphi-c stream)
      (let ((group-kernels (make-hash-table :test 'equal)))
        (loop for kernel in kernel-list
              do (push kernel (gethash (kernel-group kernel) group-kernels))
              do (emit-kernel-test-c stream kernel include-pathname)
              )
        (maphash (lambda (group-name kernels)
                   (emit-group-test stream group-name kernels))
                 group-kernels))
      (emit-run-all stream position-include call-include)
      (emit-main-for-tests stream))))
