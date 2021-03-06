(in-package #:opt-check)

(defvar *time-statistics* ())

(defun internal-run-time ()
  #+ccl (ccl::%internal-run-time)
  #-(or ccl) (values 0 0))

(defun gc-time ()
  #+ccl (ccl:gctime)
  #-(or ccl) 0)

(defun page-fault-info ()
  #+ccl (ccl::%page-fault-info)
  #-(or ccl) (values 0 0 0))

(defun total-bytes-allocated ()
  #+ccl (ccl::total-bytes-allocated)
  #-(or ccl) 0)

;; stolen from CCL, needed because the wonderful *REPORT-TIME-FUNCTION* is not
;; part of the standard.
(defun gather-time (form thunk)
  (flet ((integer-size-in-bytes (i)
           (if (typep i 'fixnum)
             0
             (* (logand (+ 2 (ccl:uvsize i)) (lognot 1)) 4))))
    (multiple-value-bind (user-start system-start)
        (internal-run-time)
      (multiple-value-bind (minor-start major-start swaps-start)
          (page-fault-info)
        (let* ((initial-real-time (get-internal-real-time))
               (initial-gc-time (gc-time))
               (initial-consed (total-bytes-allocated))           
               (initial-overhead (integer-size-in-bytes initial-consed)))
          (let* ((results (multiple-value-list (funcall thunk))))
            (declare (dynamic-extent results))
            (multiple-value-bind (user-end system-end)
                (internal-run-time)
              (multiple-value-bind (minor-end major-end swaps-end)
                  (page-fault-info)
                (let* ((new-consed (total-bytes-allocated))		     
                       (bytes-consed
                        (- new-consed (+ initial-overhead initial-consed)))
                       (elapsed-real-time
                        (- (get-internal-real-time) initial-real-time))
                       (elapsed-gc-time (- (gc-time) initial-gc-time))
                       (elapsed-user-time
                        (- user-end user-start))
                       (elapsed-system-time
                        (- system-end system-start))
                       (elapsed-minor (- minor-end minor-start))
                       (elapsed-major (- major-end major-start))
                       (elapsed-swaps (- swaps-end swaps-start)))
                  (list results
                        elapsed-real-time
                        elapsed-user-time
                        elapsed-system-time
                        elapsed-gc-time
                        bytes-consed
                        elapsed-minor
                        elapsed-major
                        elapsed-swaps
                        form))))))))))

(defvar *timing-context*)

(defvar *optimization-style* :on
  "This can be :ON, :OFF, or :COMPARE. :ON means that the optimized
  (second) form in CHECK-OPT is used, :OFF means the unoptimized (first) form
   is used, and :COMPARE means that both are used & stats are collected.")

(defmacro with-perf-context ((&optional identifier) &body body)
  "This allows the usual CHECK-OPT to be deconstructed a bit. The body is
   expected to have one instance each of BASE-PERF and COMP-PERF which contain
   the forms to be timed."
  `(let ((*timing-context* (list nil nil)))
     (multiple-value-prog1 (progn ,@body)
       (push (append (list nil)
                     (mapcar (lambda (new old)
                               (if (zerop old)
                                   (if (zerop new) 1 "INF")
                                   (/ new old)))
                             (subseq (second *timing-context*) 1 9)
                             (subseq (first *timing-context*) 1 9))
                     (list (or ,identifier "ratio")))
             *timing-context*)
       (push *timing-context* *time-statistics*))))

(defmacro base-perf (form &optional label)
  "Contains the form to be timed and treated as the baseline."
  `(let ((results (gather-time (or ,label ',form) (lambda () ,form))))
     (setf (car *timing-context*) results)
     (values-list (car results))))

(defmacro comp-perf (form &optional label)
  "Contains the optimized form to be timed."
  `(let ((results (gather-time (or ,label ',form) (lambda () ,form))))
     (setf (cadr *timing-context*) results)
     (values-list (car results))))

(defmacro check-opt
    (baseline comparison
     &key (test #'eql) (warnp t) identifier iterations never-compare-p)
  "This can be used two different ways. One is to keep direct code and
   optimized code side-by-side, so intent can be seen. In this use case, you
   can also toggle between the two, for benchmarking purposes, and to see
   the overall effect of the optimizations. You can also enable _both_ direct
   and optimized versions. It collects timing info on each, and it then ensures
   that the results match before returning them.

   The other way to use this is as a benchmarking suite, similar to a test
   suite. Used in the way that executes both forms, mentioned above, stats are
   collected and REPORT can print them out all nice.

   The keyword options only apply in the case that both forms are being
   executed.

   If WARNP is non-NIL, then TEST is the test to use to compare the results
  (and warn if they don't pass). It can be either a function, which is then
   applied to each of the values returned, or it can be a list of functions
   with each used to test one of the values.

   IDENTIFIER is the name to use for these results in the report.

   ITERATIONS is how many times to run the forms, in order to get more
   consistent results.

   NEVER-COMPARE-P is used to indicate that the forms have side-effects that
   you'd rather not duplicate, and it makes sure that both forms are never
   executed in the same run."
  #-opt-check-enabled (declare (ignore baseline test warnp identifier iterations
                                       never-compare-p))
  #-opt-check-enabled comparison
  #+opt-check-enabled
  (ecase *optimization-style*
    (:off baseline)
    (:on  comparison)
    (:compare
     (if never-compare-p
         baseline
         `(with-perf-context (,identifier)
            (let ((base-results (multiple-value-list
                                 ,(if iterations
                                      `(base-perf (dotimes (x ,(1- iterations) ,baseline) ,baseline)
                                                  (format nil "~A x ~D" ',baseline ,iterations))
                                      `(base-perf ,baseline))))
                  (comp-results (multiple-value-list
                                 ,(if iterations
                                      `(comp-perf (dotimes (x ,(1- iterations) ,comparison) ,comparison)
                                                  (format nil "~A x ~D" ',comparison ,iterations))
                                      `(comp-perf ,comparison)))))
              (declare (ignorable comp-results))
              ,(when warnp
                 `(unless (and (= (length base-results) (length comp-results))
                               ,(if (and (listp test) (listp (car test)))
                                    `(every #'funcall
                                            ,test base-results comp-results)
                                    `(every ,test base-results comp-results)))
                    (warn "Comparison results were not the same.~%~
                              Direct: ~S~%~
                           Optimized: ~S"
                          base-results comp-results)))
              (values-list base-results)))))))

(defun report (&key (stream t) group-by)
  "Display collected timing info. STREAM behaves as it does for FORMAT."
  (format stream "~&|--------time--------|  bytes  page faults   swaps  form~%~
                  real  user  system  gc         minor  major")
  (format stream "~{~:{~&~*~4F  ~4F  ~6F  ~2F  ~5F  ~5F  ~5F  ~5F  ~16S~}~%~%~}"
          *time-statistics*))

(defun clear ()
  "Erase collected timing info."
  (setf *time-statistics* ()))
