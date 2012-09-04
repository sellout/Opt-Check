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

(defvar *optimization-style* :on)

(defmacro with-perf-context ((&optional identifier) &body body)
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
  `(let ((results (gather-time (or ,label ',form) (lambda () ,form))))
     (setf (car *timing-context*) results)
     (values-list (car results))))

(defmacro comp-perf (form &optional label)
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

   The other way to use this is as a benchmarking system. Lets say, for example,
   that you have a new hash-table implementation, "
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

(defun report (stream &key group-by)
  "Display collected timing info. STREAM behaves as it does for FORMAT."
  (format stream "~&|--------time--------|  bytes  page faults   swaps  form~%~
                  real  user  system  gc         minor  major")
  (format stream "~{~:{~&~*~4F  ~4F  ~6F  ~2F  ~5F  ~5F  ~5F  ~5F  ~16S~}~%~%~}"
          *time-statistics*))

(defun clear ()
  "Erase collected timing info."
  (setf *time-statistics* ()))
