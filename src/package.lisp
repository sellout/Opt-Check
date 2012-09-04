(defpackage opt-check
  (:use #:cl)
  (:export #:with-perf-context #:base-perf #:comp-perf
           #:*optimization-style*
           #:check-opt
           #:report #:clear))
