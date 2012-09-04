(defpackage opt-check.tests
  (:use #:cl #:fiveam #:opt-check)
  (:export #:run!))

(in-package #:opt-check.tests)

(def-suite tests)

(in-suite tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *old-features* *features*)
  (defvar *old-opt-style* *optimization-style*)
  (push :opt-check-enabled *features*)
  (setf *optimization-style* :compare))

(test should-return-result
  (is (= 4 (check-opt (nth 4 '(0 1 2 3 4 5))
                      (aref #(0 1 2 3 4 5) 4)))))

(test should-warn-if-results-dont-match
  (signals warning
    (check-opt (nth 4 '(0 1 2 3 4 5))
               (aref #(0 1 2 3 4 5) 3))))

(test shouldnt-warn-if-we-tell-it-not-to
  (is (= 4 (check-opt (nth 4 '(0 1 2 3 4 5))
                      (aref #(0 1 2 3 4 5) 3)
                      :warnp nil))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (psetf *features* *old-features*
         *optimization-style* *old-opt-style*))
