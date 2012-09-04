(defsystem opt-check
  :description "Keep track of optimizations and easily benchmark them."
  :long-description "see README.md"
  :author "Greg Pfeil <greg@technomadic.org>"
  :license "MIT"
  :pathname "src/"
  :components ((:file "package")
               (:file "opt-check" :depends-on ("package")))
  :in-order-to ((test-op (load-op opt-check.tests)))
  :perform (test-op :after (op c)
                    (funcall (intern "RUN!" :opt-check.tests)
                             (intern "TESTS" :opt-check.tests))))

(defmethod operation-done-p ((op test-op) (c (eql (find-system :opt-check))))
  (values nil))

(defsystem opt-check.tests
  :depends-on (opt-check fiveam)
  :pathname "tests/"
  :components ())
