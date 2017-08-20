(in-package #:asdf-user)

(defsystem "edkit-test"
  :depends-on ("edkit" "prove")
  :defsystem-depends-on ("prove-asdf")
  :components ((:test-file "data-test"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
