(in-package #:asdf-user)

(defsystem "shake-bspc-test"
  :depends-on ("shake-bspc"
               "prove")
  :defsystem-depends-on ("prove-asdf")
  :components
  ((:module "shake-bspc"
            :components ((:test-file "shake-bspc-test"))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
