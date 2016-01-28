(in-package #:asdf-user)

(defsystem "shake-bspc"
  :description "shake-bspc: BSP compiler for shake, a Doom like game"
  :version "0.0.1"
  :author "Teon Banek <theongugl@gmail.com>"
  :licence "MIT"
  :components
  ((:module "shake-bspc"
            :components ((:file "package")
                         (:file "shake-bspc" :depends-on ("package")))))
  :in-order-to ((test-op (test-op shake-bspc-test))))
