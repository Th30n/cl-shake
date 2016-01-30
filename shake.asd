(in-package #:asdf-user)

(defsystem "shake"
  :description "shake: A Doom like game"
  :version "0.0.1"
  :author "Teon Banek <theongugl@gmail.com>"
  :licence "MIT"
  :depends-on ("sdl2" "cl-opengl" "cffi" "alexandria")
  :components
  ((:module "shake"
            :components ((:file "package")
                         (:file "shake" :depends-on ("package"))))))
