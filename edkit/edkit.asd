(in-package #:asdf-user)

(asdf:defsystem "edkit"
  :description "edkit: A simple framework for creating GUI editors"
  :author "Teon Banek <theongugl@gmail.com>"
  :license "GPL2"
  :depends-on ("trivial-garbage" "qtools" "qtcore" "qtgui")
  :serial t
  :components ((:file "package")
               (:file "data")
               (:file "forms")
               (:file "edkit")))

