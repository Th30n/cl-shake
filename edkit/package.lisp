(in-package #:cl-user)

(defpackage #:edkit.data
  (:nicknames #:edk.data)
  (:use #:closer-common-lisp)
  (:export #:ref
           #:get-val
           #:set-val
           #:data
           #:data-class
           #:defdata
           #:notify
           #:observe
           #:unobserve
           #:boxed-string
           #:boxed-double
           #:value
           #:with-change-tracker
           #:with-change-operation
           #:undo
           #:redo))

(defpackage #:edkit.forms
  (:nicknames #:edk.forms)
  (:use #:cl+qt)
  (:export #:build-widget
           #:label
           #:text
           #:left-right
           #:top-down
           #:editor
           #:text-entry
           #:button
           #:selector
           #:double-spinner))

(defpackage #:edkit
  (:nicknames #:edk)
  (:use #:cl
        #:edkit.data
        #:edkit.forms)
  (:export #:main))

