(in-package #:cl-user)

(defpackage #:edkit.data
  (:nicknames #:edk.data)
  (:use #:closer-common-lisp)
  (:export #:data
           #:data-class
           #:defdata
           #:notify
           #:observe
           #:unobserve
           #:boxed-double
           #:boxed-string
           #:boxed-symbol
           #:value
           #:with-change-tracker
           #:with-change-operation
           #:undo
           #:redo))

(defpackage #:edkit.forms
  (:nicknames #:edk.forms)
  (:use #:cl+qt)
  (:export #:build-widget
           #:destroy-widget
           #:label
           #:text
           #:left-right
           #:top-down
           #:editor
           #:compound-editor
           #:editor-data
           #:layout-info
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

