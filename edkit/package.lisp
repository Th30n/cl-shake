(in-package #:cl-user)

(defpackage #:edkit.data
  (:nicknames #:edk.data)
  (:use #:cl)
  (:export #:ref
           #:get-val
           #:set-val
           #:data
           #:notify
           #:observe
           #:unobserve
           #:boxed-string
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
           #:left-right
           #:top-down
           #:editor
           #:text-entry
           #:button
           #:selector))

(defpackage #:edkit
  (:nicknames #:edk)
  (:use #:cl
        #:edkit.data
        #:edkit.forms)
  (:export #:main))

