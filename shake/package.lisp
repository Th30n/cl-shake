(in-package #:cl-user)

(defpackage #:shake
  (:use #:cl)
  (:export #:main)
  (:import-from #:alexandria #:read-file-into-string))
