(in-package #:edkit)

(defun main (form)
  (qtools:with-main-window (window (build-widget form)
                                   :name "Editor Kit Testing")))
