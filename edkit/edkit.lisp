(in-package #:edkit)

(defun main (form)
  (qtools:with-main-window (window (build-widget form)
                                   :name "Editor Kit Testing")))

(defdata string-and-double ()
  ((str :type boxed-string :initform (make-instance 'boxed-string :value "string"))
   (dbl :type boxed-double :initform (make-instance 'boxed-double))))

(defclass string-and-double-editor (compound-editor)
  ())

(defmethod initialize-instance :after ((ed string-and-double-editor) &key)
  (setf (layout-info ed)
        '(top-down
          (left-right (:edit str text-entry) (:edit dbl double-spinner))
          (:form lbl label "label"))))

(defmethod edk.forms::set-data-from-widget :after ((ed string-and-double-editor))
  (format nil "set-data-from-widget~%")
  ;; TODO: Change lbl here, to test change propagation
  )

(defun compound-example (string-and-double)
  (check-type string-and-double string-and-double)
  (let ((ed (make-instance 'string-and-double-editor :data string-and-double)))
    (with-change-tracker (tracker)
      (qtools:with-main-window (window (build-widget ed) :name "Compound Example"))))
  string-and-double)
