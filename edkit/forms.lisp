(in-package #:edkit.forms)
(in-readtable :qtools)

;;; Forms

(defclass form ()
  ((widget :initarg :widget :initform nil :accessor widget)))

(defgeneric build-widget (form)
  (:documentation "Build underlying GUI widgets."))

(defclass label ()
  ((text :initarg :text :accessor text :type string)))

(defun label (text)
  (declare (type string text))
  (check-type text string)
  (make-instance 'label :text text))

(defmethod build-widget ((form label))
  (q+:make-qlabel (text form)))

(defclass layout ()
  ((subforms :initarg :subforms :accessor subforms)
   (orientation :initarg :orientation :accessor orientation)))

(defun left-right (&rest forms)
  (make-instance 'layout :subforms forms :orientation :horizontal))

(defun top-down (&rest forms)
  (make-instance 'layout :subforms forms :orientation :vertical))

(defmethod build-widget ((form layout))
  (let ((widget (q+:make-qwidget))
        (layout (ecase (orientation form)
                  (:horizontal (q+:make-qhboxlayout))
                  (:vertical (q+:make-qvboxlayout)))))
    (dolist (subform (subforms form))
      (q+:add-widget layout (build-widget subform)))
    (q+:set-layout widget layout)
    widget))

;;; Editors

(defclass editor (form)
  ((target :initarg :target :accessor target :type data)
   (updating-data-p :initform nil :accessor updating-data-p :type boolean)))

(defclass text-entry (editor)
  ())

(defun text-entry (target)
  ;; check is string
  (let ((text-entry (make-instance 'text-entry :target target)))
    (flet ((update-text-from-data ()
             (unless (or (updating-data-p text-entry) (not (widget text-entry)))
               (q+:set-text (widget text-entry) (edk.data:value (target text-entry))))))
      (edk.data:observe target #'update-text-from-data))
    text-entry))

(define-widget line-edit (QLineEdit)
  ((text-entry :initarg :text-entry)))

(define-slot (line-edit on-editing-finished) ()
  (declare (connected line-edit (editing-finished)))
  (setf (updating-data-p text-entry) t)
  (setf (edk.data:value (target text-entry)) (q+:text line-edit))
  (setf (updating-data-p text-entry) t))

(defmethod build-widget ((form text-entry))
  (let ((line-ed (make-instance 'line-edit :text-entry form)))
    (q+:set-text line-ed (edk.data:value (target form)))
    (setf (widget form) line-ed)))

;;; Buttons

(defclass button ()
  ((action :initarg :action :accessor action)
   (text :initarg :text :accessor text)))

(defun button (text action)
  (make-instance 'button :text text :action action))

(define-widget push-button (QPushButton)
  ((action :initarg :action)))

(define-slot (push-button on-clicked) ((checked bool))
  (declare (connected push-button (clicked bool)))
  (funcall action))

(defmethod build-widget ((form button))
  (let ((widget (make-instance 'push-button :action (action form))))
    (q+:set-text widget (text form))
    widget))

;;; Selectors

(defclass selector (editor)
  ((choices :initarg :choices :accessor choices)))

(defun selector (target choice &rest choices)
  ;; check target
  (make-instance 'selector :target target :choices (cons choice choices)))

(define-widget combo-box (QComboBox)
  ((selector :initarg :selector)))

(define-slot (combo-box on-activated) ((index int))
  (declare (connected combo-box (activated int)))
  (edk.data:set-val (target selector) (nth index (choices selector))))

(defmethod build-widget ((form selector))
  (let ((widget (make-instance 'combo-box :selector form)))
    (dolist (choice (choices form))
      (q+:add-item widget (string choice)))
    widget))
