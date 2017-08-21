(in-package #:edkit.forms)
(in-readtable :qtools)

;;; Forms

;; Silly thing to forward Qt signals to function/method calls outside of
;; QWidget classes.
(define-widget dummy-qobject (QObject)
  ((form :initarg :form)))

(defclass form ()
  ((widget :initarg :widget :initform nil :accessor widget
           :documentation "Underlying widget of this form.")
   (dummy-qobject :initform nil :reader dummy-qobject)))

(defmethod initialize-instance :after ((form form) &key)
  (setf (slot-value form 'dummy-qobject)
        (make-instance 'dummy-qobject :form form)))

(defgeneric create-widget (form)
  (:documentation "Create the underlying GUI widget."))

(defgeneric destroy-widget (form)
  (:documentation "Remove reference to underlying GUI widget."))

(defmethod destroy-widget ((form form))
  ;; No-op, since all is done in :after
  )

(defmethod destroy-widget :after ((form form))
  (when (widget form)
    (fsetf (widget form) nil)))

(define-slot (dummy-qobject on-destroyed) ()
   (destroy-widget form))

(defun build-widget (form)
  "Calls `CREATE-WIDGET' on FORM and performs additional setup of the FORM"
  (check-type form form)
  (assert (not (widget form)) (form)
          "BUILD-WIDGET called on already built FORM.")
  (let ((widget (create-widget form)))
    (assert widget (widget)
            "CREATE-WIDGET needs to make an underlying GUI widget")
    (connect! widget (destroyed) (dummy-qobject form) (on-destroyed))
    (setf (widget form) widget)))

(defclass label (form)
  ((text :initarg :text :accessor text :type string)))

(defun label (text)
  (check-type text string)
  (make-instance 'label :text text))

(defmethod create-widget ((form label))
  (q+:make-qlabel (text form)))

;;; Layout

(defclass layout (form)
  ((subforms :initarg :subforms :accessor subforms)
   (orientation :initarg :orientation :accessor orientation)))

(defun left-right (&rest forms)
  (make-instance 'layout :subforms forms :orientation :horizontal))

(defun top-down (&rest forms)
  (make-instance 'layout :subforms forms :orientation :vertical))

(defmethod create-widget ((form layout))
  (let ((widget (q+:make-qwidget))
        (layout (ecase (orientation form)
                  (:horizontal (q+:make-qhboxlayout))
                  (:vertical (q+:make-qvboxlayout)))))
    (dolist (subform (subforms form))
      (q+:add-widget layout (build-widget subform)))
    (q+:set-layout widget layout)
    widget))

;;; Buttons

(defclass button (form)
  ((action :initarg :action :accessor action)
   (text :initarg :text :accessor text)))

(defun button (text action)
  (make-instance 'button :text text :action action))

(define-widget push-button (QPushButton)
  ((action :initarg :action)))

(define-slot (push-button on-clicked) ((checked bool))
  (declare (connected push-button (clicked bool)))
  (funcall action))

(defmethod create-widget ((form button))
  (let ((widget (make-instance 'push-button :action (action form))))
    (q+:set-text widget (text form))
    widget))

;;; Editors

(defclass editor (form)
  ((data :initform nil :initarg :data :reader data :type (or null edk.data:data))
   (updating-data-p :initform nil :accessor updating-data-p :type boolean)))

(defgeneric set-data-from-widget (editor)
  (:documentation "Update DATA of EDITOR with the value in widget"))

(defmethod set-data-from-widget :before ((editor editor))
  (assert (not (updating-data-p editor)) (editor)
          "Recursively updating editor data!")
  (setf (updating-data-p editor) t))

(defmethod set-data-from-widget :after ((editor editor))
  (setf (updating-data-p editor) nil))

(defgeneric set-widget-from-data (editor)
  (:documentation "Update WIDGET of EDITOR with the value from DATA"))

(defmethod initialize-instance :after ((editor editor) &key)
  (let ((data (slot-value editor 'data)))
    (when data
      (edk.data:observe data (lambda ()
                               (unless (or (updating-data-p editor) (not (widget editor)))
                                 (set-widget-from-data editor)))
                        :tag editor))))

(defgeneric (setf data) (data editor))

(defmethod (setf data) (data (editor editor))
  (check-type data (or null edk.data:data))
  (assert (not (updating-data-p editor)) (editor)
          "Unexpected data change while updating!")
  (when (data editor)
    (edk.data:unobserve (data editor) :tag editor))
  (setf (slot-value editor 'data) data)
  (if data
      (progn
        (edk.data:observe data
                          (lambda ()
                            (unless (or (updating-data-p editor) (not (widget editor)))
                              (set-widget-from-data editor)))
                          :tag editor)
        (when (widget editor)
          (set-widget-from-data editor)))
      ;; Disable the editor when there's no data
      ;; TODO: Make disabling a method?
      (when (widget editor)
        (setf (q+:enabled (widget editor)) nil)))
  data)

(defclass text-entry (editor)
  ())

(defmethod set-data-from-widget ((text-entry text-entry))
  (edk.data:with-change-operation ("Change text")
    (setf (edk.data:value (data text-entry)) (q+:text (widget text-entry)))))

(defmethod set-widget-from-data ((text-entry text-entry))
  (q+:set-text (widget text-entry) (edk.data:value (data text-entry))))

(defun text-entry (&optional data)
  (let ((text-entry (make-instance 'text-entry :data data)))
    text-entry))

(define-widget line-edit (QLineEdit)
  ((text-entry :initarg :text-entry)))

(define-slot (line-edit on-editing-finished) ()
  (declare (connected line-edit (editing-finished)))
  (unless (or (not (data text-entry))
              (string= (edk.data:value (data text-entry)) (q+:text line-edit)))
    (set-data-from-widget text-entry)))

(defmethod create-widget ((form text-entry))
  (let ((line-ed (make-instance 'line-edit :text-entry form)))
    (if (data form)
        (setf (q+:text line-ed) (edk.data:value (data form)))
        (setf (q+:enabled line-ed) nil))
    line-ed))

;;; Selectors

(defclass selector (editor)
  ((choices :initarg :choices :accessor choices)))

(defun selector (data choice &rest choices)
  ;; check data
  (setf (edk.data:value data) choice)
  (make-instance 'selector :data data :choices (cons choice choices)))

(define-widget combo-box (QComboBox)
  ((selector :initarg :selector)))

(define-slot (combo-box on-activated) ((index int))
  (declare (connected combo-box (activated int)))
  (set-data-from-widget selector))

(defmethod set-data-from-widget ((selector selector))
  (edk.data:with-change-operation ("select")
    (setf (edk.data:value (data selector))
          (nth (q+:current-index (widget selector)) (choices selector)))))

(defmethod set-widget-from-data ((selector selector))
  (setf (q+:current-index (widget selector))
        ;; TODO: Maybe specify test predicate?
        (position (edk.data:value (data selector)) (choices selector))))

(defmethod create-widget ((form selector))
  (let ((widget (make-instance 'combo-box :selector form)))
    (dolist (choice (choices form))
      (q+:add-item widget (string choice)))
    widget))
