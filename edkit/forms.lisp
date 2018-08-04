;;;; GUI forms and editors for `EDKIT.DATA:DATA' instances.
;;;; `FORM' and `EDITOR' subtypes are used to describe the GUI. Each subtype
;;;; handles that updates are correctly propagated between underlying widgets
;;;; (Qt in our case) and data types. Ideally, the user should never have to
;;;; work with the underlying widget library.

(in-package #:edkit.forms)
(in-readtable :qtools)

;;; Forms
;;; A form is a GUI view of some data (not necessarily editable). Each form
;;; should be able to construct its underlying widget as well as destroy
;;; it. Destruction should not be tied to `FORM' finalization, since we may
;;; want to reuse forms. For example, a modal editor may present the same
;;; forms at different times. When a form isn't being shown, its underlying
;;; widgets should be destroyed and removed.

;; Silly thing to forward Qt signals to function/method calls outside of
;; QWidget classes.
(define-widget dummy-qobject (QObject)
  ((form :initarg :form)))

(defclass form ()
  ((widget :initarg :widget :initform nil :accessor widget
           :documentation "Underlying widget of this form.")
   ;; TODO: Connect enabledp with direct disable of widget
   (enabledp :initarg :enabledp :initform t :reader enabledp
             :documentation "True if the form and its widget(s) are enabled.")
   (dummy-qobject :initform nil :reader dummy-qobject))
  (:documentation "Base class for a view of some data. It encapsulates the
  underlying widget."))

(defgeneric create-widget (form)
  (:documentation "Create the underlying GUI widget."))

(defgeneric destroy-widget (form)
  (:documentation "Remove all references to underlying GUI widget(s)."))

(defmethod destroy-widget ((form form))
  ;; No-op, since all is done in :after
  )

(defmethod destroy-widget :after ((form form))
  (when (widget form)
    (fsetf (widget form) nil)))

(defmethod initialize-instance :after ((form form) &key)
  (setf (slot-value form 'dummy-qobject)
        (make-instance 'dummy-qobject :form form)))

(defmethod (setf enabledp) (value (form form))
  (check-type value boolean)
  (when (widget form)
    (setf (q+:enabled (widget form)) value)))

(define-slot (dummy-qobject on-destroyed) ()
   (destroy-widget form))

(defun build-widget (form)
  "Calls `CREATE-WIDGET' on FORM and performs additional setup of the FORM"
  (check-type form form)
  (assert (not (widget form)) (form)
          "BUILD-WIDGET called on already built FORM.")
  ;; TODO: Track live widgets for debugging
  (let ((widget (create-widget form)))
    (assert widget (widget)
            "CREATE-WIDGET needs to make an underlying GUI widget")
    (connect! widget (destroyed) (dummy-qobject form) (on-destroyed))
    (setf (widget form) widget)))

;;; Label

(defclass label (form)
  ((text :initarg :text :reader text :type string)))

(defmethod (setf text) (text (label label))
  (check-type text string)
  (setf (slot-value label 'text) text)
  (when (widget label)
    (setf (q+:text (widget label)) text)))

(defun label (text)
  (check-type text string)
  (make-instance 'label :text text))

(defmethod create-widget ((form label))
  (q+:make-qlabel (text form)))

;;; Layout
;;; A layout is a form containg multiple subforms arranged in a certain
;;; layout.

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

(defmethod destroy-widget :after ((form layout))
  (dolist (subform (subforms form))
    (destroy-widget subform)))

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
;;; Editor stores and observers `EDKIT.DATA:DATA' instance. Changes made by
;;; the user via GUI are propagated to the data. Similarly, any programatic
;;; changes to the data are propagated to the editor and its widgets.

(defclass editor (form)
  ((data :initform nil :initarg :data :reader data :type (or null edk.data:data))
   (updating-data-p :initform nil :accessor updating-data-p :type boolean)))

(defgeneric set-data-from-widget (editor)
  (:documentation "Update DATA of EDITOR with the value in
  widget. Modification of DATA is done inside a
  `EDK.DATA:WITH-CHANGE-OPERATION' block. This method is called when the GUI
  widget changes its value, e.g. a user makes an edit."))

(defgeneric set-widget-from-data (editor)
  (:documentation "Update WIDGET of EDITOR with the value from DATA. This is
  called in 2 cases. One case is when the DATA slot of the EDITOR is
  changed. The other case is when the value stored in DATA is changed, e.g. by
  calling `EDK.DATA:UNDO'"))

(defmethod set-data-from-widget :around ((editor editor))
  (assert (not (updating-data-p editor)) (editor)
          "Recursively updating editor data!")
  (setf (updating-data-p editor) t)
  (prog1 (call-next-method)
    (setf (updating-data-p editor) nil)))

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
  (when data
        (edk.data:observe data
                          (lambda ()
                            (unless (or (updating-data-p editor) (not (widget editor)))
                              (set-widget-from-data editor)))
                          :tag editor)
        (when (widget editor)
          (set-widget-from-data editor)))
  ;; Disable the editor when there's no data
  (setf (enabledp editor) (when data t))
  data)

;;; Compound editor
;;; A compound editor consists of multiple editors. Each subeditor is used to
;;; view and edit a part of a compound `EDK.DATA:DATA' type. Implementors
;;; of compound editors should only have to provide the layout of various
;;; forms and editors along with the mapping of main data slots to their
;;; corresponding sub editors.

(defclass compound-editor (editor)
  ((subeditors :initform (make-hash-table) :accessor subeditors
               :documentation "Map from slot name to a subeditor responsible for editing it.")
   (top-form :initform nil :accessor top-form
             :documentation "Top `FORM' containing all the subforms.")
   (layout-info :initform nil :accessor layout-info
                :documentation "Information on how the subeditors should be
                laid out in the GUI and mapped to slots of DATA."))
  (:documentation "Base class for editors of compound `EDK.DATA:DATA' types."))

(defmethod create-widget :before ((ed compound-editor))
  (labels ((build-form (form-info)
             (let ((form-name (car form-info))
                   (make-form (cadr form-info))
                   (form-args (mapcar #'build-layout (cddr form-info))))
               (declare (ignore form-name))
               ;; TODO: Add subforms mapping
               (apply make-form form-args)))
           (build-editor (edit-info)
             (let ((form (build-form edit-info))
                   (slot-name (car edit-info)))
               (setf (data form) (slot-value (data ed) slot-name)
                     (gethash slot-name (subeditors ed)) form)
               form))
           (build-layout (layout-info)
             (if (atom layout-info)
                 layout-info
                 (cond
                   ((eq :edit (car layout-info))
                    (build-editor (cdr layout-info)))
                   ((eq :form (car layout-info))
                    (build-form (cdr layout-info)))
                   (t (apply (car layout-info)
                             (mapcar #'build-layout (cdr layout-info))))))))
    (setf (top-form ed) (build-layout (layout-info ed)))))

(defmethod create-widget ((editor compound-editor))
  (build-widget (top-form editor)))

(defmethod destroy-widget :after ((editor compound-editor))
  (destroy-widget (top-form editor)))

(defmethod set-data-from-widget ((editor compound-editor))
  ;; Everything should be handled by the subeditors, so default implementation
  ;; can be empty.
  ;; TODO: Propagate this from subeditors
  )

(defmethod set-widget-from-data ((editor compound-editor))
  (maphash (lambda (slot-name subeditor)
             (setf (data subeditor) (slot-value (data editor) slot-name)))
           (subeditors editor)))

;;; Various editor implementations

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

(defclass double-spinner (editor)
  ((minimum :initarg :min :initform nil :type (or null real))
   (maximum :initarg :max :initform nil :type (or null real))
   (step :initarg :step :initform 0 :type (real 0))))

(defun double-spinner (&key data min max (step 1))
  (make-instance 'double-spinner :data data :min min :max max :step step))

(define-widget double-spinbox (QDoubleSpinBox)
  ((spinner :initarg :spinner)))

(define-slot (double-spinbox on-editing-finished) ()
  (declare (connected double-spinbox (editing-finished)))
  (set-data-from-widget spinner))

(defmethod set-data-from-widget ((double-spinner double-spinner))
  (edk.data:with-change-operation ("change double")
    (setf (edk.data:value (data double-spinner))
          (q+:value (widget double-spinner)))))

(defmethod set-widget-from-data ((double-spinner double-spinner))
  (setf (q+:value (widget double-spinner)) (edk.data:value (data double-spinner))))

(defmethod create-widget ((double-spinner double-spinner))
  (let ((spinbox (make-instance 'double-spinbox :spinner double-spinner)))
    (if (data double-spinner)
        (setf (q+:value spinbox) (edk.data:value (data double-spinner)))
        (setf (q+:enabled spinbox) nil))
    (with-slots (minimum maximum step) double-spinner
      (if minimum (setf (q+:minimum spinbox) (coerce minimum 'double-float)))
      (if maximum (setf (q+:maximum spinbox) (coerce maximum 'double-float)))
      (if step (setf (q+:single-step spinbox) step)))
    spinbox))

;;; Selectors

(defclass selector (editor)
  ((choices :initarg :choices :accessor choices)))

(defun selector (data choice &rest choices)
  ;; check data
  (when data
    (setf (edk.data:value data) choice))
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
