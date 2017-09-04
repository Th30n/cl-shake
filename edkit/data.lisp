;;;; Underlying data model types and operations
;;;; The defined data types are used to create a data model which can be
;;;; edited and presented in GUI editors. Every editable data must inherit
;;;; from `DATA' class. The idea behind this custom data model is that data
;;;; modifications can be tracked. This allows for storing changes for
;;;; undo/redo mechanism and notifying viewers of the data to refresh their
;;;; views (e.g. GUI displays).

(in-package #:edkit.data)

;;; Base data class and observing changes
;;; Observers can be attached to `DATA' subtypes. Modifications of data slots
;;; through accessors is reported to observers. For simplicity of
;;; implementation, there are no particular event types nor extra information
;;; on what is observed. For example, observers cannot register themselves to
;;; observe only a particular type of change. If such features become
;;; required, they should be implemented.

(defclass data ()
  ((observers :accessor observers :initform (tg:make-weak-hash-table :weakness :key)))
  (:documentation "Base class for all editable data."))

(defun observe (data observer &key tag)
  "Attach an OBSERVER function to DATA."
  (check-type data data)
  (check-type observer function)
  (pushnew observer (gethash (if tag tag observer) (observers data))))

(defun unobserve (data &key observer tag)
  "Detach an OBSERVER from DATA."
  (check-type data data)
  (assert (or observer tag) (observer tag)
          "At least one of OBSERVER or TAG must be supplied.")
  (unless tag
    (setf tag observer))
  (if observer
      (let ((observers (gethash tag (observers data))))
        (setf (gethash tag (observers data)) (remove observer observers)))
      (remhash tag (observers data))))

(defun notify-observers (data)
  "Call NOTIFY method on all of the DATA observers."
  (check-type data data)
  ;; TODO: What if NOTIFY triggers another NOTIFY-OBSERVERS?
  (maphash (lambda (_ observers)
             (declare (ignore _))
             (dolist (obs observers)
               (funcall obs)))
           (observers data)))

;;; Tracking data changes

;;; The `CHANGE-TRACKER' stores all tracked data modifications to provide undo
;;; and redo logs. Modifying a `DATA' slot via its accessor will store a
;;; `CHANGE' in the currently bound `CHANGE-OPERATION'. Multiple changes can
;;; be stored in a single operation. A single undo or redo processes all
;;; changes in a single operation. A tracker should be bound at the start of
;;; the program (`WITH-CHANGE-TRACKER'). Afterwards, the modification of
;;; `DATA' instances should be done inside `WITH-CHANGE-OPERATION'
;;; body. Otherwise, the changes will not be tracked and therefore will not be
;;; undo-able. Obviously, multiple changes should be grouped under a single
;;; operation if it makes sense for them to be treated as a single change from
;;; the perspective of undo and redo mechanism.

(defstruct change
  data
  slot
  old-value
  new-value)

(defstruct change-operation
  (description "" :type string)
  (workingp nil :type boolean)
  (changes nil))

(defclass change-tracker ()
  ((undop :accessor undop :initform nil :type boolean)
   (redop :accessor redop :initform nil :type boolean)
   (undolog :accessor undolog :initform nil)
   (redolog :accessor redolog :initform nil)))

(defun make-change-tracker ()
  (make-instance 'change-tracker))

(defun call-with-change-operation (description function)
  (declare (special *change-tracker* *change-operation*))
  ;; Nesting reuses the top operation
  (let* ((nestedp (boundp '*change-operation*))
         (*change-operation* (if nestedp
                                *change-operation*
                                (make-change-operation :description description))))
    (declare (special *change-operation*))
    (prog1 (funcall function)
      (when (and (not nestedp) (change-operation-changes *change-operation*))
        (push *change-operation* (undolog *change-tracker*))
        ;; Reset the redolog
        (setf (redolog *change-tracker*) nil)))))

(defmacro with-change-operation ((description) &body body)
  `(call-with-change-operation ,description (lambda () ,@body)))

(defun track-change (change-operation data slot old-value new-value)
  (if (change-operation-workingp change-operation)
      (error "Tracking a change while undoing/redoing!")
      (push (make-change :data data :slot slot :old-value old-value :new-value new-value)
            (change-operation-changes change-operation))))

(defun undo ()
  (declare (special *change-tracker*))
  (when (or (undop *change-tracker*) (redop *change-tracker*))
    (error "Recursive undo!"))
  (flet ((undo-change (change)
           (setf (slot-value (change-data change) (change-slot change))
                 (change-old-value change))))
    (setf (undop *change-tracker*) t)
    (let ((change-operation (pop (undolog *change-tracker*))))
      (setf (change-operation-workingp change-operation) t)
      (dolist (change (change-operation-changes change-operation))
        (undo-change change))
      (setf (change-operation-workingp change-operation) nil)
      (push change-operation (redolog *change-tracker*)))
    (setf (undop *change-tracker*) nil)))

(defun redo ()
  (declare (special *change-tracker*))
  (when (or (undop *change-tracker*) (redop *change-tracker*))
    (error "Recursive redo!"))
  (flet ((redo-change (change)
           (setf (slot-value (change-data change) (change-slot change))
                 (change-new-value change))))
    (setf (redop *change-tracker*) t)
    (let ((change-operation (pop (redolog *change-tracker*))))
      (setf (change-operation-workingp change-operation) t)
      (dolist (change (reverse (change-operation-changes change-operation)))
        (redo-change change))
      (setf (change-operation-workingp change-operation) nil)
      (push change-operation (undolog *change-tracker*)))
    (setf (redop *change-tracker*) nil)))

(defun call-with-change-tracker (function)
  (let ((*change-tracker* (make-change-tracker)))
    (declare (special *change-tracker*))
    (funcall function *change-tracker*)))

(defmacro with-change-tracker ((change-tracker) &body body)
  `(call-with-change-tracker (lambda (,change-tracker)
                               (declare (ignorable ,change-tracker))
                               ,@body)))

;;; Metaclass which sets up the observer and tracking mechanisms

(defclass data-class (standard-class)
  ()
  (:documentation
   "Metaclass for all editable data classes. It ensures that the `DATA' class
  is inherited from and sets up the obervers and tracking mechanisms."))

;; TODO: Check these methods with AMOP
(defmethod initialize-instance :around ((class data-class) &rest initargs &key direct-superclasses)
  "Add `DATA' as a superclass if it is missing."
  (if (find (find-class 'data) direct-superclasses
            :test (lambda (data-class superclass) (subtypep data-class superclass)))
      (call-next-method)
      (apply #'call-next-method
             class
             :direct-superclasses (cons (find-class 'data) direct-superclasses)
             initargs)))

(defmethod reinitialize-instance :around ((class data-class) &rest initargs
                                          &key (direct-superclasses nil direct-superclasses-p))
  (if (not direct-superclasses-p)
      (call-next-method)
      (if (find (find-class 'data) direct-superclasses
                :test (lambda (data-class superclass) (subtypep data-class superclass)))
          (call-next-method)
          (apply #'call-next-method
                 class
                 :direct-superclasses (cons (find-class 'data) direct-superclasses)
                 initargs))))

(defmethod (setf slot-value-using-class) (new-value (class data-class) object slotd)
  "Tracks slot changes in *CHANGE-OPERATION*"
  (declare (special *change-operation*))
  (let ((slot-name (slot-definition-name slotd)))
    (if (or (eq slot-name 'observers) (not (boundp '*change-operation*)))
        ;; Don't track observers change or if not inside an operation
        (call-next-method)
        (let ((old-value (slot-value object (slot-definition-name slotd))))
          (prog1 (call-next-method)
            (track-change *change-operation* object slot-name old-value new-value))))))

(defmethod (setf slot-value-using-class) :after (new-value (class data-class) object slotd)
  "Slot changes on `DATA-CLASS' classes provoke observer notification."
  (unless (eq (slot-definition-name slotd) 'observers)
    ;; It makes no sense to notify observers when observers change.
    (notify-observers object)))

(defmethod validate-superclass ((class data-class) (superclass standard-class))
  "`DATA-CLASS' is compatible with STANDARD-CLASS"
  t)

(defmacro defdata (name direct-superclasses direct-slots &rest options)
  "Macro for DEFCLASS which sets the `DATA-CLASS' as the metaclass."
  `(defclass ,name ,direct-superclasses ,direct-slots ,@options (:metaclass data-class)))

;;; Concrete data types

(defdata boxed-string ()
  ((value :accessor value :initarg :value :type string :initform "")))

(defdata boxed-double ()
  ((value :reader value :initarg :value :type double-float :initform 0d0)))

(defmethod (setf value) (value (boxed-double boxed-double))
  (setf value (coerce value 'double-float))
  (setf (slot-value boxed-double 'value) value)
  value)
