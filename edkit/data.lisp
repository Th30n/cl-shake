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
        (push *change-operation* (undolog *change-tracker*))))))

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
                 (change-old-value change))
           (notify-observers (change-data change))))
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
                 (change-new-value change))
           (notify-observers (change-data change))))
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

;;; Concrete data types

(defclass boxed-string (data)
  ((value :reader value :initarg :value :type string :initform "")))

(defgeneric (setf value) (value boxed-string))

(defmethod (setf value) (value (boxed-string boxed-string))
  (declare (special *change-operation*))
  (when (boundp '*change-operation*)
    (track-change *change-operation* boxed-string 'value (value boxed-string) value))
  (setf (slot-value boxed-string 'value) value)
  (notify-observers boxed-string)
  value)

(defclass boxed-double (data)
  ((value :reader value :initarg :value :type double-float :initform 0d0)))

(defgeneric (setf value) (value boxed-double))

(defmethod (setf value) (value (boxed-double boxed-double))
  (declare (special *change-operation*))
  (setf value (coerce value 'double-float))
  (when (boundp '*change-operation*)
    (track-change *change-operation* boxed-double 'value (value boxed-double) value))
  (setf (slot-value boxed-double 'value) value)
  (notify-observers boxed-double)
  value)
