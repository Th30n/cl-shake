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

(defgeneric notify (observer)
  (:documentation "Called by the observed data when it changes."))

(defun observe (observer data)
  "Attach an OBSERVER to DATA."
  (check-type data data)
  (setf (gethash observer (observers data)) t))

(defun unobserve (observer data)
  "Detach an OBSERVER from DATA."
  (check-type data data)
  (remhash observer (observers data)))

(defun notify-observers (data)
  "Call NOTIFY method on all of the DATA observers."
  (check-type data data)
  ;; TODO: What if NOTIFY triggers another NOTIFY-OBSERVERS?
  (maphash (lambda (obs _)
             (declare (ignore _))
             (notify obs))
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

(defclass change-tracker ()
  ((undop :accessor undop :initform nil :type boolean)
   (redop :accessor redop :initform nil :type boolean)
   (undolog :accessor undolog :initform nil)
   (redolog :accessor redolog :initform nil)))

(defun make-change-tracker ()
  (make-instance 'change-tracker))

(defun track-change (change-tracker data slot old-value new-value)
  (unless (or (undop change-tracker) (redop change-tracker))
    (push (make-change :data data :slot slot :old-value old-value :new-value new-value)
          (undolog change-tracker))))

(defun undo ()
  (declare (special *change-tracker*))
  (flet ((undo-change (change)
           (setf (slot-value (change-data change) (change-slot change))
                 (change-old-value change))
           ;; (notify-observers (change-data change))
           ))
    (setf (undop *change-tracker*) t)
    (let ((change (pop (undolog *change-tracker*))))
      (undo-change change)
      (push change (redolog *change-tracker*)))
    (setf (undop *change-tracker*) nil)))

(defun redo ()
  (declare (special *change-tracker*))
  (flet ((redo-change (change)
           (setf (slot-value (change-data change) (change-slot change))
                 (change-new-value change))
           ;; (notify-observers (change-data change))
           ))
    (setf (redop *change-tracker*) t)
    (let ((change (pop (redolog *change-tracker*))))
      (redo-change change)
      (push change (undolog *change-tracker*)))
    (setf (redop *change-tracker*) nil)))

(defun call-with-change-tracker (function)
  (let ((*change-tracker* (make-change-tracker)))
    (declare (special *change-tracker*))
    (funcall function)))

(defmacro with-change-tracker (() &body body)
  `(call-with-change-tracker (lambda () ,@body)))

;;; Concrete data types

(defclass boxed-string (data)
  ((value :reader value :initarg :value :type string :initform "")))

(defgeneric (setf value) (value boxed-string))

(defmethod (setf value) (value (boxed-string boxed-string))
  (declare (special *change-tracker*))
  (when (boundp '*change-tracker*)
    (track-change *change-tracker* boxed-string 'value (value boxed-string) value))
  (setf (slot-value boxed-string 'value) value)
  (notify-observers boxed-string))

(defmacro ref (var)
  (let ((val (gensym))
        (val-p (gensym)))
    `(lambda (&optional (,val nil ,val-p))
       (if ,val-p (setf ,var ,val) ,var))))

(defun get-val (target)
  (funcall target))

(defun set-val (target value)
  (funcall target value))
