;;; Data model of the editor

(in-package #:shake-ed.model)

(defstruct mbrush
  "Map representation of SBRUSH:BRUSH."
  (brush nil :type sbrush:brush)
  (rotation 0d0 :type double-float))

(edk.data:defdata texture ()
  ((offset-x :type edk.data:boxed-double :initarg :offset-x
             :reader texture-offset-x
             :initform (make-instance 'edk.data:boxed-double))
   (offset-y :type edk.data:boxed-double :initarg :offset-y
             :reader texture-offset-y
             :initform (make-instance 'edk.data:boxed-double))
   (name :type edk.data:boxed-string :initarg :name :reader texture-name
         :initform (make-instance 'edk.data:boxed-string))
   (draw-mode :type edk.data:boxed-symbol :initarg :draw-mode
              :reader texture-draw-mode
              :initform (make-instance 'edk.data:boxed-symbol :value :tile))))

(edk.data:defdata sector ()
  ((floor-height :type edk.data:boxed-double :initarg :floor-height
                 :reader sector-floor-height
                 :initform (make-instance 'edk.data:boxed-double))))
