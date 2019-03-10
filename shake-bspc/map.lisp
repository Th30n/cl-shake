;;;; Copyright (C) 2016 Teon Banek
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License along
;;;; with this program; if not, write to the Free Software Foundation, Inc.,
;;;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

(in-package #:shake-bspc)

(defconstant +clip-square+ 0.25
  "Size of the square used for expanding brushes in order to perform movement
  clipping (collision detection).")

(defstruct bspfile
  nodes
  clip-nodes
  things)

(defun print-bspfile-info (bspfile)
  (check-type bspfile bspfile)
  (format t "Nodes: ~A~%"
          (bsp-trav (bspfile-nodes bspfile)
                    (lambda (front back)
                      (list
                       (+ (first front) (first back)) ;; node count
                       (1+ (min (second front) (second back))) ;; min depth
                       (1+ (max (third front) (third back))))) ;; max depth
                    '(1 1 1)))
  (format t "Clip Nodes: ~A~%"
          (bsp-trav (bspfile-clip-nodes bspfile)
                    (lambda (front back)
                      (list
                       (+ (first front) (first back)) ;; node count
                       (1+ (min (second front) (second back))) ;; min depth
                       (1+ (max (third front) (third back))))) ;; max depth
                    '(1 1 1)))
  (format t "Things: ~A~%" (length (bspfile-things bspfile))))

(defstruct map-thing
  "A thing on a map. Contains slots POS and ANGLE for spawn position.  If a
  thing is brush based (e.g. a door or a platform), they are stored in BRUSHES
  slot.  The TYPE slot determines the thing type."
  (type nil :type (member :player-spawn :shotgun :enemy :door))
  (pos (v 0 0) :type (vec 2))
  (angle #.(shiva-float 0.0)
         :type (shiva-float #.(shiva-float 0.0) #.(shiva-float 360d0)))
  brushes)

(defun write-map-thing (thing stream)
  (with-struct (map-thing- type pos angle brushes) thing
    (format stream ":THING~%")
    (write-string (string-upcase
                   (format nil "~S~%~S ~S~%~S~%" type (vx pos) (vy pos) angle))
                  stream)
    (format stream "~S~%" (length brushes))
    (dolist (brush brushes)
      (sbrush:write-brush brush stream))))

(defun read-map-thing (stream)
  (let ((name (read stream))
        (type (read stream))
        (pos (v (read stream) (read stream)))
        (angle (shiva-float (read stream)))
        (brush-count (read stream)))
    (declare (ignore name))
    (make-map-thing :type type :pos pos :angle angle
                    :brushes (repeat brush-count (sbrush:read-brush stream)))))

(defstruct map-file
  brushes
  things)

(defun write-bspfile (bspfile stream)
  (with-struct (bspfile- nodes clip-nodes things) bspfile
    (write-bsp nodes stream)
    (write-bsp clip-nodes stream)
    (format stream "~S~%" (length things))
    (dolist (thing things)
      (write-map-thing thing stream))))

(defun read-bspfile (stream)
  (let ((nodes (read-bsp stream))
        (clip-nodes (read-bsp stream))
        (thing-count (read stream)))
    (make-bspfile :nodes nodes :clip-nodes clip-nodes
                  :things (repeat thing-count (read-map-thing stream)))))

(defun write-map (mapfile stream)
  (with-struct (map-file- brushes things) mapfile
    (format stream "~S~%" (length brushes))
    (dolist (brush brushes)
      (sbrush:write-brush brush stream))
    (format stream "~S~%" (length things))
    (dolist (thing things)
      (write-map-thing thing stream))))

(defun read-map (stream)
  (let* ((brush-count (read stream))
         (brushes (repeat brush-count (sbrush:read-brush stream)))
         (thing-count (read stream))
         (things (repeat thing-count (read-map-thing stream))))
    (make-map-file :brushes brushes
                   :things things)))

(define-condition invalid-map-file-error (error)
  ((message :initform "" :initarg :message :reader message)))

(defun check-map-file (map-file)
  (let ((player-spawn-count 0))
    (dolist (thing (map-file-things map-file))
      (when (eq :player-spawn (map-thing-type thing))
        (incf player-spawn-count)))
    (unless (= 1 player-spawn-count)
      (error 'invalid-map-file-error
             :message (if (zerop player-spawn-count)
                          "Missing player spawn point."
                          "More than one player spawn point found."))))
  map-file)

(defun read-and-compile-map (stream)
  (let ((map-file (check-map-file (read-map stream))))
    (with-struct (map-file- brushes things) map-file
      (flet ((compile-brushes (bs)
               (build-bsp (sbrush:prepare-brushes-for-bsp bs))))
        (make-bspfile :nodes (compile-brushes brushes)
                      :clip-nodes (compile-brushes
                                   (mapcar (lambda (b)
                                             (sbrush:expand-brush
                                              b :square +clip-square+))
                                           brushes))
                      :things things)))))

(defun compile-map-file (map-file bsp-filename)
  "Compile a map from MAP-FILE and store it into BSP-FILENAME"
  (let (bspfile)
    (with-open-file (mf map-file)
      (setf bspfile (read-and-compile-map mf)))
    (with-open-file (bf bsp-filename :direction :output :if-exists :supersede
                        :if-does-not-exist :create)
      (write-bspfile bspfile bf))))
