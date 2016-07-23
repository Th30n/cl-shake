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

(defconstant +clip-square+ 0.5
  "Size of the square used for expanding brushes in order to perform movement
  clipping (collision detection).")

(defstruct bspfile
  nodes
  clip-nodes)

(defun write-bspfile (bspfile stream)
  (with-struct (bspfile- nodes clip-nodes) bspfile
    (write-bsp nodes stream)
    (write-bsp clip-nodes stream)))

(defun read-bspfile (stream)
  (let ((nodes (read-bsp stream))
        (clip-nodes (read-bsp stream)))
    (make-bspfile :nodes nodes :clip-nodes clip-nodes)))

(defun write-map (brushes stream)
  (format stream "~S~%" (length brushes))
  (dolist (brush brushes)
    (sbrush::write-brush brush stream)))

(defun read-map (stream)
  (let ((n (read stream)))
    (repeat n (sbrush::read-brush stream))))

(defun read-and-compile-map (stream)
  (let ((brushes (read-map stream)))
    (flet ((compile-brushes (bs)
             (build-bsp (sbrush:prepare-brushes-for-bsp bs))))
      (make-bspfile :nodes (compile-brushes brushes)
                    :clip-nodes (compile-brushes
                                 (mapcar (lambda (b)
                                           (sbrush:expand-brush
                                            b :square +clip-square+))
                                         brushes))))))

(defun compile-map-file (map-file bsp-filename)
  "Compile a map from MAP-FILE and store it into BSP-FILENAME"
  (let (bspfile)
    (with-open-file (mf map-file)
      (setf bspfile (read-and-compile-map mf)))
    (with-open-file (bf bsp-filename :direction :output :if-exists :supersede
                        :if-does-not-exist :create)
      (write-bspfile bspfile bf))))
