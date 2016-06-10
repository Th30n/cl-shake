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

(defun write-map (brushes stream)
  (format stream "~S~%" (length brushes))
  (dolist (brush brushes)
    (sbrush::write-brush brush stream)))

(defun read-map (stream)
  (let ((n (read stream)))
    (loop repeat n collecting (sbrush::read-brush stream))))

(defun read-and-compile-map (stream)
  (let ((brushes (read-map stream)))
    (build-bsp (sbrush::prepare-brushes-for-bsp brushes))))

(defun compile-map-file (map-file bsp-file)
  "Compile a map from MAP-FILE and store it into BSP-FILE"
  (let ((bsp))
    (with-open-file (mf map-file)
      (setf bsp (read-and-compile-map mf)))
    (with-open-file (bf bsp-file :direction :output :if-exists :supersede
                        :if-does-not-exist :create)
      (write-bsp bsp bf))))
