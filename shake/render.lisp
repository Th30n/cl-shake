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

(in-package #:shake.render)

(defstruct batch
  vertex-array
  buffer
  mapped
  (offset 0 :type fixnum)
  texture
  (draw-count 0 :type fixnum)
  (max-bytes 0 :type fixnum)
  (free-p nil))

(defun init-batch (byte-size)
  (let ((buffer (car (gl:gen-buffers 1)))
        (vertex-array (gl:gen-vertex-array)))
    (gl:bind-buffer :array-buffer buffer)
    (%gl:buffer-data :array-buffer byte-size (cffi:null-pointer)
                     :static-draw)
    (make-batch :vertex-array vertex-array :buffer buffer
                :max-bytes byte-size
                :mapped (sgl:map-buffer :array-buffer (/ byte-size 4) :float
                                        '(:map-write-bit :map-unsynchronized-bit
                                          :map-invalidate-buffer
                                          :map-flush-explicit-bit)))))

(defun free-batch (batch)
  (with-struct (batch- vertex-array buffer mapped) batch
    (when mapped
      (gl:bind-buffer :array-buffer buffer)
      (gl:unmap-buffer :array-buffer)
      (gl:bind-buffer 0)
      (setf (batch-mapped batch) nil))
    (gl:delete-buffers (list buffer))
    (gl:delete-vertex-arrays (list vertex-array))
    (setf (batch-free-p batch) t)))

(defun finish-batch (batch)
  (gl:bind-vertex-array (batch-vertex-array batch))
  (gl:bind-buffer :array-buffer (batch-buffer batch))
  (let ((stride (* 4 (+ 3 3 3 2)))
        (color-offset (* 4 3))
        (normal-offset (* 4 (+ 3 3)))
        (uv-offset (* 4 (+ 3 3 3))))
    ;; positions
    (when (batch-mapped batch)
      (%gl:flush-mapped-buffer-range :array-buffer 0 (batch-offset batch))
      (gl:unmap-buffer :array-buffer)
      (setf (batch-mapped batch) nil))
    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 0 3 :float nil stride (cffi:null-pointer))
    ;; colors
    (gl:enable-vertex-attrib-array 1)
    (gl:vertex-attrib-pointer 1 3 :float nil stride color-offset)
    ;; normals
    (gl:enable-vertex-attrib-array 3)
    (gl:vertex-attrib-pointer 3 3 :float nil stride normal-offset)
    ;; uvs
    (gl:enable-vertex-attrib-array 2)
    (gl:vertex-attrib-pointer 2 2 :float nil stride uv-offset))
  (gl:bind-vertex-array 0))

(defun draw-batch (batch)
  (assert (not (batch-free-p batch)))
  (when (batch-mapped batch)
    (finish-batch batch))
  (let ((tex-name (batch-texture batch)))
    (gl:bind-vertex-array (batch-vertex-array batch))
    (sgl:with-uniform-locations (sdata:res "shader-prog") (has-albedo tex-albedo)
      (if tex-name
          (progn
            (gl:active-texture :texture0)
            (gl:bind-texture :texture-2d (sdata:res tex-name))
            (gl:uniformi tex-albedo-loc 0)
            (gl:uniformi has-albedo-loc 1))
          (gl:uniformi has-albedo-loc 0)))
    (gl:draw-arrays :triangles 0 (batch-draw-count batch)))
  (gl:check-error)
  (gl:bind-vertex-array 0))

(defun add-surface-vertex-data (surface batch)
  (flet ((fill-buffer (byte-offset data)
           (cffi:foreign-funcall
            "memcpy"
            :pointer (cffi:mem-aptr (gl::gl-array-pointer (batch-mapped batch))
                                    :float (/ byte-offset 4))
            :pointer (gl::gl-array-pointer (cdr data))
            :int (car data))
           (car data)))
    (with-struct (batch- offset) batch
      (let ((gl-data (smdl::surface-gl-data surface)))
        (incf (batch-draw-count batch)
              (list-length (smdl:surface-faces surface)))
        (incf (batch-offset batch)
              (the fixnum (fill-buffer offset gl-data)))
        (when-let* ((texinfo (sbsp:sidedef-texinfo surface))
                    (tex-name (string-downcase (sbsp:texinfo-name texinfo))))
          (setf (batch-texture batch) tex-name))))))

(defun init-draw-frame ()
  (make-array 10 :element-type 'batch :fill-pointer 0))

(defun finish-draw-frame (batches)
  (declare (type (vector batch) batches)
           (optimize (speed 3) (space 3)))
  (dotimes (i (length batches))
    (let ((batch (aref batches i)))
      (draw-batch batch)
      (free-batch batch))))

(defun add-new-batch (byte-size)
  (declare (special *batches*))
  (declare (optimize (speed 3) (space 3)))
  (when-let ((current-batch (get-current-batch)))
    (finish-batch current-batch))
  (let ((batch (init-batch byte-size)))
    (vector-push-extend batch *batches*)
    batch))

(defun get-current-batch ()
  (declare (special *batches*)
           (optimize (speed 3) (space 3)))
  (when (length>= 1 *batches*)
    (aref (the (vector batch) *batches*)
          (1- (length (the (vector batch) *batches*))))))

(defun render-surface (surface)
  (let ((current-batch (get-current-batch))
        (surface-space (car (smdl:surface-gl-data surface))))
    (declare (type fixnum surface-space))
    (labels ((tex-match-p (batch)
               (let ((current-texture (batch-texture batch))
                     (texinfo (sbsp:sidedef-texinfo surface)))
                 (or (and (not current-texture) (not texinfo))
                     (and current-texture texinfo
                          (string= current-texture
                                   (string-downcase (sbsp:texinfo-name texinfo)))))))
             (can-add-p (batch)
               (with-struct (batch- offset max-bytes) batch
                 (let ((free-space (- max-bytes offset)))
                   (and (> free-space surface-space)
                        (tex-match-p batch))))))
      (let ((batch
             (if (and current-batch (can-add-p current-batch))
                 current-batch
                 (add-new-batch (max surface-space (* 10 1024)))))) ;; 10kB
        (add-surface-vertex-data surface batch)))))

(defmacro with-draw-frame (() &body body)
  "Establishes the environment where SHAKE.RENDER package functions can be
  used."
  (with-gensyms (body-result)
    `(let ((*batches* (init-draw-frame)))
       (declare (special *batches*))
       (let ((,body-result (multiple-value-list (progn ,@body))))
         (finish-draw-frame *batches*)
         (values-list ,body-result)))))
