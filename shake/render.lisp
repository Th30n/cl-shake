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
  (offset 0)
  texture
  (draw-count 0)
  (max-bytes 0)
  (free-p nil))

(defun init-batch (byte-size)
  (let ((buffer (car (gl:gen-buffers 1)))
        (vertex-array (gl:gen-vertex-array)))
    (gl:bind-buffer :array-buffer buffer)
    (%gl:buffer-data :array-buffer byte-size (cffi:null-pointer)
                     :static-draw)
    (make-batch :vertex-array vertex-array :buffer buffer
                :max-bytes byte-size)))

(defun free-batch (batch)
  (gl:delete-buffers (list (batch-buffer batch)))
  (gl:delete-vertex-arrays (list (batch-vertex-array batch)))
  (setf (batch-free-p batch) t))

(defun draw-batch (batch)
  (assert (not (batch-free-p batch)))
  (gl:bind-vertex-array (batch-vertex-array batch))
  (let ((stride (* 4 (+ 3 3 3 2)))
        (color-offset (* 4 3))
        (normal-offset (* 4 (+ 3 3)))
        (uv-offset (* 4 (+ 3 3 3)))
        (tex-name (batch-texture batch)))
    ;; positions
    (gl:bind-buffer :array-buffer (batch-buffer batch))
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
    (gl:vertex-attrib-pointer 2 2 :float nil stride uv-offset)
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
  (flet ((fill-buffer (buf byte-offset data)
           (gl:bind-buffer :array-buffer buf)
           (gl:buffer-sub-data :array-buffer (cdr data)
                               :buffer-offset byte-offset)
           (car data)))
    (with-struct (batch- offset buffer) batch
      (let ((gl-data (smdl::surface-gl-data surface)))
        (incf (batch-draw-count batch)
              (list-length (smdl:surface-faces surface)))
        (incf (batch-offset batch) (fill-buffer buffer offset gl-data))
        (when-let* ((texinfo (sbsp:sidedef-texinfo surface))
                    (tex-name (string-downcase (sbsp:texinfo-name texinfo))))
          (assert (or (not (batch-texture batch))
                      (string= (batch-texture batch) tex-name)))
          (setf (batch-texture batch) tex-name))))))

(defun render-surface (surface)
  (declare (special *batches*))
  (let ((current-batch (car *batches*))
        (surface-space (car (smdl:surface-gl-data surface))))
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
                 (car (push (init-batch (max surface-space (* 10 1024))) ;; 10 kB
                            *batches*)))))
        (add-surface-vertex-data surface batch)))))

(defmacro with-draw-frame (() &body body)
  "Establishes the environment where SHAKE.RENDER package functions can be
  used."
  (with-gensyms (body-result)
    `(let ((*batches* nil))
       (declare (special *batches*))
       (let ((,body-result (multiple-value-list (progn ,@body))))
         (dolist (batch (reverse *batches*))
           (draw-batch batch)
           (free-batch batch))
         (values-list ,body-result)))))
