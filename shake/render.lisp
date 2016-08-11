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

(defun add-surface-vertex-data (surface vertex-buffers &key (offset 0))
  (destructuring-bind
        (position-buffer color-buffer uv-buffer normal-buffer) vertex-buffers
    ;; positions
    (sgl:with-gl-array (position-gl-array :float (smdl:surface-faces surface))
      (gl:bind-buffer :array-buffer position-buffer)
      (gl:buffer-sub-data :array-buffer position-gl-array))
    ;; colors
    (sgl:with-gl-array (data :float (make-list 6 :initial-element
                                               (smdl:surface-color surface)))
      (gl:bind-buffer :array-buffer color-buffer)
      (gl:buffer-sub-data :array-buffer data))
    ;; normals
    (let ((normals (make-list 6 :initial-element
                              (v2->v3 (sbsp:lineseg-normal
                                       (sbsp:sidedef-lineseg surface))))))
      (sgl:with-gl-array (data :float normals)
        (gl:bind-buffer :array-buffer normal-buffer)
        (gl:buffer-sub-data :array-buffer data)))
    ;; uvs
    (when (sbsp:sidedef-texinfo surface)
      (sgl:with-gl-array (data :float (smdl:surface-texcoords surface))
        (gl:bind-buffer :array-buffer uv-buffer)
        (gl:buffer-sub-data :array-buffer data)))))

(defun render-surface (surface)
  (sdata:with-resources "render"
    (let ((buffers (sdata:add-res "buffers" (lambda () (gl:gen-buffers 4))
                                  #'gl:delete-buffers)))
      (flet ((init-buffer (buf byte-size)
               (gl:bind-buffer :array-buffer buf)
               (%gl:buffer-data :array-buffer byte-size (cffi:null-pointer)
                                :stream-draw)))
        (dolist (buf buffers)
          (init-buffer buf (* 10 1024)))) ;; 10kB
      (add-surface-vertex-data surface buffers)
      (destructuring-bind (positions colors uvs normals) buffers
        ;; positions
        (gl:bind-buffer :array-buffer positions)
        (gl:enable-vertex-attrib-array 0)
        (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))
        ;; colors
        (gl:bind-buffer :array-buffer colors)
        (gl:enable-vertex-attrib-array 1)
        (gl:vertex-attrib-pointer 1 3 :float nil 0 (cffi:null-pointer))
        ;; normals
        (gl:bind-buffer :array-buffer normals)
        (gl:enable-vertex-attrib-array 3)
        (gl:vertex-attrib-pointer 3 3 :float nil 0 (cffi:null-pointer))
        ;; uvs
        (let ((tex-name (when-let ((texinfo (sbsp:sidedef-texinfo surface)))
                          (string-downcase (sbsp:texinfo-name texinfo)))))
          (sgl:with-uniform-locations (sdata:res "shader-prog") (has-albedo tex-albedo)
            (if tex-name
                (progn
                  (gl:active-texture :texture0)
                  (gl:bind-texture :texture-2d (sdata:res tex-name))
                  (gl:uniformi tex-albedo-loc 0)
                  (gl:uniformi has-albedo-loc 1)
                  (gl:bind-buffer :array-buffer uvs)
                  (gl:enable-vertex-attrib-array 2)
                  (gl:vertex-attrib-pointer 2 2 :float nil 0 (cffi:null-pointer)))
                (progn
                  (gl:uniformi has-albedo-loc 0)
                  (gl:disable-vertex-attrib-array 2)))))
        (gl:draw-arrays :triangles 0 (list-length (smdl:surface-faces surface)))))))
