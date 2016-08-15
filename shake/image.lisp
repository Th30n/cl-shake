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

(defstruct image
  (name nil :type string :read-only t)
  (tex-type :texture-2d :read-only t)
  (width 0 :type fixnum :read-only t)
  (height 0 :type fixnum :read-only t)
  (files nil :type list :read-only t)
  (gl-tex -1 :type integer))

(defun load-image (image)
  (assert (not (image-loaded-p image)))
  (setf (image-gl-tex image) (first (gl:gen-textures 1)))
  (bind-image image)
  (flet ((read-image-from-file (fname)
           (if-let ((fname (sdata:data-path
                            (concatenate 'string "share/textures/" fname))))
             (sdl2:load-bmp fname)
             (sdl2:load-bmp (sdata:data-path "share/textures/missing.bmp")))))
    (with-struct (image- tex-type width height files) image
      (ecase tex-type
        (:texture-2d-array
         (gl:tex-image-3d :texture-2d-array 0 :srgb8 width height
                          (list-length files) 0 :bgr :unsigned-byte
                          (cffi:null-pointer))
         (dolist-enum (layer fname files)
           (let ((data (read-image-from-file fname)))
             ;; missing.bmp is of different resolution; TODO: Procedurally
             ;; fill a default texture of required size.
             ;; (assert (= width (sdl2:surface-width data)))
             ;; (assert (= height (sdl2:surface-height data)))
             (gl:tex-sub-image-3d :texture-2d-array 0 0 0 layer
                                  (sdl2:surface-width data)
                                  (sdl2:surface-height data) 1
                                  :bgr :unsigned-byte (sdl2:surface-pixels data))
             (sdl2:free-surface data)))))
      (gl:generate-mipmap tex-type)
      (gl:tex-parameter tex-type :texture-min-filter :linear-mipmap-nearest)
      (gl:tex-parameter tex-type :texture-mag-filter :linear)))
  image)

(defun purge-image (image)
  (when (image-loaded-p image)
    (gl:delete-textures (list (image-gl-tex image)))
    (setf (image-gl-tex image) -1)))

(defun bind-image (image)
  (unless (image-loaded-p image)
    (load-image image))
  (with-struct (image- tex-type gl-tex) image
    (gl:bind-texture tex-type gl-tex)))

(defun image-loaded-p (image)
  (/= (image-gl-tex image) -1))

(defun image-storage-size (image)
  "Return the estimated size in bytes this image is using in GL."
  (if (not (image-loaded-p image))
      0d0
      (with-struct (image- width height files) image
        (* width height (list-length files)
           3 ;; 3 bytes per pixel; TODO: Calc for different formats.
           (/ 4 3))))) ;; Account for mipmaps; TODO: Calc without mipmaps.
