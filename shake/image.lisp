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
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (files nil :type list :read-only t)
  (gl-tex -1 :type integer)
  ;; In case of array texture, maps file name to layer index.
  (layer-map (make-hash-table :test #'equal)))

(defun image-path (name)
  (sdata:data-path (concatenate 'string "share/textures/" name)))

(defun get-image-layer (image name)
  (with-struct (image- tex-type layer-map) image
    (assert (eq tex-type :texture-2d-array))
    (assert (image-loaded-p image))
    (gethash name layer-map)))

(defun load-image (image)
  (assert (not (image-loaded-p image)))
  (flet ((read-image-from-file (fname)
           (sdl2:load-bmp (image-path fname))))
    (with-struct (image- tex-type files) image
      (setf (image-gl-tex image) (first (gl:gen-textures 1)))
      (bind-image image)
      (ecase tex-type
        (:texture-2d-array
         (assert (length>= 1 files))
         (let* ((binary-images
                 (mapcar (lambda (fname)
                           (cons fname (read-image-from-file fname)))
                         files))
                (width (sdl2:surface-width (cdr (first binary-images))))
                (height (sdl2:surface-height (cdr (first binary-images)))))
           (setf (image-width image) width
                 (image-height image) height)
           (gl:tex-image-3d :texture-2d-array 0 :srgb8 width height
                            (list-length files) 0 :bgr :unsigned-byte
                            (cffi:null-pointer))
           (dolist-enum (layer bimage binary-images)
             ;; TODO: Better error handling.
             (destructuring-bind (name . data) bimage
               (assert (= width (sdl2:surface-width data)))
               (assert (= height (sdl2:surface-height data)))
               (gl:tex-sub-image-3d :texture-2d-array 0 0 0 layer width height 1
                                    :bgr :unsigned-byte (sdl2:surface-pixels data))
               (setf (gethash name (image-layer-map image)) layer)
               (sdl2:free-surface data)))))
        (:texture-2d
         (assert (length= 1 files))
         (let* ((data (read-image-from-file (first files)))
                (width (sdl2:surface-width data))
                (height (sdl2:surface-height data)))
           (setf (image-width image) width
                 (image-height image) height)
           (gl:tex-image-2d :texture-2d 0 :srgb8 width height 0
                            :bgr :unsigned-byte (sdl2:surface-pixels data))
           (sdl2:free-surface data))))
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

(defstruct image-manager
  (images nil :type list)
  (image-map (make-hash-table :test #'equal) :type hash-table)
  missing-image)

(defun get-image (image-manager name)
  (with-struct (image-manager- image-map missing-image) image-manager
    (gethash name image-map missing-image)))

(defun add-image (image-manager image)
  (push image (image-manager-images image-manager))
  (setf (gethash (image-name image)
                 (image-manager-image-map image-manager)) image))

(defun load-image-from-file (image-manager fname &key (tex-type :texture-2d))
  (if-let ((found-image (get-image image-manager fname)))
    found-image
    (let ((image (make-image :name fname :tex-type tex-type
                             :files (list fname))))
      (add-image image-manager image)
      (load-image image))))

(defun load-map-images (image-manager image-names)
  (let ((image (make-image :name "map-textures" :tex-type :texture-2d-array
                           :files (remove-if (compose #'not #'image-path)
                                             image-names))))
    (add-image image-manager image)
    (load-image image)))

(defun init-image-manager ()
  (let ((im (make-image-manager)))
    (setf (image-manager-missing-image im)
          (load-image-from-file im "missing.bmp"))
    im))

(defun shutdown-image-manager (image-manager)
  (dolist (image (image-manager-images image-manager))
    (purge-image image))
  (setf (image-manager-images image-manager) nil)
  (clrhash (image-manager-image-map image-manager)))
