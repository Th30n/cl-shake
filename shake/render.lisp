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

(defstruct gl-config
  "Stores constants of various capabilities for the initialized GL context."
  (vendor nil :type string :read-only t)
  (renderer nil :type string :read-only t)
  (version-string nil :type string :read-only t)
  (glsl-version-string nil :type string :read-only t)
  (max-texture-size nil :type integer :read-only t)
  (max-3d-texture-size nil :type integer :read-only t)
  (max-array-texture-layers nil :type integer :read-only t))

(defun init-gl-config ()
  "Create GL-CONFIG and fill with information from GL context."
  (make-gl-config
   :vendor (gl:get-string :vendor)
   :renderer (gl:get-string :renderer)
   :version-string (gl:get-string :version)
   :glsl-version-string (gl:get-string :shading-language-version)
   :max-texture-size (gl:get-integer :max-texture-size)
   :max-3d-texture-size (gl:get-integer :max-3d-texture-size)
   :max-array-texture-layers (gl:get-integer :max-array-texture-layers)))

(defun print-gl-info (gl-config)
  "Print basic OpenGL information."
  (with-struct (gl-config- vendor renderer version-string glsl-version-string
                           max-texture-size max-3d-texture-size
                           max-array-texture-layers)
      gl-config
    (format t "GL Vendor: ~S~%" vendor)
    (format t "GL Renderer: ~S~%" renderer)
    (format t "GL Version: ~S~%" version-string)
    (format t "GLSL Version: ~S~%" glsl-version-string)
    (format t "Max texture size: ~S~%" max-texture-size)
    (format t "Max 3D texture size: ~S~%" max-3d-texture-size)
    (format t "Max array texture layers: ~S~%" max-array-texture-layers)
    (finish-output)))

(defstruct render-system
  "Rendering related global variables and constants."
  (gl-config nil :type gl-config :read-only t)
  batches
  (images nil :type list)
  (image-map (make-hash-table :test #'equal)))

(defun init-render-system ()
  (make-render-system :gl-config (init-gl-config)))

(defun shutdown-render-system (render-system)
  (dolist (image (render-system-images render-system))
    (purge-image image))
  (setf (render-system-images render-system) nil)
  (clrhash (render-system-image-map render-system)))

(defmacro with-render-system ((render-system window) &body body)
  (with-gensyms (context)
    `(sdl2:with-gl-context (,context ,window)
       (handler-case
           (sdl2:gl-set-swap-interval 0)
         (error () ;; sdl2 doesn't export sdl-error
           (format t "Setting swap interval not supported~%")))
       (let ((,render-system (init-render-system)))
         (unwind-protect
              (progn ,@body)
           (shutdown-render-system ,render-system))))))

(defun load-map-images (render-system image-names)
  (let ((image (make-image :name "map-textures"
                           :tex-type :texture-2d-array
                           :width 1024 :height 1024 :files image-names)))
    (load-image image)
    (push image (render-system-images render-system))
    (with-struct (render-system- image-map) render-system
      (setf (gethash "map-textures" image-map) image)
      (dolist-enum (layer image-name image-names)
        (setf (gethash image-name image-map) (cons image layer))))))

(defun print-memory-usage (render-system)
  "Print the estimate of used memory in GL."
  (with-struct (render-system- images) render-system
    (let ((image-usage (reduce #'+ (mapcar #'image-storage-size images))))
      (format t "Total image allocation: ~:D bytes~%" image-usage))))

(defstruct batch
  vertex-array
  buffer
  (offset 0 :type fixnum)
  texture
  (draw-count 0 :type fixnum)
  (max-bytes 0 :type fixnum)
  (free-p nil :type boolean)
  (ready-p nil :type boolean))

(defun init-batch (byte-size)
  (let ((buffer (car (gl:gen-buffers 1)))
        (vertex-array (gl:gen-vertex-array)))
    (gl:bind-buffer :array-buffer buffer)
    (%gl:buffer-data :array-buffer byte-size (cffi:null-pointer)
                     :static-draw)
    (make-batch :vertex-array vertex-array :buffer buffer
                :max-bytes byte-size)))

(defun free-batch (batch)
  (with-struct (batch- vertex-array buffer) batch
    (gl:delete-buffers (list buffer))
    (gl:delete-vertex-arrays (list vertex-array))
    (setf (batch-free-p batch) t
          (batch-ready-p batch) nil)))

(defun finish-batch (batch)
  (assert (not (batch-free-p batch)))
  (gl:bind-vertex-array (batch-vertex-array batch))
  (gl:bind-buffer :array-buffer (batch-buffer batch))
  (let ((stride (* 4 (+ 3 3 3 2)))
        (color-offset (* 4 3))
        (normal-offset (* 4 (+ 3 3)))
        (uv-offset (* 4 (+ 3 3 3))))
    ;; positions
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
  (gl:bind-vertex-array 0)
  (setf (batch-ready-p batch) t))

(defun draw-batch (batch)
  (assert (not (batch-free-p batch)))
  (unless (batch-ready-p batch)
    (finish-batch batch))
  (gl:bind-vertex-array (batch-vertex-array batch))
  (gl:draw-arrays :triangles 0 (batch-draw-count batch))
  (gl:check-error)
  (gl:bind-vertex-array 0))

(defun add-surface-vertex-data (surface batch)
  (flet ((fill-buffer (byte-offset data)
           (gl:bind-buffer :array-buffer (batch-buffer batch))
           (gl:buffer-sub-data :array-buffer (cdr data)
                               :buffer-offset byte-offset
                               :size (car data))
           (gl:bind-buffer :array-buffer 0)
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

(defun init-draw-frame (render-system)
  (if-let ((batches (render-system-batches render-system)))
    batches
    (setf (render-system-batches render-system)
          (make-array 10 :element-type 'batch
                      :fill-pointer 0 :adjustable t))))

(defun finish-draw-frame (render-system)
  (declare (optimize (speed 3) (space 3)))
  (with-struct (render-system- batches image-map) render-system
    (declare (type (vector batch) batches))
    (sgl:with-uniform-locations (sdata:res "shader-prog")
        (has-albedo tex-albedo)
      (gl:uniformi tex-albedo-loc 0)
      (gl:active-texture :texture0)
      (bind-image (gethash "map-textures" image-map))
    (dotimes (i (length batches))
      (let ((batch (aref batches i)))
        (if-let ((tex-name (batch-texture batch)))
          (let ((layer (cdr (gethash tex-name image-map))))
            (gl:uniformi has-albedo-loc layer))
          (gl:uniformi has-albedo-loc -1))
        (draw-batch batch)
        (free-batch batch)))
    (setf (fill-pointer batches) 0))))

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

(defmacro with-draw-frame ((render-system) &body body)
  "Establishes the environment where SHAKE.RENDER package functions can be
  used."
  (with-gensyms (body-result)
    `(let ((*batches* (init-draw-frame ,render-system)))
       (declare (special *batches*))
       (let ((,body-result (multiple-value-list (progn ,@body))))
         (finish-draw-frame ,render-system)
         (values-list ,body-result)))))
