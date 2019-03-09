;;;; Defines `image-manager' which is responsible for taking care of images
;;;; used for rendering.

(in-package #:shake.render)

(defstruct image-opts
  (width 0 :type fixnum :read-only t)
  (height 0 :type fixnum :read-only t)
  (depth 1 :type fixnum :read-only t))

(defstruct image
  (name nil :type string :read-only t)
  (tex-type :texture-2d :read-only t)
  (opts nil :type (or null image-opts))
  (files nil :type list)
  (gl-tex -1 :type integer)
  ;; In case of array texture, maps file name to layer index.
  (layer-map (make-hash-table :test #'equal)))

(defun image-path (name)
  (sdata:data-path (concatenate 'string "share/textures/" name)
                   :if-does-not-exist :error))

(defun get-image-layer (image name)
  (with-struct (image- tex-type layer-map) image
    (assert (eq tex-type :texture-2d-array))
    (assert (image-loaded-p image))
    (let ((layer (gethash name layer-map)))
      (if layer
          layer
          (progn
            ;; We need to have a valid LAYER-MAP, otherwise we are using a
            ;; defaulted image.
            (assert (string= "missing.bmp" (image-name image)))
            0)))))

(defun read-image-from-file (fname)
  (sdl2:load-bmp (image-path fname)))

(defun load-image-to-array (name binary-image image-array)
  (bind-image image-array)
  (with-struct (image-opts- width height) (image-opts image-array)
    (assert (= width (sdl2:surface-width binary-image)))
    (assert (= height (sdl2:surface-height binary-image)))
    (let ((layer (hash-table-count (image-layer-map image-array))))
      (gl:tex-sub-image-3d :texture-2d-array 0 0 0 layer width height 1 :bgr
                           :unsigned-byte (sdl2:surface-pixels binary-image))
      (gl:generate-mipmap :texture-2d-array)
      (setf (gethash name (image-layer-map image-array)) layer))))

(defun load-image (image)
  (assert (not (image-loaded-p image)))
  (with-struct (image- tex-type files) image
    (setf (image-gl-tex image) (first (gl:gen-textures 1)))
    (bind-image image)
    (ecase tex-type
      (:texture-2d-array
       (assert (length>= 1 files))
       (let* ((depth (list-length files))
              (binary-images
               (mapcar (lambda (fname)
                         (cons fname (read-image-from-file fname)))
                       files))
              (width (sdl2:surface-width (cdr (first binary-images))))
              (height (sdl2:surface-height (cdr (first binary-images)))))
         (setf (image-opts image) (make-image-opts :width width
                                                   :height height
                                                   :depth depth))
         (gl:tex-image-3d :texture-2d-array 0 :srgb8 width height depth 0
                          :bgr :unsigned-byte (cffi:null-pointer))
         (dolist (bimage binary-images)
           ;; TODO: Better error handling.
           (destructuring-bind (name . data) bimage
             (load-image-to-array name data image)
             (sdl2:free-surface data)))))
      (:texture-2d
       (assert (length= 1 files))
       (let* ((data (read-image-from-file (first files)))
              (width (sdl2:surface-width data))
              (height (sdl2:surface-height data)))
         (setf (image-opts image) (make-image-opts :width width
                                                   :height height))
         (gl:tex-image-2d :texture-2d 0 :srgb8 width height 0
                          :bgr :unsigned-byte (sdl2:surface-pixels data))
         (sdl2:free-surface data))))
    (gl:generate-mipmap tex-type)
    (gl:tex-parameter tex-type :texture-min-filter :linear-mipmap-nearest)
    (gl:tex-parameter tex-type :texture-mag-filter :linear))
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
      0
      (with-struct (image- opts) image
        (with-struct (image-opts- width height depth) opts
          (* width height depth
             3 ;; 3 bytes per pixel; TODO: Calc for different formats.
             (/ 4 3)))))) ;; Account for mipmaps; TODO: Calc without mipmaps.

(defstruct image-manager
  (images nil :type list)
  ;; Maps image name to image structure.
  (image-map (make-hash-table :test #'equal) :type hash-table)
  ;; Maps image-opts to a list of images.
  (image-opts-map (make-hash-table :test #'equalp) :type hash-table)
  (missing-image nil :type (or null image)))

(defun get-image (image-manager name &key (default (image-manager-missing-image image-manager)))
  "Return a loaded image called NAME.  If the image is not loaded (either it
failed to load or no load was requested), returns DEFAULT."
  (with-struct (image-manager- image-map) image-manager
    (gethash name image-map default)))

(defun get-images-with-opts (image-manager opts)
  (gethash opts (image-manager-image-opts-map image-manager)))

(defun add-image (image-manager image)
  (push image (image-manager-images image-manager))
  (setf (gethash (image-name image) (image-manager-image-map image-manager))
        image))

(defun find-image-array (image-manager opts)
  (with-struct (image-opts- depth) opts
    (dolist (image (get-images-with-opts image-manager opts))
      (when (> depth (hash-table-count (image-layer-map image)))
        (return image)))))

(defun get-or-create-image-array (image-manager name opts)
  (or (find-image-array image-manager opts)
      (let ((image-array (make-image :name name :tex-type :texture-2d-array
                                     :opts opts)))
        (setf (image-gl-tex image-array) (first (gl:gen-textures 1)))
        (bind-image image-array)
        (with-struct (image-opts- width height depth) opts
          (gl:tex-image-3d :texture-2d-array 0 :srgb8 width height depth
                           0 :bgr :unsigned-byte (cffi:null-pointer)))
        (gl:tex-parameter :texture-2d-array :texture-min-filter
                          :linear-mipmap-nearest)
        (gl:tex-parameter :texture-2d-array :texture-mag-filter :linear)
        (setf (gethash opts (image-manager-image-opts-map image-manager))
              (list image-array))
        (add-image image-manager image-array)
        image-array)))

(defun load-image-from-file (image-manager fname)
  "Load image from file FNAME.  If the image was already loaded, return the
existing one.  In case of any loading errors, return default image for missing
textures."
  (or (get-image image-manager fname :default nil)
      (handler-case
          (let ((binary-image (read-image-from-file fname)))
            (unwind-protect
                 (let* ((opts (make-image-opts
                               :width (sdl2:surface-width binary-image)
                               :height (sdl2:surface-height binary-image)
                               :depth 10))
                        (image-array (get-or-create-image-array image-manager fname
                                                                opts)))
                   (push fname (image-files image-array))
                   (setf (gethash fname (image-manager-image-map image-manager))
                         image-array)
                   (load-image-to-array fname binary-image image-array)
                   image-array)
              (sdl2:free-surface binary-image)))
        ;; TODO: Other errors?
        (sdata:data-file-error (c)
          (shake:printf "~A~%" c)
          (image-manager-missing-image image-manager)))))

(defun load-map-images (image-manager image-names)
  (dolist (name image-names)
    (load-image-from-file image-manager name)))

(defun init-image-manager ()
  (let ((im (make-image-manager)))
    ;; TODO: Replace missing image with a programmatic image, so we don't
    ;; depend on potentially missing file.
    (setf (image-manager-missing-image im)
          (load-image-from-file im "missing.bmp"))
    im))

(defun shutdown-image-manager (image-manager)
  (dolist (image (image-manager-images image-manager))
    (purge-image image))
  (setf (image-manager-images image-manager) nil)
  (clrhash (image-manager-image-map image-manager))
  (clrhash (image-manager-image-opts-map image-manager)))
