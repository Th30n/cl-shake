;;;; Defines `render-system' and functions for rendering to screen.

(in-package #:shake.render)

(defun load-texture (texture-file)
  "Load a texture from given string file path. Returns the OpenGL texture
  object as the primary value. Second and third value are image width and
  height."
  (let* ((surface (sdl2:load-bmp texture-file))
         (pixels (sdl2:surface-pixels surface))
         (width (sdl2:surface-width surface))
         (height (sdl2:surface-height surface))
         (tex (car (gl:gen-textures 1))))
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d tex)
    (gl:tex-image-2d :texture-2d 0 :srgb8 width height 0
                     :bgr :unsigned-byte pixels)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (sdl2:free-surface surface)
    (values tex width height)))

(defstruct font
  (texture 0 :type fixnum)
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (chars-per-line 0 :type fixnum)
  (cell-size 0 :type fixnum)
  (start-char-code 0 :type fixnum))

(defun load-font (font-file cell-size start-char)
  (multiple-value-bind (tex width height) (load-texture font-file)
    (make-font :texture tex :width width :height height
               :start-char-code (char-code start-char)
               :chars-per-line (floor width cell-size) :cell-size cell-size)))

(defun delete-font (font)
  (gl:delete-textures (list (font-texture font))))

(defstruct gl-config
  "Stores constants of various capabilities for the initialized GL context."
  (vendor nil :type string :read-only t)
  (renderer nil :type string :read-only t)
  (version-string nil :type string :read-only t)
  (glsl-version-string nil :type string :read-only t)
  (max-texture-size nil :type integer :read-only t)
  (max-3d-texture-size nil :type integer :read-only t)
  (max-array-texture-layers nil :type integer :read-only t)
  (multi-draw-indirect-p nil :type boolean :read-only t)
  (base-instance-p nil :type boolean :read-only t))

(defun init-gl-config ()
  "Create GL-CONFIG and fill with information from GL context."
  (make-gl-config
   :vendor (gl:get-string :vendor)
   :renderer (gl:get-string :renderer)
   :version-string (gl:get-string :version)
   :glsl-version-string (gl:get-string :shading-language-version)
   :max-texture-size (gl:get-integer :max-texture-size)
   :max-3d-texture-size (gl:get-integer :max-3d-texture-size)
   :max-array-texture-layers (gl:get-integer :max-array-texture-layers)
   :multi-draw-indirect-p (gl:extension-present-p "GL_ARB_multi_draw_indirect")
   :base-instance-p (gl:extension-present-p "GL_ARB_base_instance")))

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

(defstruct debug-text
  (char-string nil :type string :read-only t)
  (x 0 :type fixnum :read-only t)
  (y 0 :type fixnum :read-only t))

(defstruct render-system
  "Rendering related global variables and constants."
  (width nil :type fixnum)
  (height nil :type fixnum)
  window
  (gl-config nil :type gl-config :read-only t)
  batches
  ;; List of `debug-text' to render on the next frame.
  (debug-text-list nil)
  (image-manager nil :type image-manager)
  (prog-manager nil :type prog-manager))

(defun init-render-system (window)
  (multiple-value-bind (width height) (sdl2:get-window-size window)
    (make-render-system :gl-config (init-gl-config)
                        :image-manager (init-image-manager)
                        :prog-manager (init-prog-manager)
                        :window window
                        :width width
                        :height height)))

(defun shutdown-render-system (render-system)
  (with-struct (render-system- image-manager prog-manager) render-system
    (shutdown-image-manager image-manager)
    (shutdown-prog-manager prog-manager)))

(defmacro with-render-system ((render-system window) &body body)
  (with-gensyms (context)
    `(sdl2:with-gl-context (,context ,window)
       (handler-case
           (sdl2:gl-set-swap-interval 0)
         (error () ;; sdl2 doesn't export sdl-error
           (format t "Setting swap interval not supported~%")))
       (bracket (,render-system (init-render-system ,window)
                                shutdown-render-system)
         ,@body))))

;; Currently active RENDER-SYSTEM.
(defvar *rs*)
;; BATCHES of the currently active RENDER-SYSTEM (*RS*).
(defvar *batches*)

(defun print-memory-usage (render-system)
  "Print the estimate of used memory in GL."
  (with-struct (image-manager- images)
      (render-system-image-manager render-system)
    (let ((image-usage (reduce #'+ (mapcar #'image-storage-size images))))
      (format t "Total image allocation: ~:D bytes~%" image-usage))))

(defconstant +max-batch-size+ 512
  "Maximum count of objects in a batch. This should be consistent across
  shaders.")

(defstruct batch
  vertex-array
  buffer
  id-buffer
  (offset 0 :type fixnum)
  texture
  mvp
  (layers nil :type list)
  (objects 0 :type fixnum)
  (draw-count 0 :type fixnum)
  (max-bytes 0 :type fixnum)
  (free-p nil :type boolean)
  (ready-p nil :type boolean))

(defun init-batch (byte-size)
  (let ((buffers (gl:gen-buffers 2))
        (vertex-array (gl:gen-vertex-array)))
    (gl:bind-buffer :array-buffer (first buffers))
    (%gl:buffer-data :array-buffer byte-size (cffi:null-pointer) :static-draw)
    (make-batch :vertex-array vertex-array
                :buffer (first buffers)
                :id-buffer (second buffers)
                :max-bytes byte-size)))

(defun free-batch (batch)
  (with-struct (batch- vertex-array buffer id-buffer) batch
    (gl:delete-buffers (list buffer id-buffer))
    (gl:delete-vertex-arrays (list vertex-array))
    (setf (batch-free-p batch) t
          (batch-ready-p batch) nil)))

(defun finish-batch (batch gl-config)
  (assert (not (batch-free-p batch)))
  (gl:bind-vertex-array (batch-vertex-array batch))
  (let ((stride (* 4 (+ 3 3 3 2)))
        (color-offset (* 4 3))
        (normal-offset (* 4 (+ 3 3)))
        (uv-offset (* 4 (+ 3 3 3))))
    (gl:bind-buffer :array-buffer (batch-buffer batch))
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
    (gl:vertex-attrib-pointer 2 2 :float nil stride uv-offset)
    ;; draw-ids
    (with-struct (gl-config- base-instance-p multi-draw-indirect-p) gl-config
      (when (and multi-draw-indirect-p base-instance-p)
        (let ((id-buffer (batch-id-buffer batch))
              (id-count (list-length (batch-layers batch))))
          (gl:with-gl-array (id-array :int :count id-count)
            (dotimes (ix id-count)
              (setf (gl:glaref id-array ix) ix))
            (gl:bind-buffer :array-buffer id-buffer)
            (gl:buffer-data :array-buffer :static-draw id-array)
            (gl:vertex-attrib-ipointer 4 1 :int 0 (cffi:null-pointer))
            (%gl:vertex-attrib-divisor 4 1)
            (gl:enable-vertex-attrib-array 4))))))
  (gl:bind-buffer :array-buffer 0)
  (gl:bind-vertex-array 0)
  (setf (batch-ready-p batch) t))

(defun draw-batch (batch shader-prog gl-config)
  (assert (not (batch-free-p batch)))
  (unless (batch-ready-p batch)
    (finish-batch batch gl-config))
  (gl:bind-vertex-array (batch-vertex-array batch))
  (let ((layers (reverse (batch-layers batch)))
        (layer-count (list-length (batch-layers batch)))
        (draw-start 0))
    (sgl:with-uniform-locations shader-prog (tex-layer mvp)
      (cffi:with-foreign-object (layer-array :int layer-count)
        (dolist-enum (ix layer-pair layers)
          (let ((layer (car layer-pair)))
            (setf (cffi:mem-aref layer-array :int ix) layer)))
        (%gl:uniform-1iv tex-layer-loc layer-count layer-array))
      (sgl:with-gl-array (gl-array :float (list (batch-mvp batch)))
        (%gl:uniform-matrix-4fv mvp-loc 1 t
                                (gl::gl-array-pointer gl-array))))
    (with-struct (gl-config- multi-draw-indirect-p base-instance-p) gl-config
      (cond
        ((and multi-draw-indirect-p base-instance-p)
         (cffi:with-foreign-object
             (cmds '(:struct sgl:draw-arrays-indirect-command) layer-count)
           (dolist-enum (ix layer-pair layers)
             (let ((draw-count (cdr layer-pair))
                   (cmd (cffi:mem-aptr
                         cmds '(:struct sgl:draw-arrays-indirect-command) ix)))
               (sgl:set-draw-arrays-command cmd draw-count :first draw-start
                                            :base-instance ix)
               (incf draw-start draw-count)))
           (let ((cmd-buffer (first (gl:gen-buffers 1)))
                 (size (* (cffi:foreign-type-size
                           '(:struct sgl:draw-arrays-indirect-command))
                          layer-count)))
             (gl:bind-buffer :draw-indirect-buffer cmd-buffer)
             (%gl:buffer-data :draw-indirect-buffer size cmds :static-draw)
             (%gl:multi-draw-arrays-indirect :triangles (cffi:null-pointer) layer-count 0)
             (gl:bind-buffer :draw-indirect-buffer 0)
             (gl:delete-buffers (list cmd-buffer)))))
        (t
         (dolist-enum (ix layer-pair layers)
           (let ((draw-count (cdr layer-pair)))
             (%gl:vertex-attrib-i1i 4 ix)
             (gl:draw-arrays :triangles draw-start draw-count)
             (incf draw-start draw-count)))))))
  (gl:bind-vertex-array 0))

(defun add-surface-vertex-data (surface mvp batch)
  (flet ((fill-buffer (byte-offset data size)
           (gl:bind-buffer :array-buffer (batch-buffer batch))
           (%gl:buffer-sub-data :array-buffer byte-offset size data)
           (gl:bind-buffer :array-buffer 0)
           size))
    (with-struct (batch- offset) batch
      (let ((gl-data (smdl::surf-triangles-verts surface))
            (byte-size (smdl::surf-triangles-verts-byte-size surface))
            (draw-count (smdl::surf-triangles-num-verts surface))
            (tex-name (smdl::surf-triangles-tex-name surface)))
        (with-struct (render-system- image-manager) *rs*
          (let ((layer (if-let ((image (get-image image-manager tex-name)))
                         (progn
                           (unless (batch-texture batch)
                             (setf (batch-texture batch) image))
                           (get-image-layer image tex-name))
                         -1)))
            (push (cons layer draw-count) (batch-layers batch))))
        (setf (batch-mvp batch) mvp)
        (incf (batch-offset batch)
              (the fixnum (fill-buffer offset gl-data byte-size)))
        (incf (batch-draw-count batch) draw-count)
        (incf (batch-objects batch))))))

(defun init-draw-frame (render-system)
  (or (render-system-batches render-system)
      (setf (render-system-batches render-system)
            (make-array 10 :element-type 'batch
                        :fill-pointer 0 :adjustable t))))

(defun finish-draw-frame (render-system)
  (declare (optimize (speed 3) (space 3)))
  (with-struct (render-system- batches prog-manager) render-system
    (declare (type (vector batch) batches))
    (let ((shader-prog (get-program prog-manager "pass" "color")))
      (bind-program prog-manager shader-prog)
      (sgl:with-uniform-locations shader-prog (tex-albedo)
        (gl:uniformi tex-albedo-loc 0)
        (gl:active-texture :texture0)
        (dovector (batch batches)
          (when (batch-texture batch)
            (bind-image (batch-texture batch)))
          (draw-batch batch shader-prog (render-system-gl-config render-system))
          (free-batch batch))))
    (setf (fill-pointer batches) 0)))

(defun add-new-batch (byte-size)
  (declare (optimize (speed 3) (space 3)))
  (aif (get-current-batch)
       (finish-batch it (render-system-gl-config *rs*)))
  (let ((batch (init-batch byte-size)))
    (vector-push-extend batch *batches*)
    batch))

(defun get-current-batch ()
  (declare (optimize (speed 3) (space 3)))
  (when (length>= 1 *batches*)
    (aref (the (vector batch) *batches*)
          (1- (length (the (vector batch) *batches*))))))

(defun render-surface (surface mvp)
  (declare (type smdl::surf-triangles surface)
           (type (mat 4) mvp))
  (with-struct (render-system- image-manager) *rs*
    (let ((current-batch (get-current-batch))
          (surface-space (smdl::surf-triangles-verts-byte-size surface))
          (surface-image
           (aif (smdl::surf-triangles-tex-name surface)
                (get-image image-manager it))))
      (declare (type fixnum surface-space))
      (labels ((tex-match-p (batch)
                 (with-struct (batch- texture) batch
                   (or (not texture) (not surface-image)
                       (eq texture surface-image))))
               (can-add-p (batch)
                 (with-struct (batch- offset max-bytes objects) batch
                   (let ((free-space (- max-bytes offset)))
                     (and (> free-space surface-space)
                          (< objects +max-batch-size+)
                          (tex-match-p batch)
                          ;; TODO: Rethink this design
                          (equalp mvp (batch-mvp batch)))))))
        (let ((batch
               (if (and current-batch (can-add-p current-batch))
                   current-batch
                   ;; Each batch contains 100kB of vertex data.
                   (add-new-batch (max surface-space (* 100 1024))))))
          (add-surface-vertex-data surface mvp batch))))))

(defun draw-text (text &key x y)
  "Draw a single line of text on given window coordinates."
  (let ((pos-x (if (minusp x) (+ (render-system-width *rs*) x) x))
        (pos-y (if (minusp y) (+ (render-system-height *rs*) y) y)))
    (push (make-debug-text :char-string text :x pos-x :y pos-y)
          (render-system-debug-text-list *rs*))))

(defun char->font-cell-pos (char font)
  "Returns the char position in pixels for the given font."
  (with-struct (font- cell-size chars-per-line start-char-code) font
    (let ((char-code (- (char-code char) start-char-code)))
      (multiple-value-bind (y x) (floor char-code chars-per-line)
        (cons (* x cell-size) (* y cell-size))))))

(defun renderer-draw (renderer) (funcall renderer :draw))

(defun show-debug-text (render-system)
  (with-struct (render-system- debug-text-list width height) render-system
    (dolist (debug-text debug-text-list)
      (sdata:res-let (point-renderer font)
        (let ((progs (render-system-prog-manager render-system)))
          (with-struct (font- texture cell-size) font
            (let ((ortho (ortho 0d0 width 0d0 height -1d0 1d0))
                  (half-cell (* 0.5 cell-size))
                  (text-shader (get-program progs "billboard" "text" "billboard")))
              (gl:active-texture :texture0)
              (gl:bind-texture :texture-2d texture)
              (bind-program progs text-shader)
              (sgl:with-uniform-locations text-shader (tex-font proj size cell mv char-pos)
                (gl:uniformi tex-font-loc 0)
                (sgl:uniform-matrix-4f proj-loc (list ortho))
                (gl:uniformf size-loc half-cell)
                (gl:uniformi cell-loc cell-size cell-size)
                (let ((text (debug-text-char-string debug-text))
                      (pos-x (debug-text-x debug-text))
                      (pos-y (debug-text-y debug-text)))
                  (loop for char across text and offset from half-cell by half-cell do
                       (destructuring-bind (x . y) (char->font-cell-pos char font)
                         (gl:uniformi char-pos-loc x y)
                         (sgl:uniform-matrix-4f mv-loc
                                                (list (translation :x (+ offset pos-x)
                                                                   :y (+ pos-y half-cell))))
                         (renderer-draw point-renderer))))))))))))

(defun call-with-draw-frame (render-system fun)
  (let ((*batches* (init-draw-frame render-system))
        (*rs* render-system))
    (multiple-value-prog1 (funcall fun)
      (finish-draw-frame render-system)
      (gl:disable :depth-test)
      (show-debug-text render-system)
      (setf (render-system-debug-text-list render-system) nil)
      (sdl2:gl-swap-window (render-system-window render-system)))))

(defmacro with-draw-frame ((render-system) &body body)
  "Establishes the environment where SHAKE.RENDER package functions can be
  used."
  `(call-with-draw-frame ,render-system (lambda () ,@body)))
