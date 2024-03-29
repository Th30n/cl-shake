;;;; Defines `RENDER-SYSTEM' and functions for rendering to screen.

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

(defstruct vid-mode
  (format 0 :type (unsigned-byte 32) :read-only t)
  (width 0 :type fixnum :read-only t)
  (height 0 :type fixnum :read-only t)
  (refresh-rate 0 :type fixnum :read-only t))

(defun get-modes-for-display (display-index)
  (check-type display-index (integer 0))
  (let ((num-modes (sdl2:get-num-display-modes display-index)))
    (when (< num-modes 0) (error "SDL2:GET-NUM-DISPLAY-MODES failed"))
    (loop for mode-ix from 0 below num-modes
          collect (multiple-value-bind (format width height refresh-rate)
                      (sdl2:get-display-mode display-index mode-ix)
                    (make-vid-mode :format format :width width :height height :refresh-rate refresh-rate)))))

(defun list-modes ()
  "List available display modes when fullscreen."
  (let ((num-displays (sdl2:get-num-video-displays)))
    (when (< num-displays 0) (error "SDL2:GET-NUM-VIDEO-DISPLAYS failed"))
    (dotimes (disp-ix num-displays)
      (loop for mode in (get-modes-for-display disp-ix) and mode-ix from 0 do
        (shake:printf "Monitor ~A, mode ~A, ~A x ~A @ ~AHz~%" disp-ix mode-ix
                      (vid-mode-width mode) (vid-mode-height mode) (vid-mode-refresh-rate mode))))))

(defstruct gl-config
  "Stores constants of various capabilities for the initialized GL context."
  (vendor nil :type string :read-only t)
  (renderer nil :type string :read-only t)
  (version-string nil :type string :read-only t)
  (glsl-version-string nil :type string :read-only t)
  (max-texture-size nil :type integer :read-only t)
  (max-3d-texture-size nil :type integer :read-only t)
  (max-array-texture-layers nil :type integer :read-only t)
  (max-vertex-uniform-components nil :type integer :read-only t)
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
   :max-vertex-uniform-components (gl:get-integer :max-vertex-uniform-components)
   :multi-draw-indirect-p (gl:extension-present-p "GL_ARB_multi_draw_indirect")
   :base-instance-p (gl:extension-present-p "GL_ARB_base_instance")))

(defun print-gl-info (gl-config)
  "Print basic OpenGL information."
  (with-struct (gl-config- vendor renderer version-string glsl-version-string
                           max-texture-size max-3d-texture-size
                           max-array-texture-layers max-vertex-uniform-components
                           multi-draw-indirect-p base-instance-p)
      gl-config
    (shake:printf "GL Vendor: ~S~%" vendor)
    (shake:printf "GL Renderer: ~S~%" renderer)
    (shake:printf "GL Version: ~S~%" version-string)
    (shake:printf "GLSL Version: ~S~%" glsl-version-string)
    (shake:printf "Max texture size: ~S~%" max-texture-size)
    (shake:printf "Max 3D texture size: ~S~%" max-3d-texture-size)
    (shake:printf "Max array texture layers: ~S~%" max-array-texture-layers)
    (shake:printf "Max vertex uniform components: ~S~%" max-vertex-uniform-components)
    (shake:printf "GL_ARB_multi_draw_indirect: ~S~%" multi-draw-indirect-p)
    (shake:printf "GL_ARB_base_instance: ~S~%" base-instance-p)))

(defstruct debug-text
  (char-string nil :type string :read-only t)
  (x 0 :type fixnum :read-only t)
  (y 0 :type fixnum :read-only t)
  (scale 1.0s0 :type single-float :read-only t))

(defstruct gl-framebuffer
  (id nil :type integer :read-only t)
  (color-texture nil :type integer :read-only t)
  (depth-texture nil :type integer :read-only t))

(defun init-gl-framebuffer (width height)
  (let ((id (gl:gen-framebuffer))
        (color-texture (gl:gen-texture))
        (depth-texture (gl:gen-texture)))
    (gl:bind-framebuffer :framebuffer id)
    (gl:bind-texture :texture-2d color-texture)
    (gl:tex-image-2d :texture-2d 0 :rgba8 width height 0
                     :rgba :unsigned-byte (cffi:null-pointer))
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:bind-texture :texture-2d depth-texture)
    (gl:tex-image-2d :texture-2d 0 :depth-component width height 0
                     :depth-component :unsigned-byte (cffi:null-pointer))
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:bind-texture :texture-2d 0)
    (gl:framebuffer-texture-2d
     :framebuffer :color-attachment0 :texture-2d color-texture 0)
    (gl:framebuffer-texture-2d
     :framebuffer :depth-attachment :texture-2d depth-texture 0)
    (let ((status (gl:check-framebuffer-status :framebuffer)))
      (unless (in status :framebuffer-complete
                  :framebuffer-complete-ext :framebuffer-complete-oes)
        (error "Framebuffer status ~A" status)))
    (gl:draw-buffers '(:color-attachment0))
    (gl:bind-framebuffer :framebuffer 0)
    (make-gl-framebuffer :id id
                         :color-texture color-texture
                         :depth-texture depth-texture)))

(defun free-framebuffer (framebuffer)
  (gl:delete-textures (list (gl-framebuffer-color-texture framebuffer)
                            (gl-framebuffer-depth-texture framebuffer)))
  (gl:delete-framebuffers (list (gl-framebuffer-id framebuffer))))

(defun bind-framebuffer (target framebuffer)
  (gl:bind-framebuffer target (gl-framebuffer-id framebuffer)))

(defconstant +max-gui-verts+ 100)

(defstruct gui-model
  "Stores surfaces for rendering the 2D GUI.  Surfaces are built dynamically
  during a frame."
  (vertex-array nil :type (unsigned-byte 64) :read-only t)
  (vertex-buffer nil :type (unsigned-byte 64) :read-only t)
  (vertex-ptr (cffi:null-pointer))
  (vertex-count 0 :type fixnum))

(defun init-gui-model ()
  (let ((vertex-array (gl:gen-vertex-array))
        (vertex-buffer (gl:gen-buffer))
        (byte-size #.(* +max-gui-verts+
                        (cffi:foreign-type-size '(:struct smdl::vertex-data)))))
    (gl:bind-buffer :array-buffer vertex-buffer)
    (%gl:buffer-data :array-buffer byte-size (cffi:null-pointer) :static-draw)
    (gl:bind-buffer :array-buffer 0)
    (make-gui-model :vertex-array vertex-array
                    :vertex-buffer vertex-buffer)))

(defun free-gui-model (gui-model)
  (gl:delete-buffers (list (gui-model-vertex-buffer gui-model)))
  (gl:delete-vertex-arrays (list (gui-model-vertex-array gui-model))))

(defun gui-model-reset (gui-model)
  (setf (gui-model-vertex-count gui-model) 0)
  (gl:bind-buffer :array-buffer (gui-model-vertex-buffer gui-model))
  (setf (gui-model-vertex-ptr gui-model)
        (sgl:map-buffer :array-buffer +max-gui-verts+ :map-write-bit
                        :type (:struct smdl::vertex-data)))
  (gl:bind-buffer :array-buffer 0))

(defun gui-model-add-quad (gui-model x y w h)
  (declare (optimize (speed 3) (space 3)))
  ;; TODO: Investigate why is this function consing
  (check-type gui-model gui-model)
  (check-type x single-float)
  (check-type y single-float)
  (check-type w single-float)
  (check-type h single-float)
  (if (>= (+ 6 (gui-model-vertex-count gui-model)) +max-gui-verts+)
      (shake:print-warning "exceeded +MAX-GUI-VERTS+!~%")
      (let ((ptr (cffi:inc-pointer (gui-model-vertex-ptr gui-model)
                                   (* #.(cffi:foreign-type-size '(:struct smdl::vertex-data))
                                      (gui-model-vertex-count gui-model))))
            (top-left (smdl::make-l-vertex-data :position (v x y 0)))
            (bottom-left (smdl::make-l-vertex-data :position (v x (+ y h) 0)))
            (top-right (smdl::make-l-vertex-data :position (v (+ x w) y 0)))
            (bottom-right (smdl::make-l-vertex-data :position (v (+ x w) (+ y h) 0))))
        (declare (dynamic-extent ptr top-left bottom-left top-right bottom-right))
        (incf (gui-model-vertex-count gui-model) 6)
        (setf (cffi:mem-aref ptr '(:struct smdl::vertex-data) 0) top-left)
        (setf (cffi:mem-aref ptr '(:struct smdl::vertex-data) 1) bottom-left)
        (setf (cffi:mem-aref ptr '(:struct smdl::vertex-data) 2) bottom-right)
        (setf (cffi:mem-aref ptr '(:struct smdl::vertex-data) 3) top-left)
        (setf (cffi:mem-aref ptr '(:struct smdl::vertex-data) 4) bottom-right)
        (setf (cffi:mem-aref ptr '(:struct smdl::vertex-data) 5) top-right))))

(defun gui-model-draw (gui-model)
  (gl:bind-vertex-array (gui-model-vertex-array gui-model))
  (let ((stride #.(cffi:foreign-type-size '(:struct smdl::vertex-data)))
        (color-offset (* 4 3))
        (normal-offset (* 4 (+ 3 3)))
        (uv-offset (* 4 (+ 3 3 3))))
    (gl:bind-buffer :array-buffer (gui-model-vertex-buffer gui-model))
    (gl:unmap-buffer :array-buffer)
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
    (gl:draw-arrays :triangles 0 (gui-model-vertex-count gui-model))
    (gl:bind-buffer :array-buffer 0)))

(defstruct batches
  (array (make-array 320 :element-type 'batch :fill-pointer 0 :adjustable t) :type (vector batch))
  (length 0 :type fixnum))

(defun batches-current (batches)
  (check-type batches batches)
  (when (>= (batches-length batches) 1)
    (aref (batches-array batches) (1- (batches-length batches)))))

(defun batches-push (batches byte-size &key wireframep)
  (check-type batches batches)
  (check-type byte-size fixnum)
  (check-type wireframep boolean)
  (aif (batches-current batches)
       (finish-batch it (render-system-gl-config *rs*)))
  (if (> (fill-pointer (batches-array batches)) (batches-length batches))
      ;; Reuse already allocated batch, instead of allocating new one.
      (reinit-batch (aref (batches-array batches) (batches-length batches))
                    byte-size :wireframep wireframep)
      (vector-push-extend (init-batch byte-size :wireframep wireframep) (batches-array batches)))
  (incf (batches-length batches))
  (batches-current batches))

(declaim (inline make-render-system))
(defstruct render-system
  "Rendering related global variables and constants."
  (win-width nil :type fixnum)
  (win-height nil :type fixnum)
  (rend-width nil :type fixnum)
  (rend-height nil :type fixnum)
  window
  (gl-config nil :type gl-config :read-only t)
  (batches (make-batches) :type batches)
  ;; List of `debug-text' to render on the next frame.
  (debug-text-list nil)
  (image-manager nil :type image-manager)
  (prog-manager nil :type prog-manager)
  (framebuffer nil :type gl-framebuffer)
  (swap-timer (shake::make-timer :name "Draw & Swap") :type shake::timer)
  (gui-model nil :type gui-model))

(declaim (inline init-render-system))
(declaim (ftype (function (t fixnum fixnum) render-system) init-render-system))
(defun init-render-system (window render-width render-height)
  (multiple-value-bind (width height) (sdl2:get-window-size window)
    (make-render-system :gl-config (init-gl-config)
                        :image-manager (init-image-manager)
                        :prog-manager (init-prog-manager)
                        :framebuffer (init-gl-framebuffer render-width render-height)
                        :gui-model (init-gui-model)
                        :window window
                        :win-width width
                        :win-height height
                        :rend-width render-width
                        :rend-height render-height)))

(defun shutdown-render-system (render-system)
  (with-struct (render-system- image-manager prog-manager framebuffer gui-model)
      render-system
    (free-framebuffer framebuffer)
    (free-gui-model gui-model)
    (shutdown-image-manager image-manager)
    (shutdown-prog-manager prog-manager)))

;; cvars
(defvar *fullscreen* nil
  "Set window to real :FULLSCREEN or to :BORDERLESS to just take the desktop size (i.e. borderless fullscreen).
Use NIL for windowed mode.")
(defvar *vid-mode* 0)
(defvar *win-width* 1024)
(defvar *win-height* 768)

(defun render-system-init-commands (render-system)
  (setf *win-width* (render-system-win-width render-system))
  (setf *win-height* (render-system-win-height render-system))
  ;; TODO: FULLSCREEN should be initialized the same as RENDER-SYSTEM-WINDOW.
  (shake:add-variable '*fullscreen* :type '(member t :fullscreen :desktop :borderless nil))
  (shake:add-variable '*win-width* :type 'fixnum)
  (shake:add-variable '*win-height* :type 'fixnum)
  (shake:add-variable '*vid-mode*
                      :setter (lambda (mode-ix)
                                (check-type mode-ix fixnum)
                                (assert (>= mode-ix 0))
                                (setf *vid-mode* mode-ix)))
  (shake:add-command
   'vid-restart
   (lambda ()
     (let ((win (render-system-window render-system))
           (width *win-width*) (height *win-height*))
       (ecase *fullscreen*
         ((t :fullscreen)
          (sdl2:set-window-fullscreen win :fullscreen)
          (let ((mode (nth *vid-mode* (get-modes-for-display 0))))
            (unless mode
              (shake:printf "Resetting vid-mode from ~A to 0~%" *vid-mode*)
              (setf *vid-mode* 0)
              (setf mode (car (get-modes-for-display 0))))
            (autowrap:with-alloc (mode-ptr 'sdl2-ffi:sdl-display-mode)
              (sdl2-ffi.functions:sdl-get-display-mode 0 *vid-mode* mode-ptr)
              (sdl2-ffi.functions:sdl-set-window-display-mode
               win mode-ptr))
            (setf width (vid-mode-width mode))
            (setf height (vid-mode-height mode))
            (shake:printf "Setting mode to ~A x ~A @ ~AHz~%"
                          width height (vid-mode-refresh-rate mode))))
         ((:borderless :desktop)
          (sdl2:set-window-fullscreen win :desktop)
          (multiple-value-setq (width height)
            (sdl2:get-window-size win))
          (shake:printf "Setting borderless fullscreen ~A x ~A~%" width height))
         ((nil)
          (sdl2:set-window-fullscreen win nil)
          (sdl2:set-window-size win *win-width* *win-height*)
          (shake:printf "Setting window ~A x ~A~%" width height)))
       (free-framebuffer (render-system-framebuffer render-system))
       (setf (render-system-framebuffer render-system)
             (init-gl-framebuffer width height))
       (setf (render-system-rend-width render-system) width)
       (setf (render-system-win-width render-system) width)
       (setf (render-system-rend-height render-system) height)
       (setf (render-system-win-height render-system) height)
       (shake:printf "VID-RESTART done~%"))))
  (shake:add-variable
   '*v-sync*
   :documentation
   "0 for immediate updates;
1 for updates synchronized with the vertical retrace;
-1 for adaptive vsync (if available)"
   :type '(integer -1 1)
   :getter #'sdl2:gl-get-swap-interval
   :setter (lambda (val)
             (check-type val (integer -1 1) "-1, 0 or 1")
             (handler-case
                 (= 0 (sdl2:gl-set-swap-interval val))
               (error () ;; sdl2 doesn't export sdl-error
                 (shake:print-error "setting swap interval not supported~%")))))
  (shake:add-command
   'print-memory-usage (lambda () (print-memory-usage render-system)))
  (shake:add-command
   'print-gl-info
   (lambda () (print-gl-info (render-system-gl-config render-system))))
  (shake:add-command 'list-modes #'list-modes))

(defun set-gl-attrs ()
  "Set OpenGL context attributes. This needs to be called before window
  and context creation."
  (sdl2:gl-set-attrs
   :context-major-version 3
   :context-minor-version 3
   ;; set CONTEXT_FORWARD_COMPATIBLE (#x2)
   :context-flags sdl2-ffi:+sdl-gl-context-forward-compatible-flag+
   ;; set CONTEXT_PROFILE_CORE (#x1)
   :context-profile-mask sdl2-ffi:+sdl-gl-context-profile-core+))

(defun call-with-render-system (fun)
  (check-type fun function)
  (set-gl-attrs)
  (sdl2:with-window (window :title "shake" :w *win-width* :h *win-height* :flags '(:opengl))
    (sdl2:with-gl-context (context window)
      (bracket (render-system (init-render-system window *win-width* *win-height*)
                              shutdown-render-system)
        (sdl2:set-relative-mouse-mode 1)
        (render-system-init-commands render-system)
        (shake:command-progn ()
          ;; Turn off V-Sync
          (set *v-sync* 0)
          (print-gl-info))
        (funcall fun render-system)))))

(defmacro with-render-system ((render-system) &body body)
  `(call-with-render-system (lambda (,render-system) ,@body)))

;; Currently active RENDER-SYSTEM.
(defvar *rs*)

(defun print-memory-usage (render-system)
  "Print the estimate of used memory in GL."
  (with-struct (image-manager- images)
      (render-system-image-manager render-system)
    (let ((image-usage (reduce #'+ (mapcar #'image-storage-size images))))
      (shake:printf "Total image allocation: ~:D bytes~%" image-usage))))

(defconstant +max-batch-size+ 512
  "Maximum count of objects in a batch. This should be consistent across
  shaders.")

(declaim (inline make-layers %make-layers))
(defstruct (layers (:constructor %make-layers))
  ;; Every 2 elements are (layer-ix, draw-count) pairs.
  (draw-counts nil :type (vector fixnum #.(* 2 +max-batch-size+))))

(defun make-layers ()
  (%make-layers :draw-counts (make-array #.(* 2 +max-batch-size+)
                                         :element-type 'fixnum
                                         :fill-pointer 0)))

(declaim (ftype (function (layers) fixnum) layers-count))
(defun layers-count (layers)
  (coerce (floor (length (layers-draw-counts layers)) 2) 'fixnum))

(defun layers-push (layers layer-ix draw-count)
  (check-type layers layers)
  (vector-push layer-ix (layers-draw-counts layers))
  (vector-push draw-count (layers-draw-counts layers)))

(declaim (inline layers-nth))
(defun layers-nth (layers i)
  (check-type layers layers)
  (check-type i fixnum)
  (assert (< i (layers-count layers)))
  (let* ((draw-counts (layers-draw-counts layers))
         (layer-ix (aref draw-counts (* i 2)))
         (draw-count (aref draw-counts (1+ (* i 2)))))
    (values layer-ix draw-count)))

(defun layers-reset (layers)
  (check-type layers layers)
  (setf (fill-pointer (layers-draw-counts layers)) 0))

(defstruct batch
  (vertex-array nil :type (unsigned-byte 64))
  (buffer nil :type (unsigned-byte 64))
  (buffer-ptr (cffi:null-pointer))
  (offset 0 :type fixnum)
  (mapped-p nil :type boolean)
  (id-buffer nil :type (unsigned-byte 64))
  (texture nil :type (or null image))
  ;; MVP for each drawn object
  (mvp nil :type (vector (mat 4) #.+max-batch-size+))
  ;; Indices into texture array image layer for each object.  The 2nd value is
  ;; the draw-count for object vertices.
  (layers nil :type layers)
  ;; TODO: objects should always be equal to (length layers), redundant?
  (objects 0 :type fixnum)
  (draw-count 0 :type fixnum)
  (max-bytes 0 :type fixnum)
  ;; T if this batch should be drawn in wireframe mode.
  (wireframe-p nil :type boolean)
  (free-p nil :type boolean)
  (ready-p nil :type boolean))

(defun init-batch (byte-size &key wireframep)
  (let ((buffers (gl:gen-buffers 2))
        (vertex-array (gl:gen-vertex-array)))
    (gl:bind-buffer :array-buffer (first buffers))
    (%gl:buffer-data :array-buffer byte-size (cffi:null-pointer) :static-draw)
    (prog1
        (make-batch :vertex-array vertex-array
                    :buffer (first buffers)
                    :buffer-ptr (sgl:map-buffer :array-buffer (:bytes byte-size) :map-write-bit)
                    :mapped-p t
                    :id-buffer (second buffers)
                    :mvp (make-array +max-batch-size+
                                     :element-type '(mat 4)
                                     :fill-pointer 0)
                    :layers (make-layers)
                    :max-bytes byte-size
                    :wireframe-p wireframep)
      (gl:bind-buffer :array-buffer 0))))

;; TODO: Resolve this code duplication.
(defun reinit-batch (batch byte-size &key wireframep)
  (let ((buffers (gl:gen-buffers 2))
        (vertex-array (gl:gen-vertex-array)))
    (gl:bind-buffer :array-buffer (first buffers))
    (%gl:buffer-data :array-buffer byte-size (cffi:null-pointer) :static-draw)
    (setf (batch-vertex-array batch) vertex-array)
    (setf (batch-buffer batch) (first buffers))
    (setf (batch-buffer-ptr batch)
          (sgl:map-buffer :array-buffer (:bytes byte-size) :map-write-bit))
    (setf (batch-offset batch) 0)
    (setf (batch-mapped-p batch) t)
    (setf (batch-id-buffer batch) (second buffers))
    (setf (batch-texture batch) nil)
    (setf (fill-pointer (batch-mvp batch)) 0)
    (layers-reset (batch-layers batch))
    (setf (batch-objects batch) 0)
    (setf (batch-draw-count batch) 0)
    (setf (batch-max-bytes batch) byte-size)
    (setf (batch-wireframe-p batch) wireframep)
    (setf (batch-free-p batch) nil)
    (setf (batch-ready-p batch) nil)
    (gl:bind-buffer :array-buffer 0))
  batch)

(defun free-batch (batch)
  (declare (optimize (speed 3) (space 3)))
  (check-type batch batch)
  (assert (not (batch-mapped-p batch)))
  (with-struct (batch- vertex-array buffer id-buffer) batch
    (let ((buffers (list buffer id-buffer))
          (vertex-arrays (list vertex-array)))
      (declare (dynamic-extent buffers vertex-arrays))
      ;; NOTE: gl:delete-* functions don't perform stack allocation since they
      ;; cannot know the total size of arguments up front.  If this becomes an
      ;; issue, we should add our own wrappers which handle statically known
      ;; number of arguments.
      (gl:delete-buffers buffers)
      (gl:delete-vertex-arrays vertex-arrays))
    (setf (batch-free-p batch) t)
    (setf (batch-ready-p batch) nil)
    (setf (batch-mapped-p batch) nil)))

(defun finish-batch (batch gl-config)
  (declare (optimize (speed 3) (speed 3)))
  (assert (not (batch-free-p batch)))
  (gl:bind-vertex-array (batch-vertex-array batch))
  (let ((stride (* 4 (+ 3 3 3 2)))
        (color-offset (* 4 3))
        (normal-offset (* 4 (+ 3 3)))
        (uv-offset (* 4 (+ 3 3 3))))
    (gl:bind-buffer :array-buffer (batch-buffer batch))
    (gl:unmap-buffer :array-buffer)
    (setf (batch-mapped-p batch) nil)
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
              (id-count (layers-count (batch-layers batch))))
          (assert (<= id-count +max-batch-size+))
          (cffi:with-foreign-object (array-ptr :int +max-batch-size+)
            (dotimes (ix id-count)
              (setf (cffi:mem-aref array-ptr :int ix) ix))
            (gl:bind-buffer :array-buffer id-buffer)
            (%gl:buffer-data :array-buffer
                             ;; NOTE: (cffi:foreign-type-size :int) conses
                             (* 4 id-count)
                             array-ptr :static-draw)
            (gl:vertex-attrib-ipointer 4 1 :int 0 (cffi:null-pointer))
            (%gl:vertex-attrib-divisor 4 1)
            (gl:enable-vertex-attrib-array 4))))))
  (gl:bind-buffer :array-buffer 0)
  (gl:bind-vertex-array 0)
  (setf (batch-ready-p batch) t))

(defun draw-batch (batch shader-prog gl-config)
  (declare (optimize (speed 3) (space 3)))
  (check-type batch batch)
  (check-type shader-prog (unsigned-byte 32))
  (check-type gl-config gl-config)
  (assert (not (batch-free-p batch)))
  (unless (batch-ready-p batch)
    (finish-batch batch gl-config))
  (assert (not (batch-mapped-p batch)))
  (gl:bind-vertex-array (batch-vertex-array batch))
  (let* ((layers (batch-layers batch))
         (layer-count (layers-count layers))
         (mvps (batch-mvp batch))
         (draw-start 0))
    (declare (type fixnum layer-count draw-start))
    (assert (< 0 layer-count))
    (assert (= layer-count (batch-objects batch)))
    (assert (= layer-count (length mvps)))
    (sgl:with-uniform-locations shader-prog (tex-layer mvp)
      (cffi:with-foreign-object (layer-array :int (* +max-batch-size+ 4))
        (cffi:with-foreign-object (mvp-array :float (* +max-batch-size+ 4 4))
          (loop for object-ix fixnum from 0 below layer-count
                and mvp across mvps and mvp-offset fixnum from 0 by (* 4 4) do
                  ;; Set the layer
                  (let ((layer (layers-nth layers object-ix)))
                    (setf (cffi:mem-aref layer-array :int object-ix) layer))
                  ;; Set the mvp
                  (dotimes (ix (* 4 4))
                    (setf (cffi:mem-aref mvp-array :float (the fixnum (+ mvp-offset ix)))
                          (coerce (row-major-aref mvp ix) 'single-float))))
          (%gl:uniform-1iv tex-layer-loc layer-count layer-array)
          (%gl:uniform-matrix-4fv mvp-loc layer-count t mvp-array))))
    (when (batch-wireframe-p batch)
      (gl:polygon-mode :front-and-back :line))
    (with-struct (gl-config- multi-draw-indirect-p base-instance-p) gl-config
      (cond
        ((and multi-draw-indirect-p base-instance-p)
         (let ((sizeof-cmd #.(cffi:foreign-type-size
                              '(:struct sgl:draw-arrays-indirect-command))))
           (declare (type fixnum sizeof-cmd))
           (cffi:with-foreign-object
               (cmds '(:struct sgl:draw-arrays-indirect-command) +max-batch-size+)
             (loop for ix fixnum from 0 below layer-count do
               (let ((draw-count (the fixnum (nth-value 1 (layers-nth layers ix))))
                     (cmd (cffi:inc-pointer cmds (the fixnum (* sizeof-cmd ix)))))
                 (sgl:set-draw-arrays-command cmd draw-count :first draw-start
                                                             :base-instance ix)
                 (incf draw-start draw-count)))
             (let ((cmd-buffer (first (gl:gen-buffers 1)))
                   (size (* sizeof-cmd layer-count)))
               (gl:bind-buffer :draw-indirect-buffer cmd-buffer)
               (%gl:buffer-data :draw-indirect-buffer size cmds :static-draw)
               (%gl:multi-draw-arrays-indirect :triangles (cffi:null-pointer) layer-count 0)
               (gl:bind-buffer :draw-indirect-buffer 0)
               (gl:delete-buffers (list cmd-buffer))))))
        (t
         (loop for ix fixnum from 0 below layer-count do
           (let ((draw-count (the fixnum (nth-value 1 (layers-nth layers ix)))))
             (%gl:vertex-attrib-i1i 4 ix)
             (gl:draw-arrays :triangles draw-start draw-count)
             (incf draw-start draw-count)))))))
  (gl:polygon-mode :front-and-back :fill)
  (gl:bind-vertex-array 0))

(defun add-surface-vertex-data (surface mvp batch)
  (declare (optimize (speed 3) (space 3)))
  (declare (type smdl::surf-triangles surface))
  (declare (type (mat 4) mvp))
  (declare (type batch batch))
  (with-struct (batch- offset) batch
    (let ((gl-data (smdl::surf-triangles-verts surface))
          (byte-size (smdl::surf-triangles-verts-byte-size surface))
          (draw-count (smdl::surf-triangles-num-verts surface))
          (tex-name (smdl::surf-triangles-tex-name surface)))
      (with-struct (render-system- image-manager) *rs*
        (let ((layer (if-let ((image (and tex-name (get-image image-manager tex-name))))
                       (progn
                         (unless (batch-texture batch)
                           (setf (batch-texture batch) image))
                         (get-image-layer image tex-name))
                       -1)))
          (layers-push (batch-layers batch) layer draw-count)))
      (vector-push mvp (batch-mvp batch))
      (assert (>= (batch-max-bytes batch) (+ byte-size offset)))
      (sgl:memcpy (cffi:inc-pointer (batch-buffer-ptr batch) offset)
                  gl-data byte-size)
      (incf (batch-offset batch) byte-size)
      (incf (batch-draw-count batch) draw-count)
      (incf (batch-objects batch)))))

(defun finish-draw-frame (render-system)
  (declare (optimize (speed 3) (space 3)))
  (with-struct (render-system- batches prog-manager) render-system
    (let ((shader-prog (get-program prog-manager "pass" "color")))
      (bind-program prog-manager shader-prog)
      (sgl:with-uniform-locations shader-prog (tex-albedo)
        (gl:uniformi tex-albedo-loc 0)
        (gl:active-texture :texture0)
        (dotimes (i (batches-length batches))
          (let ((batch (aref (batches-array batches) i)))
            (when (batch-texture batch)
              (bind-image (batch-texture batch)))
            (draw-batch batch shader-prog (render-system-gl-config render-system))
            (free-batch batch))))
      (setf (batches-length batches) 0))))

(defun render-surface (surface mvp &key wireframep)
  (declare (optimize (speed 3) (space 3)))
  (check-type surface smdl::surf-triangles)
  (check-type mvp (mat 4))
  (check-type wireframep boolean)
  (check-type *rs* render-system)
  (with-struct (render-system- image-manager) *rs*
    (let ((current-batch (batches-current (render-system-batches *rs*)))
          (surface-space (smdl::surf-triangles-verts-byte-size surface))
          (surface-image
           (aif (smdl::surf-triangles-tex-name surface)
                (get-image image-manager it))))
      (declare (type fixnum surface-space))
      (labels ((tex-match-p (batch)
                 (declare (type batch batch))
                 (with-struct (batch- texture) batch
                   (or (not texture) (not surface-image)
                       (eq texture surface-image))))
               (can-add-p (batch)
                 (declare (type batch batch))
                 (with-struct (batch- offset max-bytes objects) batch
                   (let ((free-space (- max-bytes offset)))
                     (and (> free-space surface-space)
                          (< objects +max-batch-size+)
                          (tex-match-p batch)
                          (eq wireframep (batch-wireframe-p batch)))))))
        (let ((batch
               (if (and current-batch (can-add-p current-batch))
                   current-batch
                   (batches-push (render-system-batches *rs*)
                                 ;; Each batch contains 100kB of vertex data.
                                 (max surface-space (* 100 1024))
                                 :wireframep wireframep))))
          (add-surface-vertex-data surface mvp batch))))))

(defun draw-text (text &key x y (scale 1.0s0))
  "Draw a single line of text on given render buffer coordinates."
  (check-type *rs* render-system)
  (check-type text string)
  (check-type x fixnum)
  (check-type y fixnum)
  (check-type scale real)
  ;; TODO: Do we want window or render dimensions?
  (let ((pos-x (if (minusp x) (+ (render-system-rend-width *rs*) x) x))
        (pos-y (if (minusp y) (+ (render-system-rend-height *rs*) y) y)))
    (push (make-debug-text :char-string text :x pos-x :y pos-y :scale (coerce scale 'single-float))
          (render-system-debug-text-list *rs*))))

(defun draw-gui-quad (x y w h)
  (check-type *rs* render-system)
  (check-type x real)
  (check-type y real)
  (check-type w real)
  (check-type h real)
  (gui-model-add-quad (render-system-gui-model *rs*)
                      (coerce x 'single-float) (coerce y 'single-float)
                      (coerce w 'single-float) (coerce h 'single-float)))

(defun draw-gui-text (text &key x y (scale 1.0s0))
  ;; TODO: Don't use debug text drawing
  (draw-text text :x x :y y :scale scale))

(defun char->font-cell-pos (char font)
  "Returns the char position in pixels for the given font."
  (with-struct (font- cell-size chars-per-line start-char-code) font
    (assert (= start-char-code 32))
    (let ((char-code (if (<= 32 (char-code char) 126)
                         ;; printable characters
                         (- (char-code char) start-char-code)
                         ;; special symbol for non-printable
                         (- 127 start-char-code))))
      (multiple-value-bind (y x) (floor char-code chars-per-line)
        (values (* x cell-size) (* y cell-size))))))

(defun renderer-draw (renderer) (funcall renderer :draw))

(defun show-debug-text (render-system)
  ;; Try to keep the overhead of drawing debug-text to a minimum.
  (declare (optimize (speed 3) (space 3)))
  ;; TODO: Do we want window or render dimensions? We should probably use a
  ;; virtual screen resolution.
  (with-struct (render-system- debug-text-list rend-width rend-height) render-system
    (let ((progs (render-system-prog-manager render-system))
          (ortho (ortho 0.0 rend-width 0.0 rend-height -1.0 1.0)))
      (sdata:res-let (point-renderer font)
        (with-struct (font- texture cell-size) font
          (let ((half-cell (* 0.5 cell-size))
                (text-shader (get-program progs "billboard" "text" "billboard")))
            (gl:active-texture :texture0)
            (gl:bind-texture :texture-2d texture)
            (bind-program progs text-shader)
            (sgl:with-uniform-locations text-shader
                (tex-font proj size cell mv char-pos)
              (gl:uniformi tex-font-loc 0)
              (sgl:uniform-matrix-4f proj-loc ortho)
              (gl:uniformi cell-loc cell-size cell-size)
              (dolist (debug-text debug-text-list)
                (let ((text (debug-text-char-string debug-text))
                      (pos-x (debug-text-x debug-text))
                      (pos-y (debug-text-y debug-text))
                      (size (* half-cell (debug-text-scale debug-text))))
                  (gl:uniformf size-loc size)
                  (loop for char across text
                     and offset of-type single-float from size by size do
                       (multiple-value-bind (x y) (char->font-cell-pos char font)
                         (gl:uniformi char-pos-loc x y)
                         (let ((translation
                                (translation :x (+ offset pos-x)
                                             :y (+ pos-y size))))
                           (declare (dynamic-extent translation))
                           ;; NOTE: This is consing a lot!
                           (sgl:uniform-matrix-4f mv-loc translation))
                         (renderer-draw point-renderer))))))))))))

(defun call-with-draw-frame (render-system fun)
  (check-type render-system render-system)
  (check-type fun function)
  (bind-framebuffer :framebuffer (render-system-framebuffer render-system))
  (gl:viewport 0 0
               (render-system-rend-width render-system)
               (render-system-rend-height render-system))
  (sgl:clear-buffer-fv :color 0 0 0 0)
  (sgl:clear-buffer-fv :depth 0 1)
  (let ((*rs* render-system)
        (prog-manager (render-system-prog-manager render-system)))
    (gui-model-reset (render-system-gui-model render-system))
    (multiple-value-prog1 (funcall fun)
      (shake::with-timer ((render-system-swap-timer render-system))
        (finish-draw-frame render-system)
        (gl:disable :depth-test)
        ;; GUI
        (let ((shader-prog (get-program prog-manager "pass" "color"))
              (mvp (ortho 0.0 (render-system-rend-width render-system)
                            (render-system-rend-height render-system) 0.0
                            -1.0 1.0)))
          (bind-program prog-manager shader-prog)
          (sgl:with-uniform-locations shader-prog (mvp tex-layer)
            (cffi:with-foreign-object (layer-array :int)
              (cffi:with-foreign-object (mvp-array :float (* 4 4))
                (setf (cffi:mem-aref layer-array :int) -1)
                ;; Set the mvp
                (dotimes (ix (* 4 4))
                  (setf (cffi:mem-aref mvp-array :float ix)
                        (coerce (row-major-aref mvp ix) 'single-float)))
                (%gl:uniform-1iv tex-layer-loc 1 layer-array)
                (%gl:uniform-matrix-4fv mvp-loc 1 t mvp-array)))))
        (gui-model-draw (render-system-gui-model render-system))
        ;; Debug text on top everything
        (show-debug-text render-system)
        (setf (render-system-debug-text-list render-system) nil)
        ;; Draw our framebuffer to window
        (gl:bind-framebuffer :framebuffer 0)
        (gl:viewport 0 0
                     (render-system-win-width render-system)
                     (render-system-win-height render-system))
        (sgl:clear-buffer-fv :color 0 0 0 0)
        (sgl:clear-buffer-fv :depth 0 1)
        (let ((shader-prog
               (get-program prog-manager "billboard" "billboard" "billboard")))
          (bind-program prog-manager shader-prog)
          (sgl:with-uniform-locations shader-prog (tex-sprite)
            (gl:uniformi tex-sprite-loc 0)
            (gl:active-texture :texture0)
            (gl:bind-texture :texture-2d
                             (gl-framebuffer-color-texture
                              (render-system-framebuffer render-system)))
            (sdata:res-let (point-renderer)
              (renderer-draw point-renderer))))
        (gl:bind-texture :texture-2d 0)
        (sdl2:gl-swap-window (render-system-window render-system))))))

(defmacro with-draw-frame ((render-system) &body body)
  "Establishes the environment where SHAKE.RENDER package functions can be
  used."
  `(call-with-draw-frame ,render-system (lambda () ,@body)))
