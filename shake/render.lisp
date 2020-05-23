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

(declaim (inline make-render-system))
(defstruct render-system
  "Rendering related global variables and constants."
  (win-width nil :type fixnum)
  (win-height nil :type fixnum)
  (rend-width nil :type fixnum)
  (rend-height nil :type fixnum)
  window
  (gl-config nil :type gl-config :read-only t)
  batches
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

(defun call-with-render-system (window render-width render-height fun)
  (check-type render-width fixnum)
  (check-type render-height fixnum)
  (check-type fun function)
  (sdl2:with-gl-context (context window)
    (handler-case
        ;; Turn off V-Sync
        (sdl2:gl-set-swap-interval 0)
      (error () ;; sdl2 doesn't export sdl-error
        (shake:print-error "setting swap interval not supported~%")))
    (bracket (render-system (init-render-system window render-width render-height)
                            shutdown-render-system)
      (shake:add-command
       'print-memory-usage (lambda () (print-memory-usage render-system)))
      (shake:add-command
       'print-gl-info
       (lambda () (print-gl-info (render-system-gl-config render-system))))
      (funcall fun render-system))))

(defmacro with-render-system ((render-system window render-width render-height)
                              &body body)
  `(call-with-render-system ,window ,render-width ,render-height
                            (lambda (,render-system) ,@body)))

;; Currently active RENDER-SYSTEM.
(defvar *rs*)
;; BATCHES of the currently active RENDER-SYSTEM (*RS*).
(defvar *batches*)

(defun print-memory-usage (render-system)
  "Print the estimate of used memory in GL."
  (with-struct (image-manager- images)
      (render-system-image-manager render-system)
    (let ((image-usage (reduce #'+ (mapcar #'image-storage-size images))))
      (shake:printf "Total image allocation: ~:D bytes~%" image-usage))))

(defconstant +max-batch-size+ 512
  "Maximum count of objects in a batch. This should be consistent across
  shaders.")

(defstruct batch
  (vertex-array nil :type (unsigned-byte 64) :read-only t)
  (buffer nil :type (unsigned-byte 64) :read-only t)
  (buffer-ptr (cffi:null-pointer) :read-only t)
  (id-buffer nil :type (unsigned-byte 64) :read-only t)
  (offset 0 :type fixnum)
  (texture nil :type (or null image))
  ;; MVP for each drawn object
  (mvp nil :type (vector (mat 4) #.+max-batch-size+))
  ;; Indices into texture array image layer for each object.  The 2nd value is
  ;; the draw-count for object vertices.
  (layers nil :type (vector (cons fixnum fixnum) #.+max-batch-size+))
  ;; TODO: objects should always be equal to (length layers), redundant?
  (objects 0 :type fixnum)
  (draw-count 0 :type fixnum)
  (max-bytes 0 :type fixnum)
  ;; T if this batch should be drawn in wireframe mode.
  (wireframe-p nil :type boolean)
  (free-p nil :type boolean)
  (ready-p nil :type boolean)
  (mapped-p nil :type boolean))

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
                    :layers (make-array +max-batch-size+
                                        :element-type '(cons fixnum fixnum)
                                        :fill-pointer 0)
                    :max-bytes byte-size
                    :wireframe-p wireframep)
      (gl:bind-buffer :array-buffer 0))))

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
              (id-count (length (batch-layers batch))))
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
  (let ((layers (batch-layers batch))
        (layer-count (length (batch-layers batch)))
        (mvps (batch-mvp batch))
        (draw-start 0))
    (assert (< 0 layer-count))
    (assert (= layer-count (batch-objects batch)))
    (assert (= layer-count (length mvps)))
    (sgl:with-uniform-locations shader-prog (tex-layer mvp)
      (cffi:with-foreign-object (layer-array :int (* #.+max-batch-size+ 4))
        (cffi:with-foreign-object (mvp-array :float (* #.+max-batch-size+ 4 4))
          (loop for layer-pair across layers and object-ix fixnum from 0
             and mvp across mvps and mvp-offset fixnum from 0 by (* 4 4) do
             ;; Set the layer
               (let ((layer (car layer-pair)))
                 (setf (cffi:mem-aref layer-array :int object-ix) layer))
             ;; Set the mvp
               (dotimes (ix (* 4 4))
                 (setf (cffi:mem-aref mvp-array :float (+ mvp-offset ix))
                       (coerce (row-major-aref mvp ix) 'single-float))))
          (%gl:uniform-1iv tex-layer-loc layer-count layer-array)
          (%gl:uniform-matrix-4fv mvp-loc layer-count t mvp-array))))
    (when (batch-wireframe-p batch)
      (gl:polygon-mode :front-and-back :line))
    (with-struct (gl-config- multi-draw-indirect-p base-instance-p) gl-config
      (cond
        ((and multi-draw-indirect-p base-instance-p)
         (cffi:with-foreign-object
             (cmds '(:struct sgl:draw-arrays-indirect-command) layer-count)
           (loop for layer-pair across layers and ix fixnum from 0 do
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
         (loop for layer-pair across layers and ix fixnum from 0 do
              (let ((draw-count (cdr layer-pair)))
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
          (vector-push (cons layer draw-count) (batch-layers batch))))
      (vector-push mvp (batch-mvp batch))
      (sgl:memcpy (cffi:inc-pointer (batch-buffer-ptr batch) offset)
                  gl-data byte-size)
      (incf (batch-offset batch) byte-size)
      (incf (batch-draw-count batch) draw-count)
      (incf (batch-objects batch)))))

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

(defun add-new-batch (byte-size &key wireframep)
  (declare (optimize (speed 3) (space 3)))
  (aif (get-current-batch)
       (finish-batch it (render-system-gl-config *rs*)))
  ;; TODO: Reuse already allocated batches, instead of allocating new ones.
  (let ((batch (init-batch byte-size :wireframep wireframep)))
    (vector-push-extend batch *batches*)
    batch))

(defun get-current-batch ()
  (declare (optimize (speed 3) (space 3)))
  (when (length>= 1 *batches*)
    (aref (the (vector batch) *batches*)
          (1- (length (the (vector batch) *batches*))))))

(defun render-surface (surface mvp &key wireframep)
  (declare (optimize (speed 3) (space 3)))
  (check-type surface smdl::surf-triangles)
  (check-type mvp (mat 4))
  (check-type wireframep boolean)
  (check-type *rs* render-system)
  (with-struct (render-system- image-manager) *rs*
    (let ((current-batch (get-current-batch))
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
                   ;; Each batch contains 100kB of vertex data.
                   (add-new-batch (max surface-space (* 100 1024))
                                  :wireframep wireframep))))
          (add-surface-vertex-data surface mvp batch))))))

(defun draw-text (text &key x y (scale 1.0s0))
  "Draw a single line of text on given window coordinates."
  (check-type *rs* render-system)
  (check-type text string)
  (check-type x fixnum)
  (check-type y fixnum)
  (check-type scale single-float)
  ;; TODO: Do we want window or render dimensions?
  (let ((pos-x (if (minusp x) (+ (render-system-win-width *rs*) x) x))
        (pos-y (if (minusp y) (+ (render-system-win-height *rs*) y) y)))
    (push (make-debug-text :char-string text :x pos-x :y pos-y :scale scale)
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
    (let ((char-code (- (char-code char) start-char-code)))
      (multiple-value-bind (y x) (floor char-code chars-per-line)
        (values (* x cell-size) (* y cell-size))))))

(defun renderer-draw (renderer) (funcall renderer :draw))

(defun show-debug-text (render-system)
  ;; Try to keep the overhead of drawing debug-text to a minimum.
  (declare (optimize (speed 3) (space 3)))
  ;; TODO: Do we want window or render dimensions?
  (with-struct (render-system- debug-text-list win-width win-height) render-system
    (let ((progs (render-system-prog-manager render-system))
          (ortho (ortho 0.0 win-width 0.0 win-height -1.0 1.0)))
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
                     and offset of-type single-float from half-cell by half-cell do
                       (multiple-value-bind (x y) (char->font-cell-pos char font)
                         (gl:uniformi char-pos-loc x y)
                         (let ((translation
                                (translation :x (+ offset pos-x)
                                             :y (+ pos-y half-cell))))
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
  (let ((*batches* (init-draw-frame render-system))
        (*rs* render-system)
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
