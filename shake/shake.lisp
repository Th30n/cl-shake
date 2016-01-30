(in-package #:shake)

(defun main ()
  (sdl2:with-init (:video)
    (set-gl-attrs)
    (sdl2:with-window (win :title "shake" :flags '(:shown :opengl))
      (sdl2:with-gl-context (context win)
        (print-gl-info)
        (let ((vertex-array (gl:gen-vertex-array)))
          (sdl2:with-event-loop (:method :poll)
            (:quit () t)
            (:idle ()
                   (render win vertex-array))))))))

(defun set-gl-attrs ()
  (sdl2:gl-set-attr :context-major-version 3)
  (sdl2:gl-set-attr :context-minor-version 3)
  ;; set CONTEXT_FORWARD_COMPATIBLE
  (sdl2:gl-set-attr :context-flags #x2)
  ;; set CONTEXT_PROFILE_CORE
  (sdl2:gl-set-attr :context-profile-mask #x1))

(defun print-gl-info ()
  (let ((vendor (gl:get-string :vendor))
        (renderer (gl:get-string :renderer))
        (gl-version (gl:get-string :version))
        (glsl-version (gl:get-string :shading-language-version)))
    (format t "GL Vendor: ~S~%" vendor)
    (format t "GL Renderer: ~S~%" renderer)
    (format t "GL Version: ~S~%" gl-version)
    (format t "GLSL Version: ~S~%" glsl-version)))

(defun render (win vertex-array)
  (gl:bind-vertex-array vertex-array)
  (clear-buffer-fv :color 0 0 0 0)
  (sdl2:gl-swap-window win)
  (gl:bind-vertex-array 0))

(defun clear-buffer-fv (buffer drawbuffer &rest values)
  (let ((len (list-length values)))
    (cffi:with-foreign-object (value-ptr :float len)
      (loop for i below len and val in values
         do (setf (cffi:mem-aref value-ptr :float i) (coerce val 'single-float)))
      (%gl:clear-buffer-fv buffer drawbuffer value-ptr))))
