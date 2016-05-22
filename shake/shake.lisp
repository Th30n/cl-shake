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

(in-package #:shake)

(declaim (optimize (debug 3)))

(defmacro with-struct ((name . fields) struct &body body)
  "Bind variables to FIELDS from STRUCT. NAME is a prefix associated with
the structure."
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
       (let ,(mapcar (lambda (f)
                       `(,f (,(symbolicate name f) ,gs)))
                     fields)
         ,@body))))

(defstruct camera
  "A camera structure, storing the projection matrix, position in world space
and rotation as a quaternion."
  projection
  (position (v 0 0 0) :type (simple-array double-float (3)))
  (rotation (q 0 0 0 1) :type (cons (simple-array double-float (3)) double-float)))

(defun camera-view-transform (camera)
  (declare (type camera camera))
  (let* ((pos (camera-position camera))
         (translation (translation
                       :x (- (vx pos)) :y (- (vy pos)) :z (- (vz pos))))
         (q (qconj (camera-rotation camera))))
    (m* (q->mat q) translation)))

(defparameter *bsp*
  (with-open-file (file "test.bsp") (sbsp:read-bsp file)))

(defun get-endpoints (lineseg)
  (let* ((linedef (sbsp::lineseg-orig-line lineseg))
         (line-vec (v- (sbsp::linedef-end linedef)
                       (sbsp::linedef-start linedef)))
         (start (v+ (sbsp::linedef-start linedef)
                    (vscale (sbsp::lineseg-t-start lineseg) line-vec)))
         (end (v+ (sbsp::linedef-start linedef)
                  (vscale (sbsp::lineseg-t-end lineseg) line-vec))))
    (list start end)))

(defun get-triangles (lineseg)
  (let* ((endpoints (get-endpoints lineseg))
         (start-2d (car endpoints))
         (start-3d (v (aref start-2d 0) 1 (aref start-2d 1)))
         (end-2d (cadr endpoints))
         (end-3d (v (aref end-2d 0) 1 (aref end-2d 1))))
    (list start-3d end-3d (v- start-3d (v 0 1 0))
          (v- start-3d (v 0 1 0)) end-3d (v- end-3d (v 0 1 0)))))

(defun get-color (lineseg)
  (sbsp::linedef-color (sbsp::lineseg-orig-line lineseg)))

(defun repeat (obj n)
  "Repeat N times the given OBJ."
  (declare (type fixnum n))
  (loop repeat n collecting obj))

(defun nrotate-camera (xrel yrel camera)
  "Rotate the CAMERA for XREL degrees around the world Y axis and YREL degrees
around the local X axis. The vertical angle is clamped."
  (let* ((xrot (q* (qrotation (v 0 1 0) (* deg->rad (- xrel)))
                   (camera-rotation camera)))
         (old-v-angle (* rad->deg (q->euler-x xrot)))
         (v-angle-diff yrel))
    (cond
      ((>= old-v-angle 90d0) (decf old-v-angle 180d0))
      ((<= old-v-angle -90d0) (incf old-v-angle 180d0)))
    (let ((v-angle (clamp (+ old-v-angle v-angle-diff) -89d0 89d0)))
      (setf (camera-rotation camera)
            (q* xrot (qrotation (v 1 0 0) (* deg->rad (- v-angle old-v-angle)))))
      camera)))

(defun view-dir (dir-name camera)
  (let ((dir (ecase dir-name
               (:forward (v 0 0 -1))
               (:back (v 0 0 1))
               (:right (v 1 0 0))
               (:left (v -1 0 0)))))
    (vrotate (camera-rotation camera) dir)))

(defun nmove-camera (dir-name delta-time camera)
  (let* ((speed (* 10 delta-time))
         (pos (v+ (camera-position camera)
                  (vscale speed (view-dir dir-name camera)))))
    (setf (camera-position camera) pos)
    camera))

(defun performance-delta (start stop)
  "Return the delta in seconds between two performance counters."
  (coerce (/ (- stop start) (sdl2:get-performance-frequency)) 'double-float))

(defstruct frame-timer
  (frame 0 :type fixnum)
  (total-time 0d0 :type double-float)
  (max-time 0d0 :type double-float))

(defun nupdate-frame-timer (frame-timer dt)
  (incf (frame-timer-frame frame-timer))
  (incf (frame-timer-total-time frame-timer) dt)
  (setf (frame-timer-max-time frame-timer)
        (max (frame-timer-max-time frame-timer) dt))
  frame-timer)

(defun load-texture (texture-file)
  "Load a texture from given string file path. Returns the OpenGL texture
object as the primary value. Second and third value are image width and height."
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

(defun make-point-renderer ()
  "Creates a function which draws a single point. The function takes symbols
DRAW and DELETE for drawing and deleting respectively."
  (let ((vao (gl:gen-vertex-array))
        deleted)
    (gl:bind-vertex-array vao)
    (gl:vertex-attrib 0 0 0 0)
    (gl:bind-vertex-array 0)
    (lambda (action)
      (if (not deleted)
          (ecase action
            (draw (gl:bind-vertex-array vao)
                  (gl:draw-arrays :points 0 1))
            (delete (gl:delete-vertex-arrays (list vao))
                     (setf deleted t)))
          (error "Trying to render with deleted point-renderer.")))))

(defun renderer-draw (renderer) (funcall renderer 'draw))
(defun renderer-delete (renderer) (funcall renderer 'delete))

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

(defun char->font-cell-pos (char font)
  "Returns the char position in pixels for the given font."
  (with-struct (font- cell-size chars-per-line start-char-code) font
    (let ((char-code (- (char-code char) start-char-code)))
      (multiple-value-bind (y x) (floor char-code chars-per-line)
        (cons (* x cell-size) (* y cell-size))))))

(defun render-text (renderer text pos-x pos-y width height shader font)
  (with-struct (font- texture cell-size) font
    (let ((ortho (ortho 0d0 width 0d0 height -1d0 1d0))
          (half-cell (* 0.5 cell-size)))
      (gl:active-texture :texture0)
      (gl:bind-texture :texture-2d texture)
      (gl:use-program shader)
      (with-uniform-locations shader (tex-font proj size cell mv char-pos)
        (gl:uniformi tex-font-loc 0)
        (uniform-matrix-4f proj-loc (list ortho))
        (gl:uniformf size-loc half-cell)
        (gl:uniformi cell-loc cell-size cell-size)
        (loop for char across text and offset from half-cell by half-cell do
             (destructuring-bind (x . y) (char->font-cell-pos char font)
               (gl:uniformi char-pos-loc x y)
               (uniform-matrix-4f mv-loc
                                  (list (translation :x (+ offset pos-x)
                                                     :y (+ pos-y half-cell))))
               (renderer-draw renderer)))))))

(defun draw-stats (cpu-max cpu-avg point-renderer text-shader font)
  (render-text point-renderer
               (format nil "CPU time: ~,2Fms (max)" cpu-max)
               600 580 800d0 600d0 text-shader font)
  (render-text point-renderer
               (format nil "CPU time: ~,2Fms (avg)" cpu-avg)
               600 564 800d0 600d0 text-shader font))

(declaim (type (unsigned-byte 32) +max-frame-skip+ +ticrate+
               *last-time* *base-time* *gametic*))
(defconstant +ticrate+ 60)
(defconstant +max-frame-skip+ 5)
(defvar *base-time* 0)
(defvar *last-time* 0)
(defvar *gametic* 0)

(defun get-time ()
  "Return time in 1/TICRATE second tics offset by *BASE-TIME*."
  (declare (optimize (speed 2)))
  (floor (* +ticrate+ (the (unsigned-byte 32) (- (sdl2:get-ticks) *base-time*)))
         1000))

(defun start-game-loop ()
  (setf *base-time* (sdl2:get-ticks)
        *last-time* *base-time*))

(defun try-run-tics (update-fun)
  (let ((new-tics (- (get-time) *last-time*)))
    (incf *last-time* new-tics)
    (loop repeat (min new-tics +max-frame-skip+) do
         (funcall update-fun)
         (incf *gametic*))))

(defvar *mouse* (cons 0 0))

(defun update-mouse-relative (xrel yrel)
  (incf (car *mouse*) xrel)
  (incf (cdr *mouse*) yrel))

(defvar *game-keys* (make-hash-table)
  "Mapping of currently pressed keys. Used for building a tick command.")

(defun reset-game-keys ()
  (clrhash *game-keys*))

(defun press-game-key (key)
  (setf (gethash key *game-keys*) t))

(defun release-game-key (key)
  (setf (gethash key *game-keys*) nil))

(defun game-key-down-p (key)
  (gethash key *game-keys*))

(defun build-ticcmd (camera)
  "TODO: Convert the camera movement into building a ticcmd for moving the camera."
  (let ((dt (coerce (/ +ticrate+) 'double-float))
        (xrel (car *mouse*))
        ;; Invert the Y movement.
        (yrel (- (cdr *mouse*)))
        (sens 1.5d0))
    (when (game-key-down-p :scancode-w)
      (nmove-camera :forward dt camera))
    (when (game-key-down-p :scancode-s)
      (nmove-camera :back dt camera))
    (when (game-key-down-p :scancode-a)
      (nmove-camera :left dt camera))
    (when (game-key-down-p :scancode-d)
      (nmove-camera :right dt camera))
    (nrotate-camera (* xrel (/ sens 10d0)) (* yrel (/ sens 10d0)) camera)
    (setf (car *mouse*) 0)
    (setf (cdr *mouse*) 0)))

(defun main ()
  (sdl2:with-init (:video)
    (set-gl-attrs)
    (sdl2:with-window (win :title "shake" :flags '(:shown :opengl))
      (sdl2:with-gl-context (context win)
        (sdl2:gl-set-swap-interval 0)
        (print-gl-info)
        (sdl2:set-relative-mouse-mode 1)
        ;; (gl:enable :depth-test :cull-face)
        (let* ((vertex-array (gl:gen-vertex-array))
               (shader-prog (load-shader #P"shaders/pass.vert"
                                         #P"shaders/color.frag"))
               (text-shader (load-shader #P"shaders/billboard.vert"
                                         #P"shaders/text.frag"
                                         #P"shaders/billboard.geom"))
               (font (load-font "share/font-16.bmp" 16 #\Space))
               (proj (perspective (* deg->rad 60d0)
                                  (/ 800d0 600d0) 0.1d0 100d0))
               (camera (make-camera :projection proj :position (v 1 0.5 8)))
               (start-time 0)
               (current-time 0)
               (delta-time 0d0) (max-time 0d0) (avg-time 0d0)
               (frame-timer (make-frame-timer))
               (point-renderer (make-point-renderer)))
          (with-uniform-locations text-shader (tex-font)
            (gl:use-program text-shader)
            (gl:uniformi tex-font-loc 0))
          (symbol-macrolet ((input-focus-p
                             (member :input-focus (sdl2:get-window-flags win)))
                            (minimized-p
                             (member :minimized (sdl2:get-window-flags win))))
            (start-game-loop)
            (sdl2:with-event-loop (:method :poll)
              (:quit () t)
              (:keydown
               (:keysym keysym)
               (when input-focus-p
                 (press-game-key (sdl2:scancode keysym))))
              (:keyup
               (:keysym keysym)
               (when input-focus-p
                 (release-game-key (sdl2:scancode keysym))))
              (:mousemotion
               (:xrel xrel :yrel yrel)
               (when input-focus-p
                 (update-mouse-relative xrel yrel)))
              (:idle ()
                     (try-run-tics (lambda () (build-ticcmd camera)))
                     (setf delta-time (performance-delta start-time current-time)
                           start-time current-time
                           current-time (sdl2:get-performance-counter))
                     (nupdate-frame-timer frame-timer delta-time)
                     (when (>= (frame-timer-total-time frame-timer) 1d0)
                       (setf avg-time (/ (frame-timer-total-time frame-timer)
                                         (frame-timer-frame frame-timer))
                             max-time (frame-timer-max-time frame-timer)
                             frame-timer (make-frame-timer)))
                     (unless minimized-p
                       (clear-buffer-fv :color 0 0 0 0)
                       (gl:use-program shader-prog)
                       (uniform-mvp shader-prog
                                    (m* (camera-projection camera)
                                        (camera-view-transform camera)))
                       (render win vertex-array camera)
                       (draw-stats (* 1d3 max-time) (* 1d3 avg-time)
                                   point-renderer text-shader font)
                       (sdl2:gl-swap-window win))))))))))

(defun set-gl-attrs ()
  "Set OpenGL context attributes. This needs to be called before window
  and context creation."
  (sdl2:gl-set-attrs
   :context-major-version 3
   :context-minor-version 3
   ;; set CONTEXT_FORWARD_COMPATIBLE
   :context-flags #x2
   ;; set CONTEXT_PROFILE_CORE
   :context-profile-mask #x1))

(defun print-gl-info ()
  "Print basic OpenGL information."
  (let ((vendor (gl:get-string :vendor))
        (renderer (gl:get-string :renderer))
        (gl-version (gl:get-string :version))
        (glsl-version (gl:get-string :shading-language-version)))
    (format t "GL Vendor: ~S~%" vendor)
    (format t "GL Renderer: ~S~%" renderer)
    (format t "GL Version: ~S~%" gl-version)
    (format t "GLSL Version: ~S~%" glsl-version)
    (finish-output)))

(defun get-map-walls (camera bsp)
  (let* ((pos (camera-position camera))
         (pos-2d (v (vx pos) (vz pos)))
         (segs (sbsp:back-to-front pos-2d bsp)))
    (values (mapcan #'get-triangles segs)
            (mapcan (lambda (s) (repeat (get-color s) 6)) segs))))

(defun render (win vertex-array camera)
  (let ((vbo (car (gl:gen-buffers 1)))
        (color-buffer (car (gl:gen-buffers 1))))
    (multiple-value-bind (triangles triangle-colors) (get-map-walls camera *bsp*)
      (gl:viewport 0 0 800 600)
      (gl:bind-vertex-array vertex-array)

      (gl:bind-buffer :array-buffer vbo)
      (buffer-data :array-buffer :static-draw :float triangles)
      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))

      (gl:bind-buffer :array-buffer color-buffer)
      (buffer-data :array-buffer :static-draw :float triangle-colors)
      (gl:enable-vertex-attrib-array 1)
      (gl:vertex-attrib-pointer 1 3 :float nil 0 (cffi:null-pointer))

      ;;    (clear-buffer-fv :depth 0 1)
      (gl:draw-arrays :triangles 0 (list-length triangles))
      ;;    (gl:draw-arrays :lines 0 10)
      (gl:bind-vertex-array 0)
      (gl:delete-buffers (list vbo color-buffer))
      (gl:check-error))))

(defun uniform-mvp (program mvp)
  (with-uniform-locations program mvp
    (uniform-matrix-4f mvp-loc (list mvp))))
