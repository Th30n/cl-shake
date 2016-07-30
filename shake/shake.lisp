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

(defvar *base-dir*
  #.(directory-namestring (or *compile-file-truename* *load-truename*)))

(defstruct camera
  "A camera structure, storing the projection matrix, position in world space
and rotation as a quaternion."
  projection
  (position (v 0 0 0) :type (vec 3))
  (rotation (q 0 0 0 1) :type quat))

(defun camera-view-transform (camera)
  (declare (type camera camera))
  (let* ((pos (camera-position camera))
         (translation (translation
                       :x (- (vx pos)) :y (- (vy pos)) :z (- (vz pos))))
         (q (qconj (camera-rotation camera))))
    (m* (q->mat q) translation)))

(defparameter *bsp* nil)

(defun get-endpoints (lineseg)
  (list (sbsp:lineseg-start lineseg) (sbsp:lineseg-end lineseg)))

(defun get-triangles (surface)
  (let* ((lineseg (sbsp:sidedef-lineseg surface))
         (endpoints (get-endpoints lineseg))
         (start-2d (car endpoints))
         (start-3d (v (vx start-2d) 1 (vy start-2d)))
         (end-2d (cadr endpoints))
         (end-3d (v (vx end-2d) 1 (vy end-2d))))
    (list start-3d end-3d (v- start-3d (v 0 1 0))
          (v- start-3d (v 0 1 0)) end-3d (v- end-3d (v 0 1 0)))))

(defun get-color (surface) (sbsp:sidedef-color surface))

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
  (zap #'max (frame-timer-max-time frame-timer) dt)
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

(defun delete-font (font)
  (gl:delete-textures (list (font-texture font))))

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

(defun draw-stats (cpu-max cpu-avg)
  (res-let (point-renderer
            text-shader
            font)
    (render-text point-renderer
                 (format nil "CPU time: ~,2Fms (max)" cpu-max)
                 600 580 800d0 600d0 text-shader font)
    (render-text point-renderer
                 (format nil "CPU time: ~,2Fms (avg)" cpu-avg)
                 600 564 800d0 600d0 text-shader font)))

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

(defun try-run-tics (build-ticcmd run-tic)
  (let ((new-tics (- (get-time) *last-time*)))
    (incf *last-time* new-tics)
    (repeat (min new-tics +max-frame-skip+)
      (funcall run-tic (funcall build-ticcmd))
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

(defstruct ticcmd
  (forward-move 0d0 :type double-float)
  (side-move 0d0 :type double-float)
  (angle-turn (cons 0d0 0d0) :type (cons double-float double-float)))

(defun build-ticcmd ()
  (let ((xrel (car *mouse*))
        ;; Invert the Y movement.
        (yrel (- (cdr *mouse*)))
        (sens 1.5d0)
        (move-speed 5)
        (cmd (make-ticcmd)))
    (when (game-key-down-p :scancode-w)
      (incf (ticcmd-forward-move cmd) move-speed))
    (when (game-key-down-p :scancode-s)
      (decf (ticcmd-forward-move cmd) move-speed))
    (when (game-key-down-p :scancode-d)
      (incf (ticcmd-side-move cmd) move-speed))
    (when (game-key-down-p :scancode-a)
      (decf (ticcmd-side-move cmd) move-speed))
    (incf (car (ticcmd-angle-turn cmd)) (* xrel (/ sens 10d0)))
    (incf (cdr (ticcmd-angle-turn cmd)) (* yrel (/ sens 10d0)))
    (setf (car *mouse*) 0)
    (setf (cdr *mouse*) 0)
    cmd))

(defun clip-velocity (velocity normal)
  "Clip the given VELOCITY by projecting it on NORMAL and return a parallel
  vector to the surface."
  (let ((change (vscale (vdot velocity normal) normal)))
    (v- velocity change)))

(defun player-ground-move (origin velocity hull)
  (let ((mtrace (recursive-hull-check hull origin (v+ origin velocity))))
    (if (= (mtrace-fraction mtrace) 1d0)
        ;; Completed the whole move.
        (mtrace-endpos mtrace)
        ;; Partial move, try sliding
        (let ((time-left (- 1d0 (mtrace-fraction mtrace)))
              (new-origin (mtrace-endpos mtrace))
              (new-vel (clip-velocity velocity (mtrace-normal mtrace))))
          (mtrace-endpos
           (recursive-hull-check hull new-origin
                                 (v+ new-origin (vscale time-left new-vel))))))))

(defun move-player (player forward-move side-move &key (noclip nil))
  (let ((forward-dir (view-dir :forward player)))
    (unless noclip
      ;; Project forward-dir to plane of movement.
      (setf forward-dir (vnormalize (v (vx forward-dir) 0 (vz forward-dir)))))
    (let* ;; Intentionally make diagonal movement faster.
        ((velocity (v+ (vscale forward-move forward-dir)
                       (vscale side-move (view-dir :right player))))
         (origin (camera-position player))
         (end-pos (v+ origin velocity)))
      (if noclip
          (setf (camera-position player) end-pos)
          (let ((hull (sbsp:bspfile-clip-nodes *bsp*)))
            (setf (camera-position player)
                  (player-ground-move origin velocity hull)))))))

(defun run-tic (camera cmd)
  (with-struct (ticcmd- forward-move side-move angle-turn) cmd
    (let ((dt (coerce (/ +ticrate+) 'double-float)))
      (unless (and (zerop forward-move) (zerop side-move))
        (move-player camera (* forward-move dt) (* side-move dt) :noclip nil))
      (destructuring-bind (x-turn . y-turn) angle-turn
        (unless (and (zerop x-turn) (zerop y-turn))
          (nrotate-camera x-turn y-turn camera))))))

(defun load-main-resources ()
  (add-res "vertex-array" #'gl:gen-vertex-array
           (lambda (va) (gl:delete-vertex-arrays (list va))))
  (add-res "point-renderer" #'make-point-renderer #'renderer-delete)
  (add-res "text-shader"
           (lambda ()
             (load-shader (data-path "shaders/billboard.vert")
                          (data-path "shaders/text.frag")
                          (data-path "shaders/billboard.geom")))
           #'gl:delete-program)
  (add-res "shader-prog"
           (lambda ()
             (load-shader (data-path "shaders/pass.vert")
                          (data-path "shaders/color.frag")))
           #'gl:delete-program)
  (add-res "font"
           (lambda ()
             (load-font (data-path "share/font-16.bmp") 16 #\Space))
           #'delete-font))

(defun main ()
  (sdl2:with-init (:video)
    (set-gl-attrs)
    (sdl2:with-window (win :title "shake" :flags '(:shown :opengl))
      (sdl2:with-gl-context (context win)
        (sdl2:gl-set-swap-interval 0)
        (print-gl-info)
        (sdl2:set-relative-mouse-mode 1)
        ;; (gl:enable :depth-test :cull-face)
        (with-data-dirs *base-dir*
          (setf *bsp* (with-data-file (file  "test.bsp")
                        (sbsp:read-bspfile file)))
          (with-resources "main"
            (load-main-resources)
            (let* ((proj (perspective (* deg->rad 60d0)
                                      (/ 800d0 600d0) 0.1d0 100d0))
                   (camera (make-camera :projection proj :position (v 1 0.5 8)))
                   (start-time 0)
                   (current-time 0)
                   (delta-time 0d0) (max-time 0d0) (avg-time 0d0)
                   (frame-timer (make-frame-timer)))
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
                         (try-run-tics #'build-ticcmd
                                       (lambda (tic) (run-tic camera tic)))
                         (setf delta-time (performance-delta start-time
                                                             current-time)
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
                           (res-let (shader-prog)
                             (gl:use-program shader-prog)
                             (uniform-mvp shader-prog
                                          (m* (camera-projection camera)
                                              (camera-view-transform camera))))
                           (render win camera)
                           (draw-stats (* 1d3 max-time) (* 1d3 avg-time))
                           (sdl2:gl-swap-window win))))))))))))

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
         (surfs (sbsp:back-to-front pos-2d (sbsp:bspfile-nodes bsp))))
    (values (mapcan #'get-triangles surfs)
            (mapcan (lambda (s) (make-list 6 :initial-element (get-color s)))
                    surfs))))

(defun render (win camera)
  (with-resources "render"
    (destructuring-bind (vbo color-buffer)
        (add-res "buffers" (lambda () (gl:gen-buffers 2)) #'gl:delete-buffers)
      (multiple-value-bind (triangles triangle-colors)
          (get-map-walls camera *bsp*)
        (gl:viewport 0 0 800 600)
        (gl:bind-vertex-array (res "vertex-array"))

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
        (gl:check-error)))))

(defun uniform-mvp (program mvp)
  (with-uniform-locations program mvp
    (uniform-matrix-4f mvp-loc (list mvp))))
