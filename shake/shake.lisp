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

(defparameter *test-linedefs*
  (list (sbsp:make-linedef :start (v 0 -2) :end (v 0 5))
        (sbsp:make-linedef :start (v -2 1) :end (v 5 1))
        (sbsp:make-linedef :start (v 3 2) :end (v 3 -2))))
(defparameter *test-linesegs* (mapcar #'sbsp:linedef->lineseg *test-linedefs*))

(defparameter *bsp*
  (sbsp::build-bsp (car *test-linesegs*) (cdr *test-linesegs*)))

(defun determine-side (point lineseg)
  "Determine on which side of a LINESEG is the given POINT located.
Returns BACK or FRONT."
  (let* ((line (sbsp::lineseg-orig-line lineseg))
         (normal (sbsp::linedef-normal line)))
    (if (>= 0 (- (vdot normal point)
                 (vdot normal (sbsp::linedef-start line))))
        'front
        'back)))

(defun back-to-front (point bsp)
  "Traverse the BSP in back to front order relative to given POINT."
  (unless (null bsp)
    (let ((node (car bsp)))
      (ecase (determine-side point node)
        (front (append (back-to-front point (caddr bsp))
                       (cons node (back-to-front point (cadr bsp)))))
        (back (append (back-to-front point (cadr bsp))
                      (cons node (back-to-front point (caddr bsp)))))))))

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

(defun get-line-list (point bsp)
  (let ((endpoints (mapcan #'get-endpoints (back-to-front point bsp))))
    (apply #'concatenate 'list endpoints)))

(defun get-triangle-list (point bsp)
  (let ((triangle-points (mapcan #'get-triangles (back-to-front point bsp))))
    (apply #'concatenate 'list triangle-points)))

(defun repeat (obj n)
  "Repeat N times the given OBJ."
  (declare (type fixnum n))
  (loop repeat n collecting obj))

(defun repeat-list (list n)
  (apply #'concatenate 'list (repeat list n)))

(defparameter *line-colors*
  (concatenate 'list
               (repeat-list (list 1 0 0) 2)
               (repeat-list (list 0 1 0) 2)
               (repeat-list (list 0 0 1) 2)
               (repeat-list (list 0.5 0.5 0) 2)
               (repeat-list (list 0 0.5 0.5) 2)))
(defparameter *triangle-colors*
  (concatenate 'list
               (repeat-list (list 1 0 0) 6)
               (repeat-list (list 0 1 0) 6)
               (repeat-list (list 0 0 1) 6)
               (repeat-list (list 0.5 0.5 0) 6)
               (repeat-list (list 0 0.5 0.5) 6)))

(defun nrotate-camera (delta-time xrel yrel camera)
  (let* ((sens 10d0)
         (speed (* sens delta-time))
         (xrot (q* (qrotation (v 0 1 0) (* speed deg->rad (- xrel)))
                   (camera-rotation camera)))
         (old-v-angle (* rad->deg (q->euler-x xrot)))
         (v-angle-diff (* speed (- yrel))))
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
  (let ((cell-size (font-cell-size font))
        (chars-per-line (font-chars-per-line font))
        (char-code (- (char-code char) (font-start-char-code font))))
    (multiple-value-bind (y x) (floor char-code chars-per-line)
      (cons (* x cell-size) (* y cell-size)))))

(defun render-text (renderer text pos-x pos-y width height shader font)
  (let* ((ortho (ortho 0d0 width 0d0 height -1d0 1d0))
         (font-tex (font-texture font))
         (cell-size (font-cell-size font))
         (half-cell (* 0.5 cell-size)))
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d font-tex)
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
             (renderer-draw renderer))))))

(defun draw-stats (cpu-max cpu-avg point-renderer text-shader font)
  (render-text point-renderer
               (format nil "CPU time: ~,2Fms (max)" cpu-max)
               600 580 800d0 600d0 text-shader font)
  (render-text point-renderer
               (format nil "CPU time: ~,2Fms (avg)" cpu-avg)
               600 564 800d0 600d0 text-shader font))

(defun main ()
  (sdl2:with-init (:video)
    (set-gl-attrs)
    (sdl2:with-window (win :title "shake" :flags '(:shown :opengl))
      (sdl2:with-gl-context (context win)
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
            (sdl2:with-event-loop (:method :poll)
              (:quit () t)
              (:keydown
               (:keysym keysym)
               (let ((scancode (sdl2:scancode-value keysym)))
                 (when input-focus-p
                   (cond
                     ((sdl2:scancode= scancode :scancode-w)
                      (nmove-camera :forward delta-time camera))
                     ((sdl2:scancode= scancode :scancode-s)
                      (nmove-camera :back delta-time camera))
                     ((sdl2:scancode= scancode :scancode-a)
                      (nmove-camera :left delta-time camera))
                     ((sdl2:scancode= scancode :scancode-d)
                      (nmove-camera :right delta-time camera))))))
              (:mousemotion
               (:xrel xrel :yrel yrel)
               (when input-focus-p
                 (nrotate-camera delta-time xrel yrel camera)))
              (:idle ()
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
                       (draw-stats (* 1d3 max-time) (* 1d3 avg-time)
                                   point-renderer text-shader font)
                       (gl:use-program shader-prog)
                       (uniform-mvp shader-prog
                                    (m* (camera-projection camera)
                                        (camera-view-transform camera)))
                       (render win vertex-array camera))))))))))

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
         (triangles (get-triangle-list pos-2d bsp)))
    triangles))

(defun render (win vertex-array camera)
  (let ((vbo (car (gl:gen-buffers 1)))
        (color-buffer (car (gl:gen-buffers 1)))
        (triangles (get-map-walls camera *bsp*)))
    (gl:viewport 0 0 800 600)
    (gl:bind-vertex-array vertex-array)

    (gl:bind-buffer :array-buffer vbo)
    (with-foreign-array vertices-ptr :float triangles
      (let ((vertices (gl::make-gl-array-from-pointer vertices-ptr :float
                                                      (list-length triangles))))
        (gl:buffer-data :array-buffer :static-draw vertices)))
    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))

    (gl:bind-buffer :array-buffer color-buffer)
    (with-foreign-array color-ptr :float *triangle-colors*
      (let ((colors (gl::make-gl-array-from-pointer color-ptr :float
                                                    (list-length *triangle-colors*))))
        (gl:buffer-data :array-buffer :static-draw colors)))
    (gl:enable-vertex-attrib-array 1)
    (gl:vertex-attrib-pointer 1 3 :float nil 0 (cffi:null-pointer))

;;    (clear-buffer-fv :depth 0 1)
    (gl:draw-arrays :triangles 0 30)
;;    (gl:draw-arrays :lines 0 10)
    (sdl2:gl-swap-window win)
    (gl:bind-vertex-array 0)
    (gl:delete-buffers (list vbo color-buffer))
    (gl:check-error)))

(defun uniform-mvp (program mvp)
  (with-uniform-locations program mvp
    (uniform-matrix-4f mvp-loc (list mvp))))
