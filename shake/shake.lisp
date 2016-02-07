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
  (position (v 0 0 0) :type (vector double-float 3))
  (rotation (q 0 0 0 1) :type (cons (vector double-float 3) double-float)))

(defun camera-view-transform (camera)
  (declare (type camera camera))
  (let* ((pos (camera-position camera))
         (translation (translation
                       :x (- (vx pos)) :y (- (vy pos)) :z (- (vz pos))))
         (q (qconj (camera-rotation camera))))
    (m* (q->mat q) translation)))

(defparameter *test-linesegs* (mapcar #'shake-bspc::linedef->lineseg
                                      shake-bspc-test::*test-linedefs*))

(defun inorder (bsp)
  (unless (null bsp)
    (append (inorder (cadr bsp))
            (cons (car bsp)
                  (inorder (caddr bsp))))))

(defun get-endpoints (lineseg)
  (let* ((linedef (shake-bspc::lineseg-orig-line lineseg))
         (line-vec (v- (shake-bspc::linedef-end linedef)
                       (shake-bspc::linedef-start linedef)))
         (start (v+ (shake-bspc::linedef-start linedef)
                    (vscale (shake-bspc::lineseg-t-start lineseg) line-vec)))
         (end (v+ (shake-bspc::linedef-start linedef)
                  (vscale (shake-bspc::lineseg-t-end lineseg) line-vec))))
    (list start end)))

(defun get-triangles (lineseg)
  (let* ((endpoints (get-endpoints lineseg))
         (start-2d (car endpoints))
         (start-3d (v (aref start-2d 0) 1 (aref start-2d 1)))
         (end-2d (cadr endpoints))
         (end-3d (v (aref end-2d 0) 1 (aref end-2d 1))))
    (list start-3d end-3d (v- start-3d (v 0 1 0))
          (v- start-3d (v 0 1 0)) end-3d (v- end-3d (v 0 1 0)))))

(defun get-line-list ()
  (let* ((bsp (shake-bspc::build-bsp (car *test-linesegs*) (cdr *test-linesegs*)))
         (endpoints (mapcan #'get-endpoints (inorder bsp))))
    (apply #'concatenate 'list endpoints)))

(defun get-triangle-list ()
  (let* ((bsp (shake-bspc::build-bsp (car *test-linesegs*) (cdr *test-linesegs*)))
         (triangle-points (mapcan #'get-triangles (inorder bsp))))
    (apply #'concatenate 'list triangle-points)))

(defun repeat (obj n)
  "Repeat N times the given OBJ."
  (loop repeat n collecting obj))

(defun repeat-list (list n)
  (apply #'concatenate 'list (repeat list n)))

(defparameter *lines* (get-line-list))
(defparameter *triangles* (get-triangle-list))
(defparameter *colors*
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

(defun nrotate-camera (xrel yrel camera)
  (let* ((xrot (q* (qrotation (v 0 1 0) (* deg->rad (- xrel)))
                   (camera-rotation camera)))
         (yrot (q* xrot (qrotation (v 1 0 0) (* deg->rad (- yrel))))))
    (setf (camera-rotation camera) yrot)
    camera))

(defun main ()
  (sdl2:with-init (:video)
    (set-gl-attrs)
    (sdl2:with-window (win :title "shake" :flags '(:shown :opengl))
      (sdl2:with-gl-context (context win)
        (print-gl-info)
        (let* ((vertex-array (gl:gen-vertex-array))
               (shader-prog (load-shader #P"shaders/pass.vert"
                                         #P"shaders/color.frag"))
               (proj (perspective (* deg->rad 60d0)
                                  (/ 800d0 600d0) 0.1d0 100d0))
               (camera (make-camera :projection proj :position (v -2 0 8))))
          (gl:use-program shader-prog)
          ;; (uniform-mvp shader-prog (ortho -6d0 6d0 -6d0 6d0 -2d0 2d0))
          (sdl2:with-event-loop (:method :poll)
            (:quit () t)
            (:mousemotion
             (:xrel xrel :yrel yrel)
             (nrotate-camera xrel yrel camera))
            (:idle ()
                   (uniform-mvp shader-prog
                                (m* (camera-projection camera)
                                    (camera-view-transform camera)))
                   (render win vertex-array))))))))

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

(defun load-shader (vs-file fs-file)
  "Return shader program, created from given VS-FILE and FS-FILE paths to a
  vertex shader and fragment shader, respectively."
  (let* ((vs (compile-shader-file vs-file :vertex-shader))
         (fs (compile-shader-file fs-file :fragment-shader)))
    (link-program fs vs)))

(defun compile-shader (source type)
  "Create a shader of given TYPE, compiled with given SOURCE string."
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader source)
    (gl:compile-shader shader)
    (print (gl:get-shader-info-log shader))
    shader))

(defun compile-shader-file (source-file shader-type)
  (let ((src (read-file-into-string source-file)))
    (compile-shader src shader-type)))

(defun link-program (shader &rest shaders)
  "Create a program, linked with given SHADER objects."
  (let ((program (gl:create-program)))
    (dolist (sh (cons shader shaders))
      (gl:attach-shader program sh))
    (gl:link-program program)
    (print (gl:get-program-info-log program))
    program))

(defmacro with-foreign-array (ptr ftype ltype values &body body)
  (with-gensyms (len i val)
    `(let ((,len (list-length ,values)))
       (cffi:with-foreign-object (,ptr ,ftype ,len)
         (loop for ,i below ,len and ,val in ,values
            do (setf (cffi:mem-aref ,ptr ,ftype ,i)
                     (coerce ,val ,ltype)))
         ,@body))))

(defun render (win vertex-array)
  (let ((vbo (car (gl:gen-buffers 1)))
        (color-buffer (car (gl:gen-buffers 1))))
    (gl:viewport 0 0 800 600)
    (gl:bind-vertex-array vertex-array)

    (gl:bind-buffer :array-buffer vbo)
    (with-foreign-array vertices-ptr :float 'single-float *triangles*
      (let ((vertices (gl::make-gl-array-from-pointer vertices-ptr :float
                                                      (list-length *triangles*))))
        (gl:buffer-data :array-buffer :static-draw vertices)))
    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))

    (gl:bind-buffer :array-buffer color-buffer)
    (with-foreign-array color-ptr :float 'single-float *triangle-colors*
      (let ((colors (gl::make-gl-array-from-pointer color-ptr :float
                                                    (list-length *triangle-colors*))))
        (gl:buffer-data :array-buffer :static-draw colors)))
    (gl:enable-vertex-attrib-array 1)
    (gl:vertex-attrib-pointer 1 3 :float nil 0 (cffi:null-pointer))

    (clear-buffer-fv :color 0 0 0 0)
    (gl:draw-arrays :triangles 0 30)
;;    (gl:draw-arrays :lines 0 10)
    (sdl2:gl-swap-window win)
    (gl:bind-vertex-array 0)
    (gl:delete-buffers (list vbo color-buffer))
    (gl:check-error)))

(defun clear-buffer-fv (buffer drawbuffer &rest values)
  (with-foreign-array value-ptr :float 'single-float values
    (%gl:clear-buffer-fv buffer drawbuffer value-ptr)))

(defmacro with-foreign-matrix (ptr ftype ltype matrices comps &body body)
  (with-gensyms (rows cols offset m i j)
    `(cffi:with-foreign-object (,ptr ,ftype (* (length ,matrices) ,comps))
       (loop for ,offset by ,comps and ,m in ,matrices
          do (let ((,rows (array-dimension ,m 0))
                   (,cols (array-dimension ,m 1)))
               (loop for ,i below ,rows
                  do (loop for ,j below ,cols
                        do (setf (cffi:mem-aref ,ptr ,ftype
                                                (+ ,offset (* ,i ,cols) ,j))
                                 (coerce (aref ,m ,i ,j) ,ltype))))))
       ,@body)))

(defun uniform-matrix-4f (location matrices &key (transpose t))
  (with-foreign-matrix ptr :float 'single-float matrices 16
    (%gl:uniform-matrix-4fv location (length matrices) transpose ptr)))

(defun uniform-mvp (program mvp)
  (let ((mvp-loc (gl:get-uniform-location program "mvp")))
    (uniform-matrix-4f mvp-loc (list mvp))))
