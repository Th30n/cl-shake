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

(in-package #:shake-gl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun foreign-type->lisp-type (foreign-type)
    "Convert a foreign type to lisp type. This is a convenience for using cffi
and is implementation dependant."
    (ecase foreign-type
      (:float 'single-float))))

(cffi:defcfun "memcpy" :pointer (dest :pointer) (src :pointer) (n :unsigned-int))

(defmacro with-gl-array ((gl-array ftype arrays) &body body)
  "Bind GL-ARRAY to a GL:GL-ARRAY containing all the elements from the given
  list of ARRAYS."
  (with-gensyms (ltype garrays total-size ptr)
    `(let* ((,garrays (ensure-list ,arrays))
            (,ltype ,(if (constantp ftype)
                         `(quote ,(foreign-type->lisp-type ftype))
                         `(foreign-type->lisp-type ftype)))
            (,total-size (reduce #'+ ,garrays :key #'array-total-size)))
       ;; For some reason, gl:with-gl-array and gl:glaref slow things down.
       (cffi:with-foreign-object (,ptr ,ftype ,total-size)
         ;; No need for genysms here as the body is outside the loop.
         (let ((findex 0))
           (dolist (a ,garrays)
             (dotimes (i (array-total-size a))
               (setf (cffi:mem-aref ,ptr ,ftype findex)
                     (coerce (row-major-aref a i) ,ltype))
               (incf findex))))
         (let ((,gl-array (gl::make-gl-array-from-pointer ,ptr ,ftype ,total-size)))
           (declare (dynamic-extent ,gl-array))
           ,@body)))))

(cffi:defcstruct draw-arrays-indirect-command
  (count :unsigned-int)
  (prim-count :unsigned-int)
  (first :unsigned-int)
  (base-instance :unsigned-int))

(defun set-draw-arrays-command (cmd-ptr count &key (prim-count 1)
                                                (first 0) (base-instance 0))
  (macrolet ((cmd-slot (slot)
               `(cffi:foreign-slot-value
                 cmd-ptr '(:struct draw-arrays-indirect-command) ',slot)))
    (setf (cmd-slot count) count
          (cmd-slot prim-count) prim-count
          (cmd-slot first) first
          (cmd-slot base-instance) base-instance)))

(defun clear-buffer-fv (buffer drawbuffer &rest values)
  (with-gl-array (value-array :float (apply #'vector values))
    (%gl:clear-buffer-fv buffer drawbuffer (gl::gl-array-pointer value-array))))

(defmacro map-buffer (target count type access &key (offset 0))
  "Maps a part of a buffer bound to TARGET. Foreign TYPE and COUNT elements of
  that type determine the map length. Optional OFFSET determines the start of
  the buffer. Returns the mapping as a POINTER"
  (let ((foreign-size (cffi:foreign-type-size type))
        (access-flags (cffi:foreign-bitfield-value '%gl::mapbufferusagemask access)))
    `(let ((length ,(if (constantp count)
                        (* count foreign-size)
                        `(* ,count ,foreign-size)))
           (byte-offset ,(if (constantp offset)
                             (* offset foreign-size)
                             `(* ,offset ,foreign-size))))
       (declare (dynamic-extent length byte-offset))
       (%gl:map-buffer-range ,target byte-offset length ,access-flags))))

(defmacro buffer-data (target usage type arrays)
  `(with-gl-array (data ,type ,arrays)
     (gl:buffer-data ,target ,usage data)))

(defun load-shader (vs-file fs-file &optional gs-file)
  "Return shader program, created from given VS-FILE and FS-FILE paths to a
vertex shader and fragment shader, respectively. Optional GS-FILE is a path
to a geometry shader."
  (let ((shaders
         (cons (compile-shader-file vs-file :vertex-shader)
               (cons
                (compile-shader-file fs-file :fragment-shader)
                (when gs-file
                  (list (compile-shader-file gs-file :geometry-shader)))))))
    (apply #'link-program shaders)))

(defun compile-shader (source type)
  "Create a shader of given TYPE, compiled with given SOURCE string."
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader source)
    (gl:compile-shader shader)
    (let ((log (gl:get-shader-info-log shader)))
      (unless (emptyp log) (print log))
      (unless (gl:get-shader shader :compile-status)
        (gl:delete-shader shader)
        (error "Error compiling shader.~%~S~%" log)))
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
    (let ((log (gl:get-program-info-log program)))
      (unless (emptyp log) (print log))
      (unless (gl:get-program program :link-status)
        (gl:delete-program program)
        (error "Error linking program.~%~S~%" log)))
    program))

(defmacro with-uniform-locations (program names &body body)
  "Get uniform locations for list of NAMES in PROGRAM and bind them to symbols.
Names of uniforms are formatted such that all '-' are replaced with '_' and
the characters are lowercased. Bound symbol's name is formatted as uniform
name with '-LOC' suffix.

Example usage:

  (with-uniform-locations program (mvp color-tex)
    ;; mvp-loc is the location of mvp uniform
    (uniform-matrix-4f mvp-loc (list mvp-matrix))
    ;; color-tex-loc is the location of color_tex uniform
    (gl:uniformi color-tex-loc 0))"
  (labels ((to-uniform-name (name)
             (ppcre:regex-replace-all "-" (string-downcase (string name)) "_"))
           (to-symbol (name)
             (if (stringp name)
                 ;; User convenience if passed in string.
                 (ppcre:regex-replace-all "_" (string-upcase name) "-")
                 name))
           (get-location (name)
             (let ((uniform-name (to-uniform-name name))
                   (var-name (symbolicate (to-symbol name) "-LOC")))
               `(,var-name
                 (let ((loc (gl:get-uniform-location ,program ,uniform-name)))
                   (declare (type integer loc))
                   (if (= loc -1)
                       (error "Uniform ~S not found" ,uniform-name)
                       loc))))))
    (let ((locations (if (consp names)
                         (mapcar #'get-location names)
                         ;; User convenience for one uniform.
                         (list (get-location names)))))
      `(let ,locations
         ,@body))))

(defun uniform-matrix-4f (location matrix &key (transpose t))
  (declare (optimize (speed 3) (space 3)))
  (check-type location (signed-byte 64))
  (check-type matrix (mat 4))
  (check-type transpose boolean)
  ;; Keep this small and optimized.  Unfortunately, sb-profile says this
  ;; performs a lot of allocations.  I cannot tell why, everything should be
  ;; stack allocated inside.
  (cffi:with-foreign-object (ptr :float 16)
    (dotimes (i 16)
      (setf (cffi:mem-aref ptr :float i)
            (coerce (row-major-aref matrix i) 'single-float)))
    (%gl:uniform-matrix-4fv location 1 transpose ptr)))
