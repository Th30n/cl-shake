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

(defmacro with-foreign-array ((ptr ftype arrays) &body body)
  "Bind PTR to a foreign array containing all the elements from the given list
of ARRAYS. All arrays must have the same number of elements."
  (with-gensyms (comps ltype garrays total-size)
    `(let* ((,garrays (ensure-list ,arrays))
            (,ltype ,(if (constantp ftype)
                         `(quote ,(foreign-type->lisp-type ftype))
                         `(foreign-type->lisp-type ftype)))
            (,comps (array-total-size (car ,garrays)))
            (,total-size (* (list-length ,garrays) ,comps)))
       (dolist (a ,garrays)
         (when (/= ,comps (array-total-size a))
           (error "All arrays must have the same number of elements")))
       (cffi:with-foreign-object (,ptr ,ftype ,total-size)
         ;; No need for genysms here as the body is outside the loop.
         (loop for offset by ,comps and a in ,garrays
            do (dotimes (i ,comps)
                 (setf (cffi:mem-aref ,ptr ,ftype (+ offset i))
                       (coerce (row-major-aref a i) ,ltype))))
         ,@body))))

(defun clear-buffer-fv (buffer drawbuffer &rest values)
  (with-foreign-array (value-ptr :float (apply #'vector values))
    (%gl:clear-buffer-fv buffer drawbuffer value-ptr)))

(defun map-buffer (target count type access &key (offset 0))
  "Maps a part of a buffer bound to TARGET. Foreign TYPE and COUNT elements of
  that type determine the map length. Optional OFFSET determines the start of
  the buffer. Returns the mapping as a GL:GL-ARRAY."
  (let ((length (* count (cffi:foreign-type-size type)))
        (byte-offset (* offset (cffi:foreign-type-size type)))
        (access-flags (cffi:foreign-bitfield-value
                       '%gl::mapbufferusagemask access)))
    (gl::make-gl-array-from-pointer
     (%gl:map-buffer-range target byte-offset length access-flags)
     type count)))

(defmacro with-mapped-buffer ((gl-array target count type access &key (offset 0))
                              &body body)
  `(let ((,gl-array (map-buffer ,target ,count ,type ,access :offset ,offset)))
     (declare (dynamic-extent ,gl-array))
     (unwind-protect
          (progn ,@body)
       (gl:unmap-buffer ,target))))

(defmacro buffer-data (target usage type arrays)
  `(with-foreign-array (ptr ,type ,arrays)
     (let* ((arrays ,arrays)
            (comps (array-total-size (car arrays)))
            (data (gl::make-gl-array-from-pointer
                   ptr ,type (* comps (list-length ,arrays)))))
       (gl:buffer-data ,target ,usage data))))

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
                   (if (= loc -1)
                       (error "Uniform ~S not found" ,uniform-name)
                       loc))))))
    (let ((locations (if (consp names)
                         (mapcar #'get-location names)
                         ;; User convenience for one uniform.
                         (list (get-location names)))))
      `(let ,locations
         ,@body))))

(defun uniform-matrix-4f (location matrices &key (transpose t))
  (with-foreign-array (ptr :float matrices)
    (%gl:uniform-matrix-4fv location (length matrices) transpose ptr)))
