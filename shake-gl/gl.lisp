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

(defmacro with-foreign-array (ptr ftype values &body body)
  (let ((ltype (foreign-type->lisp-type ftype)))
    (with-gensyms (len i val)
      `(let ((,len (list-length ,values)))
         (cffi:with-foreign-object (,ptr ,ftype ,len)
           (loop for ,i below ,len and ,val in ,values
              do (setf (cffi:mem-aref ,ptr ,ftype ,i)
                       (coerce ,val (quote ,ltype))))
           ,@body)))))

(defmacro with-foreign-matrix (ptr ftype matrices comps &body body)
  (let ((ltype (foreign-type->lisp-type ftype)))
    (with-gensyms (rows cols offset m i j)
      `(cffi:with-foreign-object (,ptr ,ftype (* (length ,matrices) ,comps))
         (loop for ,offset by ,comps and ,m in ,matrices
            do (let ((,rows (array-dimension ,m 0))
                     (,cols (array-dimension ,m 1)))
                 (loop for ,i below ,rows
                    do (loop for ,j below ,cols
                          do (setf (cffi:mem-aref ,ptr ,ftype
                                                  (+ ,offset (* ,i ,cols) ,j))
                                   (coerce (aref ,m ,i ,j) (quote ,ltype)))))))
         ,@body))))

(defun clear-buffer-fv (buffer drawbuffer &rest values)
  (with-foreign-array value-ptr :float values
    (%gl:clear-buffer-fv buffer drawbuffer value-ptr)))

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
                         (loop for n in names collect (get-location n))
                         ;; User convenience for one uniform.
                         (list (get-location names)))))
      `(let ,locations
         ,@body))))

(defun uniform-matrix-4f (location matrices &key (transpose t))
  (with-foreign-matrix ptr :float matrices 16
    (%gl:uniform-matrix-4fv location (length matrices) transpose ptr)))
