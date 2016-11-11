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

(in-package #:shake.render-progs)

(defun shader-path (name ext)
  (sdata:data-path (concatenate 'string "shaders/" name ext)))

(defstruct prog-manager
  (vertex-shaders (make-hash-table))
  (fragment-shaders (make-hash-table))
  (geometry-shaders (make-hash-table))
  (programs (make-hash-table :test #'equal))
  (bound-program 0 :type fixnum))

(defun init-prog-manager ()
  (make-prog-manager))

(defun shutdown-prog-manager (prog-manager)
  (purge-all-programs prog-manager))

(defmacro with-prog-manager (prog-manager &body body)
  `(bracket (,prog-manager (init-prog-manager) shutdown-prog-manager)
     ,@body))

(defun bind-program (prog-manager shader-prog)
  (unless (= shader-prog (prog-manager-bound-program prog-manager))
    (gl:use-program shader-prog)
    (setf (prog-manager-bound-program prog-manager) shader-prog)))

(defun unbind-program (prog-manager)
  (unless (zerop (prog-manager-bound-program prog-manager))
    (gl:use-program 0)
    (setf (prog-manager-bound-program prog-manager) 0)))

(defun purge-all-programs (prog-manager)
  (unbind-program prog-manager)
  (with-struct (prog-manager- vertex-shaders fragment-shaders geometry-shaders
                              programs) prog-manager
    (dolist (shaders (list vertex-shaders fragment-shaders geometry-shaders))
      (maphash-values #'gl:delete-shader shaders))
    (maphash-values #'gl:delete-program programs)))

(defun find-shader (shaders name)
  (gethash name shaders nil))

(defun add-shader (shaders name gl-shader)
  (setf (gethash name shaders) gl-shader))

(defun load-shader (shader-type name)
  )

(defun find-program (prog-manager vs-name fs-name gs-name)
  (gethash (list vs-name fs-name gs-name) (prog-manager-programs prog-manager)
           nil))

(defun add-program (prog-manager vs-name fs-name gs-name gl-prog)
  (setf (gethash (list vs-name fs-name gs-name)
                 (prog-manager-programs prog-manager))
        gl-prog))

(defun load-program (vs-name fs-name &optional gs-name)
  )

(defun reload-programs (prog-manager)
  (purge-all-programs prog-manager)
  (with-struct (prog-manager- vertex-shaders fragment-shaders geometry-shaders
                              programs) prog-manager
    (loop for shaders in (list vertex-shaders fragment-shaders geometry-shaders)
       and shader-type in (:vertex-shader :fragment-shader :geometry-shader) do
         (maphash-keys (lambda (name)
                         (add-shader shaders name
                                     (load-shader shader-type name)))
                       shaders))
    (maphash-keys (lambda (shader-names)
                    (destructuring-bind (vs-name fs-name gs-name) shader-names
                      (add-program prog-manager vs-name fs-name gs-name
                                   (load-program vs-name fs-name gs-name))))
                  programs)))

(defun get-shader (prog-manager name shader-type)
  (destructuring-bind (ext . shaders)
      (ecase shader-type
        (:vertex-shader
         (cons ".vert" (prog-manager-vertex-shaders prog-manager)))
        (:fragment-shader
         (cons ".frag" (prog-manager-fragment-shaders prog-manager)))
        (:geometry-shader
         (cons ".geom" (prog-manager-geometry-shaders prog-manager))))
    (or (find-shader shaders name)
        (let* ((source (sdata:with-data-file (file (shader-path name ext))
                         (read-stream-content-into-string file)))
               (gl-shader (sgl::compile-shader source shader-type)))
          (add-shader shaders name gl-shader)))))

(defun get-vertex-shader (prog-manager name)
  (get-shader prog-manager name :vertex-shader))

(defun get-fragment-shader (prog-manager name)
  (get-shader prog-manager name :fragment-shader))

(defun get-geometry-shader (prog-manager name)
  (get-shader prog-manager name :geometry-shader))

(defun get-program (prog-manager vs-name fs-name &optional gs-name)
  (or (find-program prog-manager vs-name fs-name gs-name)
      (let* ((vs (get-vertex-shader prog-manager vs-name))
             (fs (get-fragment-shader prog-manager fs-name))
             (gs (when gs-name (get-geometry-shader prog-manager gs-name)))
             (gl-prog (apply #'sgl::link-program
                             (cons vs (cons fs (when gs (list gs)))))))
        (add-program prog-manager vs-name fs-name gs-name gl-prog))))
