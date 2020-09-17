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

;;; Shader program management: loading, compiling and reusing.

(in-package #:shake.render)

(defun shader-path (name &optional ext)
  "Return the data path for shaders of given NAME and file extension EXT."
  (sdata:data-path (concatenate 'string "shaders/" name ext) :if-does-not-exist :error))

(defstruct prog-manager
  "Manager for shader programs. Loading and using shaders should only be done
  through a single instance of this type."
  (vertex-shaders (make-hash-table))
  (fragment-shaders (make-hash-table))
  (geometry-shaders (make-hash-table))
  (programs (make-hash-table :test #'equal))
  (bound-program 0 :type fixnum))

(defun init-prog-manager ()
  "Create and setup a new PROG-MANAGER."
  (make-prog-manager))

(defun shutdown-prog-manager (prog-manager)
  "Unload and clean any resources related to shader programs."
  (purge-all-programs prog-manager))

(defmacro with-prog-manager (prog-manager &body body)
  "Setup a new PROG-MANAGER and bind it to given symbol. Perform clean up
  after the evaluation of BODY."
  `(bracket (,prog-manager (init-prog-manager) shutdown-prog-manager)
     ,@body))

(defun bind-program (prog-manager shader-prog)
  "Bind the given SHADER-PROG to GL state."
  (unless (= shader-prog (prog-manager-bound-program prog-manager))
    (gl:use-program shader-prog)
    (setf (prog-manager-bound-program prog-manager) shader-prog)))

(defun unbind-program (prog-manager)
  "Remove any shader program binding from GL state."
  (unless (zerop (prog-manager-bound-program prog-manager))
    (gl:use-program 0)
    (setf (prog-manager-bound-program prog-manager) 0)))

(defun purge-all-programs (prog-manager)
  "Unload all shader programs. Do not use programs after this function is
  called, because they will not be loaded. To avoid errors, this function
  should only be called on shutdown or through RELOAD-PROGRAMS."
  (unbind-program prog-manager)
  (with-struct (prog-manager- vertex-shaders fragment-shaders geometry-shaders
                              programs) prog-manager
    (dolist (shaders (list vertex-shaders fragment-shaders geometry-shaders))
      (maphash-values #'gl:delete-shader shaders))
    ;; TODO: It would be a good idea to wrap shader and program objects into a
    ;; type, which stores loaded state so dangling object references cannot be
    ;; accidentally used. Additional benefit would be to scan and store
    ;; locations of all active uniforms on the first load.
    (maphash-values #'gl:delete-program programs)))

(defun find-shader (shaders name)
  (gethash name shaders))

(defun add-shader (shaders name gl-shader)
  (setf (gethash name shaders) gl-shader))

(defun load-shader (shader-type name)
  "Create and compile a shader of SHADER-TYPE from file NAME."
  (let ((ext (ecase shader-type
               (:vertex-shader ".vert")
               (:fragment-shader ".frag")
               (:geometry-shader ".geom"))))
    (labels ((strip (string)
               (string-trim '(#\Space #\Tab #\Linefeed #\Return) string))
             (get-include-path (include-string)
               (cond
                 ((<= (length include-string) 2)
                  (error "Expected a quoted non-empty include path"))
                 ((and (starts-with #\" include-string) (ends-with #\" include-string))
                  (shader-path (subseq include-string 1 (1- (length include-string)))))
                 ((and (starts-with #\< include-string) (ends-with #\> include-string))
                  (shader-path (subseq include-string 1 (1- (length include-string)))))
                 (t (error "Unable to parse '#include ~A'" include-string))))
             (read-shader-file (path)
               (sdata:with-data-file (file path)
                 (format nil "~{~A~%~}"
                         (loop as line = (read-line file nil) while line collect
                              (multiple-value-bind (includep include-path)
                                  (starts-with-subseq "#include" (strip line) :return-suffix t)
                                (if includep
                                    ;; TODO: Recursive include and multiple includes
                                    (read-shader-file (get-include-path (strip include-path)))
                                    line)))))))
      (sgl:compile-shader (read-shader-file (shader-path name ext)) shader-type))))

(defun find-program (prog-manager vs-name fs-name gs-name)
  (gethash (list vs-name fs-name gs-name) (prog-manager-programs prog-manager)))

(defun add-program (prog-manager vs-name fs-name gs-name gl-prog)
  (setf (gethash (list vs-name fs-name gs-name)
                 (prog-manager-programs prog-manager))
        gl-prog))

(defun load-program (prog-manager vs-name fs-name &optional gs-name)
  "Load and return the linked program using given shader file names."
  (let ((vs (get-vertex-shader prog-manager vs-name))
        (fs (get-fragment-shader prog-manager fs-name))
        (gs (when gs-name (get-geometry-shader prog-manager gs-name))))
    (apply #'sgl::link-program (cons vs (cons fs (when gs (list gs)))))))

(defun reload-programs (prog-manager)
  "Recompile and relink all shader programs."
  (purge-all-programs prog-manager)
  (with-struct (prog-manager- vertex-shaders fragment-shaders geometry-shaders
                              programs) prog-manager
    (loop for shaders in (list vertex-shaders fragment-shaders geometry-shaders)
       and shader-type in '(:vertex-shader :fragment-shader :geometry-shader) do
         (maphash-keys (lambda (name)
                         (add-shader shaders name (load-shader shader-type name)))
                       shaders))
    (maphash-keys (lambda (shader-names)
                    (apply #'add-program prog-manager shader-names
                           (apply #'load-program prog-manager shader-names)))
                  programs)))

(defun get-shader (prog-manager name shader-type)
  "Get the shader object specified by file NAME and SHADER-TYPE. The shader is
  loaded if necessary."
  (let ((shaders
         (ecase shader-type
           (:vertex-shader (prog-manager-vertex-shaders prog-manager))
           (:fragment-shader (prog-manager-fragment-shaders prog-manager))
           (:geometry-shader (prog-manager-geometry-shaders prog-manager)))))
    (or (find-shader shaders name)
        (add-shader shaders name (load-shader shader-type name)))))

(defun get-vertex-shader (prog-manager name)
  (get-shader prog-manager name :vertex-shader))

(defun get-fragment-shader (prog-manager name)
  (get-shader prog-manager name :fragment-shader))

(defun get-geometry-shader (prog-manager name)
  (get-shader prog-manager name :geometry-shader))

(defun get-program (prog-manager vs-name fs-name &optional gs-name)
  "Get the shader program specified with given shader file names: VS-NAME is a
  vertex shader, FS-NAME is a fragment shader and GS-NAME is a geometry
  shader. Shaders are loaded if necessary."
  (or (find-program prog-manager vs-name fs-name gs-name)
      (let ((gl-prog (load-program prog-manager vs-name fs-name gs-name)))
        (add-program prog-manager vs-name fs-name gs-name gl-prog))))
