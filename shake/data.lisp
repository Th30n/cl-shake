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

(in-package #:shake.data)

(defun component-present-p (value)
  "Test if given pathname component is present."
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (p)
  "Test if given pathname is in directory form."
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))

(defun pathname-as-directory (name)
  "Convert given path name to directory form."
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
	(make-pathname
	 :directory (append (or (pathname-directory pathname) (list :relative))
			    (list (file-namestring pathname)))
	 :name nil
	 :type nil
	 :defaults pathname)
	pathname)))

(defun file-exists-p (pathname)
  "Checks if given PATHNAME exists and returns it, otherwise returns NIL."
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)
  #+(or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))
  #+clisp
  (or (ignore-errors
	(probe-file (pathname-as-file pathname)))
      (ignore-errors
	(let ((directory-form (pathname-as-directory pathname)))
	  (when (ext:probe-directory directory-form)
	    directory-form))))
  #-(or sbcl cmu lispworks openmcl allegro clisp)
  (error "file-exists-p not implemented"))

(defun exe-dir ()
  "Return directory of our executable image or NIL if not run from image."
  (when uiop:*image-dumped-p*
    (cond
      ((uiop:os-windows-p)
       (uiop:pathname-directory-pathname
        (cffi:with-foreign-pointer-as-string (name-ptr 260)
          (let ((res (cffi:foreign-funcall "GetModuleFileNameA"
                                           :pointer (cffi:null-pointer)
                                           (:pointer :char) name-ptr
                                           :int 260
                                           :int)))
            (if (= res 260) (error "Insufficient buffer size for GetModuleFileName"))
            (if (= res 0) (error "GetModuleFileName failed with error code: ~A"
                                 (cffi:foreign-funcall "GetLastError" :int)))))))
      ((eq :linux (uiop:operating-system))
       (uiop:pathname-directory-pathname (uiop:resolve-symlinks "/proc/self/exe")))
      (t (error "EXECUTABLE-DIR not supported on ~A" (uiop:operating-system))))))

(defun call-with-data-dirs (basedir fun)
  "Sets up the environment for searching data files. BASEDIR is added to a
  list of search paths, which by default contains the current directory."
  (let ((*search-paths* (union (list (uiop:getcwd))
                               (list (pathname-as-directory basedir))
                               :test #'uiop:pathname-equal)))
    (declare (special *search-paths*))
    (funcall fun)))

(defmacro with-data-dirs (basedir &body body)
  "Sets up the environment for searching data files. BASEDIR is added to a
  list of search paths, which by default contains the current directory."
  `(call-with-data-dirs ,basedir (lambda ()
                                   (declare (special *search-paths*))
                                   ,@body)))

(define-condition data-file-error (error)
  ((filename :initarg :filename :reader data-file-error-filename)
   (search-paths :initarg :search-paths :reader data-file-error-search-paths)
   (message :initarg :message :reader data-file-error-message))
  (:report (lambda (e stream)
             (format stream "Error opening file '~A'. ~A~%Looked for in: ~{~%  '~A'~}~%"
                     (data-file-error-filename e) (data-file-error-message e)
                     (data-file-error-search-paths e)))))

(defun data-path (filename &key (if-does-not-exist nil))
  "Construct a path to FILENAME by looking for it in *SEARCH-PATHS*. When
  IF-DOES-NOT-EXIST is :ERROR, then DATA-FILE-ERROR is signaled. Otherwise,
  returns NIL."
  (declare (special *search-paths*))
  (unless (boundp '*search-paths*)
    (error "This function needs to be called inside with-data-dirs."))
  (or (dolist (search *search-paths*)
        (when-let ((full-path (file-exists-p (merge-pathnames filename search))))
          (unless (directory-pathname-p full-path)
            (return full-path))))
      (ccase if-does-not-exist
        ((:error) (flet ((read-new-filename ()
                           (format t "Enter a new filename: ")
                           (multiple-value-list (eval (read)))))
                    (restart-case (error 'data-file-error
                                         :filename filename
                                         :message "File is missing."
                                         :search-paths *search-paths*)
                      (use-value (new-filename)
                        :interactive read-new-filename
                        :report "Use another filename."
                        (data-path new-filename :if-does-not-exist if-does-not-exist)))))
        ((nil) nil))))

(defmacro with-data-file ((stream filespec) &body body)
  "Behaves like WITH-OPEN-FILE, but searches the FILESPEC in search paths. If
  the file is not found, DATA-FILE-ERROR will be signaled."
  `(with-open-file (,stream (data-path ,filespec :if-does-not-exist :error))
     ,@body))

(defstruct resource
  loadedp
  data
  load-fun
  free-fun)

(defstruct res-scope
  name
  (res-map (make-hash-table :test 'equal)))

(defvar *resource-scopes* nil
  "Currently active resource scopes, orderer from nearest.")

(defun get-resource-from-scope (res-scope res-name)
  (with-struct (res-scope- res-map) res-scope
    (gethash res-name res-map)))

(symbol-macrolet
    ((resource-scopes
      ((lambda ()
         (declare (special *resource-scopes*))
         (when (boundp '*resource-scopes*)
           *resource-scopes*))))
     (current-res-scope
      (car resource-scopes)))

  (defun add-res (res-name load-fun free-fun)
    "Add a resource named RES-NAME to the current resource scope. The
    resource is immediately loaded and returned. Raises an error if a resource
    with the same name already exists."
    (with-struct (res-scope- res-map) current-res-scope
      (when (gethash res-name res-map)
        (error "Resource ~S already added" res-name))
      (let ((loadedp t)
            (data (funcall load-fun)))
        (flet ((load-res ()
                 (unless loadedp
                   (setf data (funcall load-fun)
                         loadedp t))
                 data)
               (free ()
                 (when loadedp
                   (funcall free-fun data)
                   (setf data nil
                         loadedp nil))))
          (setf (gethash res-name res-map)
                (make-resource :loadedp loadedp
                               :data data
                               :load-fun #'load-res
                               :free-fun #'free))
          data))))

  (defun get-resource-scope (name)
    (find-if (lambda (scope) (string= name (res-scope-name scope)))
             resource-scopes))

  (defun get-resource (res-name &key scope)
    (if scope
        (when-let ((scope (get-resource-scope scope)))
          (get-resource-from-scope scope res-name))
        (dolist (scope resource-scopes)
          (when-let ((res (get-resource-from-scope scope res-name)))
            (return res)))))

  (defun res (res-name &key scope)
    "Find a resource named RES-NAME in given SCOPE and load it if needed.
    If no scope given, start the search from the nearest scope. An error is
    raised if unable to find the resource."
    (if-let ((res (get-resource res-name :scope scope)))
      (funcall (resource-load-fun res))
      (error "Unable to find resource ~S" res-name)))

  (defun free-res (res-name &key scope)
    "Free the resource named RES-NAME in given scope. Double free and
    freeing a non existent resource are silently ignored."
    (when-let ((res (get-resource res-name :scope scope)))
      (funcall (resource-free-fun res))))

  (defun free-resources ()
    "Free all the resources in the current scope."
    (with-struct (res-scope- res-map) current-res-scope
      (maphash-values (lambda (res) (funcall (resource-free-fun res)))
                      res-map))))

(defmacro with-resources (scope-name &body body)
  "Run BODY within a resource scope named SCOPE-NAME."
  `(let ((scope (make-res-scope :name ,scope-name)))
     (push scope *resource-scopes*)
     (unwind-protect
          (progn ,@body)
       (free-resources)
       (pop *resource-scopes*))))

(defmacro res-let (names &body body)
  "Combines LET and RES calls. For example:
    (res-let (a)
      ...)
  should be the same as
    (let ((a (res \"a\")))
      ...)"
  (labels ((to-res-name (name)
             (string-downcase (string name)))
           (expand-res-call (name)
             `(,name (res ,(to-res-name name)))))
    (let ((bindings (mapcar #'expand-res-call names)))
      `(let ,bindings
         ,@body))))
