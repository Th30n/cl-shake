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

(defmacro with-data-dirs (basedir &body body)
  `(let ((*search-paths* (list "" (pathname-as-directory ,basedir))))
     (declare (special *search-paths*))
     ,@body))

(defmacro define-data-fun (name lambda-list &body body)
  (let* ((doc-string (when (stringp (car body)) (car body)))
         (rem-body (if doc-string (cdr body) body)))
    `(defun ,name ,lambda-list
       ,doc-string
       (declare (special *search-paths*))
       (unless (boundp '*search-paths*)
         (error "This function needs to be called inside with-data-dirs."))
       ,@rem-body)))

(define-data-fun data-path (filename)
  "Construct a path to FILENAME by looking for it in to *SEARCH-PATHS*."
  (dolist (search *search-paths*)
    (when-let ((full-path (file-exists-p (merge-pathnames filename search))))
      (unless (directory-pathname-p full-path)
        (format t "Found data file ~S~%" full-path)
        (return full-path)))))

(defmacro with-data-file ((stream filespec) &body body)
  `(with-open-file (,stream (data-path ,filespec))
     ,@body))

(defstruct resource
  loadedp
  data
  load-fun
  free-fun)

(defstruct res-type
  name
  load-fun)

(defstruct res-category
  name
  (res-map (make-hash-table :test 'equal)))

(symbol-macrolet
    ((res-types
      ((lambda ()
         (declare (special *resource-types*))
         (when (boundp '*resource-types*)
           *resource-types*)))))

  (defun get-res-type (res-name)
    (flet ((matches-ext (res-type)
             (let ((ext (concatenate 'string "." (res-type-name res-type))))
               (ends-with-subseq ext res-name))))
      (find-if #'matches-ext res-types))))

(defun get-resource-from-category (res-category res-name)
  (with-struct (res-category- res-map) res-category
    (if-let ((res (gethash res-name res-map)))
      res
      (when-let (res-type (get-res-type res-name))
        (funcall (res-type-load-fun res-type) res-name)))))

(symbol-macrolet
    ((resource-categories
      ((lambda ()
         (declare (special *resource-categories*))
         (when (boundp '*resource-categories*)
           *resource-categories*))))
     (current-res-category
      ((lambda ()
         (declare (special *current-res-category*))
         *current-res-category*))))

  (defun add-res (res-name load-fun free-fun)
    (with-struct (res-category- res-map) current-res-category
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

  (defun get-resource-category (name)
    (find-if (lambda (cat) (string= name (res-category-name cat)))
             resource-categories))

  (defun get-resource (res-name &key category)
    (if category
        (when-let ((cat (get-resource-category category)))
          (get-resource-from-category cat res-name))
        (dolist (category resource-categories)
          (when-let ((res (get-resource-from-category category res-name)))
            (return res)))))

  (defun res (res-name &key category)
    (if-let ((res (get-resource res-name :category category)))
      (funcall (resource-load-fun res))
      (error "Unable to find resource ~S" res-name)))

  (defun free-res (res-name &key category)
    (when-let ((res (get-resource res-name :category category)))
      (funcall (resource-free-fun res))))

  (defun free-resources ()
    (with-struct (res-category- res-map) current-res-category
      (maphash-values (lambda (res) (funcall (resource-free-fun res)))
                      res-map))))

(defvar *resource-categories* (list))

(defmacro with-resources (category-name &body body)
  `(let ((*current-res-category* (make-res-category :name ,category-name)))
     (declare (special *current-res-category*))
     (push *current-res-category* *resource-categories*)
     (unwind-protect
          (progn ,@body)
       (free-resources)
       (pop *resource-categories*))))
