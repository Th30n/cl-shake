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

(defmacro with-data-dirs (basedir &body body)
  `(let ((*basedir* (pathname-as-directory ,basedir)))
     (declare (special *basedir*))
     ,@body))

(defmacro define-data-fun (name lambda-list &body body)
  (let* ((doc-string (when (stringp (car body)) (car body)))
         (rem-body (if doc-string (cdr body) body)))
    `(defun ,name ,lambda-list
       ,doc-string
       (declare (special *basedir*))
       (unless (boundp '*basedir*)
         (error "This function needs to be called inside with-data-dirs."))
       ,@rem-body)))

(define-data-fun data-path (filename)
  "Construct a path to FILENAME relative to *BASE-DIR*."
  (merge-pathnames filename *basedir*))

(defmacro with-data-file ((stream filespec) &body body)
  `(with-open-file (,stream (data-path ,filespec))
     ,@body))
