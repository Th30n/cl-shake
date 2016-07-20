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

(in-package #:shake-utils)

(defmacro with-struct ((name . fields) struct &body body)
  "Bind variables to FIELDS from STRUCT. NAME is a prefix associated with
the structure."
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
       (let ,(mapcar (lambda (f)
                       `(,f (,(symbolicate name f) ,gs)))
                     fields)
         ,@body))))

(defmacro notf (&rest args)
  "Invert the values of places pointed to by ARGS."
  `(setf ,@(mapcan (lambda (a) `(,a (not ,a))) args)))

(defun length>= (n sequence) (>= (length sequence) n))

(defun repeat (obj n)
  "Repeat N times the given OBJ."
  (declare (type fixnum n))
  (loop repeat n collecting obj))
