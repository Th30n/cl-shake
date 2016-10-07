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
  "Bind variables to FIELDS from STRUCT. NAME is a prefix associated with the
  structure. A binding can be just the field name or a list of 2 elements:
  (VAR FIELD).

  The following example will bind VAL-A to the value of MYSTRUCT-VAL-A and B
  to MYSTRUCT-VAL-B.
  (with-struct (mystruct- val-a (b val-b)) struct-instance
    ..."
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
       (let ,(mapcar (lambda (f)
                       (if (atom f)
                           `(,f (,(symbolicate name f) ,gs))
                           `(,(first f) (,(symbolicate name (second f)) ,gs))))
                     fields)
         ,@body))))

(defmacro aif (test then &optional else)
  "Anaphoric IF from On Lisp by Paul Graham. Behaves like regular IF, but the
  TEST result is bound to IT symbol.

  For example:
  (aif (long-computation)
       (foo it))"
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro awhen (test &body forms)
  "Anaphoric WHEN from On Lisp by Paul Graham. Behaves like regular WHEN, but
  the TEST result is bound to IT symbol.

  For example:
  (awhen (long-computation)
    (foo it)
    (bar it))"
  `(aif ,test (progn ,@forms)))

(defmacro bracket ((var acquire release) &body body)
  "Gets a resource via ACQUIRE form and binds it to VAR. Before returning,
  frees the resource by calling RELEASE function on the resource."
  `(let ((,var ,acquire))
     (declare (dynamic-extent ,var))
     (unwind-protect (progn ,@body)
       (,release ,var))))

(defmacro doproduct (((var-a list-a) (var-b list-b) &optional result) &body body)
  "Iterate over a Cartesian product of LIST-A and LIST-B."
  `(dolist (,var-a ,list-a ,result)
     (dolist (,var-b ,list-b)
       ,@body)))

(defmacro dolist-enum ((idx-var elt-var list &optional result) &body body)
  "Behaves like DOLIST, but binds the IDX-VAR to the index of the current
  element."
  `(let ((,idx-var 0))
     (declare (type fixnum ,idx-var))
     (dolist (,elt-var ,list ,result)
       ,@body
       (incf ,idx-var))))

(defmacro dovector ((var vector &optional result) &body body)
  "Behaves like DOLIST, but on a one dimensional array."
  (with-gensyms (i n gvector)
    `(let* ((,gvector ,vector)
            (,n (length ,vector)))
       (dotimes (,i ,n ,result)
         (let ((,var (aref ,gvector ,i)))
           ,@body)))))

(defmacro in (obj &rest choices)
  "Test whether the OBJ is EQL to any of the given CHOICES."
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar (lambda (c) `(eql ,insym ,c)) choices)))))

(defmacro inq (obj &rest choices)
  "Like IN, but choices are quoted."
  `(in ,obj ,@(mapcar (lambda (c) `',c) choices)))

(defmacro zap (fn place &rest args)
  "Set the PLACE to the result of applying the function FN to the current
  value of place and ARGS. For example:

    (zap #'1+ place) === (incf place)
    (zap #'cons (car list) 'a) === (setf (car list) (cons (car list) 'a))"
  (multiple-value-bind (temps exprs stores writer reader)
      (get-setf-expansion place)
    `(let* (,@(mapcar #'list temps exprs)
            (,(car stores) (funcall ,fn ,reader ,@args)))
       ,writer)))

(defmacro notf (&rest args)
  "Invert the values of places pointed to by ARGS."
  `(setf ,@(mappend (lambda (a) `(,a (not ,a))) args)))

(defmacro repeat (n &body body)
  "Evaluate N times the given BODY and collect the results into a list."
  `(loop repeat ,n collecting (progn ,@body)))

(defun length>= (n sequence) (>= (length sequence) n))

(defun group-by (test list)
  "Groups the LIST elements into sublists containing only elements that are
  equal according to TEST function. For example:

  (group-by #'= '(1 2 3 3 2 3 3 2 4 4 2)) ===
  '((1) (2) (3 3) (2) (3 3) (2) (4 4) (2))"
  (let (groups last-group)
    (dolist (elem list)
      (if (or (not last-group) (funcall test (first last-group) elem))
          (push elem last-group)
          (progn
            (push (reverse last-group) groups)
            (setf last-group (list elem)))))
    (push (reverse last-group) groups)
    (reverse groups)))
