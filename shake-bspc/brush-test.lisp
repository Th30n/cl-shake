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

(in-package #:cl-user)

(defpackage #:shake-bspc.brush-test
  (:use #:cl
        #:prove
        #:sbsp
        #:sbrush)
  (:import-from #:alexandria
                #:length=
                #:rotate
                #:set-equal)
  (:import-from #:shiva
                #:deg->rad
                #:v=
                #:v))

(in-package #:shake-bspc.brush-test)

(plan nil)

(defparameter *square-linedefs*
  (list (make-linedef :start (v -1d0 0d0) :end (v -1d0 -1d0)) ;; left
        (make-linedef :start (v -1d0 -1d0) :end (v 0d0 -1d0)) ;; bottom
        (make-linedef :start (v 0d0 -1d0) :end (v 0d0 0d0))   ;; right
        (make-linedef :start (v 0d0 0d0) :end (v -1d0 0d0)))) ;; top

(defmacro select-brush-lines (brush (&rest selectors))
  (when selectors
    `(list
      ,@(mapcar (lambda (sel) `(,sel (brush-lines ,brush)))
                selectors))))

(defun lineseg-set-equal (list1 list2)
  (set-equal list1 list2 :test #'equalp))

(subtest "Clipping two neighbour square brushes"
  (let* ((b1 (make-brush :lines *square-linedefs*))
         (b2 (sbrush::brush-translate b1 (v 1d0 0d0))))
    (let ((expected
           (mapcar #'linedef->lineseg
                   (append (select-brush-lines b1 (first second fourth))
                           (select-brush-lines b2 (second third fourth))))))
      (is (sbrush::prepare-brushes-for-bsp (list b1 b2))
          expected :test #'lineseg-set-equal))))

(subtest "Brush is clipped twice."
  (let* ((b1 (make-brush :lines *square-linedefs*))
         (b2 (sbrush::brush-translate b1 (v 1d0 0d0)))
         (b3 (sbrush::brush-translate b1 (v 0d0 1d0)))
         (expected
          (mapcar #'linedef->lineseg
                  (append (select-brush-lines b1 (first second))
                          (select-brush-lines b2 (second third fourth))
                          (select-brush-lines b3 (first third fourth))))))
    (is (sbrush::prepare-brushes-for-bsp (list b1 b2 b3))
        expected :test #'lineseg-set-equal)))

(subtest "Brush is correctly rotated."
  (let* ((b (make-brush :lines *square-linedefs*))
         (expected (rotate (copy-seq (brush-lines b)) -1)))
    (is (brush-lines (sbrush::brush-rotate b (* deg->rad 90)))
        expected :test (lambda (got exp)
                         (and (length= got exp)
                              (every #'linedef= got exp))))))

(subtest "Test serialization"
  (let ((brush (make-brush :lines *square-linedefs*)))
    (with-input-from-string (in (with-output-to-string (out)
                                  (sbrush::write-brush brush out)))
      (is (sbrush::read-brush in) brush :test #'equalp))))

(finalize)
