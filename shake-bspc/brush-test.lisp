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
                #:set-equal)
  (:import-from #:shiva
                #:v=
                #:v))

(in-package #:shake-bspc.brush-test)

(plan nil)

(defparameter *square-linedefs*
  (list (make-linedef :start (v -1d0 0d0) :end (v -1d0 -1d0))
        (make-linedef :start (v -1d0 -1d0) :end (v 0d0 -1d0))
        (make-linedef :start (v 0d0 -1d0) :end (v 0d0 0d0))
        (make-linedef :start (v 0d0 0d0) :end (v -1d0 0d0))))

(defmacro select-brush-lines (brush (&rest selectors))
  (when selectors
    `(list
      ,@(mapcar (lambda (sel) `(,sel (sbrush::brush-lines ,brush)))
                selectors))))

(defun lineseg-set-equal (list1 list2)
  (set-equal list1 list2 :test #'equalp))

(subtest "brush-clip on neighbour square brushes"
  (let* ((b1 (sbrush::make-brush :lines *square-linedefs*))
         (b2 (sbrush::brush-translate b1 (v 1d0 0d0))))
    (multiple-value-bind (outside inside) (sbrush::brush-clip b1 b2)
      (let ((expected-outside
             (mapcar #'linedef->lineseg
                     (select-brush-lines b1 (first second fourth)))))
        (is inside (list (linedef->lineseg (third *square-linedefs*)))
            :test #'equalp)
        (is outside expected-outside :test #'lineseg-set-equal)
        (is (sbrush::prepare-brushes-for-bsp (list b1 b2))
            (append expected-outside
                    (mapcar #'linedef->lineseg
                            (select-brush-lines b2 (second third fourth))))
            :test #'lineseg-set-equal)))))

(finalize)
