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

;; XXX: Duplicated in world-test
(defparameter *square-linedefs*
  (make-linedef-loop
   ;; top left, bottom left, bottom right, top right
   (v -1d0 0d0) (v -1d0 -1d0) (v 0d0 -1d0) (v 0d0 0d0)))

(defparameter *expanded-square-linedefs*
  (make-linedef-loop (v -2 1) (v -2 -2) (v 1 -2) (v 1 1)))

(defparameter *triangle-linedefs*
  (make-linedef-loop (v 1 0) (v 0 1) (v -1 0)))

(defparameter *expanded-triangle-linedefs*
  (make-linedef-loop (v 1.5 -0.5) (v 1.5 0.5) (v 0.5 1.5)
                     (v -0.5 1.5) (v -1.5 0.5) (v -1.5 -0.5)))

(defmacro select-brush-lines (brush (&rest selectors))
  (when selectors
    `(list
      ,@(mapcar (lambda (sel) `(,sel (brush-lines ,brush)))
                selectors))))

(defun linedef-set-equal (list1 list2)
  (set-equal list1 list2 :test #'linedef=))

(defun lineseg-set-equal (list1 list2)
  (flet ((lineseg= (seg1 seg2)
           (and (v= (lineseg-start seg1) (lineseg-start seg2))
                (v= (lineseg-end seg1) (lineseg-end seg2)))))
    (set-equal list1 list2 :test #'lineseg=)))

(defun lines->brush (lines &key (floor-height nil))
  (make-brush :surfaces
              (mapcar (lambda (line)
                        (let ((side (linedef->sidedef line)))
                          (when floor-height
                            (setf (sidedef-back-sector side)
                                  (make-sector :floor-height floor-height)))
                          side))
                      lines)))

(subtest "Clipping two neighbour square brushes"
  (let* ((b1 (lines->brush *square-linedefs*))
         (b2 (brush-translate b1 (v 1d0 0d0)))
         (expected
          (mapcar #'linedef->lineseg
                  (append (select-brush-lines b1 (first second fourth))
                          (select-brush-lines b2 (second third fourth))))))
    (is (mapcar #'sidedef-lineseg (prepare-brushes-for-bsp (list b1 b2)))
        expected :test #'lineseg-set-equal)))

(subtest "Brush is clipped twice."
  (let* ((b1 (lines->brush *square-linedefs*))
         (b2 (brush-translate b1 (v 1d0 0d0)))
         (b3 (brush-translate b1 (v 0d0 1d0)))
         (expected
          (mapcar #'linedef->lineseg
                  (append (select-brush-lines b1 (first second))
                          (select-brush-lines b2 (second third fourth))
                          (select-brush-lines b3 (first third fourth))))))
    (is (mapcar #'sidedef-lineseg (prepare-brushes-for-bsp (list b1 b2 b3)))
        expected :test #'lineseg-set-equal)))

(subtest "Test brush clipping with variable floors"
  (let* ((step-brush (lines->brush *square-linedefs* :floor-height 0.2d0))
         (wall-brush (brush-translate (lines->brush *expanded-square-linedefs*)
                                      (v 1.5 0)))
         (expected (mapcar #'linedef->lineseg
                           (append
                            (list (first *square-linedefs*)
                                  (make-linedef :start (v -1 -1)
                                                :end (v -0.5 -1))
                                  (make-linedef :start (v -0.5 0)
                                                :end (v -1 0)))
                            (brush-lines wall-brush)))))
    (is (mapcar #'sidedef-lineseg
                (prepare-brushes-for-bsp (list step-brush wall-brush)))
        expected :test #'lineseg-set-equal)))

(subtest "Brush is correctly rotated."
  (let* ((b (lines->brush *square-linedefs*))
         (expected (rotate (copy-seq (brush-lines b)) -1)))
    (is (brush-lines (brush-rotate b (* deg->rad 90)))
        expected :test (lambda (got exp)
                         (and (length= got exp)
                              (every #'linedef= got exp))))))

(subtest "Test serialization"
  (let ((brush (lines->brush *square-linedefs*)))
    (with-input-from-string (in (with-output-to-string (out)
                                  (write-brush brush out)))
      (is (read-brush in) brush :test #'equalp))))

(subtest "Test brush expansion"
  (let ((brush (lines->brush *square-linedefs*)))
    (is (brush-lines (expand-brush brush :square 2))
        *expanded-square-linedefs* :test #'linedef-set-equal))
  (let ((brush (lines->brush *triangle-linedefs*)))
    (is (brush-lines (expand-brush brush :square 1))
        *expanded-triangle-linedefs* :test #'linedef-set-equal)))

(finalize)
