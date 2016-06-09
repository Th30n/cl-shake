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

;;; Functions for basic operations with brushes. A brush is a convex polygon
;;; with content details.

(in-package #:shake-bspc.brush)

(defstruct brush
  "A convex polygon defining a geometry with contents.
  LINES is a list of LINEDEFs defining the polygon.
  CONTENTS can be one of:
  * :CONTENTS-EMPTY
  * :CONTENTS-SOLID"
  lines
  (contents :contents-solid))

(defun brush-translate (brush vector)
  "Translate the given BRUSH along the given VECTOR."
  (flet ((linedef-translate (line)
           (make-linedef :start (v+ (linedef-start line) vector)
                         :end (v+ (linedef-end line) vector)
                         :color (sbsp::linedef-color line))))
    (make-brush :lines (mapcar #'linedef-translate (brush-lines brush))
                :contents (brush-contents brush))))

(defun brush-clip (b1 b2)
  "Subtracts the geometry of brush B2 from brush B1. The result is a list of
  remaining LINESEGs on the outside. Second value are removed segments."
  (let (outside inside)
    (dolist (split-line (brush-lines b2))
      (let ((split-normal (linedef-normal split-line)))
        (dolist (line (brush-lines b1))
          ;; XXX: duplicated from sbsp::partition-linesegs
          (let ((lineseg (linedef->lineseg line)))
            (multiple-value-bind (num den)
                (sbsp::line-intersect-ratio split-line lineseg)
              (if (double-float-rel-eq 0d0 den)
                  ;; parallel lines
                  (cond
                    ((double-float-rel-eq num 0d0)
                     ;; on the same line
                     (if (v= split-normal (linedef-normal line))
                         ;; leave same facing on the outside
                         (push lineseg outside)
                         ;; clip opposite facing
                         (push lineseg inside)))
                    ((plusp num)
                     (push lineseg outside))
                    (t
                     (push lineseg inside)))
                  ;; split
                  (destructuring-bind (front . back)
                      (sbsp::split-partition split-line lineseg num den)
                    (when front
                      (push front outside))
                    (when back
                      (push back inside)))))))))
    (values (delete-duplicates outside :test #'equalp)
            (delete-duplicates (nset-difference inside outside :test #'equalp)
                               :test #'equalp))))

(defun prepare-brushes-for-bsp (brushes)
  "Takes a list of BRUSHES, performs clipping, merging and returns LINESEGs
  ready for binary space partitioning."
  (let (segs)
    (dolist (b1 brushes)
      (dolist (b2 brushes)
        (unless (eq b1 b2)
          (let ((outside-segs (brush-clip b1 b2)))
            (setf segs (nconc segs outside-segs))))))
    segs))
