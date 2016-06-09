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

(defun prepare-brushes-for-bsp (brushes)
  "Takes a list of BRUSHES, performs clipping, merging and returns LINESEGs
  ready for binary space partitioning."
  (let (segs)
    (dolist (b1 brushes)
      (let ((outside (mapcar #'linedef->lineseg (brush-lines b1)))
            inside)
        (dolist (b2 brushes)
          (unless (eq b1 b2)
            (shiftf inside outside nil)
            ;; clip brush b1 against b2
            (dolist (split-line (brush-lines b2))
              (multiple-value-bind (new-outside new-inside)
                  (sbsp::partition-linesegs split-line inside)
                (unionf outside new-outside)
                (setf inside new-inside)))))
        (nconcf segs outside)))
    segs))
