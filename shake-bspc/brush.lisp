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

(defstruct (brush (:constructor make-brush-raw))
  "A convex polygon defining a geometry with contents.
  LINES is a list of LINEDEFs defining the polygon.
  CONTENTS can be one of:
  * :CONTENTS-EMPTY
  * :CONTENTS-SOLID"
  lines
  (contents :contents-solid))

(define-condition non-convex-brush-error (error)
  ((lines :initarg :lines :reader lines)))

(defun make-brush (&key lines (contents :contents-solid))
  (if (sbsp::convex-hull-p (mapcar #'linedef->lineseg lines))
      (make-brush-raw :lines lines :contents contents)
      (error 'non-convex-brush-error :lines lines)))

(defun write-brush (brush stream)
  (with-struct (brush- lines contents) brush
    (format stream "~@{~S~%~}"
            :brush
            contents
            (length lines))
    (dolist (line lines)
      (sbsp::write-linedef line stream))))

(defun read-brush (stream)
  (let ((name (read stream))
        (contents (read stream))
        (lines-count (read stream)))
    (declare (ignore name))
    (make-brush :contents contents
                :lines (repeat lines-count (sbsp::read-linedef stream)))))

(defun bounds-of-linedefs (lines)
  (let ((mins (copy-seq (linedef-start (car lines))))
        (maxs (copy-seq (linedef-start (car lines)))))
    (doproduct ((line lines)
                (point (list (linedef-start line) (linedef-end line)))
                (cons mins maxs))
      (dotimes (i (length mins))
        (let ((x (aref point i))
              (max-x (aref maxs i))
              (min-x (aref mins i)))
          (setf (aref maxs i) (max x max-x))
          (setf (aref mins i) (min x min-x)))))))

(defun brush-center (brush)
  (destructuring-bind (mins . maxs) (bounds-of-linedefs (brush-lines brush))
    (vscale 0.5d0 (v+ mins maxs))))

(defun brush-rotate (brush rad-angle)
  "Rotate the given BRUSH for RAD-ANGLE around the center."
  (let ((rot (rotation (v 0 0 1) rad-angle))
        (center (brush-center brush)))
    (labels ((rotate-point (point)
               (let* ((translated (v- point center))
                      (rotated (vtransform
                                rot (v (vx translated) (vy translated) 0 1))))
                 (v+ (v (vx rotated) (vy rotated)) center)))
             (rotate-line (line)
               (make-linedef :start (rotate-point (linedef-start line))
                             :end (rotate-point (linedef-end line))
                             :color (sbsp::linedef-color line))))
      (make-brush :lines (mapcar #'rotate-line (brush-lines brush))
                  :contents (brush-contents brush)))))

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
