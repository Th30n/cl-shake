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
  SURFACES is a list of SIDEDEFs defining the polygon.
  CONTENTS can be one of:
  * :CONTENTS-EMPTY
  * :CONTENTS-SOLID"
  surfaces
  (contents :contents-solid))

(defun brush-lines (brush)
  (mapcar (compose #'lineseg-orig-line #'sidedef-lineseg)
          (brush-surfaces brush)))

(define-condition non-convex-brush-error (error)
  ((lines :initarg :lines :reader lines)))

(defun make-brush (&key lines (contents :contents-solid))
  (if-let (side (sbsp:convex-hull-p (mapcar #'linedef->lineseg lines)))
    (flet ((flip-line (line)
             (with-struct (linedef- start end) line
               (make-linedef :start end :end start
                             :color (sbsp::linedef-color line))))
           (linedef->sidedef (line)
             (make-sidedef :lineseg (linedef->lineseg line)
                           :color (sbsp::linedef-color line))))
      (make-brush-raw :surfaces
                      (mapcar #'linedef->sidedef
                              (if (eq side :back)
                                  lines
                                  ;; Flip the order so that normals point outwards.
                                  (mapcar #'flip-line (reverse lines))))
                      :contents contents))
    (error 'non-convex-brush-error :lines lines)))

(defun write-brush (brush stream)
  (with-struct (brush- surfaces contents) brush
    (format stream "~@{~S~%~}"
            :brush
            contents
            (length surfaces))
    (dolist (surf surfaces)
      (sbsp:write-sidedef surf stream))))

(defun read-brush (stream)
  (let ((name (read stream))
        (contents (read stream))
        (surf-count (read stream)))
    (declare (ignore name))
    (make-brush-raw :contents contents
                    :surfaces (repeat surf-count (sbsp:read-sidedef stream)))))

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

(defun brush-bounds (brush)
  (bounds-of-linedefs (brush-lines brush)))

(defun brush-center (brush)
  (destructuring-bind (mins . maxs) (brush-bounds brush)
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
                             :color (sbsp::linedef-color line)))
             (rotate-surf (surf)
               (let ((line (lineseg-orig-line (sidedef-lineseg surf))))
                 (make-sidedef :lineseg (linedef->lineseg (rotate-line line))
                               :color (sidedef-color surf)))))
      (make-brush-raw :surfaces (mapcar #'rotate-surf (brush-surfaces brush))
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
  "Takes a list of BRUSHES, performs clipping, merging and returns SIDEDEFs
  ready for binary space partitioning."
  (let (segs)
    (dolist (b1 brushes)
      (let ((outside (brush-surfaces b1))
            inside)
        (dolist (b2 brushes)
          (unless (eq b1 b2)
            (shiftf inside outside nil)
            ;; clip brush b1 against b2
            (dolist (split-surf (brush-surfaces b2))
              (let ((split-line (lineseg-orig-line (sidedef-lineseg split-surf))))
                (multiple-value-bind (new-outside new-inside)
                    (sbsp:partition-surfaces split-line inside)
                  (unionf outside new-outside :test #'equalp)
                  (setf inside new-inside))))))
        (nconcf segs outside)))
    segs))

(defun construct-convex-hull (points)
  "Create a convex hull of given POINTS. Gift wrapping algorithm is used,
  which is O(nh) where n is the number of points and h the resulting convex
  hull points."
  (let ((min-point (reduce (lambda (a b)
                             (if (double= (vx a) (vx b))
                                 (if (< (vy a) (vy b)) a b)
                                 (if (< (vx a) (vx b)) a b))) points)))
    (flet ((in-front-p (line point)
             (let ((side (determine-side (linedef->lineseg line) point)))
               (if (eq :on-line side)
                   (with-struct (linedef- start end) line
                     (double> (vdistsq start point) (vdistsq start end)))
                   (eq :front side)))))
      (loop with hull-point = min-point and endpoint = min-point collect
           (progn
             (dolist (p points)
               (when (or (v= endpoint hull-point)
                         (in-front-p (make-linedef :start hull-point :end endpoint)
                                     p))
                 (setf endpoint p)))
             (setf hull-point endpoint))
         until (v= endpoint min-point)))))

(defun expand-brush (brush &key square)
  "Expand the given BRUSH by the given size of the SQUARE. This will perform a
  Minkowski sum of the brush polygon and the square polygon."
  (let* ((half-size (* 0.5d0 (coerce square 'double-float)))
         (hull-size (list half-size (- half-size))))
    (flet ((add-square (point)
             (cons point (map-product (lambda (x y) (v+ point (v x y)))
                                      hull-size hull-size)))
           (brush-from-points (points)
             (make-brush :lines (apply #'make-linedef-loop points)
                         :contents (brush-contents brush))))
      (let ((hull-points (mapcan (compose #'add-square #'linedef-start)
                                 (brush-lines brush))))
        (brush-from-points (construct-convex-hull hull-points))))))
