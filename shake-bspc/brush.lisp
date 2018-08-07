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

(defun make-brush (&key surfaces (contents :contents-solid))
  (when surfaces
    (let ((sector (sidedef-back-sector (car surfaces)))
          (back-sectors (mapcar #'sidedef-back-sector (cdr surfaces))))
      (assert (every (curry #'equalp sector) back-sectors))))
  (if-let (side (sbsp:convex-hull-p (mapcar #'sidedef-lineseg surfaces)))
    (flet ((flip-line (surf)
             (zap (lambda (line)
                    (with-struct (linedef- start end) line
                      (make-linedef :start end :end start)))
                  (lineseg-orig-line (sidedef-lineseg surf)))
             surf))
      (make-brush-raw :surfaces
                      (if (eq side :back)
                          surfaces
                          ;; Flip the order so that normals point outwards.
                          (mapcar #'flip-line (reverse surfaces)))
                      :contents contents))
    (error 'non-convex-brush-error
           :lines (mapcar (compose #'lineseg-orig-line #'sidedef-lineseg)
                          surfaces))))

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
    (make-brush :contents contents
                :surfaces (repeat surf-count (sbsp:read-sidedef stream)))))

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
                             :end (rotate-point (linedef-end line))))
             (rotate-surf (surf)
               (let ((line (lineseg-orig-line (sidedef-lineseg surf)))
                     (new-side (copy-sidedef surf)))
                 (setf (sidedef-lineseg new-side)
                       (linedef->lineseg (rotate-line line)))
                 new-side)))
      (make-brush :surfaces (mapcar #'rotate-surf (brush-surfaces brush))
                  :contents (brush-contents brush)))))

(defun brush-translate (brush vector)
  "Translate the given BRUSH along the given VECTOR."
  (labels ((translate-line (line)
             (make-linedef :start (v+ (linedef-start line) vector)
                           :end (v+ (linedef-end line) vector)))
           (translate-surf (surf)
             (let ((line (lineseg-orig-line (sidedef-lineseg surf)))
                   (new-side (copy-sidedef surf)))
               (setf (sidedef-lineseg new-side)
                     (linedef->lineseg (translate-line line)))
               new-side)))
    (make-brush :surfaces (mapcar #'translate-surf (brush-surfaces brush))
                :contents (brush-contents brush))))

(defun brush-lower-p (b1 b2)
  "Return BRUSH B1 if its floor height is lower of B2. If brush B2 has no
  floor, it is always higher than B1 unless B1 also has no floor."
  (flet ((brush-sectors (brush)
           (mapcar #'sidedef-back-sector (brush-surfaces brush))))
    (let* ((sectors1 (brush-sectors b1))
           (s1 (first sectors1))
           (sectors2 (brush-sectors b2))
           (s2 (first sectors2)))
      (dolist (sectors (list sectors1 sectors2))
        (assert (every (curry #'equalp (car sectors)) (cdr sectors))))
      (when (or (and (not s2) s1)
                (and s1 s2 (double> (sector-floor-height s2)
                                    (sector-floor-height s1))))
        b1))))

(defun brush-consume-p (b1 b2)
  "Return T if BRUSH B2 can be merged into B1, i.e. B1 consumes B2. Note, the
relation is not symmetrical, because certain properties allow consuming in one
direction."
  (flet ((brush-sectors (brush)
           (mapcar #'sidedef-back-sector (brush-surfaces brush))))
    (let* ((s1 (first (brush-sectors b1)))
           (s2 (first (brush-sectors b2))))
      ;;(when (and s1 s2) (break))
      (or
       ;; Nil sector consumes all
       (not s1)
       ;; Consume all lower sectors
       (brush-lower-p b2 b1)
       ;; Consume if s2 has same contents as s1
       (and s1 s2
            (double= (sector-ceiling-height s1) (sector-ceiling-height s2))
            (double= (sector-floor-height s1) (sector-floor-height s2)))))))

(defun prepare-brushes-for-bsp (brushes)
  "Takes a list of BRUSHES, performs clipping, merging and returns SIDEDEFs
  ready for binary space partitioning."
  (let (surfs)
    (dolist (b1 brushes)
      (let ((outside (brush-surfaces b1))
            inside)
        (dolist (b2 brushes)
          (unless (eq b1 b2)
              ;; (or (eq b1 b2)
              ;;     (brush-lower-p b2 b1))
            (shiftf inside outside nil)
            ;; Clip brush b1 against b2.
            (dolist (split-surf (brush-surfaces b2))
              (let ((split-line (lineseg-orig-line (sidedef-lineseg split-surf))))
                (multiple-value-bind (new-outside new-inside)
                    (sbsp:partition-surfaces split-line inside)
                  (unionf outside new-outside :test #'equalp)
                  (setf inside new-inside))))
            ;; Keep the inside surfaces if we can't continue in another brush.
            (when (not (brush-consume-p b2 b1))
              (unionf outside
                      (mapcar (lambda (surf)
                                (let ((front-sector (sidedef-front-sector surf))
                                      (back-sector (sidedef-back-sector
                                                    (first (brush-surfaces b2)))))
                                  ;; Due to comparison ordering, we may set
                                  ;; front-sector multiple times, so take the
                                  ;; one with the highest priorty.
                                  (when (or (not front-sector)
                                            (double> (sector-floor-height
                                                      back-sector)
                                                     (sector-floor-height
                                                      front-sector)))
                                    (setf (sidedef-front-sector surf)
                                          back-sector)))
                                surf)
                              inside)
                      :test #'equalp))))
        (appendf surfs outside)))
    surfs))

(defun construct-convex-hull (points)
  "Create a convex hull of given POINTS. Gift wrapping algorithm is used,
  which is O(nh) where n is the number of points and h the resulting convex
  hull points. Points are returned in counter clockwise order."
  (let ((min-point (reduce (lambda (a b)
                             (if (double= (vx a) (vx b))
                                 (if (< (vy a) (vy b)) a b)
                                 (if (< (vx a) (vx b)) a b))) points)))
    (flet ((in-front-p (line point)
             (let ((side (determine-side line point)))
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
         (hull-size (list half-size (- half-size)))
         (back-sector (sidedef-back-sector (first (brush-surfaces brush)))))
    (labels ((add-square (point)
               (cons point (map-product (lambda (x y) (v+ point (v x y)))
                                        hull-size hull-size)))
             (set-back-sectors (surfaces)
               (dolist (surf surfaces surfaces)
                 (setf (sidedef-back-sector surf) back-sector)))
             (brush-from-points (points)
               (make-brush :surfaces (set-back-sectors
                                      (mapcar #'linedef->sidedef
                                              (apply #'make-linedef-loop points)))
                           :contents (brush-contents brush))))
      (let ((hull-points (mappend (compose #'add-square #'linedef-start)
                                  (brush-lines brush))))
        (brush-from-points (construct-convex-hull hull-points))))))
