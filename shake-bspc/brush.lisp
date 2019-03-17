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
    (vscale (shiva-float 0.5) (v+ mins maxs))))

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

(defun brush-consume-p (b1 b2)
  "Return T if BRUSH B2 can be merged into B1, i.e. B1 consumes B2. Note, the
relation is not symmetrical, because certain properties allow consuming in one
direction."
  (flet ((brush-sectors (brush)
           (mapcar #'sidedef-back-sector (brush-surfaces brush))))
    (let* ((s1 (first (brush-sectors b1)))
           (s2 (first (brush-sectors b2))))
      (or
       ;; Nil sector consumes all
       (not s1)
       ;; Consume lower sectors and (TODO) if s2 has same contents as s1
       (and s1 s2
            (float>= (sector-floor-height s1) (sector-floor-height s2))
            (float<= (sector-ceiling-height s1) (sector-ceiling-height s2)))))))

(defun surf-on-brush-p (surf brush)
  (check-type surf sidedef)
  (check-type brush brush)
  (dolist (split-surf (brush-surfaces brush))
    (let ((split-line (lineseg-orig-line (sidedef-lineseg split-surf))))
      (multiple-value-bind (num den)
          (sbsp::line-intersect-ratio split-line (sidedef-lineseg surf))
        (when (and (float= den (shiva-float 0.0))
                   (float= num (shiva-float 0.0))
                   (v= (linedef-normal split-line) (lineseg-normal (sidedef-lineseg surf))))
          (return-from surf-on-brush-p t))))))

(defun sidedef-merge-contents (surf brush)
  (check-type surf sidedef)
  (check-type brush brush)
  (let ((front-sector (sidedef-front-sector surf))
        (back-sector (sidedef-back-sector surf))
        (in-sector (sidedef-back-sector
                    (first (brush-surfaces brush)))))
    ;; Due to comparison ordering, we may set sector multiple times, so take
    ;; the one with the highest priorty.
    (if (surf-on-brush-p surf brush)
        ;; If we have same surf as BRUSH, then we only need to overwrite
        ;; back sector, because front sector is pointing *outside* of BRUSH.
        (setf front-sector (make-sector))
        (progn
          (setf (sidedef-front-sector surf)
                (sbsp::copy-sector (if front-sector front-sector in-sector)))
          (setf front-sector (sidedef-front-sector surf))))

    (if back-sector
        (progn
          (setf (sidedef-back-sector surf) (sbsp::copy-sector back-sector))
          (setf back-sector (sidedef-back-sector surf)))
        ;; Nil sector never gets overwritten
        ;; TODO: No nil sectors
        ;; TODO: Contents priority
        (setf back-sector (make-sector)))

    ;; Higher floor overrides lower
    (when (float> (sector-floor-height in-sector) (sector-floor-height front-sector))
      (setf (sector-floor-height front-sector) (sector-floor-height in-sector))
      (setf (sector-floor-texinfo front-sector) (sector-floor-texinfo in-sector)))
    (when (float> (sector-floor-height in-sector) (sector-floor-height back-sector))
      (setf (sector-floor-height back-sector) (sector-floor-height in-sector))
      (setf (sector-floor-texinfo back-sector) (sector-floor-texinfo in-sector)))
    ;; Lower ceiling overrides higher
    (when (float< (sector-ceiling-height in-sector) (sector-ceiling-height front-sector))
      (setf (sector-ceiling-height front-sector) (sector-ceiling-height in-sector))
      (setf (sector-ceiling-texinfo front-sector) (sector-ceiling-texinfo in-sector)))
    (when (float< (sector-ceiling-height in-sector) (sector-ceiling-height back-sector))
      (setf (sector-ceiling-height back-sector) (sector-ceiling-height in-sector))
      (setf (sector-ceiling-texinfo back-sector) (sector-ceiling-texinfo in-sector)))
    ;; TODO: Ambient light?
    )
  surf)

;; TODO: Test this function *thoroughly*!
(defun prepare-brushes-for-bsp (brushes)
  "Takes a list of BRUSHES, performs clipping, merging and returns SIDEDEFs
  ready for binary space partitioning."
  (let (surfs)
    (dolist (b1 brushes)
      (let ((outside (brush-surfaces b1))
            inside)
        (dolist (b2 brushes)
          (unless (eq b1 b2)
            (shiftf inside outside nil)
            ;; Clip brush b1 against b2.
            (dolist (split-surf (brush-surfaces b2))
              (let ((split-line (lineseg-orig-line (sidedef-lineseg split-surf))))
                (multiple-value-bind (new-outside new-inside)
                    ;; TODO: This kills shared surfaces when consuming, it
                    ;; shouldn't happen always :/
                    (sbsp:partition-surfaces split-line inside :same-to-back-p t)
                  (unionf outside new-outside :test #'equalp)
                  (setf inside new-inside))))
            ;; Keep the inside surfaces if we can't continue in another brush.
            (if (not (brush-consume-p b2 b1))
                (unionf outside (mapcar (lambda (surf)
                                          (sidedef-merge-contents surf b2))
                                        inside)
                        :test #'equalp)
                ;; Restore killed shared surfaces, but overwrite our contents
                ;; with those of b2, because we are consumed by it.
                (unionf outside
                        (loop for surf in inside
                           when (surf-on-brush-p surf b2)
                           collect (if (sidedef-back-sector
                                        (first (brush-surfaces b2)))
                                       (prog1 (sidedef-merge-contents surf b2)
                                         (assert (equalp (sidedef-back-sector surf)
                                                         (sidedef-back-sector (first (brush-surfaces b2))))))
                                       (prog1 surf
                                         (setf (sidedef-back-sector surf) nil))))
                        :test #'equalp))))
        (appendf surfs outside)))
    surfs))

(defun construct-convex-hull (points)
  "Create a convex hull of given POINTS. Gift wrapping algorithm is used,
  which is O(nh) where n is the number of points and h the resulting convex
  hull points. Points are returned in counter clockwise order."
  (let ((min-point (reduce (lambda (a b)
                             (if (float= (vx a) (vx b))
                                 (if (< (vy a) (vy b)) a b)
                                 (if (< (vx a) (vx b)) a b))) points)))
    (flet ((in-front-p (line point)
             (let ((side (determine-side line point)))
               (if (eq :on-line side)
                   (with-struct (linedef- start end) line
                     (float> (vdistsq start point) (vdistsq start end)))
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
  (let* ((half-size (* 0.5 (shiva-float square)))
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
