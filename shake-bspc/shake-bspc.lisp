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

(in-package #:shake-bspc)

(defstruct linedef
  (start (v 0 0) :type (simple-array double-float (2)) :read-only t)
  (end  (v 0 0) :type (simple-array double-float (2)) :read-only t)
  (color (v 1 0 1) :type (simple-array double-float (3)) :read-only t))

(defun linedef-vec (linedef)
  (v- (linedef-end linedef) (linedef-start linedef)))

(defun linedef-normal (linedef)
  (let ((vec (linedef-vec linedef)))
    (rotatef (aref vec 0) (aref vec 1))
    (setf (aref vec 1) (- (aref vec 1)))
    (vnormalize vec)))

(defstruct lineseg
  "Segment of a line. Each line starts as a full segment, but may be split
  into multiple segments."
  (orig-line (make-linedef) :type linedef)
  (t-start 0d0 :type double-float)
  (t-end 1d0 :type double-float))

(defun lineseg-start (lineseg)
  (let* ((line (lineseg-orig-line lineseg))
         (l-vec (linedef-vec line)))
    (v+ (linedef-start line) (vscale (lineseg-t-start lineseg) l-vec))))

(defun lineseg-end (lineseg)
  (let* ((line (lineseg-orig-line lineseg))
         (l-vec (linedef-vec line)))
    (v+ (linedef-start line) (vscale (lineseg-t-end lineseg) l-vec))))

(defstruct node
  (segs nil :type list)
  (front nil :type (or node null))
  (back nil :type (or node null)))

(defun line-intersect-ratio (splitter lineseg)
  "Takes a SPLITTER and LINESEG linesegs. Returns numerator and denominator
  values for calculating the parameter T of the intersection."
  (declare (type lineseg splitter lineseg))
  (let* ((n (linedef-normal (lineseg-orig-line splitter)))
         (-n (v- (v 0 0) n))
         (l-vec (linedef-vec (lineseg-orig-line lineseg)))
         (s-start (lineseg-start splitter))
         (l-start (lineseg-start lineseg))
         (sl-vec (v- l-start s-start))
         (numer (vdot n sl-vec))
         (denom (vdot -n l-vec)))
    (values numer denom)))

(defun split-lineseg (lineseg t-split)
  "Split the given LINESEG at the given T-SPLIT parameter into a pair
  of LINESEG. Returns NIL if T-SPLIT does not split the line segment."
  (declare (type lineseg lineseg) (type double-float t-split))
  (when (< (lineseg-t-start lineseg) t-split (lineseg-t-end lineseg))
    (let ((l1 (copy-lineseg lineseg))
          (l2 (copy-lineseg lineseg)))
      (setf (lineseg-t-end l1) t-split)
      (setf (lineseg-t-start l2) t-split)
      (cons l1 l2))))

(defun build-bsp (rootseg linesegs)
  (declare (type lineseg rootseg) (type list linesegs))
  (if (null linesegs)
      (make-node :segs (list rootseg))
      (let ((splitter (lineseg-orig-line rootseg))
            (front nil)
            (back nil))
        (dolist (seg linesegs)
          (multiple-value-bind (num den) (line-intersect-ratio rootseg seg)
            (if (double-float-rel-eq den 0d0)
                ;; parallel lines
                (if (< num 0d0)
                    (push seg front)
                    (push seg back))
                ;; lines intersect
                (let ((splitted (split-lineseg seg (/ num den))))
                  (if (null splitted)
                      ;; no split
                      (progn
                        (when (double-float-rel-eq num 0d0)
                          ;; Points are collinear, use other end for numerator.
                          (let ((n (linedef-normal splitter))
                                (sl-vec (v- (lineseg-end seg)
                                            (lineseg-start rootseg))))
                            (setf num (vdot n sl-vec))))
                        (if (< num 0d0)
                            (push seg front)
                            (push seg back)))
                      ;; split
                      (cond
                        ((< num 0d0)
                         (push (car splitted) front)
                         (push (cdr splitted) back))
                        (t
                         (push (car splitted) back)
                         (push (cdr splitted) front))))))))
        (make-node
         :segs (list rootseg)
         :front (if (null front) nil (build-bsp (car front) (cdr front)))
         :back (if (null back) nil (build-bsp (car back) (cdr back)))))))

(defun linedef->lineseg (linedef)
  (declare (type linedef linedef))
  (make-lineseg :orig-line linedef))

(defun read-map-linedef (stream)
  (destructuring-bind (x1 y1 x2 y2) (read stream)
    (let ((color (v (read stream) (read stream) (read stream))))
      (make-linedef :start (v x1 y1) :end (v x2 y2) :color color))))

(defun read-map (stream)
  (let ((n (read stream)))
    (loop repeat n collecting (read-map-linedef stream))))

(defun read-and-compile-map (stream)
  (let* ((map (read-map stream))
         (segs (mapcar #'linedef->lineseg map)))
    (build-bsp (car segs) (cdr segs))))

(defun write-linedef (linedef stream)
  (let ((start (linedef-start linedef))
        (end (linedef-end linedef))
        (color (linedef-color linedef)))
    (format stream "~S ~S ~S ~S~%~S ~S ~S~%"
            (vx start) (vy start) (vx end) (vy end)
            (vx color) (vy color) (vz color))))

(defun read-linedef (stream)
  (let ((start (v (read stream) (read stream)))
        (end (v (read stream) (read stream)))
        (color (v (read stream) (read stream) (read stream))))
    (make-linedef :start start :end end :color color)))

(defun write-lineseg (lineseg stream)
  (let ((linedef (lineseg-orig-line lineseg))
        (t-start (lineseg-t-start lineseg))
        (t-end (lineseg-t-end lineseg)))
    (write-linedef linedef stream)
    (format stream "~S ~S~%" t-start t-end)))

(defun read-lineseg (stream)
  (let ((linedef (read-linedef stream))
        (t-start (read stream))
        (t-end (read stream)))
    (make-lineseg :orig-line linedef :t-start t-start :t-end t-end)))

(defun write-bsp (bsp stream)
  (cond
    ((null bsp) (format stream "~S~%" bsp))
    (t
     ;; preorder traverse write
     (format stream "~S~%" :lineseg)
     (write-lineseg (car (node-segs bsp)) stream)
     (write-bsp (node-front bsp) stream)
     (write-bsp (node-back bsp) stream))))

(defun read-bsp (stream)
  (let ((node-type (read stream)))
    (ecase node-type
      (:lineseg (let ((root (read-lineseg stream))
                      (front (read-bsp stream))
                      (back (read-bsp stream)))
                  (make-node :segs (list root) :front front :back back)))
      ((nil) nil))))

(defun compile-map-file (map-file bsp-file)
  "Compile a map from MAP-FILE and store it into BSP-FILE"
  (let ((bsp))
    (with-open-file (mf map-file)
      (setf bsp (read-and-compile-map mf)))
    (with-open-file (bf bsp-file :direction :output :if-exists :supersede
                        :if-does-not-exist :create)
      (write-bsp bsp bf))))

(defun determine-side (point lineseg)
  "Determine on which side of a LINESEG is the given POINT located.
Returns BACK or FRONT."
  (let* ((normal (linedef-normal (lineseg-orig-line lineseg))))
    (if (>= 0 (- (vdot normal point)
                 (vdot normal (lineseg-start lineseg))))
        'front
        'back)))

(defun back-to-front (point bsp)
  "Traverse the BSP in back to front order relative to given POINT."
  (unless (null bsp)
    (let ((seg (car (node-segs bsp))))
      (ecase (determine-side point seg)
        (front (append (back-to-front point (node-back bsp))
                       (cons seg (back-to-front point (node-front bsp)))))
        (back (append (back-to-front point (node-front bsp))
                      (cons seg (back-to-front point (node-back bsp)))))))))
