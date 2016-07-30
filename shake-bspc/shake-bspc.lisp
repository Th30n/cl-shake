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
  (start (v 0 0) :type (vec 2) :read-only t)
  (end  (v 0 0) :type (vec 2) :read-only t)
  (color (v 1 0 1) :type (vec 3) :read-only t))

(defun linedef-vec (linedef)
  (v- (linedef-end linedef) (linedef-start linedef)))

(defun linedef-normal (linedef)
  (let ((vec (linedef-vec linedef)))
    (rotatef (vx vec) (vy vec))
    (setf (vy vec) (- (vy vec)))
    (vnormalize vec)))

(defun linedef= (a b)
  (declare (type linedef a))
  (or (eq a b)
      (and (linedef-p b)
           (v= (linedef-start a) (linedef-start b))
           (v= (linedef-end a) (linedef-end b))
           (v= (linedef-color a) (linedef-color b)))))

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

(defun lineseg-normal (lineseg)
  (linedef-normal (lineseg-orig-line lineseg)))

(defun make-linedef-loop (p1 p2 p3 &rest points)
  "Create a list of linedefs by making a loop through the given points."
  (let ((start-points (append (list p1 p2 p3) points))
        (end-points (append (list p2 p3) points (list p1))))
    (mapcar (lambda (start end) (make-linedef :start start :end end))
            start-points end-points)))

(defstruct sidedef
  "A side definition for a line segment."
  (lineseg nil :type lineseg)
  (color (v 1 0 1) :type (vec 3)))

(defun read-sidedef (stream)
  (let ((lineseg (read-lineseg stream))
        (color (v (read stream) (read stream) (read stream))))
    (make-sidedef :lineseg lineseg :color color)))

(defun write-sidedef (sidedef stream)
  (with-struct (sidedef- lineseg color) sidedef
    (write-lineseg lineseg stream)
    (format stream "~S ~S ~S~%" (vx color) (vy color) (vz color))))

(defstruct leaf
  "A leaf node in the BSP tree. The SEGS slot stores the geometry as a LIST of
  SIDEDEFs. The CONTENTS stores the type of the leaf, used for collision
  detection. It can be one of:
    * :CONTENTS-EMPTY -- free space
    * :CONTENTS-SOLID -- solid, impassable space"
  (surfaces nil :type list)
  (contents :contents-empty))

(defstruct node
  "A node in the BSP tree. The geometry is stored in leaves. LINE is the
  splitting LINESEG used at this node. Children are stored in FRONT and BACK
  slots."
  (line nil :type lineseg)
  (front nil :type (or node leaf))
  (back nil :type (or node leaf)))

(defun line-intersect-ratio (splitter lineseg)
  "Takes a SPLITTER and LINESEG linesegs. Returns numerator and denominator
  values for calculating the parameter T of the intersection."
  (declare (type linedef splitter) (type lineseg lineseg))
  (let* ((n (linedef-normal splitter))
         (-n (v- n))
         (l-vec (v- (lineseg-end lineseg) (lineseg-start lineseg)))
         (s-start (linedef-start splitter))
         (l-start (lineseg-start lineseg))
         (sl-vec (v- l-start s-start))
         (numer (vdot n sl-vec))
         (denom (vdot -n l-vec)))
    (values numer denom)))

(defun split-lineseg (lineseg t-split &key (relative-t t))
  "Split the given LINESEG at the given T-SPLIT parameter into a pair of
  LINESEG. Returns NIL if T-SPLIT does not split the line segment. When
  RELATIVE-T is not NIL, T-SPLIT parameter is treated relative to the
  segment."
  (declare (type lineseg lineseg) (type double-float t-split))
  (with-struct (lineseg- t-start t-end) lineseg
    (when relative-t
      (let ((t-diff (- t-end t-start)))
        (setf t-split (+ t-start (* t-split t-diff)))))
    (when (double> t-end t-split t-start)
      (let ((l1 (copy-lineseg lineseg))
            (l2 (copy-lineseg lineseg)))
        (setf (lineseg-t-end l1) t-split)
        (setf (lineseg-t-start l2) t-split)
        (cons l1 l2)))))

(defun determine-side (lineseg point)
  "Determine on which side of a LINESEG is the given POINT located. Returns
  :BACK, :FRONT or :ON-LINE as the primary value and distance as the second."
  (let ((d (vdot (lineseg-normal lineseg)
                 (v- point (lineseg-start lineseg)))))
    (values (cond
              ((double= d 0d0) :on-line)
              ((plusp d) :front)
              ((minusp d) :back))
            d)))

(defun convex-hull-p (linesegs)
  "Checks if the given list of LINESEG instances forms a convex hull."
  (block test
    (let ((picked-side :on-line))
      (doproduct ((test-seg linesegs) (seg linesegs))
        (unless (eq seg test-seg)
          (let ((p1-side (determine-side seg (lineseg-start test-seg)))
                (p2-side (determine-side seg (lineseg-end test-seg))))
            (when (eq picked-side :on-line)
              (cond
                ((not (eq p1-side :on-line)) (setf picked-side p1-side))
                ((not (eq p2-side :on-line)) (setf picked-side p2-side))))
            (dolist (side (list p1-side p2-side))
              (when (and (not (eq side :on-line)) (not (eq side picked-side)))
                (return-from test nil))))))
      picked-side)))

(defun choose-splitter (surfaces splitters)
  (declare (type list surfaces splitters))
  (let (rest splitter-surf)
    (dolist (surf surfaces)
      (if (or splitter-surf (member (lineseg-orig-line (sidedef-lineseg surf))
                                    splitters :test #'equalp))
          (push surf rest)
          (setf splitter-surf surf)))
    (values splitter-surf rest)))

(defun split-partition (splitter surface num den)
  (declare (type linedef splitter) (type sidedef surface)
           (type double-float num den))
  (with-struct (sidedef- lineseg) surface
    (let ((splitted (split-lineseg lineseg (/ num den))))
      (if (null splitted)
          ;; no split
          (progn
            (when (double= num 0d0)
              ;; Points are collinear, use other end for numerator.
              (let ((n (linedef-normal splitter))
                    (sl-vec (v- (lineseg-end lineseg) (linedef-start splitter))))
                (setf num (vdot n sl-vec))))
            (if (plusp num)
                (cons surface nil)
                (cons nil surface)))
          ;; split
          (flet ((make-split-surface (seg)
                   (let ((copy (copy-sidedef surface)))
                     (setf (sidedef-lineseg copy) seg)
                     copy)))
            (setf splitted (cons (make-split-surface (car splitted))
                                 (make-split-surface (cdr splitted))))
            (cond
              ((plusp num)
               splitted)
              (t
               ;; reverse the split order
               (cons (cdr splitted) (car splitted)))))))))

(defun partition-surfaces (splitter surfaces)
  (declare (type linedef splitter) (type list surfaces))
  (let (front
        back
        (on-splitter (list splitter)))
    (dolist (surf surfaces)
      (with-struct (sidedef- lineseg) surf
        (multiple-value-bind (num den) (line-intersect-ratio splitter lineseg)
          (if (double= den 0d0)
              ;; parallel lines
              (cond
                ((double= num 0d0)
                 ;; on the same line
                 (push (lineseg-orig-line lineseg) on-splitter)
                 (if (v= (linedef-normal splitter) (lineseg-normal lineseg))
                     ;; same facing go to the front
                     (push surf front)
                     ;; opposite facing go in the back
                     (push surf back)))
                ((plusp num)
                 (push surf front))
                (t
                 (push surf back)))
              ;; lines intersect
              (destructuring-bind (split-front . split-back)
                  (split-partition splitter surf num den)
                (when split-front
                  (push split-front front))
                (when split-back
                  (push split-back back)))))))
    (values front back on-splitter)))

(defun build-bsp (surfaces &optional (splitters nil))
  (if (null surfaces)
      (make-leaf :contents :contents-solid)
      (multiple-value-bind
            (splitter-surf rest) (choose-splitter surfaces splitters)
        (if (not splitter-surf)
            ;; We've selected all the segments and they form a convex hull.
            (progn
              (assert (convex-hull-p (mapcar #'sidedef-lineseg rest)))
              (make-leaf :surfaces rest))
            ;; Split the remaining into front and back.
            (let ((splitter (lineseg-orig-line (sidedef-lineseg splitter-surf))))
              (multiple-value-bind
                    (front back on-splitter) (partition-surfaces splitter rest)
                (let ((used-splitters (append on-splitter splitters)))
                  (make-node :line (sidedef-lineseg splitter-surf)
                             ;; Add the splitter itself to front.
                             :front (build-bsp (cons splitter-surf front)
                                               used-splitters)
                             :back (build-bsp back used-splitters)))))))))

(defun linedef->lineseg (linedef)
  (declare (type linedef linedef))
  (make-lineseg :orig-line linedef))

(defun write-linedef (linedef stream)
  (with-struct (linedef- start end) linedef
    (format stream "~S ~S ~S ~S~%"
            (vx start) (vy start) (vx end) (vy end))))

(defun read-linedef (stream)
  (let ((start (v (read stream) (read stream)))
        (end (v (read stream) (read stream))))
    (make-linedef :start start :end end)))

(defun write-lineseg (lineseg stream)
  (with-struct (lineseg- orig-line t-start t-end) lineseg
    (write-linedef orig-line stream)
    (format stream "~S ~S~%" t-start t-end)))

(defun read-lineseg (stream)
  (let ((linedef (read-linedef stream))
        (t-start (read stream))
        (t-end (read stream)))
    (make-lineseg :orig-line linedef :t-start t-start :t-end t-end)))

(defun write-bsp (bsp stream)
  (cond
    ((node-p bsp)
     ;; preorder traverse write
     (format stream "~S~%" :node)
     (with-struct (node- line front back) bsp
       (write-lineseg line stream)
       (write-bsp front stream)
       (write-bsp back stream)))
    (t
     (format stream "~S~%" :leaf)
     (with-struct (leaf- contents surfaces) bsp
       (format stream "~S~%" contents)
       (format stream "~S~%" (list-length surfaces))
       (dolist (surf surfaces)
         (write-sidedef surf stream))))))

(defun read-bsp (stream)
  (let ((node-type (read stream)))
    (ecase node-type
      (:node (let ((seg (read-lineseg stream))
                   (front (read-bsp stream))
                   (back (read-bsp stream)))
               (make-node :line seg :front front :back back)))
      (:leaf (let* ((contents (read stream))
                    (num-surfs (read stream))
                    (surfs (repeat num-surfs (read-sidedef stream))))
               (make-leaf :surfaces surfs :contents contents))))))

(defun back-to-front (point bsp)
  "Traverse the BSP in back to front order relative to given POINT."
  (if (leaf-p bsp)
      (leaf-surfaces bsp)
      (with-struct (node- line front back) bsp
        (ecase (determine-side line point)
          ((or :front :on-line) (append (back-to-front point back)
                                        (back-to-front point front)))
          (:back (append (back-to-front point front)
                         (back-to-front point back)))))))
