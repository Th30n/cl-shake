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

(defun lineseg-normal (lineseg)
  (linedef-normal (lineseg-orig-line lineseg)))

(defstruct leaf
  "A leaf node in the BSP tree. The SEGS slot stores the geometry as a LIST of
  LINESEGs. The CONTENTS stores the type of the leaf, used for collision
  detection. It can be one of:
    * :CONTENTS-EMPTY -- free space
    * :CONTENTS-SOLID -- solid, impassable space"
  (segs nil :type list)
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
  (declare (type lineseg splitter lineseg))
  (let* ((n (lineseg-normal splitter))
         (-n (v- n))
         (l-vec (v- (lineseg-end lineseg) (lineseg-start lineseg)))
         (s-start (lineseg-start splitter))
         (l-start (lineseg-start lineseg))
         (sl-vec (v- l-start s-start))
         (numer (vdot n sl-vec))
         (denom (vdot -n l-vec)))
    (values numer denom)))

(defun split-lineseg (lineseg t-split &key (relative-t nil))
  "Split the given LINESEG at the given T-SPLIT parameter into a pair of
  LINESEG. Returns NIL if T-SPLIT does not split the line segment. When
  RELATIVE-T is not NIL, T-SPLIT parameter is treated relative to the
  segment."
  (declare (type lineseg lineseg) (type double-float t-split))
  (when relative-t
    (let ((t-diff (- (lineseg-t-end lineseg) (lineseg-t-start lineseg))))
      (setf t-split (+ (lineseg-t-start lineseg)
                       (* t-split t-diff)))))
  (when (< (lineseg-t-start lineseg) t-split (lineseg-t-end lineseg))
    (let ((l1 (copy-lineseg lineseg))
          (l2 (copy-lineseg lineseg)))
      (setf (lineseg-t-end l1) t-split)
      (setf (lineseg-t-start l2) t-split)
      (cons l1 l2))))

(defun determine-side (lineseg point)
  "Determine on which side of a LINESEG is the given POINT located.  Returns
  :BACK or :FRONT as the primary value and distance as the second."
  (let ((d (vdot (lineseg-normal lineseg)
                 (v- point (lineseg-start lineseg)))))
    (values (if (or (plusp d) (double-float-rel-eq d 0d0))
                :front
                :back)
            d)))

(defun convex-hull-p (linesegs)
  "Checks if the given list of LINESEG instances forms a convex hull."
  (block test
    (dolist (test-seg linesegs)
      (dolist (seg linesegs)
        (unless (eq seg test-seg)
          (when (eq :back (determine-side seg (lineseg-start test-seg)))
            (return-from test nil))
          (when (eq :back (determine-side seg (lineseg-end test-seg)))
            (return-from test nil)))))
    t))

(defun choose-splitter (linesegs splitters)
  (declare (type list linesegs splitters))
  (let (rest splitter-seg)
    (dolist (seg linesegs)
      (if (or splitter-seg (member (lineseg-orig-line seg) splitters
                                   :test #'equalp))
          (push seg rest)
          (setf splitter-seg seg)))
    (values splitter-seg rest)))

(defun split-partition (splitter-seg seg num den)
  (let ((splitted (split-lineseg seg (/ num den) :relative-t t)))
    (if (null splitted)
        ;; no split
        (progn
          (when (double-float-rel-eq num 0d0)
            ;; Points are collinear, use other end for numerator.
            (let ((n (lineseg-normal splitter-seg))
                  (sl-vec (v- (lineseg-end seg)
                              (lineseg-start splitter-seg))))
              (setf num (vdot n sl-vec))))
          (if (plusp num)
              (cons seg nil)
              (cons nil seg)))
        ;; split
        (cond
          ((plusp num)
           splitted)
          (t
           ;; reverse the split order
           (cons (cdr splitted) (car splitted)))))))

(defun partition-linesegs (splitter-seg linesegs)
  (declare (type lineseg splitter-seg) (type list linesegs))
  (let (front
        back
        (splitter (lineseg-orig-line splitter-seg)))
    (dolist (seg linesegs)
      (multiple-value-bind (num den) (line-intersect-ratio splitter-seg seg)
        (if (double-float-rel-eq den 0d0)
            ;; parallel lines
            (cond
              ((double-float-rel-eq num 0d0)
               ;; on the same line
               (if (v= (linedef-normal splitter)
                       (linedef-normal (lineseg-orig-line seg)))
                   ;; same facing
                   (push seg front)
                   ;; opposite facing
                   (push seg back)))
              ((plusp num)
               (push seg front))
              (t
               (push seg back)))
            ;; lines intersect
            (destructuring-bind (split-front . split-back)
                (split-partition splitter-seg seg num den)
              (when split-front
                (push split-front front))
              (when split-back
                (push split-back back))))))
    (values front back)))

(defun build-bsp (linesegs &optional (splitters nil))
  (declare (optimize (speed 0) (debug 3)))
  (if (null linesegs)
      (make-leaf :contents :contents-solid)
      (multiple-value-bind
            (splitter-seg rest) (choose-splitter linesegs splitters)
        (if (null splitter-seg)
            ;; We've selected all the segments and they form a convex hull.
            (progn
              (assert (convex-hull-p rest))
              (make-leaf :segs rest))
            ;; Split the remaining into front and back.
            (multiple-value-bind
                  (front back) (partition-linesegs splitter-seg rest)
              (let ((splitter (lineseg-orig-line splitter-seg)))
                (make-node :line splitter-seg
                           ;; Add the splitter itself to front.
                           :front (build-bsp (cons splitter-seg front)
                                             (cons splitter splitters))
                           :back (build-bsp back
                                            (cons splitter splitters)))))))))

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
    (build-bsp segs)))

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
    ((node-p bsp)
     ;; preorder traverse write
     (format stream "~S~%" :node)
     (write-lineseg (node-line bsp) stream)
     (write-bsp (node-front bsp) stream)
     (write-bsp (node-back bsp) stream))
    (t
     (format stream "~S~%" :leaf)
     (format stream "~S~%" (leaf-contents bsp))
     (format stream "~S~%" (list-length (leaf-segs bsp)))
     (dolist (seg (leaf-segs bsp))
       (write-lineseg seg stream)))))

(defun read-bsp (stream)
  (let ((node-type (read stream)))
    (ecase node-type
      (:node (let ((seg (read-lineseg stream))
                   (front (read-bsp stream))
                   (back (read-bsp stream)))
               (make-node :line seg :front front :back back)))
      (:leaf (let ((contents (read stream))
                   (num-segs (read stream))
                   segs)
               (loop repeat num-segs do
                    (push (read-lineseg stream) segs))
               (make-leaf :segs (reverse segs) :contents contents))))))

(defun compile-map-file (map-file bsp-file)
  "Compile a map from MAP-FILE and store it into BSP-FILE"
  (let ((bsp))
    (with-open-file (mf map-file)
      (setf bsp (read-and-compile-map mf)))
    (with-open-file (bf bsp-file :direction :output :if-exists :supersede
                        :if-does-not-exist :create)
      (write-bsp bsp bf))))

(defun back-to-front (point bsp)
  "Traverse the BSP in back to front order relative to given POINT."
  (if (leaf-p bsp)
      (leaf-segs bsp)
      (let ((seg (node-line bsp)))
        (ecase (determine-side seg point)
          (:front (append (back-to-front point (node-back bsp))
                          (back-to-front point (node-front bsp))))
          (:back (append (back-to-front point (node-front bsp))
                         (back-to-front point (node-back bsp))))))))
