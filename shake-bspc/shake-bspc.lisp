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
  (end (v 0 0) :type (vec 2) :read-only t))

(defun linedef-vec (linedef)
  (v- (linedef-end linedef) (linedef-start linedef)))

(defun linedef-normal (linedef)
  (let ((vec (linedef-vec linedef)))
    (rotatef (vx vec) (vy vec))
    (zap #'- (vy vec))
    (vnormalize vec)))

(defun linedef= (a b)
  (declare (type linedef a))
  (or (eq a b)
      (and (linedef-p b)
           (v= (linedef-start a) (linedef-start b))
           (v= (linedef-end a) (linedef-end b)))))

(defun bounds-of-points (points)
  (let ((mins (copy-seq (first points)))
        (maxs (copy-seq (first points))))
    (dolist (point points (cons mins maxs))
      (dotimes (i (length mins))
        (let ((x (aref point i)))
          (maxf (aref maxs i) x)
          (minf (aref mins i) x))))))

(defun bounds-of-linedefs (lines)
  (bounds-of-points (mapcan (lambda (line)
                              (list (linedef-start line) (linedef-end line)))
                            lines)))

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

(defstruct texinfo
  "Details about the texture used on a line surface.
  OFFSET is a 2D vector of offsets applied to ST coordinates.
  NAME is a file name of the texture.
  DRAW-MODE is one of :TILE or :SCALE-TO-FIT."
  (offset (v 0 0) :type (vec 2))
  (name nil :type string)
  (draw-mode :tile))

(defun write-vec (vec stream)
  (format stream "(v")
  (dotimes (i (length vec))
    (format stream " ~S" (aref vec i)))
  (format stream ")"))

(defun read-vec-form (form)
  (destructuring-bind (type-name . args) form
    (declare (ignore type-name))
    (apply #'v args)))

(defun read-vec (stream)
  (read-vec-form (read stream)))

(defun write-texinfo (texinfo stream)
  (with-struct (texinfo- offset name draw-mode) texinfo
    (format stream "(texinfo ")
    (write-vec offset stream)
    (format stream " ~S ~S)" name draw-mode)))

(defun read-texinfo-form (form)
  (destructuring-bind (type-name . args) form
    (declare (ignore type-name))
    (destructuring-bind (offset name draw-mode) args
      (make-texinfo :offset (read-vec-form offset)
                    :name name :draw-mode draw-mode))))

(defun read-texinfo (stream)
  (read-texinfo-form (read stream)))

(defstruct sector
  "A sector surrounded by lines. Stores information about floor and ceiling."
  (lines nil :type list)
  (floor-height 0d0 :type double-float)
  (floor-texinfo nil :type (or null texinfo))
  (ceiling-height 1d0 :type double-float)
  (ceiling-texinfo nil :type (or null texinfo))
  (ambient-light (v 0 0 0) :type (vec 3)))

(defun write-sector (sector stream)
  (with-struct (sector- floor-height floor-texinfo ceiling-height
                        ceiling-texinfo ambient-light) sector
    (format stream "(sector ~S " floor-height)
    (if floor-texinfo
        (write-texinfo floor-texinfo stream)
        (prin1 floor-texinfo stream))
    (format stream " ~S " ceiling-height)
    (if ceiling-texinfo
        (write-texinfo ceiling-texinfo stream)
        (prin1 ceiling-texinfo stream))
    (format stream " ")
    (write-vec ambient-light stream)
    (format stream ")")))

(defun read-sector-form (form)
  (destructuring-bind (type-name . args) form
    (declare (ignore type-name))
    (destructuring-bind (floor-height floor-texinfo
                                      ceiling-height ceiling-texinfo
                                      ambient-light) args
      (make-sector :floor-height floor-height
                   :floor-texinfo (when floor-texinfo
                                    (read-texinfo-form floor-texinfo))
                   :ceiling-height ceiling-height
                   :ceiling-texinfo (when ceiling-texinfo
                                      (read-texinfo-form ceiling-texinfo))
                   :ambient-light (read-vec-form ambient-light)))))

(defun read-sector (stream)
  (read-sector-form (read stream)))

(defstruct subsector
  "A sub sector generated via BSP. Stores lines which surround it."
  (lines nil :type list))

(defstruct sidedef
  "A side definition for a line segment."
  (lineseg nil :type lineseg)
  (front-sector nil :type (or null sector))
  (back-sector nil :type (or null sector))
  (color (v 1 0 1) :type (vec 3))
  (texinfo nil))

(defun read-sidedef (stream)
  (let ((lineseg (read-lineseg stream))
        (color (v (read stream) (read stream) (read stream)))
        (texinfo (read stream)))
    (make-sidedef :lineseg lineseg
                  :color color
                  :texinfo (if (or (eq :caulk texinfo) (null texinfo))
                               texinfo
                               (read-texinfo-form texinfo)))))

(defun write-sidedef (sidedef stream)
  (with-struct (sidedef- lineseg color texinfo) sidedef
    (write-lineseg lineseg stream)
    (format stream "~S ~S ~S~%" (vx color) (vy color) (vz color))
    (cond
      ((or (eq :caulk texinfo) (null texinfo)) (format stream "~S~%" texinfo))
      (t (write-texinfo texinfo stream)
         (format stream "~%")))))

(defun linedef->sidedef (line)
  (make-sidedef :lineseg (linedef->lineseg line)))

(defstruct leaf
  "A leaf node in the BSP tree. The SEGS slot stores the geometry as a LIST of
  SIDEDEFs. The CONTENTS stores the type of the leaf, used for collision
  detection. It can be one of:
    * :CONTENTS-EMPTY -- free space
    * :CONTENTS-SOLID -- solid, impassable space"
  (bounds nil :type (cons (vec 2) (vec 2)))
  (surfaces nil :type list)
  (contents :contents-empty))

(defstruct node
  "A node in the BSP tree. The geometry is stored in leaves. LINE is the
  splitting LINEDEF used at this node. Children are stored in FRONT and BACK
  slots."
  (bounds nil :type (cons (vec 2) (vec 2)))
  (line nil :type linedef)
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

(defun dist-line-point (line point)
  "Return the distance between the given LINE and POINT."
  (declare (type linedef line) (type (vec 2) point))
  (vdot (linedef-normal line)
        (v- point (linedef-start line))))

(defun determine-side (line point)
  "Determine on which side of a LINE is the given POINT located. Returns
  :BACK, :FRONT or :ON-LINE as the primary value and distance as the second."
  (let ((d (dist-line-point line point)))
    (values (cond
              ((double= d 0d0) :on-line)
              ((plusp d) :front)
              ((minusp d) :back))
            d)))

(defun point-in-hull-p (point linesegs)
  "Returns true if the point is no the inner side of given linesegs which form
  a convex hull."
  (flet ((point-inside-p (seg)
           (let ((side (determine-side (lineseg-orig-line seg) point)))
             (or (eq side :back) (eq side :on-line)))))
    (every #'point-inside-p linesegs)))

(defun convex-hull-p (linesegs)
  "Checks if the given list of LINESEG instances forms a convex hull."
  (block test
    (let ((picked-side :on-line))
      (doproduct ((test-seg linesegs) (seg linesegs))
        (unless (eq seg test-seg)
          (with-struct (lineseg- orig-line) seg
            (let ((p1-side (determine-side orig-line (lineseg-start test-seg)))
                  (p2-side (determine-side orig-line (lineseg-end test-seg))))
                  (when (eq picked-side :on-line)
                    (cond
                      ((not (eq p1-side :on-line)) (setf picked-side p1-side))
                      ((not (eq p2-side :on-line)) (setf picked-side p2-side))))
                  (dolist (side (list p1-side p2-side))
                    (when (and (not (eq side :on-line)) (not (eq side picked-side)))
                      (return-from test nil)))))))
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
            (zap #'make-split-surface (car splitted))
            (zap #'make-split-surface (cdr splitted))
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

(defun bounds-of-surfaces (surfaces)
  (bounds-of-linedefs (mapcar (compose #'lineseg-orig-line #'sidedef-lineseg)
                              surfaces)))

(defun divide-bounds (bounds split-line)
  (flet ((intersect-line (line)
           (let ((lineseg (make-lineseg :orig-line line)))
             (multiple-value-bind (num den)
                 (line-intersect-ratio split-line lineseg)
               (unless (double= den 0d0)
                 (let ((t-split (/ num den)))
                   (cond
                     ((double= t-split 0d0) (linedef-start line))
                     ((double= t-split 1d0) (linedef-end line))
                     ((double> 1d0 t-split 0d0)
                      (lineseg-end (car (split-lineseg lineseg t-split)))))))))))
    (destructuring-bind (mins . maxs) bounds
      (let* ((rect-lines (make-linedef-loop mins (v (vx mins) (vy maxs))
                                            maxs (v (vx maxs) (vy mins))))
             (intersects (remove nil (mapcar #'intersect-line rect-lines)))
             ;; Treat :on-line side as :front.
             (mins-side (if (eq :back (determine-side split-line mins))
                            :back :front))
             (maxs-side (if (eq :back (determine-side split-line maxs))
                            :back :front)))
        (destructuring-bind (split-mins . split-maxs)
            (bounds-of-points intersects)
          (let ((front-bounds
                 (cons split-mins (if (eq mins-side maxs-side)
                                      split-maxs
                                      (copy-seq maxs))))
                (back-bounds
                 (cons (copy-seq mins) (if (eq mins-side maxs-side)
                                           (copy-seq maxs)
                                           split-maxs))))
            (if (eq :back mins-side)
                (cons front-bounds back-bounds)
                (cons back-bounds front-bounds))))))))

(defun build-bsp (surfaces &optional (splitters nil)
                             (bounds (bounds-of-surfaces surfaces)))
  (if (null surfaces)
      (make-leaf :bounds bounds :contents :contents-solid)
      (multiple-value-bind
            (splitter-surf rest) (choose-splitter surfaces splitters)
        (if (not splitter-surf)
            ;; We've selected all the segments and they form a convex hull.
            (progn
              (assert (convex-hull-p (mapcar #'sidedef-lineseg rest)))
              (let ((front-sectors (mapcar #'sidedef-front-sector rest)))
                (assert (every (curry #'equalp (car front-sectors))
                               (cdr front-sectors))))
              (make-leaf :bounds bounds :surfaces rest))
            ;; Split the remaining into front and back.
            (let ((splitter (lineseg-orig-line (sidedef-lineseg splitter-surf))))
              (multiple-value-bind
                    (front back on-splitter) (partition-surfaces splitter rest)
                (let ((used-splitters (append on-splitter splitters)))
                  (destructuring-bind (front-bounds . back-bounds)
                      (divide-bounds bounds splitter)
                    (make-node :bounds bounds
                               :line splitter
                               ;; Add the splitter itself to front.
                               :front (build-bsp (cons splitter-surf front)
                                                 used-splitters front-bounds)
                               :back (build-bsp back used-splitters
                                                back-bounds))))))))))

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

(defun bsp-trav (bsp node-fun &optional (base #'identity))
  "Recurse the given BSP node and apply NODE-FUN to results of traversing
  front and back side. BASE is applied on leaf nodes."
  (labels ((rec (node)
             (if (leaf-p node)
                 (if (functionp base)
                     (funcall base node)
                     base)
                 (funcall node-fun
                          (rec (node-front node))
                          (rec (node-back node))))))
    (rec bsp)))

(defun bsp-rec (bsp node-fun &optional (base #'identity))
  "Recurse the given BSP node. NODE-FUN is called with 3 arguments: NODE,
  FRONT and BACK. The NODE is the current split node, while FRONT and BACK are
  functions for recursing to front and back, respectively. Optional BASE
  argument is the function to be called on leaf nodes."
  (labels ((rec (node)
             (if (leaf-p node)
                 (if (functionp base)
                     (funcall base node)
                     base)
                 (funcall node-fun node
                          (lambda () (rec (node-front node)))
                          (lambda () (rec (node-back node)))))))
    (rec bsp)))

(defun write-bsp (bsp stream)
  ;; preorder traverse write
  (flet ((write-bounds (bounds)
           (destructuring-bind (mins . maxs) bounds
             (write-vec mins stream)
             (format stream " ")
             (write-vec maxs stream))))
    (bsp-rec bsp
           (lambda (node front back)
             (format stream "~S~%" :node)
             (write-bounds (node-bounds node))
             (format stream "~%")
             (write-linedef (node-line node) stream)
             (funcall front)
             (funcall back))
           (lambda (leaf)
             (format stream "~S~%" :leaf)
             (with-struct (leaf- bounds contents surfaces) leaf
               (write-bounds bounds)
               (format stream "~%~@{~S~%~}" contents (list-length surfaces))
               (dolist (surf surfaces)
                 (write-sidedef surf stream)))))))

(defun read-bsp (stream)
  (let ((node-type (read stream)))
    (ecase node-type
      (:node (let ((bounds (cons (read-vec stream) (read-vec stream)))
                   (line (read-linedef stream))
                   (front (read-bsp stream))
                   (back (read-bsp stream)))
               (make-node :bounds bounds :line line :front front :back back)))
      (:leaf (let* ((bounds (cons (read-vec stream) (read-vec stream)))
                    (contents (read stream))
                    (num-surfs (read stream))
                    (surfs (repeat num-surfs (read-sidedef stream))))
               (make-leaf :bounds bounds :surfaces surfs :contents contents))))))

(defun back-to-front (point bsp)
  "Traverse the BSP in back to front order relative to given POINT."
  (bsp-rec bsp
           (lambda (node front back)
             (ecase (determine-side (node-line node) point)
               ((or :front :on-line) (append (funcall back) (funcall front)))
               (:back (append (funcall front) (funcall back)))))
           #'leaf-surfaces))
