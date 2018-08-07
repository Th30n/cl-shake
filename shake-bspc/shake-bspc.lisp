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
  (bounds-of-points (mappend (lambda (line)
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
    (write-string (string-upcase (format nil " ~S" (aref vec i)))
                  stream))
  (format stream ")"))

(defun read-vec-form (form)
  (destructuring-bind (type-name . args) form
    (declare (ignore type-name))
    (apply #'v args)))

(defun read-vec (stream)
  (read-vec-form (read stream)))

(defmacro define-disk-struct (name &rest slot-descriptions)
  (let* ((inhibit-read-p (member :inhibit-read (ensure-list name)))
         (name (car (ensure-list name)))
         (slot-descriptions (mapcar #'ensure-list slot-descriptions))
         (slot-names (mapcar #'first slot-descriptions)))
    (labels ((base-type-p (slot-type)
               (inq slot-type string float64))
             (nullablep (slot-type)
               (and (atom slot-type)
                    (starts-with #\? (string slot-type))))
             (write-nullable-slot (slot-name slot-type)
               `(if ,slot-name
                    ,(write-slot (list slot-name
                                       (subseq (string slot-type) 1)))
                    (format stream " ~S" ,slot-name)))
             (write-slot (desc)
               (destructuring-bind (slot-name slot-type) desc
                 (cond
                   ((nullablep slot-type)
                    (write-nullable-slot slot-name slot-type))
                   ((and (atom slot-type)
                         (not (base-type-p slot-type)))
                    `(progn
                       (format stream " ")
                       ;; This will not work if write-slot-type is not imported.
                       (,(symbolicate 'write- slot-type) ,slot-name stream)))
                   ((eq 'float64 slot-type)
                    `(write-string (string-upcase (format nil " ~S" ,slot-name))
                                   stream))
                   (t
                    `(format stream " ~S" ,slot-name)))))
             (read-nullable-slot (slot-name slot-type)
               `(when ,slot-name
                  ,(read-slot-form (list slot-name
                                         (subseq (string slot-type) 1)))))
             (read-slot-form (desc)
               (destructuring-bind (slot-name slot-type) desc
                 (cond
                   ((nullablep slot-type)
                    (read-nullable-slot slot-name slot-type))
                   ((and (atom slot-type)
                         (not (base-type-p slot-type)))
                    ;; This will not work if read-slot-type is not imported.
                    `(,(symbolicate 'read- slot-type '-form) ,slot-name))
                   (t
                    slot-name)))))
      `(progn
         (defun ,(symbolicate 'write- name) (obj stream)
           (with-struct (,(symbolicate name '-) ,@slot-names) obj
             (format stream ,(format nil "(~S" name))
             ,@(mapcar #'write-slot slot-descriptions))
           (format stream ")~%"))
         ,@(unless inhibit-read-p
             `((defun ,(symbolicate 'read- name '-form) (form)
                 (destructuring-bind (type-name . args) form
                   (declare (ignore type-name))
                   (destructuring-bind (,@slot-names) args
                     (,(symbolicate 'make- name)
                       ,@(mappend (lambda (desc)
                                    (list (make-keyword (car desc))
                                          (read-slot-form desc)))
                                  slot-descriptions)))))

               (defun ,(symbolicate 'read- name) (stream)
                 (with-standard-io-syntax
                   (let (*read-eval*)
                     (,(symbolicate 'read- name '-form) (read stream)))))))))))

(define-disk-struct texinfo
  (offset vec)
  (name string)
  (draw-mode (enum :tile :scale-to-fit)))

(defstruct sector
  "A sector surrounded by lines. Stores information about floor and ceiling."
  ;; (lines nil :type list) ;; Unused?
  (floor-height 0d0 :type double-float)
  (floor-texinfo nil :type (or null texinfo))
  (ceiling-height 1d0 :type double-float)
  (ceiling-texinfo nil :type (or null texinfo))
  (ambient-light (v 0 0 0) :type (vec 3)))

(define-disk-struct sector
  (floor-height float64)
  (floor-texinfo ?texinfo)
  (ceiling-height float64)
  (ceiling-texinfo ?texinfo)
  (ambient-light vec))

(defstruct subsector
  "A sub sector generated via BSP. Stores lines which surround it."
  (lines nil :type list)
  ;; TODO: Share this, instead of duplicating.
  (orig-sector nil :type (or null sector)))

(defstruct sidedef
  "A side definition for a line segment."
  (lineseg nil :type lineseg)
  (front-sector nil :type (or null sector))
  (back-sector nil :type (or null sector))
  (color (v 1 0 1) :type (vec 3))
  (texinfo nil))

(define-disk-struct sidedef
  (lineseg lineseg)
  (front-sector ?sector)
  (back-sector ?sector)
  (color vec)
  (texinfo ?texinfo))

(defun linedef->sidedef (line)
  (make-sidedef :lineseg (linedef->lineseg line)))

(defstruct leaf
  "A leaf node in the BSP tree. The SURFACES slot stores the geometry as a
  LIST of SIDEDEFs. The CONTENTS stores the type of the leaf, used for
  collision detection. It can be one of:
    * :CONTENTS-EMPTY -- free space
    * :CONTENTS-SOLID -- solid, impassable space
  The SUBSECTOR is a convex region of this leaf."
  (bounds nil :type (cons (vec 2) (vec 2)))
  (surfaces nil :type list)
  (contents :contents-empty)
  (subsector nil :type subsector))

(defstruct node
  "A node in the BSP tree. The geometry is stored in leaves. LINE is the
  splitting LINEDEF used at this node. Children are stored in FRONT and BACK
  slots."
  (bounds nil :type (cons (vec 2) (vec 2)))
  (line nil :type linedef)
  (surface nil :type sidedef)
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
    (when (and (double> t-end t-split)
               (double> t-split t-start))
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
           (in (determine-side (lineseg-orig-line seg) point)
               :back :on-line)))
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

(defun triangulate (linesegs)
  "Takes a convex polygon described with LINESEGS and returns a list of
  triangles, represented as linesegs."
  (assert (length>= 3 linesegs))
  (assert (convex-hull-p linesegs))
  (let ((start (lineseg-start (first linesegs)))
        (end (lineseg-end (first linesegs)))
        (points (mapcar #'lineseg-start (cddr linesegs)))
        triangles)
    (dolist (point points triangles)
      (push (mapcar #'linedef->lineseg (make-linedef-loop start end point))
            triangles)
      (setf end point))))

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

(defun intersect-line (line split-line)
  "Return the intersection point if lines intersect."
  (declare (type linedef line split-line))
  (let ((lineseg (make-lineseg :orig-line line)))
    (multiple-value-bind (num den)
        (line-intersect-ratio split-line lineseg)
      (unless (double= den 0d0)
        (let ((t-split (/ num den)))
          (cond
            ((double= t-split 0d0) (linedef-start line))
            ((double= t-split 1d0) (linedef-end line))
            ((and (double> 1d0 t-split)
                  (double> t-split 0d0))
             (lineseg-end (car (split-lineseg lineseg t-split))))))))))

(defun linedefs<-bounds (bounds)
  "Return list of LINEDEF looped around the bounds in CCW order."
  (destructuring-bind (mins . maxs) bounds
    (make-linedef-loop maxs (v (vx mins) (vy maxs))
                       mins (v (vx maxs) (vy mins)))))

(defun divide-bounds (bounds split-line)
  (flet ((intersect (line) (intersect-line line split-line)))
    (destructuring-bind (mins . maxs) bounds
      (let* ((rect-lines (linedefs<-bounds bounds))
             (intersects (remove nil (mapcar #'intersect rect-lines)))
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

(defun divide-region (convex-region split-line)
  "Split given CONVEX-REGION of LINEDEF by given SPLIT-LINE. The result is a
pair of front and back regions. Degenerate regions (i.e. lines) are returned
as NIL."
  (let ((intersects (remove nil (mapcar (lambda (line)
                                          (intersect-line line split-line))
                                        convex-region)))
        (points (mappend (lambda (line)
                           (list (linedef-start line) (linedef-end line)))
                         convex-region))
        (front-points nil)
        (back-points nil))
    (dolist (point points)
      (ecase (determine-side split-line point)
        (:front (push point front-points))
        (:back (push point back-points))
        (:on-line (push point front-points)
                  (push point back-points))))
    (let ((front-convex
           (sbrush::construct-convex-hull (append intersects front-points)))
          (back-convex
           (sbrush::construct-convex-hull (append intersects back-points))))
      (cons (when (length>= 3 front-convex)
              (apply #'make-linedef-loop front-convex))
            (when (length>= 3 back-convex)
              (apply #'make-linedef-loop back-convex))))))

(defun build-bsp (surfaces &optional splitters
                             (bounds (bounds-of-surfaces surfaces))
                             (convex-region (linedefs<-bounds bounds))
                             sector)
  (assert (or (not convex-region) ;; degenerate region
              (convex-hull-p (mapcar #'linedef->lineseg convex-region))))
  (if (null surfaces)
      (progn
        (make-leaf :bounds bounds :contents :contents-solid
                   :subsector (make-subsector :lines convex-region
                                              :orig-sector sector)))
      (multiple-value-bind
            (splitter-surf rest) (choose-splitter surfaces splitters)
        (if (not splitter-surf)
            ;; We've selected all the segments and they form a convex hull.
            (progn
              (assert (convex-hull-p (mapcar #'sidedef-lineseg rest)))
              (let ((front-sectors (mapcar #'sidedef-front-sector rest)))
                ;; TODO: Hmm, this doesn't have to hold
                ;; (assert (every (curry #'equalp sector) front-sectors))
                (make-leaf :bounds bounds :surfaces rest
                           :subsector
                           (make-subsector :lines convex-region
                                           :orig-sector sector))))
            ;; Split the remaining into front and back.
            (let ((splitter (lineseg-orig-line (sidedef-lineseg splitter-surf))))
              (multiple-value-bind
                    (front back on-splitter) (partition-surfaces splitter rest)
                (let ((used-splitters (append on-splitter splitters)))
                  (destructuring-bind (front-bounds . back-bounds)
                      (divide-bounds bounds splitter)
                    (destructuring-bind (front-region . back-region)
                        (divide-region convex-region splitter)
                      (make-node :bounds bounds
                                 :line splitter
                                 ;; XXX: Other surfaces on splitter?
                                 :surface splitter-surf
                                 ;; Add the splitter itself to front.
                                 :front (build-bsp (cons splitter-surf front)
                                                   used-splitters front-bounds
                                                   front-region
                                                   (sidedef-front-sector splitter-surf))
                                 :back (build-bsp back used-splitters
                                                  back-bounds
                                                  back-region
                                                  (sidedef-back-sector splitter-surf))))))))))))

(defun linedef->lineseg (linedef)
  (declare (type linedef linedef))
  (make-lineseg :orig-line linedef))

(define-disk-struct linedef
  (start vec)
  (end vec))

(define-disk-struct lineseg
  (orig-line linedef)
  (t-start float64)
  (t-end float64))

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
               (write-sidedef (node-surface node) stream)
               (funcall front)
               (funcall back))
             (lambda (leaf)
               (format stream "~S~%" :leaf)
               (with-struct (leaf- bounds contents surfaces subsector) leaf
                 (write-bounds bounds)
                 (format stream "~%~@{~S~%~}" contents (list-length surfaces))
                 (dolist (surf surfaces)
                   (write-sidedef surf stream))
                 (let ((lines (subsector-lines subsector)))
                   (format stream "~S~%" (list-length lines))
                   (when lines
                     (dolist (line lines)
                       (write-linedef line stream))
                     (if (not (subsector-orig-sector subsector))
                         (format stream "NIL~%")
                         (write-sector
                          (subsector-orig-sector subsector) stream)))))))))

(defun read-bsp (stream)
  (let ((node-type (read stream)))
    (ecase node-type
      (:node (let ((bounds (cons (read-vec stream) (read-vec stream)))
                   (line (read-linedef stream))
                   (sidedef (read-sidedef stream))
                   (front (read-bsp stream))
                   (back (read-bsp stream)))
               (make-node :bounds bounds :line line :surface sidedef
                          :front front :back back)))
      (:leaf (let* ((bounds (cons (read-vec stream) (read-vec stream)))
                    (contents (read stream))
                    (num-surfs (read stream))
                    (surfs (repeat num-surfs (read-sidedef stream)))
                    (num-lines (read stream))
                    (region (repeat num-lines (read-linedef stream)))
                    (sectorp (when (plusp num-lines) (read stream))))
               (make-leaf
                :bounds bounds :surfaces surfs :contents contents
                :subsector (make-subsector
                            :lines region
                            :orig-sector (when sectorp
                                           (read-sector-form sectorp)))))))))

(defun back-to-front (point bsp)
  "Traverse the BSP in back to front order relative to given POINT."
  (bsp-rec bsp
           (lambda (node front back)
             (ecase (determine-side (node-line node) point)
               ((or :front :on-line) (append (funcall back) (funcall front)))
               (:back (append (funcall front) (funcall back)))))
           #'leaf-surfaces))
