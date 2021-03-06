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

;;; Functions for querying collisions with the world.

(in-package #:shake)

(defstruct mtrace
  "Movement trace information. FRACTION is determines the completed part of
  the path. ENDPOS is the clipped position. NORMAL is the surface normal at
  impact."
  (fraction #.(shiva-float 1.0) :type shiva-float)
  (endpos nil :type (vec 3))
  (normal nil :type (or null (vec 3)))
  (start-solid-p nil :type boolean))

(defun hull-point-leaf (hull point)
  "Traverse the HULL to the leaf where POINT is located and return it."
  (check-type hull (or sbsp:leaf sbsp:node))
  (check-type point (vec 2))
  (if (sbsp:leaf-p hull)
      hull
      (ecase (sbsp:determine-side (sbsp:node-line hull) point)
        ((or :front :on-line)
         (hull-point-leaf (sbsp:node-front hull) point))
        (:back
         (hull-point-leaf (sbsp:node-back hull) point)))))

(defun hull-point-contents (hull point)
  "Traverse the HULL to the leaf where POINT is located and return
  LEAF-CONTENTS. Secondary value is a parent NODE."
  (check-type hull (or sbsp:leaf sbsp:node))
  (check-type point (vec 2))
  (let ((leaf (hull-point-leaf hull point)))
    (values (sbsp:leaf-contents leaf) leaf)))

(defvar *ignore-empty-leaves* nil
  "If T hull clip functions will treat reaching a leaf with :CONTENTS-EMPTY as
  success, regardless of potential blockage due to height difference.  This is
  primarily used to test colliding into a solid BSP model, i.e. brush based
  things.")

(defun hull-point-sector (hull-node point)
  "Get the hull SECTOR which contains the POINT. Returns NIL if no sector
  found, this is the case when the point is inside a solid leaf."
  (check-type hull-node (or sbsp:leaf sbsp:node))
  (check-type point (vec 2))
  (let* ((leaf (hull-point-leaf hull-node point))
         (sector (sbsp::subsector-orig-sector (sbsp::leaf-subsector leaf))))
    (or sector
        ;; TODO: Make sure all non solid leafs have a sector.
        (and (not (eq (sbsp:leaf-contents leaf) :contents-solid))
             (sbsp:make-sector)))))

(defun crossp (hull-node point height)
  "Return T if the POINT can cross into a non-solid space."
  (let ((contents (hull-point-contents hull-node (v3->v2 point))))
    (if (and *ignore-empty-leaves* (eq contents :contents-empty))
        t
        (when-let* ((sector (hull-point-sector hull-node (v3->v2 point)))
                    (floor-height (sbsp:sector-floor-height sector))
                    (ceiling-height (sbsp:sector-ceiling-height sector)))
          (and (or *ignore-empty-leaves* (not (eq contents :contents-solid)))
               (float<= (+ (vy point) height) ceiling-height)
               (float>= (vy point) floor-height))))))

(defparameter *dist-epsilon*
  #-shiva-double-float 5s-6
  #+shiva-double-float 1d-10)

(defun cross-fraction (t1 t2)
  "Calculate the crosspoint fraction. The fraction is offset by *DIST-EPSILON*
  towards the near side."
  (let ((frac (clamp (/ (if (minusp t1)
                            (+ t1 *dist-epsilon*)
                            (- t1 *dist-epsilon*))
                        (- t1 t2))
                     #.(shiva-float 0) #.(shiva-float 1))))
    frac))

(defun adjust-midf (hull p1 p2 p1f p2f mid midf frac)
  "Returns adjusted splitting fraction MIDF as the primary value, and adjusted
  MID point as the secondary. Adjustment is done such that the split point is
  moved outside of the solid area if it ended up inside due to floating point
  errors."
  (do ()
      ((not (eq (hull-point-contents hull (v3->v2 mid)) :contents-solid))
       (values midf mid))
    (decf frac #.(shiva-float 0.01d0))
    (if (minusp frac)
        (return (values midf mid))
        (setf midf (+ p1f (* frac (- p2f p1f)))
              mid (v+ p1 (vscale frac (v- p2 p1)))))))

(defun adjust-midf-vertical (hull p1 p2 p1f p2f mid midf frac height)
  "Returns adjusted splitting fraction MIDF as the primary value, and adjusted
  MID point as the secondary. Adjustment is done such that the split point is
  moved inside of the sector's vertical bounds if it ended up outside due to
  floating point errors."
  (flet ((insidep ()
           (let* ((sector (hull-point-sector hull (v3->v2 mid)))
                  (floor-height (sbsp:sector-floor-height sector))
                  (ceiling-height (sbsp:sector-ceiling-height sector)))
             (and (float>= (vy mid) floor-height)
                  (float<= (+ (vy mid) height) ceiling-height)))))
    (do ()
        ((insidep)
         (values midf mid))
      (decf frac #.(shiva-float 0.01d0))
      (if (minusp frac)
          (return (values midf mid))
          (setf midf (+ p1f (* frac (- p2f p1f)))
                mid (v+ p1 (vscale frac (v- p2 p1))))))))

(defun split-hull-check (hull height node t1 t2 p1 p2 p1f p2f)
  "Check for collision on both sides of given hull NODE. T1 and T2 are
  distances to splitting node line for points P1 and P2. P1F and P2F are
  fractions of the movement line from P1 to P2."
  (let* ((frac (cross-fraction t1 t2))
         (midf (+ p1f (* frac (- p2f p1f))))
         (mid (v+ p1 (vscale frac (v- p2 p1)))))
    (multiple-value-bind (pre-trace hit-p)
        (recursive-hull-check hull p1 mid
                              height
                              (if (minusp t1)
                                  (sbsp:node-back node)
                                  (sbsp:node-front node))
                              p1f midf)
      (if hit-p
          ;; Collision in front part.
          (values pre-trace hit-p)
          ;; Check the other part.
          (let ((other-side (if (minusp t1)
                                (sbsp:node-front node)
                                (sbsp:node-back node))))
            (if (crossp other-side mid height)
                ;; Continue through the other part.  TODO: It's possible that
                ;; mid -- p2 is a solid sector, and we end up inside a solid
                ;; body. CROSSP reported the cross is possible due to floating
                ;; point error of mid ending up just outside. Usually happens
                ;; when p1 is exactly on the line of a solid sector and p2
                ;; inside. Can this happen with vertical clipping?
                (recursive-hull-check hull mid p2 height other-side midf p2f)
                ;; Other side is solid, this is the impact point.
                (let ((normal (sbsp:linedef-normal (sbsp:node-line node))))
                  (when (minusp t1)
                    (setf normal (v- normal)))
                  (multiple-value-bind (adj-midf adj-mid)
                      (adjust-midf hull p1 p2 p1f p2f mid midf frac)
                    (values
                     (make-mtrace :fraction adj-midf :endpos adj-mid
                                  :normal (v2->v3 normal))
                     t)))))))))

(defun recursive-hull-check (hull p1 p2 height
                             &optional (node hull)
                               (p1f #.(shiva-float 0))
                               (p2f #.(shiva-float 1)))
  (declare (type (or sbsp:node sbsp:leaf) hull node))
  (declare (type (vec 3) p1 p2))
  (declare (type shiva-float height p1f p2f))
  (if (sbsp:leaf-p node)
      (let ((contents (hull-point-contents node (v3->v2 p2)))
            (sector (hull-point-sector node (v3->v2 p2))))
        (cond
          ((and (not *ignore-empty-leaves*) (eq :contents-solid contents))
           ;; We probably started P1 in solid area. This can also happen if
           ;; CROSSP reports that we can cross, because P1 actually ends up
           ;; outside.
           (values (make-mtrace :fraction p1f :endpos p1 :start-solid-p t) t))
          ((and *ignore-empty-leaves* (eq :contents-empty contents))
           (values (make-mtrace :endpos p2) nil))
          (sector
           ;; Clip vertical movement, e.g. gravity.
           (let ((floor-height (sbsp:sector-floor-height sector))
                 (ceiling-height (sbsp:sector-ceiling-height sector)))
             (assert floor-height)
             (assert ceiling-height)
             (let ((t1 (vdot (v 0 1 0)
                             (v- p1 (v 0 floor-height 0))))
                   (t2 (vdot (v 0 1 0)
                             (v- p2 (v 0 floor-height 0)))))
               ;; TODO: What if we are below the floor?
               (if (or (and (minusp t1) (minusp t2))
                       (and (<= 0.0 t1) (<= 0.0 t2)))
                   ;; We aren't crossing the floor, so check ceiling
                   (let ((t1 (vdot (v 0 -1 0)
                                   (v- p1 (v 0 (- ceiling-height height) 0))))
                         (t2 (vdot (v 0 -1 0)
                                   (v- p2 (v 0 (- ceiling-height height) 0)))))
                     ;; TODO: What if we are above the ceiling?
                     (if (or (and (minusp t1) (minusp t2))
                             (and (<= 0.0 t1) (<= 0.0 t2)))
                         (values (make-mtrace :endpos p2) nil)
                         (let* ((frac (cross-fraction t1 t2))
                                (midf (+ p1f (* frac (- p2f p1f))))
                                (mid (v+ p1 (vscale frac (v- p2 p1)))))
                           (multiple-value-bind (adj-midf adj-mid)
                               (adjust-midf-vertical hull p1 p2 p1f p2f mid midf frac height)
                             (values (make-mtrace :fraction adj-midf
                                                  :normal (v 0 1 0)
                                                  :endpos adj-mid)
                                     t)))))
                   ;; We are crossing the floor
                   (let* ((frac (cross-fraction t1 t2))
                          (midf (+ p1f (* frac (- p2f p1f))))
                          (mid (v+ p1 (vscale frac (v- p2 p1)))))
                     (multiple-value-bind (adj-midf adj-mid)
                         (adjust-midf-vertical hull p1 p2 p1f p2f mid midf frac height)
                       (values (make-mtrace :fraction adj-midf
                                            :normal (v 0 1 0)
                                            :endpos adj-mid)
                               t)))))))
          ;; Missing sector, so allow movement. TODO: We should probably
          ;; have no missing sectors, maybe log a warning here.
          (t (values (make-mtrace :endpos p2) nil))))
      ;; Hull-node case
      (let ((t1 (sbsp:dist-line-point (sbsp:node-line node) (v3->v2 p1)))
            (t2 (sbsp:dist-line-point (sbsp:node-line node) (v3->v2 p2))))
        (cond
          ((and (minusp t1) (minusp t2))
           ;; Path is behind the line.
           (recursive-hull-check hull p1 p2 height (sbsp:node-back node) p1f p2f))
          ((and (<= 0.0 t1) (<= 0.0 t2))
           ;; Path is in front of the line.
           (recursive-hull-check hull p1 p2 height (sbsp:node-front node) p1f p2f))
          (t
           (split-hull-check hull height node t1 t2 p1 p2 p1f p2f))))))

(defun clip-hull (hull p1 p2 &key (height #.(shiva-float 0.0)) ignore-empty-leaves)
  "Checks the HULL for the nearest collision on the way from P1 to P2. Returns
  the MTRACE of the final movement. The secondary value is T if there were
  collisions.  If IGNORE-EMPTY-LEAVES is T, the movement automatically
  succeeds if it manages to get to a leaf with :CONTENTS-EMPTY."
  (check-type hull (or sbsp:node sbsp:leaf))
  (check-type p1 (vec 3))
  (check-type p2 (vec 3))
  (check-type height (shiva-float #.(shiva-float 0.0)))
  (check-type ignore-empty-leaves boolean)
  (let ((*ignore-empty-leaves* ignore-empty-leaves))
    (declare (special *ignore-empty-leaves*))
    (recursive-hull-check hull p1 p2 height)))

(declaim (inline make-oobb))
(defstruct oobb
  "Object Oriented Bounding Box"
  (center (v 0 0 0) :type (vec 3))
  ;; Positive half-lengths from the center in each direction
  (half-lengths (v 0.5 0.5 0.5) :type (vec 3))
  ;; Normalized side directions
  (directions (make-array 3 :element-type '(vec 3)
                          :initial-contents (list (v 1 0 0) (v 0 1 0) (v 0 0 1)))
              :type (simple-array (vec 3) (3))))

(defun ray-oobb-intersect (origin dir oobb &key ray-length)
  "Return distance from ORIGIN along normalized DIR where ray intersects OOBB.
If there is no intersection, return NIL.  Referenced algorithm from Real-Time
Rendering 3rd edition, 16.7.1 Slabs Method."
  (check-type origin (vec 3))
  (check-type dir (vec 3))
  (check-type oobb oobb)
  (check-type ray-length (or null shiva-float))
  (assert (float= (v3dot dir dir) #.(shiva-float 1)))
  (let ((t-min nil)
        (t-max nil)
        (p (v- (oobb-center oobb) origin)))
    (dotimes (i 3)
      (let ((e (v3dot (aref (oobb-directions oobb) i) p))
            (f (v3dot (aref (oobb-directions oobb) i) dir))
            (h (aref (oobb-half-lengths oobb) i)))
        (if (float> (abs f) #.(shiva-float 0))
            ;; Not perpendicular to direction
            (let* ((1/f (/ #.(shiva-float 1) f))
                   (t1 (* (+ e h) 1/f))
                   (t2 (* (- e h) 1/f)))
              (if (float> t1 t2) (rotatef t1 t2))
              (if (or (not t-min) (float> t1 t-min)) (setf t-min t1))
              (if (or (not t-max) (float< t2 t-max)) (setf t-max t2))
              (if (float> t-min t-max) (return-from ray-oobb-intersect))
              (if (float< t-max #.(shiva-float 0)) (return-from ray-oobb-intersect)))
            ;; Ray perpendicular to direction
            (if (or (float< (+ e h) #.(shiva-float 0))
                    (float< (- h e) #.(shiva-float 0)))
                (return-from ray-oobb-intersect)))
        (if (and ray-length t-min (float>= t-min ray-length))
            (return-from ray-oobb-intersect))))
    (if (float> t-min #.(shiva-float 0))
        t-min
        t-max)))
