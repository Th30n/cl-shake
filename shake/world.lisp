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
  (normal nil :type (or null (vec 3))))

(defun hull-point-leaf (hull point)
  "Traverse the HULL to the leaf where POINT is located and return it."
  (declare (type (vec 2) point))
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
  (declare (type (vec 2) point))
  (let ((leaf (hull-point-leaf hull point)))
    (values (sbsp:leaf-contents leaf) leaf)))

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
  (when-let* ((sector (hull-point-sector hull-node (v3->v2 point)))
              (floor-height (sbsp:sector-floor-height sector))
              (ceiling-height (sbsp:sector-ceiling-height sector)))
    (and (float>= ceiling-height (+ (vy point) height))
         (float>= (vy point) floor-height))))

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
                     (shiva-float 0) (shiva-float 1))))
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

(defun split-hull-check (hull node t1 t2 p1 p2 p1f p2f)
  "Check for collision on both sides of given hull NODE. T1 and T2 are
  distances to splitting node line for points P1 and P2. P1F and P2F are
  fractions of the movement line from P1 to P2."
  (let* ((frac (cross-fraction t1 t2))
         (midf (+ p1f (* frac (- p2f p1f))))
         (mid (v+ p1 (vscale frac (v- p2 p1)))))
    (multiple-value-bind (pre-trace hit-p)
        (recursive-hull-check hull p1 mid
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
            (if (crossp other-side mid (shiva-float 0.5)) ;; TODO: Don't hardcode height
                ;; Continue through the other part.
                (recursive-hull-check hull mid p2 other-side midf p2f)
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

(defun recursive-hull-check (hull p1 p2 &optional (node nil)
                                          (p1f #.(shiva-float 0))
                                          (p2f #.(shiva-float 1)))
  "Checks the HULL for the nearest collision on the way from P1 to P2. Returns
  the MTRACE of the final movement. The secondary value is T if there were
  collisions."
  (declare (type (vec 3) p1 p2))
  (unless node
    (setf node hull))
  (if (sbsp:leaf-p node)
      ;; TODO: Clip direct/only vertical movement, e.g. gravity.
      (values (make-mtrace :endpos p2) nil)
      (let ((t1 (sbsp:dist-line-point (sbsp:node-line node) (v3->v2 p1)))
            (t2 (sbsp:dist-line-point (sbsp:node-line node) (v3->v2 p2))))
        (cond
          ((and (minusp t1) (minusp t2))
           ;; Path is behind the line.
           (recursive-hull-check hull p1 p2 (sbsp:node-back node) p1f p2f))
          ((and (<= 0.0 t1) (<= 0.0 t2))
           ;; Path is in front of the line.
           (recursive-hull-check hull p1 p2 (sbsp:node-front node) p1f p2f))
          (t
           (split-hull-check hull node t1 t2 p1 p2 p1f p2f))))))
