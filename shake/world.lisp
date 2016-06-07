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
  (fraction 1d0 :type double-float)
  (endpos nil :type (vec 3))
  (normal nil :type (or null (vec 3))))

(defun v3->v2 (v)
  "Convert VEC 3 to VEC 2 by dropping the y axis."
  (declare (type (vec 3) v))
  (v (vx v) (vz v)))

(defun v2->v3 (v)
  "Convert VEC 2 to VEC 3 by adding the y axis, set to 0."
  (declare (type (vec 2) v))
  (v (vx v) 0d0 (vy v)))

(defun dist-line-point (lineseg point)
  (declare (type sbsp:lineseg lineseg) (type (vec 2) point))
  (vdot (sbsp:lineseg-normal lineseg)
        (v- point (sbsp:lineseg-start lineseg))))

(defun hull-point-contents (hull point)
  "Traverse the HULL to the leaf where POINT is located and return
  LEAF-CONTENTS. Splitting line is offset by given RADIUS."
  (declare (type (vec 2) point))
  (if (sbsp:leaf-p hull)
      (sbsp:leaf-contents hull)
      (let ((d (dist-line-point (sbsp:node-line hull) point)))
        (if (minusp d)
            (hull-point-contents (sbsp:node-back hull) point)
            (hull-point-contents (sbsp:node-front hull) point)))))

(defun cross-fraction (t1 t2)
  "Calculate the crosspoint fraction."
  (let ((frac (clamp (/ t1 (- t1 t2))
                     0d0 1d0)))
    (format t "Frac: ~S, T1: ~S, T2: ~S~%" frac t1 t2)
    frac))

(defun recursive-hull-check (hull p1 p2 &optional (p1f 0d0) (p2f 1d0))
  "Checks the HULL for the nearest collision on the way from P1 to P2."
  (declare (type (vec 3) p1 p2))
  (if (sbsp:leaf-p hull)
      (values (make-mtrace :endpos p2) nil)
      (let ((t1 (dist-line-point (sbsp:node-line hull) (v3->v2 p1)))
            (t2 (dist-line-point (sbsp:node-line hull) (v3->v2 p2))))
        (cond
          ((and (minusp t1) (minusp t2))
           ;; Path is behind the line.
           (recursive-hull-check (sbsp:node-back hull) p1 p2 p1f p2f))
          ((and (<= 0d0 t1) (<= 0d0 t2))
           ;; Path is in front of the line.
           (recursive-hull-check (sbsp:node-front hull) p1 p2 p1f p2f))
          (t
           ;; Path crosses the line, needs to be split.
           (let* ((frac (cross-fraction t1 t2))
                  (midf (+ p1f (* frac (- p2f p1f))))
                  (mid (v+ p1 (vscale frac (v- p2 p1)))))
             (multiple-value-bind (pre-trace hit-p)
                 (recursive-hull-check (if (minusp t1)
                                           (sbsp:node-back hull)
                                           (sbsp:node-front hull))
                                       p1 mid p1f midf)
               (if hit-p
                   ;; Collision in front part.
                   (values pre-trace hit-p)
                   ;; Check the other part.
                   (let ((other-side (if (minusp t1)
                                         (sbsp:node-front hull)
                                         (sbsp:node-back hull))))
                     (if (not (eq (hull-point-contents other-side (v3->v2 mid))
                                  :contents-solid))
                         ;; Continue through the other part.
                         (recursive-hull-check other-side mid p2 midf p2f)
                         ;; Other side is solid, this is the impact point.
                         (let ((normal (sbsp:lineseg-normal (sbsp:node-line hull))))
                           (when (minusp t1)
                             (setf normal (v- normal)))
                           (values
                            (make-mtrace :fraction midf :endpos mid
                                         :normal (v2->v3 normal))
                            t))))))))))))
