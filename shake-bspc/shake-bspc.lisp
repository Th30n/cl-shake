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
  (end  (v 0 0) :type (simple-array double-float (2)) :read-only t))

(defun linedef-normal (linedef)
  (let ((vec (v- (linedef-end linedef) (linedef-start linedef))))
    (rotatef (aref vec 0) (aref vec 1))
    (setf (aref vec 1) (- (aref vec 1)))
    vec))

(defstruct lineseg
  "Segment of a line. Each line starts as a full segment, but may be split
  into multiple segments."
  (orig-line (make-linedef) :type linedef)
  (t-start 0d0 :type double-float)
  (t-end 1d0 :type double-float))

(defun line-intersect-ratio (splitter line)
  "Takes a SPLITTER and LINE linedefs. Returns numerator and denominator values
  for calculating the parameter T of the intersection."
  (declare (type linedef splitter line))
  (let* ((n (linedef-normal splitter))
         (-n (v- (v 0 0) n))
         (l-vec (v- (linedef-end line) (linedef-start line)))
         (sl-vec (v- (linedef-start line) (linedef-start splitter)))
         (numer (vdot n sl-vec))
         (denom (vdot -n l-vec)))
    (values numer denom)))

(defun split-lineseg (lineseg t-split)
  "Split the given LINESEG at the given T-SPLIT parameter into a pair
  of LINESEG. Returns NIL if T-SPLIT does not split the line segment."
  (declare (type lineseg lineseg) (type double-float t-split))
  (when (and (> t-split (lineseg-t-start lineseg))
             (< t-split (lineseg-t-end lineseg)))
    (let ((l1 (copy-lineseg lineseg))
          (l2 (copy-lineseg lineseg)))
      (setf (lineseg-t-end l1) t-split)
      (setf (lineseg-t-start l2) t-split)
      (cons l1 l2))))

(defun build-bsp (rootseg linesegs)
  (declare (type lineseg rootseg) (type list linesegs))
  (if (null linesegs)
      (list rootseg)
      (let ((splitter (lineseg-orig-line rootseg))
            (front nil)
            (back nil))
        (dolist (seg linesegs)
          (let ((line (lineseg-orig-line seg)))
            (multiple-value-bind (num den) (line-intersect-ratio splitter line)
              (if (= den 0d0)
                  ;; parallel lines
                  (if (< num 0d0)
                      (setf front (cons seg front))
                      (setf back (cons seg back)))
                  ;; lines intersect
                  (let ((splitted (split-lineseg seg (/ num den))))
                    (if (null splitted)
                        (if (< num 0d0)
                            (setf front (cons seg front))
                            (setf back (cons seg back)))
                        (if (< num 0d0)
                            (setf front (cons (car splitted) front)
                                  back (cons (cdr splitted) back))
                            (setf back (cons (car splitted) back)
                                  front (cons (cdr splitted) front)))))))))
        (list rootseg
              (if (null front) nil (build-bsp (car front) (cdr front)))
              (if (null back) nil (build-bsp (car back) (cdr back)))))))

(defun linedef->lineseg (linedef)
  (declare (type linedef linedef))
  (make-lineseg :orig-line linedef))

(defun read-linedef (stream)
  (destructuring-bind (x1 y1 x2 y2) (read stream)
    (make-linedef :start (v x1 y1) :end (v x2 y2))))

(defun read-map (stream)
  (let ((n (read stream)))
    (loop repeat n collecting (read-linedef stream))))

(defun read-and-compile-map (stream)
  (let* ((map (read-map stream))
         (segs (mapcar #'linedef->lineseg map)))
    (build-bsp (car segs) (cdr segs))))
