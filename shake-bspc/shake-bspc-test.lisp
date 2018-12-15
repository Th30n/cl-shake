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

(in-package #:cl-user)

(defpackage #:shake-bspc-test
  (:use #:cl
        #:prove
        #:shake-bspc)
  (:import-from #:shiva
                #:shiva-float
                #:v=
                #:v))

(in-package #:shake-bspc-test)

(plan nil)

(defparameter *test-linedefs*
  (list (make-linedef :start (v 0 -2) :end (v 0 5))
        (make-linedef :start (v -2 1) :end (v 5 1))
        (make-linedef :start (v 3 2) :end (v 3 -2))))

(defparameter *expected-boxes*
  (list
   (cons (v -2 -2) (v 5 5))
   (list (cons (v 0 -2) (v 5 5))
         (list (cons (v 0 -2) (v 5 1))
               (cons (v 0 -2) (v 3 1))
               (cons (v 3 -2) (v 5 1)))
         (list (cons (v 0 1) (v 5 5))
               (cons (v 0 1) (v 3 5))
               (cons (v 3 1) (v 5 5))))
   (list (cons (v -2 -2) (v 0 5))
         (cons (v -2 -2) (v 0 1))
         (cons (v -2 1) (v 0 5)))))

(subtest "Testing split-lineseg"
  (let* ((line (car *test-linedefs*))
         (seg (linedef->lineseg line)))
    (is (sbsp:split-lineseg seg (shiva-float -1.0))
        nil)
    (is (sbsp:split-lineseg seg (shiva-float 0.0))
        nil)
    (is (sbsp:split-lineseg seg (shiva-float 1.0))
        nil)
    (let* ((t-split (shiva-float 0.8d0))
           (split-segs (sbsp:split-lineseg seg t-split)))
      (is split-segs
          (cons (make-lineseg :orig-line line
                              :t-end t-split)
                (make-lineseg :orig-line line
                              :t-start t-split))
          :test #'equalp)
      (is (lineseg-start (car split-segs))
          (linedef-start line) :test #'v=)
      (is (lineseg-end (cdr split-segs))
          (linedef-end line) :test #'v=)
      (is (lineseg-start (cdr split-segs)) (v 0 3.6d0) :test #'v=)
      (is (lineseg-start (cdr split-segs))
          (lineseg-end (car split-segs)) :test #'v=))))

(subtest "Test serialization"
  (let* ((surfs (mapcar #'linedef->sidedef *test-linedefs*))
         (bsp (build-bsp surfs)))
    (with-input-from-string (in (with-output-to-string (out)
                                  (sbsp:write-bsp bsp out)))
      (is (read-bsp in) bsp :test #'equalp))))

(defparameter *coincident-linedefs*
  (list (make-linedef :start (v -3d0 1.0) :end (v -2.0 1.0))
        (make-linedef :start (v -4.0 2.0) :end (v -4.0 0.0))
        (make-linedef :start (v -1.0 1.0) :end (v 0.0 1.0))))

(defparameter *double-split-linedefs*
  (list (make-linedef :start (v 1.0 1.0) :end (v 1.0 3d0))
        (make-linedef :start (v 4.0 0.0) :end (v 0.0 0.0))
        (make-linedef :start (v 3d0 1.0) :end (v 3d0 2.0))))

(subtest "Test build-bsp produces correct back-to-front"
  (subtest "Coincident segments"
    (let* ((*splitter-choice-strategy* :first)
           (surfs (mapcar #'linedef->sidedef *coincident-linedefs*))
           (bsp (build-bsp surfs)))
      (is (mapcar #'sidedef-lineseg (back-to-front (v -0.5 1.5d0) bsp))
          (list (make-lineseg :orig-line (second *coincident-linedefs*)
                              :t-start (shiva-float 0.5) :t-end (shiva-float 1.0))
                (sidedef-lineseg (third surfs))
                (sidedef-lineseg (first surfs))
                (make-lineseg :orig-line (second *coincident-linedefs*)
                              :t-start (shiva-float 0.0) :t-end (shiva-float 0.5)))
          :test #'equalp)))
  (subtest "Double split segment"
    (let* ((*splitter-choice-strategy* :first)
           (surfs (mapcar #'linedef->sidedef *double-split-linedefs*))
           (bsp (build-bsp surfs)))
      (is (mapcar #'sidedef-lineseg (back-to-front (v 3.5d0 1.5d0) bsp))
          (list (make-lineseg :orig-line (second *double-split-linedefs*)
                              :t-start (shiva-float 0.75) :t-end (shiva-float 1.0))
                (sidedef-lineseg (first surfs))
                (make-lineseg :orig-line (second *double-split-linedefs*)
                              :t-start (shiva-float 0.25) :t-end (shiva-float 0.75))
                (make-lineseg :orig-line (second *double-split-linedefs*)
                              :t-start (shiva-float 0.0) :t-end (shiva-float 0.25))
                (sidedef-lineseg (third surfs)))
          :test #'equalp))))

(subtest "Test build-bsp produces correct axis aligned bounding boxes"
  (let* ((*splitter-choice-strategy* :first)
         (boxes (bsp-rec (build-bsp (mapcar #'linedef->sidedef *test-linedefs*))
                         (lambda (node front back)
                           (list (node-bounds node) (funcall front) (funcall back)))
                         #'leaf-bounds)))
    (is boxes *expected-boxes* :test #'equalp)))

(finalize)
