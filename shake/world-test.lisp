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

(defpackage #:shake-test
  (:use #:cl
        #:prove
        #:shake)
  (:import-from #:shiva
                #:v)
  (:import-from #:sbsp
                #:make-linedef
                #:linedef->lineseg
                #:build-bsp))

(in-package #:shake-test)

(plan nil)

(defparameter *square-linedefs*
  (list (make-linedef :start (v -0.5d0 0.5d0) :end (v -0.5d0 -0.5d0))
        (make-linedef :start (v -0.5d0 -0.5d0) :end (v 0.5d0 -0.5d0))
        (make-linedef :start (v 0.5d0 -0.5d0) :end (v 0.5d0 0.5d0))
        (make-linedef :start (v 0.5d0 0.5d0) :end (v -0.5d0 0.5d0))))

(subtest "Test collision detection"
  (subtest "hull-point-contents"
    (let* ((segs (mapcar #'linedef->lineseg *square-linedefs*))
           (hull (build-bsp segs)))
      (is (shake::hull-point-contents hull (v 0d0 0d0)) :contents-solid)
      (is (shake::hull-point-contents hull (v 1d0 0d0)) :contents-empty)
      (is (shake::hull-point-contents hull (v 0.5d0 0.5d0)) :contents-empty)
      (is (shake::hull-point-contents hull (v 0.49d0 0.49d0)) :contents-solid))))

(finalize)
