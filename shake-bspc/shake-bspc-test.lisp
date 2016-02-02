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
                #:v))

(in-package #:shake-bspc-test)

(plan nil)

(defparameter *test-linedefs*
  (list (make-linedef :start (v 0 -2) :end (v 0 5))
        (make-linedef :start (v -2 1) :end (v 5 1))
        (make-linedef :start (v 3 2) :end (v 3 -2))))

(subtest "Testing split-lineseg"
  (let ((seg (shake-bspc::linedef->lineseg (car *test-linedefs*))))
    (is (shake-bspc::split-lineseg seg -1d0)
        nil)
    (is (shake-bspc::split-lineseg seg 0d0)
        nil)
    (is (shake-bspc::split-lineseg seg 1d0)
        nil)
    (let ((t-split 0.8d0))
      (is (shake-bspc::split-lineseg seg t-split)
          (cons (make-lineseg :orig-line (car *test-linedefs*)
                              :t-end t-split)
                (make-lineseg :orig-line (car *test-linedefs*)
                              :t-start t-split))
          :test #'equalp))))

(finalize)
