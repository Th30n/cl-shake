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

(defpackage #:shiva-test
  (:use #:cl
        #:prove
        #:shiva))

(in-package #:shiva-test)

(plan nil)

(subtest "Testing v-"
  (is (v- (v 5 3)
          (v 4 7))
      (v 1 -4) :test #'equalp)
  (is-type (v- (v 3 4) (v 4 4))
           '(vector double-float 2)))

(subtest "Testing vdot"
  (is (vdot (v 3 5)
            (v 4 2))
      22d0)
  (is (vdot (v 2 4)
            (v 8 -4))
      0d0))

(subtest "Testing m*"
  (is (m* (mat '(1 2 3)
               '(4 5 6))
          (mat '(5 6)
               '(7 8)
               '(9 10)))
      (mat '(46 52)
           '(109 124)) :test #'equalp)
  (is-type (m* (mat '(1 2)
                    '(3 4))
               (mat '(1 0)
                    '(0 1)))
           '(simple-array double-float (2 2))))

(subtest "Testing transforms"
  (is (vtransform (translation :x 2 :z -2) (v 3 5 0 1))
      (v 5 5 -2 1) :test #'equalp)
  (is (vtransform (translation :y 3) (v 4 5 6 0))
      (v 4 5 6 0) :test #'equalp)
  (is (vtransform (scale :x 5) (v 2 3 4 1))
      (v 10 3 4 1) :test #'equalp)
  (is (vtransform (scale :y 3 :z 4) (v 1 2 3 0))
      (v 1 6 12 0) :test #'equalp)
  (let ((scale (scale :x 2))
        (translation (translation :x 1 :y 3)))
    (is (vtransform (m* scale translation) (v 1 2 3 1))
        (v 4 5 3 1) :test #'equalp)
    (is (vtransform (m* translation scale) (v 1 2 3 1))
        (v 3 5 3 1) :test #'equalp)
    (is (vtransform (m* translation scale) (v 1 2 3 0))
        (vtransform (m* scale translation) (v 1 2 3 0)) :test #'equalp)))

(finalize)
