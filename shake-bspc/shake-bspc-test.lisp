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
        #:shake-bspc))

(in-package #:shake-bspc-test)

(plan nil)

(subtest "Testing v-"
  (is (v- (v2 5 3)
          (v2 4 7))
      (v2 1 -4) :test #'equalp)
  (is-type (v- (v2 3 4) (v2 4 4))
           '(vector double-float 2)))

(subtest "Testing vdot"
  (is (vdot (v2 3 5)
            (v2 4 2))
      22d0)
  (is (vdot (v2 2 4)
            (v2 8 -4))
      0d0))

(finalize)
