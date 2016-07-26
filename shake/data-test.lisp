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

(defpackage #:shake.data-test
  (:use #:cl
        #:prove
        #:sdata))

(in-package #:shake.data-test)

(plan nil)

(subtest "Test basic resource management"
  (let ((freep t)
        (test-res 42))
    (flet ((load-fun ()
             (setf freep nil)
             test-res)
           (free-fun (res)
             (declare (ignore res))
             (setf freep t)))
      (with-resources "basic"
        (add-res "res1" #'load-fun #'free-fun)
        (is (res "res1") test-res)
        (ok (not freep))
        (free-res "res1")
        (ok freep)
        (is (res "res1") test-res)
        (ok (not freep)))
      (ok freep))))

(subtest "Test double free"
  (let ((free-count 0))
    (flet ((free-fun (res)
             (declare (ignore res))
             (incf free-count)))
      (with-resources "double-free"
        (add-res "res" (lambda ()) #'free-fun)
        (is free-count 0)
        (free-res "res")
        (is free-count 1)
        (free-res "res")
        (free-res "res")
        (is free-count 1))
      (is free-count 1))))

(subtest "Test scopes"
  (let ((first-free 0)
        (second-free 0))
  (with-resources "first"
    (add-res "res" (constantly 'first) (lambda (res)
                                         (declare (ignore res))
                                         (incf first-free)))
    (with-resources "second"
      (add-res "res" (constantly 'second) (lambda (res)
                                            (declare (ignore res))
                                            (incf second-free)))
      (is (res "res") 'second)
      (is (res "res" :scope "second") 'second)
      (is (res "res" :scope "first") 'first)
      (free-res "res")
      (free-res "res")
      (is second-free 1)
      (is first-free 0)
      (is (res "res") 'second)
      (free-res "res" :scope "first")
      (is first-free 1)
      (is (res "res" :scope "first") 'first))
    (is second-free 2)
    (is first-free 1)
    (is (res "res") 'first))
  (is first-free 2)))

(finalize)
