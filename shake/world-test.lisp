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
                #:shiva-float
                #:float=
                #:v
                #:v=
                #:v3->v2)
  (:import-from #:sbsp
                #:make-linedef-loop
                #:linedef->lineseg
                #:build-bsp))

(in-package #:shake-test)

(plan nil)

;; XXX: Duplicated from brush-test
(defparameter *square-linedefs*
  (make-linedef-loop
   ;; top left, bottom left, bottom right, top right
   (v -1d0 0d0) (v -1d0 -1d0) (v 0d0 -1d0) (v 0d0 0d0)))

(defmacro let-mtrace (p1 p2 &body body)
  `(let* ((p1 ,p1)
          (p2 ,p2)
          (mtrace (shake:recursive-hull-check hull p1 p2)))
     ,@body))

(defmacro test-recursive-hull-check (p1 p2 &key fraction endpos normal)
  `(let-mtrace ,p1 ,p2
     ,@(when fraction
             (list
              `(is (shake:mtrace-fraction mtrace) (shiva-float ,fraction)
                   :test #'float=)))
     (is (shake:mtrace-endpos mtrace) ,endpos :test #'v=)
     (is (shake:mtrace-normal mtrace) ,normal
         :test (lambda (a b)
                 (if (null a)
                     (null b)
                     (and b (v= a b)))))))

(subtest "hull-point-contents"
  (let* ((surfs (mapcar #'sbsp:linedef->sidedef *square-linedefs*))
         (hull (build-bsp surfs)))
    (is (shake:hull-point-contents hull (v -0.5d0 -0.5d0)) :contents-solid)
    (is (shake:hull-point-contents hull (v 0.5d0 -0.5d0)) :contents-empty)
    (is (shake:hull-point-contents hull (v 0d0 0d0)) :contents-empty)
    (is (shake:hull-point-contents hull (v -0.01d0 -0.01d0)) :contents-solid)))

(subtest "recursive-hull-check"
  (let* ((segs (mapcar #'sbsp:linedef->sidedef *square-linedefs*))
         (hull (build-bsp segs)))
    (test-recursive-hull-check
     (v 0.5d0 0d0 -1d0) (v 0d0 0d0 -0.5d0)
     :fraction 1d0 :endpos p2 :normal nil)
    (test-recursive-hull-check
     (v 0.5d0 0d0 -0.5d0) (v -0.5d0 0d0 -0.5d0)
     :fraction 0.5d0 :endpos (v 0d0 0d0 -0.5d0) :normal (v 1d0 0d0 0d0))
    (test-recursive-hull-check
     (v -0.5d0 0d0 -1.5d0) (v -0.5d0 0d0 0.5d0)
     :fraction 0.25d0 :endpos (v -0.5d0 0d0 -1d0) :normal (v 0d0 0d0 -1d0))))

(subtest "pathological recursive-hull-check"
  (let* ((segs (mapcar #'sbsp:linedef->sidedef *square-linedefs*))
         (hull (build-bsp segs)))
    (let-mtrace
        (v 0.059055107469946466d0 0.5d0 -0.45058874753714245d0)
        (v -0.023507212569340574d0 0.5d0 -0.46189837857333405d0)
      (ok (not (= (shake:mtrace-fraction mtrace) 1d0)))
      (is (shake:mtrace-normal mtrace) (v 1d0 0d0 0d0) :test #'v=)
      (is (shake:hull-point-contents
           hull (v3->v2 (shake::mtrace-endpos mtrace)))
          :contents-empty))
    (let-mtrace
        (v 0.00654330262651126d0 0.5d0 -0.5254935858404913d0)
        (v -0.07672578372688232d0 0.5d0 -0.5222219345272358d0)
      (ok (not (= (shake:mtrace-fraction mtrace) 1d0)))
      (is (shake:mtrace-normal mtrace) (v 1d0 0d0 0d0) :test #'v=)
      (is (shake:hull-point-contents
           hull (v3->v2 (shake:mtrace-endpos mtrace)))
          :contents-empty))))

(finalize)
