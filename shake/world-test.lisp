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
                #:vnormalize
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
          (mtrace (shake:clip-hull hull p1 p2)))
     ,@body))

(defmacro test-clip-hull (p1 p2 &key fraction endpos normal start-solid-p)
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
                     (and b (v= a b)))))
     (is (shake:mtrace-start-solid-p mtrace) ,start-solid-p)))

(subtest "hull-point-contents"
  (let* ((surfs (mapcar #'sbsp:linedef->sidedef *square-linedefs*))
         (hull (build-bsp surfs)))
    (is (shake:hull-point-contents hull (v -0.5d0 -0.5d0)) :contents-solid)
    (is (shake:hull-point-contents hull (v 0.5d0 -0.5d0)) :contents-empty)
    (is (shake:hull-point-contents hull (v 0d0 0d0)) :contents-empty)
    (is (shake:hull-point-contents hull (v -0.01d0 -0.01d0)) :contents-solid)))

(subtest "clip-hull"
  (let* ((segs (mapcar #'sbsp:linedef->sidedef *square-linedefs*))
         (hull (build-bsp segs)))
    (test-clip-hull
     (v 0.5d0 0d0 -1d0) (v 0d0 0d0 -0.5d0)
     :fraction 1d0 :endpos p2 :normal nil)
    (test-clip-hull
     (v 0.5d0 0d0 -0.5d0) (v -0.5d0 0d0 -0.5d0)
     :fraction 0.5d0 :endpos (v 0d0 0d0 -0.5d0) :normal (v 1d0 0d0 0d0))
    (test-clip-hull
     (v -0.5d0 0d0 -1.5d0) (v -0.5d0 0d0 0.5d0)
     :fraction 0.25d0 :endpos (v -0.5d0 0d0 -1d0) :normal (v 0d0 0d0 -1d0))
    (test-clip-hull
     (v -0.5d0 0d0 0d0) (v -0.5d0 0d0 -0.5d0)
     :fraction 0d0 :endpos (v -0.5d0 0d0 0d0) :normal (v 0d0 0d0 1d0))
    (test-clip-hull
     (v -0.5d0 0d0 -0.01d0) (v -0.5d0 0d0 -0.5d0)
     :fraction 0d0 :endpos (v -0.5d0 0d0 -0.01d0) :start-solid-p t)))

(subtest "pathological clip-hull"
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

(subtest "ray-oobb-intersection"
  (let ((oobb (shake::make-oobb
               :center (v 0 0 0)
               :half-lengths (v 1 1 1)
               :directions (make-array 3 :initial-contents
                                       ;; Axis aligned bounds
                                       (list (v 1 0 0) (v 0 1 0) (v 0 0 1))))))
    (ok (not (shake::ray-oobb-intersect (v 1.1 0 0) (v 1 0 0) oobb)))
    (is (shake::ray-oobb-intersect (v 1.1 0 0) (v -1 0 0) oobb)
        #.(shiva-float 0.10000002384185791d0) :test #'float=)
    (is (shake::ray-oobb-intersect (v 1.1 0 0) (v -1 0 0) oobb
                                   :ray-length #.(shiva-float 0.25))
        #.(shiva-float 0.10000002384185791d0) :test #'float=)
    (ok (not (shake::ray-oobb-intersect (v 1.1 0 0) (v -1 0 0) oobb
                                        :ray-length #.(shiva-float 0.1)))))
  (let ((oobb (shake::make-oobb
               :center (v 0 0 0)
               :half-lengths (v 0.5 0.5 0.5)
               :directions (make-array 3 :initial-contents
                                       ;; Rotated for 45 degrees around Z-axis
                                       (list (vnormalize (v 1 -1 0))
                                             (vnormalize (v 1 1 0))
                                             (v 0 0 1))))))
    (ok (not (shake::ray-oobb-intersect (v 1.1 0 0) (v 1 0 0) oobb)))
    (is (shake::ray-oobb-intersect (v 1 2 0) (vnormalize (v -1 -2 0)) oobb)
        #.(shiva-float 1.7090217008050597d0) :test #'float=)
    (is (shake::ray-oobb-intersect (v 1 2 0) (vnormalize (v -1 -2 0)) oobb
                                   :ray-length #.(shiva-float 2.0))
        #.(shiva-float 1.7090217008050597d0) :test #'float=)
    (ok (not (shake::ray-oobb-intersect (v 1 2 0) (vnormalize (v -1 -2 0)) oobb
                                        :ray-length #.(shiva-float 1.5))))))

(finalize)
