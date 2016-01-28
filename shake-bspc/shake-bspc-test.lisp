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
