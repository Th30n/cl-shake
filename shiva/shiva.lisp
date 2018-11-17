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

(in-package #:shiva)
(declaim (optimize (speed 3)))

(deftype vec (size)
  `(simple-array double-float (,size)))

;; purpose-specific language compiler for matrix computation

(defmacro tensor (indices arrays (&key scalars simple-arrays) &body body)
  (let ((base-arrays `(,@(mapcar #'car arrays)
		       ,@(mapcar (lambda (x) (if (consp x) (car x) x)) simple-arrays)))
	(loop-collectors '((sum . sum) (min . minimize) (max . maximize))))
    (labels ((expand-array (dims &key (type 'double-float))
	       `(make-array ,dims :element-type ',type))
	     (expand-array-declaration (name dims &key (type 'double-float))
	       (declare (ignore dims))
	       `(declare (type (simple-array ,type) ,name)))
	     (let-arrays (code)
	       (let ((decls (iter (for array in arrays)
				  (collect `(,(car array) ,(apply #'expand-array (cdr array))))))
		     (new-array-decls (iter (for array in arrays)
					    (collect (apply #'expand-array-declaration array))))
		     (other-decls `(,@(iter (for scalar in scalars)
					    (collect `(declare (type double-float ,scalar))))
				    ,@(iter (for simple-array in simple-arrays)
					    (if (consp simple-array)
						(bind (((name &key type) simple-array))
						  (collect `(declare (type (simple-array ,type) ,name))))
						(collect `(declare (type (simple-array double-float) ,simple-array))))))))
		 (cond
		   ((or decls other-decls) `(let ,decls ,@new-array-decls ,@other-decls ,@code))
		   (t `(progn ,@code)))))
	     (expand-index (index)
	       (bind (((iname ifrom ito &key down) (assoc index indices)))
		 (if down
		     `((for ,iname from ,ifrom downto ,ito) (declare (type fixnum ,iname)))
		     `((for ,iname from ,ifrom to ,ito) (declare (type fixnum ,iname))))))
	     (expand-compare (term comparison)
	       (let ((iname (car (assoc (cadr term) indices))))
		 `(iter ,@(expand-index (cadr term))
		   (finding ,iname ,comparison ,@(expand-code (cddr term))))))
	     (expand-term (term)
	       (cond
		 ((not (consp term)) term)
		 ((member (car term) base-arrays)
		  `(aref ,@(expand-code term)))
		 ((eq (car term) '$)
		  `(iter ,@(expand-index (cadr term)) ,@(expand-code (cddr term))))
		 ((assoc (car term) loop-collectors)
		  (let ((name (cdr (assoc (car term) loop-collectors))))
		    (with-gensyms (acc)
		      `(iter
			,@(expand-index (cadr term))
			(,name (progn ,@(expand-code (cddr term))) into ,acc)
			(declare (type double-float ,acc))
			(finally (return ,acc))))))
		 ((eq (car term) 'min-index)
		  (expand-compare term 'minimizing))
		 ((eq (car term) 'max-index)
		  (expand-compare term 'maximizing))
		 (t (expand-code term))))
	     (expand-code (code)
	       (mapcar #'expand-term code)))
      (let-arrays (expand-code body)))))

;; basic matrix/vector ops

(defmacro defmacro-vector-apply (name op-name)
  `(defmacro ,name (&rest args)
    (let ((arg-vars (iter (for arg in args) (collect (gensym)))))
      (with-gensyms (i out)
	`(let ((n (1- (array-dimension ,(car args) 0)))
	       ,@(mapcar #'list arg-vars args))
	  (tensor ((,i 0 n)) ((,out (1+ n))) (:simple-arrays ,arg-vars)
	    ($ ,i (setf (,out ,i) (,',op-name ,@(mapcar (lambda (arg) `(aref ,arg ,i)) ,'arg-vars))))
	    ,out))))))

(defmacro-vector-apply v+ +)
(defmacro-vector-apply v- -)
(defmacro-vector-apply vmin min)
(defmacro-vector-apply vmax max)

(defun vscale (scalar vector &aux (n (1- (array-dimension vector 0))))
  "Scale a VECTOR by given SCALAR."
  (declare (type double-float scalar))
  (declare (type (simple-array double-float) vector))
  (tensor ((i 0 n)) ((out (1+ n))) (:scalars (scalar) :simple-arrays (vector))
	  ($ i (setf (out i) (* scalar (vector i))))
	  out))

(defun vtransform (matrix vector
		   &aux (m (1- (array-dimension matrix 0))) (n (1- (array-dimension matrix 1))))
  "Transform the given VECTOR using the given MATRIX."
  (tensor ((i 0 m) (j 0 n)) ((out (1+ m))) (:simple-arrays (vector matrix))
	  ($ i (setf (out i) (sum j (* (matrix i j) (vector j)))))
	  out))

(defun m* (m1 m2
	   &aux (n1 (1- (array-dimension m1 0))) (n2 (1- (array-dimension m1 1))) (n3 (1- (array-dimension m2 1))))
  (tensor ((i 0 n1) (j 0 n2) (k 0 n3)) ((out (list (1+ n1) (1+ n3)))) (:simple-arrays (m1 m2))
	  ($ i ($ k (setf (out i k) (sum j (* (m1 i j) (m2 j k))))))
	  out))

(declaim (inline v3dot))
(declaim (ftype (function ((vec 3) (vec 3)) double-float) v3dot))
(defun v3dot (v1 v2)
  (declare (type (vec 3) v1 v2))
  (+ (* (aref v1 0) (aref v2 0))
     (* (aref v1 1) (aref v2 1))
     (* (aref v1 2) (aref v2 2))))

(defun vdot (v1 v2 &aux (n (1- (array-dimension v1 0))))
  "Return the dot product of vectors V1 and V2."
  (tensor ((i 0 n)) () (:simple-arrays (v1 v2))
	  (sum i (* (v1 i) (v2 i)))))

(declaim (inline vnorm vnormalize vdist vdistsq))
(defun vnorm (v) (sqrt (the (double-float 0d0) (vdot v v))))
(defun vnormalize (v) (vscale (/ 1d0 (vnorm v)) v))
(defun vdist (v1 v2) (vnorm (v- v1 v2)))
(defun vdistsq (v1 v2) (let ((d (v- v1 v2))) (vdot d d)))
(defun angle (v)
  "Return the angle in radians of vector V around the Z axis."
  (declare (type (simple-array double-float) v))
  (atan (vy v) (vx v)))
(defun direction (v1 v2)
  "Return the direction angle in radians between points V1 and V2"
  (angle (v- v2 v1)))
(defconstant rad->deg (/ 180 pi)
  "Constant for conversion from radians to degrees.")
(defconstant deg->rad (/ pi 180)
  "Constant for conversion from degrees to radians.")
(defun deg-angle (v) (* rad->deg (angle v)))
(defun deg-direction (v1 v2) (* rad->deg (direction v1 v2)))

;; LR-decomposition, linear system solver and matrix inversion

(defun lr (m &aux (n (1- (array-dimension m 0))))
  (tensor
      ((i 0 n) (j 0 n) (col-right (1+ i) n) (col-swap 0 n) (row-down (1+ i) n) (row-search i n))
      ((a (list (1+ n) (1+ n))) (pivots (1+ n) :type fixnum))
      (:simple-arrays (m))
    ($ i (setf (pivots i) i))
    ($ i ($ j (setf (a i j) (m i j))))
    ($ i
       (let ((pivot (max-index row-search (a row-search i))))
	 (rotatef (pivots i) (pivots pivot))
	 ($ col-swap (rotatef (a pivot col-swap) (a i col-swap)))
	 (let ((scale (/ 1d0 (a i i))))
	   ($ row-down
	      (let ((row-value (a row-down i)))
		(setf (a row-down i) (* row-value scale))
		($ col-right (decf (a row-down col-right)
				   (* scale row-value (a i col-right)))))))))
    (values a pivots)))

(defun lr* (lr v pivots &aux (n (1- (array-dimension lr 0))))
  (tensor
      ((i 0 n) (j i n) (k 0 (1- i)))
      ((tmp (1+ n)) (out (1+ n)))
      (:simple-arrays (lr v (pivots :type fixnum)))
   ($ i (setf (tmp i) (sum j (* (lr i j) (v j)))))
   ($ i (setf (out (pivots i)) (+ (tmp i) (sum k (* (lr i k) (tmp k))))))
   out))


(defun lr-solve (lr b pivots &aux (n (1- (array-dimension lr 0))))
  (tensor
      ((i 0 n) (ii n 0 :down t) (j (1+ ii) n) (k 0 (1- i)))
      ((tmp (1+ n)))
      (:simple-arrays (lr b (pivots :type fixnum)))
    ($ i (setf (tmp i) (b (pivots i))))
    ($ i (decf (tmp i) (sum k (* (lr i k) (tmp k)))))
    ($ ii
       (decf (tmp ii) (sum j (* (lr ii j) (tmp j))))
       (setf (tmp ii) (/ (tmp ii) (lr ii ii))))
    tmp))

(defun lr-inverse (lr pivots &aux (n (1- (array-dimension lr 0))))
  (tensor
      ((col 0 n) (ip 0 (1- pcol)) (i (1+ pcol) n) (ii n 0 :down t) (j (1+ ii) n) (k pcol (1- i)))
      ((ipivots (1+ n) :type fixnum) (tmp (1+ n)) (out (list (1+ n) (1+ n))))
      (:simple-arrays (lr (pivots :type fixnum)))
    ($ col (setf (ipivots (pivots col)) col))
    ($ col
       (let ((pcol (ipivots col)))
	 ($ ip (setf (tmp ip) 0d0))
	 (setf (tmp pcol) 1d0)
	 ($ i (setf (tmp i) (- (sum k (* (lr i k) (tmp k)))))))
       ($ ii
	  (decf (tmp ii) (sum j (* (lr ii j) (tmp j))))
	  (setf (tmp ii) (/ (tmp ii) (lr ii ii)))
	  (setf (out ii col) (tmp ii))))
   out))

(defun v (&rest elements)
  "Create a vector of double-float and fill it with ELEMENTS."
  (declare (dynamic-extent elements))
  (let* ((n (length elements))
	 (vector (make-array n :element-type 'double-float)))
    (iter (for i from 0)
          (declare (type fixnum i))
	  (for elt in elements)
	  (setf (aref vector i) (coerce elt 'double-float)))
    vector))

(defmacro vx (vector) `(aref ,vector 0))
(defmacro vy (vector) `(aref ,vector 1))
(defmacro vz (vector) `(aref ,vector 2))
(defmacro vw (vector) `(aref ,vector 3))

(defmacro define-swizzle (&rest components)
  "Define a swizzle operator for vector using given COMPONENTS. Inspired by
  GLSL swizzling."
  (flet ((comp->accessor (comp)
           (ecase comp
             ((x r) 'vx)
             ((y g) 'vy)
             ((z b) 'vz)
             ((w a) 'vw))))
    (let ((accessors (mapcar #'comp->accessor components))
          (op-name (apply #'symbolicate 'v components)))
      `(defun ,op-name (vector)
         (v ,@(mapcar (lambda (accessor) `(,accessor vector)) accessors))))))

(defmacro define-swizzles (&rest component-lists)
  "Generate swizzle operators using a Cartesian product of COMPONENT-LISTS."
  `(progn
     ,@(apply #'map-product (lambda (&rest components)
                              `(define-swizzle ,@components))
              component-lists)))

(define-swizzles (x y) (x y))
(define-swizzles (x y z) (x y z) (x y z))
(define-swizzles (r g b) (r g b) (r g b))

(defun v3->v2 (v)
  "Convert VEC 3 to VEC 2 by dropping the y axis."
  (declare (type (vec 3) v))
  (v (vx v) (vz v)))

(defun v2->v3 (v &optional (y 0d0))
  "Convert VEC 2 to VEC 3 by adding the y axis, set to 0."
  (declare (type (vec 2) v))
  (v (vx v) y (vy v)))

(deftype mat (n &optional m)
  `(simple-array double-float (,n ,(if (null m) n m))))

(defun mat (&rest rows)
  "Construct a row major matrix as a 2D vector of double-float and fill it
with ROWS."
  (declare (dynamic-extent rows))
  (let* ((n (list-length rows))
         (m (length (car rows)))
         (matrix (make-array (list n m) :element-type 'double-float)))
    (iter (for i from 0)
          (for row in rows)
          (iter (for j from 0)
                (for elt in row)
                (setf (aref matrix i j) (coerce elt 'double-float))))
    matrix))

(defun mat-row (matrix row-index)
  "Return a matrix row as a vector."
  (destructuring-bind (rows cols) (array-dimensions matrix)
    (declare (ignore rows))
    (apply #'v
           (iter (for col-index below cols)
                 (collect (aref matrix row-index col-index))))))

(defun mat-col (matrix col-index)
  "Return a matrix column as a vector."
  (destructuring-bind (rows cols) (array-dimensions matrix)
    (declare (ignore cols))
    (apply #'v
           (iter (for row-index below rows)
                 (collect (aref matrix row-index col-index))))))

(declaim (inline midentity4))
(defun midentity4 ()
  "Construct a 4x4 identity matrix."
  (let ((out (make-array '(4 4) :element-type 'double-float
                         :initial-element 0d0)))
    (setf (aref out 0 0) 1d0)
    (setf (aref out 1 1) 1d0)
    (setf (aref out 2 2) 1d0)
    (setf (aref out 3 3) 1d0)
    out))

(defmacro deftransform (row-index values col-index)
  "Iterate over first three rows and set given VALUES on given COL-INDEX."
  (with-gensyms (out)
    `(let ((,out (midentity4)))
       ,@(iter (for i below 3) (for val in values)
               (collect `(let ((,row-index ,i))
                           (setf (aref ,out ,row-index ,col-index)
                                 (coerce ,val 'double-float)))))
       ,out)))

(declaim (inline translation))
(defun translation (&key (x 0d0) (y 0d0) (z 0d0))
  "Construct a translation matrix."
  (deftransform i (x y z) 3))

(declaim (inline scale))
(defun scale (&key (x 1d0) (y 1d0) (z 1d0))
  "Construct a scale matrix."
  (deftransform i (x y z) i))

(defun ortho (left right bottom top near far)
  "Create orthographic projection from 6 clipping planes."
  (check-type left real)
  (check-type right real)
  (check-type bottom real)
  (check-type top real)
  (check-type near real)
  (check-type far real)
  (let ((rml (coerce (- right left) 'double-float))
        (tmb (coerce (- top bottom) 'double-float))
        ;; negate near and far, to switch OpenGL view direction.
        (nmf (coerce (- near far) 'double-float))
        (rpl (coerce (+ right left) 'double-float))
        (tpb (coerce (+ top bottom) 'double-float))
        (fpn (coerce (+ far near) 'double-float)))
    (declare (type double-float rml tmb nmf rpl tpb fpn))
    (mat (list (/ 2d0 rml) 0d0 0d0 (- (/ rpl rml)))
         (list 0d0 (/ 2d0 tmb) 0d0 (- (/ tpb tmb)))
         (list 0d0 0d0 (/ 2d0 nmf) (/ fpn nmf))
         (list 0d0 0d0 0d0 1d0))))

(defun perspective (fovy aspect near far)
  "Create a perspective projection with symmetric view frustum."
  (check-type fovy (real 0))
  (check-type aspect (real 0))
  (check-type near (real 0))
  (check-type far (real 0))
  (let* ((tan-half-fov (tan (/ (coerce fovy 'double-float) 2d0)))
         (x (/ 1d0 (* (coerce aspect 'double-float) tan-half-fov)))
         (y (/ 1d0 tan-half-fov))
         (fmn (coerce (- far near) 'double-float))
         (fpn (coerce (+ far near) 'double-float)))
    (declare (type double-float tan-half-fov x y fmn fpn))
    (mat (list x 0d0 0d0 0d0)
         (list 0d0 y 0d0 0d0)
         (list 0d0 0d0 (- (/ fpn fmn)) (/ (* -2d0 far near) fmn))
         (list 0d0 0d0 -1d0 0d0))))

(declaim (inline double=))
(defun double= (a b &key (epsilon 1d-9) (rel-epsilon double-float-epsilon))
  "Compare floating points using epsilon difference and fallback to relative
epsilon. Doesn't handle infinities."
  (declare (type double-float a b epsilon rel-epsilon))
  (let ((diff (abs (- a b)))
        (max (max (abs a) (abs b))))
    (declare (type double-float diff max))
    (or (<= diff epsilon) ;; Needed when near zero.
        (<= diff (* max rel-epsilon)))))

(declaim (inline double>))
(defun double> (a b)
  (declare (type double-float a b))
  (and (not (double= a b)) (> a b)))

(declaim (inline double>=))
(defun double>= (a b)
  (declare (type double-float a b))
  (or (double= a b) (> a b)))

(declaim (inline double<))
(defun double< (a b)
  (declare (type double-float a b))
  (and (not (double= a b)) (< a b)))

(declaim (inline double<=))
(defun double<= (a b)
  (declare (type double-float a b))
  (or (double= a b) (< a b)))

(defun v= (v1 v2 &key (test #'double=))
  "Perform a comparison of two vectors."
  (let ((n1 (array-dimension v1 0))
        (n2 (array-dimension v2 0)))
    (when (= n1 n2)
      (iter (for i below n1)
            (always (funcall test (aref v1 i) (aref v2 i)))))))

(defun vcross (v1 v2)
  "Get the cross product of two vectors."
  (tensor ((i 0 2)) ((out 3)) (:simple-arrays (v1 v2))
    ($ i (let* ((j (mod (1+ i) 3))
                (k (mod (1+ j) 3)))
           (setf (out i) (- (* (v1 j) (v2 k)) (* (v1 k) (v2 j))))))
    out))

(deftype quat () `(cons (vec 3) double-float))

(defun q (x y z w)
  "Construct a quaternion as a cons of vector X Y Z and W."
  (cons (v x y z) (coerce w 'double-float)))

(defmacro qx (quaternion) `(vx (car ,quaternion)))
(defmacro qy (quaternion) `(vy (car ,quaternion)))
(defmacro qz (quaternion) `(vz (car ,quaternion)))
(defmacro qw (quaternion) `(cdr ,quaternion))

(defun q+ (q1 q2)
  "Add two quaternions."
  (cons (v+ (car q1) (car q2)) (+ (cdr q1) (cdr q2))))

(defun q* (q1 q2)
  "Multiply two quaternions."
  (let ((v1 (car q1))
        (w1 (cdr q1))
        (v2 (car q2))
        (w2 (cdr q2)))
    (cons (v+ (vcross v1 v2) (vscale w2 v1) (vscale w1 v2))
          (- (* w1 w2) (vdot v1 v2)))))

(defun qconj (q)
  "Conjugate a quaternion."
  (cons (vscale -1d0 (car q)) (cdr q)))

(defun qrotation (axis rad-angle)
  "Construct a quaternion for rotation of RAD-ANGLE around AXIS vector."
  (let ((half-angle (/ (coerce rad-angle 'double-float) 2d0)))
    (cons (vscale (sin half-angle) axis) (cos half-angle))))

(defun vrotate (quaternion vector)
  "Rotate a 3 component VECTOR by given QUATERNION."
  (declare (type quat quaternion) (type (vec 3) vector))
  (vnormalize
   (car (q* (q* quaternion (q (vx vector) (vy vector) (vz vector) 0))
            (qconj quaternion)))))

(defun q->mat (quaternion)
  "Construct a rotation matrix from given unit QUATERNION."
  (let ((x (qx quaternion))
        (y (qy quaternion))
        (z (qz quaternion))
        (w (qw quaternion)))
    (mat (list (- 1 (* 2 (+ (* y y) (* z z)))) (* 2 (- (* x y) (* w z))) (* 2 (+ (* x z) (* w y))) 0)
         (list (* 2 (+ (* x y) (* w z))) (- 1 (* 2 (+ (* x x) (* z z)))) (* 2 (- (* y z) (* w x))) 0)
         (list (* 2 (- (* x z) (* w y))) (* 2 (+ (* y z) (* w x))) (- 1 (* 2 (+ (* x x) (* y y)))) 0)
         (list 0 0 0 1))))

(defun rotation (axis rad-angle)
  "Construct a rotation matrix for RAD-ANGLE around AXIS vector."
  (q->mat (qrotation axis rad-angle)))

(defun q->euler-x (quaternion)
  "Extract the euler angle about the x-axis (roll) from QUATERNION."
  (let ((qx (qx quaternion))
        (qy (qy quaternion))
        (qz (qz quaternion))
        (qw (qw quaternion)))
    (atan (* 2d0 (+ (* qw qx) (* qy qz)))
          (- 1d0 (* 2d0 (+ (* qx qx) (* qy qy)))))))
