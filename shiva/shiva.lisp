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

(defun vdot (v1 v2 &aux (n (1- (array-dimension v1 0))))
  "Return the dot product of vectors V1 and V2."
  (tensor ((i 0 n)) () (:simple-arrays (v1 v2))
	  (sum i (* (v1 i) (v2 i)))))

(defun vnorm (v) (sqrt (vdot v v)))
(defun vdist (v1 v2) (vnorm (v- v1 v2)))
(defun vdistsq (v1 v2) (let ((d (v- v1 v2))) (vdot d d)))
(defun angle (v)
  "Return the angle in radians of vector V around the Z axis."
  (declare (type (simple-array double-float) v))
  (atan (aref v 1) (aref v 0)))
(defun direction (v1 v2)
  "Return the direction angle in radians between points V1 and V2"
  (angle (v- v2 v1)))
(defconstant rad->deg (/ 180 pi)
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
  "Create a vector of double-float and will it with ELEMENTS."
  (let* ((n (length elements))
	 (vector (make-array n :element-type 'double-float)))
    (iter (for i from 0)
	  (for elt in elements)
	  (setf (aref vector i) (coerce elt 'double-float)))
    vector))

(defun mat (&rest rows)
  "Construct a row major matrix as a 2D vector of double-float and fill it
with ROWS."
  (let* ((n (length rows))
         (m (length (car rows)))
         (matrix (make-array (list n m) :element-type 'double-float)))
    (iter (for i from 0)
          (for row in rows)
          (iter (for j from 0)
                (for elt in row)
                (setf (aref matrix i j) (coerce elt 'double-float))))
    matrix))

(defun midentity (n)
  "Construct a square identity matrix of dimensions N."
  (let ((out (make-array (list n n) :element-type 'double-float
                         :initial-element 0d0)))
    (dotimes (i n out)
      (setf (aref out i i) 1d0))))

(defmacro deftransform (row-index values col-index)
  "Iterate over first three rows and set given VALUES on given COL-INDEX."
  (with-gensyms (out)
    `(let ((,out (midentity 4)))
       ,@(iter (for i below 3) (for val in values)
               (collect `(let ((,row-index ,i))
                           (setf (aref ,out ,row-index ,col-index)
                                 (coerce ,val 'double-float)))))
       ,out)))

(defun translation (&key (x 0d0) (y 0d0) (z 0d0))
  "Construct a translation matrix."
  (deftransform i (x y z) 3))

(defun scale (&key (x 1d0) (y 1d0) (z 1d0))
  "Construct a scale matrix."
  (deftransform i (x y z) i))
