(in-package #:shake-bspc)

(defun v2 (x y)
  (make-array 2
              :element-type 'double-float
              :initial-contents (list (coerce x 'double-float)
                                      (coerce y 'double-float))))

(defun v- (v1 v2)
  (map '(vector double-float 2) #'- v1 v2))

(defun vdot (v1 v2)
  (reduce #'+ (map '(vector double-float 2) #'* v1 v2)))

(defstruct linedef
  (start (v2 0 0) :type (vector double-float 2) :read-only t)
  (end  (v2 0 0) :type (vector double-float 2) :read-only t))

(defun linedef-normal (linedef)
  (let ((vec (v- (linedef-end linedef) (linedef-start linedef))))
    (rotatef (aref vec 0) (aref vec 1))
    (setf (aref vec 1) (- (aref vec 1)))
    vec))

(defstruct lineseg
  "Segment of a line. Each line starts as a full segment, but may be split
  into multiple segments."
  (orig-line (make-linedef) :type linedef)
  (t-start 0d0 :type double-float)
  (t-end 1d0 :type double-float))

(defun line-intersect-ratio (splitter line)
  "Takes a SPLITTER and LINE linedefs. Returns numerator and denominator values
  for calculating the parameter T of the intersection."
  (declare (type linedef splitter line))
  (let* ((n (linedef-normal splitter))
         (-n (v- (v2 0 0) n))
         (l-vec (v- (linedef-end line) (linedef-start line)))
         (sl-vec (v- (linedef-start line) (linedef-start splitter)))
         (numer (vdot n sl-vec))
         (denom (vdot -n l-vec)))
    (values numer denom)))

(defun split-lineseg (lineseg t-split)
  "Split the given LINESEG at the given T-SPLIT parameter into a pair
  of LINESEG. Returns NIL if T-SPLIT does not split the line segment."
  (declare (type lineseg lineseg) (type double-float t-split))
  (when (and (> t-split (lineseg-t-start lineseg))
             (< t-split (lineseg-t-end lineseg)))
    (let ((l1 (copy-lineseg lineseg))
          (l2 (copy-lineseg lineseg)))
      (setf (lineseg-t-end l1) t-split)
      (setf (lineseg-t-start l2) t-split)
      (cons l1 l2))))

(defun build-bsp (rootseg linesegs)
  (declare (type lineseg rootseg) (type list linesegs))
  (if (null linesegs)
      rootseg
      (let ((splitter (lineseg-orig-line rootseg))
            (front nil)
            (back nil))
        (dolist (seg linesegs)
          (let ((line (lineseg-orig-line seg)))
            (multiple-value-bind (num den) (line-intersect-ratio splitter line)
              (if (= den 0d0)
                  ;; parallel lines
                  (if (< num 0d0)
                      (setf front (cons seg front))
                      (setf back (cons seg back)))
                  ;; lines intersect
                  (let ((splitted (split-lineseg seg (/ num den))))
                    (if (null splitted)
                        (if (< num 0d0)
                            (setf front (cons seg front))
                            (setf back (cons seg back)))
                        (if (< num 0d0)
                            (setf front (cons (car splitted) front)
                                  back (cons (cdr splitted) back))
                            (setf back (cons (car splitted) back)
                                  front (cons (cdr splitted) front)))))))))
        (list rootseg
              (if (null front) nil (build-bsp (car front) (cdr front)))
              (if (null back) nil (build-bsp (car back) (cdr back)))))))

(defparameter *test-linedefs*
  (list (make-linedef :start (v2 0 -2) :end (v2 0 5))
        (make-linedef :start (v2 -2 1) :end (v2 5 1))
        (make-linedef :start (v2 3 2) :end (v2 3 -2))))

(defun linedef->lineseg (linedef)
  (declare (type linedef linedef))
  (make-lineseg :orig-line linedef))
