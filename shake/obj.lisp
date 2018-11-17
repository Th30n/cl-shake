;;;; Loading Wavefront OBJ 3D model files

(in-package #:shake.obj)

(alexandria:define-constant +cube+
"
# Bottom vertices
v 1 -1 -1
v 1 -1 1
v -1 -1 1
v -1 -1 -1

# Top vertices
v 1 1 -1
v 1 1 1
v -1 1 1
v -1 1 -1

vn 0 -1 0
vn 0 1 0
vn 1 -0 0
vn 0 -0 1
vn -1 -0 -0
vn 0 0 -1

vt 0 0
vt 1 0
vt 1 1
vt 0 1

# Front face (+Z towards camera)
f 6/2/4 3/4/4 2/3/4
f 6/2/4 7/1/4 3/4/4
# Back face
f 1/3/6 8/1/6 5/4/6
f 1/3/6 4/2/6 8/1/6
# Top face (+Y)
f 8/2/2 6/4/2 5/3/2
f 8/2/2 7/1/2 6/4/2
# Bottom face
f 2/2/1 4/4/1 1/3/1
f 2/2/1 3/1/1 4/4/1
# Right face (+X)
f 5/3/3 2/1/3 1/4/3
f 5/3/3 6/2/3 2/1/3
# Left face
f 3/1/5 8/3/5 4/2/5
f 3/1/5 7/4/5 8/3/5
"
  :test #'string=
  :documentation "OBJ string defining a cube, counter clockwise.")

(declaim (inline make-token))
(defstruct token
  (type :unknown :read-only t
        :type (member :eof :word :number :comment :slash :unknown))
  (string "" :type string :read-only t)
  (leading-whitespace "" :type string :read-only t)
  (number 0 :type (or double-float fixnum) :read-only t))

(declaim (inline make-token-error))
(defstruct token-error
  (string "" :type string :read-only t)
  (message "" :type string :read-only t))

(declaim (inline make-tokenizer))
(defstruct tokenizer
  (stream nil :type stream :read-only t)
  (next-token nil :type (or null token token-error)))

(declaim (inline whitespace-char-p))
(defun whitespace-char-p (c)
  (when (member c '(#\Tab #\Newline #\Linefeed #\Page #\Return #\Space)
                :test #'char=)
    c))

(declaim (inline word-char-p))
(defun word-char-p (c)
  (when (or (alphanumericp c) (member c '(#\_ #\.) :test #'char=))
    c))

(defun read-number (stream &optional (leading-whitespace ""))
  (flet ((make-number (sign significand exponent)
           (if (= exponent 0)
               (let ((int (* sign significand)))
                 (if (<= most-negative-fixnum int most-positive-fixnum)
                     (coerce int 'fixnum)
                     (error "Integer '~S' doesn't fit in fixnum" int)))
               (coerce (* sign significand (expt 10 exponent)) 'double-float))))
    (do ((c (read-char stream nil :eof) (read-char stream nil :eof))
         (string (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
         (first-char-p t nil)
         (floating-point-p nil)
         (sign 1)
         (significand 0)
         (exponent 0))
        ((eq c :eof) (make-token :type (if first-char-p :eof :number)
                                 :number (make-number sign significand exponent)
                                 :string string :leading-whitespace leading-whitespace))
      (declare (type (integer 0) significand)
               (type (integer * 0) exponent))
      (cond
        ((and first-char-p (char= c #\-)) (setf sign -1))
        ((let ((digit (digit-char-p c)))
           (when digit
             (when floating-point-p (incf exponent -1))
             (setf significand (+ (* 10 significand) digit)))))
        ((char= c #\.)
         (if floating-point-p
             (return (make-token-error :string (format nil "~A~A" string c)
                                       :message "unexpected floating point"))
             (setf floating-point-p t)))
        ((or (char= c #\/) (whitespace-char-p c))
         (unread-char c stream)
         (return (make-token :type :number
                             :number (make-number sign significand exponent)
                             :string string :leading-whitespace leading-whitespace)))
        (t (return (make-token-error :string (format nil "~A~A" string c)
                                     :message (format nil "unexpected character ~S" c)))))
      (vector-push-extend c string))))

(defun read-word (stream &optional (leading-whitespace ""))
  (do ((c (read-char stream nil :eof) (read-char stream nil :eof))
       (string (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
      ((eq c :eof) (make-token :type :word :string string
                               :leading-whitespace leading-whitespace))
    (cond
      ((word-char-p c)
       (vector-push-extend c string))
      ((whitespace-char-p c)
       (unread-char c stream)
       (return (make-token :type :word :string string
                           :leading-whitespace leading-whitespace)))
      (t (return (make-token-error :string (format nil "~A~A" string c)
                                   :message (format nil "unexpected word character ~S" c)))))))

(defun read-comment (stream &optional (leading-whitespace ""))
  (do ((c (read-char stream nil :eof) (read-char stream nil :eof))
       (string (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
      ((or (eq c :eof) (member c '(#\Newline #\Linefeed #\Return) :test #'char=))
       (make-token :type :comment :string string :leading-whitespace leading-whitespace))
    (vector-push-extend c string)))

(defun read-token (tokenizer)
  (let ((token (tokenizer-next-token tokenizer)))
    (when token
      (setf (tokenizer-next-token tokenizer) nil)
      (return-from read-token token)))
  (let ((stream (tokenizer-stream tokenizer)))
    (do ((c (read-char stream nil :eof) (read-char stream nil :eof))
         (leading-whitespace (make-array 0 :element-type 'character
                                         :adjustable t :fill-pointer 0)))
        ((eq c :eof) (make-token :type :eof :leading-whitespace leading-whitespace))
      (when (char= c #\\)
        (setf c (read-char stream nil :eof))
        (when (eq c :eof)
          (return (make-token :type :eof :leading-whitespace leading-whitespace))))
      (cond
        ((or (digit-char-p c) (char= c #\-))
         (unread-char c stream)
         (return (read-number stream leading-whitespace)))
        ((word-char-p c)
         (unread-char c stream)
         (return (read-word stream leading-whitespace)))
        ((char= c #\#)
         ;; Don't unread, since `read-comment' does no checking and simply reads
         ;; the whole line.
         (return (read-comment stream leading-whitespace)))
        ((char= c #\/)
         (return (make-token :type :slash :string "/" :leading-whitespace leading-whitespace)))
        ((whitespace-char-p c)
         (vector-push-extend c leading-whitespace))
        (t (return (make-token-error :string (string c)
                                     :message (format nil "unexpected character ~S" c))))))))

(declaim (inline peek-token))
(defun peek-token (tokenizer)
  (setf (tokenizer-next-token tokenizer) (read-token tokenizer)))


;;; Vertex Data

;; geometric vertex -- v x y z (w 1.0, required for rational curve and surface)
;; texture vertex -- vt u (v 0.0) (w 0.0)
;; vertex normal -- vn x y z

;;; Element Data

;; Each element indexes into vertex data, indexing starts from 1. Negative
;; index means immediately before the element in the file.
;; points -- p v1 (...)
;; lines -- l v1(/vt1) *2 (...)
;; faces -- f v1(/(vt1)(/vn1)) *3 (...)

;;; Grouping

;; Each grouping statement sets the state until the next one. Elements may
;; belong to multiple groups if multiple names are specified in the grouping
;; statement.
;; group name -- g name (...), default is the default group
;; object name -- o name, optional statement (may be skipped)

(declaim (inline make-vertex))
(defstruct vertex
  (type :geometric :read-only t :type (member :geometric :texture :normal))
  (val nil :type (vec 4) :read-only t))

(declaim (inline make-vref))
(defstruct vref
  "Reference to vertex data. Indexes are normalized to (0, vertex-data-size - 1)."
  (geometric nil :type fixnum :read-only t)
  (texture nil :type (or null fixnum) :read-only t)
  (normal nil :type (or null fixnum) :read-only t))

(declaim (inline make-element))
(defstruct element
  (type nil :read-only t :type (member :point :line :face))
  (points nil :type (simple-array vref (*)) :read-only t))

(declaim (inline make-obj))
(defstruct obj
  (geometric-vertices
   (make-array 0 :element-type 'vertex :adjustable t :fill-pointer 0)
   :type (vector vertex) :read-only t)
  (texture-vertices
   (make-array 0 :element-type 'vertex :adjustable t :fill-pointer 0)
   :type (vector vertex) :read-only t)
  (normal-vertices
   (make-array 0 :element-type 'vertex :adjustable t :fill-pointer 0)
   :type (vector vertex) :read-only t)
  (element-data (make-array 0 :element-type 'element :adjustable t :fill-pointer 0)
                :type (vector element) :read-only t))

(defun read-geometric-vertex (tokenizer)
  (flet ((expect-number ()
           (let ((token (read-token tokenizer)))
             (when (or (token-error-p token)
                       (not (eq (token-type token) :number)))
               (error "Expected number, got ~S" token))
             (token-number token)))
         (maybe-number (default-value)
           (let ((token (peek-token tokenizer)))
             (if (and (token-p token) (eq (token-type token) :number))
                 (token-number (read-token tokenizer))
                 default-value))))
    (make-vertex :type :geometric
                 :val (v (expect-number) (expect-number) (expect-number) (maybe-number 1.0d0)))))

(defun read-texture-vertex (tokenizer)
  (flet ((expect-number ()
           (let ((token (read-token tokenizer)))
             (when (or (token-error-p token)
                       (not (eq (token-type token) :number)))
               (error "Expected number, got ~S" token))
             (token-number token)))
         (maybe-number (default-value)
           (let ((token (peek-token tokenizer)))
             (if (and (token-p token) (eq (token-type token) :number))
                 (token-number (read-token tokenizer))
                 default-value))))
    (make-vertex :type :texture
                 :val (v (expect-number) (maybe-number 0.0d0) (maybe-number 0.0d0) 0d0))))

(defun read-normal-vertex (tokenizer)
  (flet ((expect-number ()
           (let ((token (read-token tokenizer)))
             (when (or (token-error-p token)
                       (not (eq (token-type token) :number)))
               (error "Expected number, got ~S" token))
             (token-number token))))
    (make-vertex :type :normal
                 :val (v (expect-number) (expect-number) (expect-number) 0d0))))

(defun read-faces (tokenizer obj)
  (labels ((expect-fixnum ()
             (let ((token (read-token tokenizer)))
               (if (and (token-p token) (eq (token-type token) :number)
                        (typep (token-number token) 'fixnum))
                   (token-number token)
                   (error "Expected fixnum, got ~S" token))))
           (normalize-index (index size)
             (declare (type (or null fixnum) index))
             (when index
               (if (minusp index)
                   (setf index (+ size index))
                   ;; Use indexing starting from 0.
                   (incf index -1))
               (if (<= 0 index (1- size))
                   index
                   (error "Index '~S' is out of bounds for size '~S'" index size))))
           (read-point ()
             (let ((geometric-ref (expect-fixnum))
                   (texture-ref nil)
                   (normal-ref nil))
               (when (and (not (token-error-p (peek-token tokenizer)))
                          (eq (token-type (peek-token tokenizer)) :slash))
                 (when (< 0 (length (token-leading-whitespace (read-token tokenizer))))
                   (error "There shouldn't be any whitespace before '/'"))
                 (cond
                   ((token-error-p (peek-token tokenizer))
                    (error "Expected '/' or an integer, got ~S" (read-token tokenizer)))
                   ;; Maybe read texture
                   ((eq (token-type (peek-token tokenizer)) :number)
                    (setf texture-ref (expect-fixnum))
                    ;; Maybe read a normal after texture
                    (when (eq (token-type (peek-token tokenizer)) :slash)
                      (read-token tokenizer) ;; skip slash
                      (setf normal-ref (expect-fixnum))))
                   ;; Missing texture, but maybe read normal
                   ((eq (token-type (peek-token tokenizer)) :slash)
                    (read-token tokenizer) ;; skip slash
                    (setf normal-ref (expect-fixnum)))
                   (t (error "Expected '/' or an integer, got ~S" (read-token tokenizer)))))
               (make-vref :geometric (normalize-index geometric-ref (length (obj-geometric-vertices obj)))
                          :texture (normalize-index texture-ref (length (obj-texture-vertices obj)))
                          :normal (normalize-index normal-ref (length (obj-normal-vertices obj)))))))
    (let ((points
           (loop while (and (token-p (peek-token tokenizer))
                            (eq (token-type (peek-token tokenizer)) :number))
              collect (read-point))))
      ;; TODO: Check that points have uniform references to texture or normals
      (vector-push-extend (make-element :type :face :points (apply #'vector points))
                          (obj-element-data obj)))))

(defun read-obj (stream)
  (let ((tokenizer (make-tokenizer :stream stream))
        (obj (make-obj)))
    (do ((token (read-token tokenizer) (read-token tokenizer)))
        ((and (token-p token) (eq (token-type token) :eof)) obj)
      (when (token-error-p token) (return token))
      (case (token-type token)
        (:comment nil)
        (:word
         (cond
           ((string= "v" (token-string token))
            (vector-push-extend (read-geometric-vertex tokenizer)
                                (obj-geometric-vertices obj)))
           ((string= "vt" (token-string token))
            (vector-push-extend (read-texture-vertex tokenizer)
                                (obj-texture-vertices obj)))
           ((string= "vn" (token-string token))
            (vector-push-extend (read-normal-vertex tokenizer)
                                (obj-normal-vertices obj)))
           ((string= "f" (token-string token))
            (read-faces tokenizer obj))
           (t nil ;; (format t "Ignoring ~S~%" token)
            )))
        (otherwise (error "Unexpected token ~S" token))))))
