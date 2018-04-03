;;;; Loading Wavefront OBJ 3D model files

(in-package #:shake)

;; token-type can be :eof :word :number :comment :slash :unknown
(declaim (inline make-token))
(defstruct token
  (type :unknown :type keyword :read-only t)
  (string "" :type string :read-only t)
  (leading-whitespace "" :type string :read-only t)
  (number 0 :type real :read-only t))

(declaim (inline whitespace-char-p))
(defun whitespace-char-p (c)
  (when (member c '(#\Tab #\Newline #\Linefeed #\Page #\Return #\Space)
                :test #'char=)
    c))

(declaim (inline word-char-p))
(defun word-char-p (c)
  (when (or (alpha-char-p c) (member c '(#\_ #\.) :test #'char=))
    c))

(defun read-number (stream &optional (leading-whitespace ""))
  (flet ((make-number (sign significand exponent)
           (if (= exponent 0)
               (* sign significand)
               (* sign significand (expt 10 exponent)))))
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
      (declare (type (or character keyword) c)
               (type boolean first-char-p floating-point-p)
               (type (integer -1 1) sign)
               (type integer significand)
               (type (integer * 0) exponent))
      (cond
        ((and first-char-p (char= c #\-)) (setf sign -1))
        ((let ((digit (digit-char-p c)))
           (when digit
             (when floating-point-p (incf exponent -1))
             (setf significand (+ (* 10 significand) digit)))))
        ((char= c #\.)
         (if floating-point-p
             (error "unexpected floating point")
             (setf floating-point-p t)))
        ((or (char= c #\/) (whitespace-char-p c))
         (unread-char c stream)
         (return (make-token :type :number
                             :number (make-number sign significand exponent)
                             :string string :leading-whitespace leading-whitespace)))
        (t (error "unexpected character ~S" c)))
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
      (t (error "unexpected word character ~S" c)))))

(defun read-comment (stream &optional (leading-whitespace ""))
  (do ((c (read-char stream nil :eof) (read-char stream nil :eof))
       (string (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
      ((or (eq c :eof) (member c '(#\Newline #\Linefeed #\Return) :test #'char=))
       (make-token :type :comment :string string :leading-whitespace leading-whitespace))
    (vector-push-extend c string)))

(defun read-token (stream)
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
      (t (error "unexpected character ~S" c)))))
