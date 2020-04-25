;;;; Loading Autodesk Kaydara Filmbox (FBX) 3D model files

(in-package #:shake.fbx)

(declaim (inline float32-ieee-754-sign))
(defun float32-ieee-754-sign (binary)
  "Return sign bit value from BINARY representation of a float."
  (check-type binary (unsigned-byte 32))
  (ldb (byte 1 31) binary))

(declaim (inline float32-ieee-754-exponent))
(defun float32-ieee-754-exponent (binary)
  "Return exponent value after offseting by 127 from the BINARY representation"
  (check-type binary (unsigned-byte 32))
  (- (ldb (byte 8 23) binary) 127))

(declaim (inline float32-ieee-754-mantissa))
(defun float32-ieee-754-mantissa (binary)
  "Return value mantissa bits from BINARY representation of a float."
  (check-type binary (unsigned-byte 32))
  (ldb (byte 23 0) binary))

(defun decode-float32-iee-754 (binary)
  "Return either a SINGLE-FLOAT, :NAN, :POSITIVE-INFINITY or
  :NEGATIVE-INFINITY by decoding the BINARY IEEE 754 representation."
  (declare (optimize (speed 3) (space 3)))
  (check-type binary (unsigned-byte 32))
  (let ((mantissa (float32-ieee-754-mantissa binary))
        (exponent (float32-ieee-754-exponent binary))
        (sign (float32-ieee-754-sign binary)))
    (when (= 128 exponent)
      ;; Handle infinity or NaN
      (return-from decode-float32-iee-754
        (cond ((/= 0 mantissa) :nan)
              ((zerop sign) :positive-infinity)
              (t :negative-infinity))))
    (if (= -127 exponent)
        ;; Denormalized
        (setf exponent -126)
        ;; Regular
        (setf (ldb (byte 1 23) mantissa) 1))
    (scale-float (float (if (zerop sign) mantissa (- mantissa)))
                 (- exponent 23))))

(defun decode-float64-iee-754 (binary)
  "Return either a DOUBLE-FLOAT, :NAN, :POSITIVE-INFINITY or
  :NEGATIVE-INFINITY by decoding the BINARY IEEE 754 representation."
  (declare (optimize (speed 3) (space 3)))
  (check-type binary (unsigned-byte 64))
  (let ((mantissa (ldb (byte 52 0) binary))
        (exponent (- (ldb (byte 11 52) binary) 1023))
        (sign (ldb (byte 1 63) binary)))
    (when (= 1024 exponent)
      ;; Handle infinity or NaN
      (return-from decode-float64-iee-754
        (cond ((/= 0 mantissa) :nan)
              ((zerop sign) :positive-infinity)
              (t :negative-infinity))))
    (if (= -1023 exponent)
        ;; Denormalized
        (setf exponent -1022)
        ;; Regular
        (setf (ldb (byte 1 52) mantissa) 1))
    (scale-float (float (if (zerop sign) mantissa (- mantissa)) 1d0)
                 (- exponent 52))))

(defun fbx-read-header-from-stream (stream)
  "Read the header from byte STREAM and return FBX version."
  (declare (optimize (speed 3) (space 3)))
  (check-type stream stream)
  (assert (input-stream-p stream))
  (assert (equalp (stream-element-type stream) '(unsigned-byte 8)))
  (loop for c across "Kaydara FBX Binary  "
        unless (= (char-code c) (read-byte stream))
          do (error "Not a valid FBX file!"))
  (unless (and (= #x00 (read-byte stream))
               (= #x1a (read-byte stream))
               (= #x00 (read-byte stream)))
    (error "Not a valid FBX file!"))
  (uiop:read-little-endian stream 4))

(defstruct node-header
  (end-offset 0 :type (unsigned-byte 32))
  (num-properties 0 :type (unsigned-byte 32))
  ;; byte length
  (property-list-len 0 :type (unsigned-byte 32))
  (name-len 0 :type (unsigned-byte 8))
  (name "" :type string)
  (properties nil)
  (nested-nodes nil))

(defun read-array-property-from-stream (stream element-type element-reader)
  (let* ((len (uiop:read-little-endian stream 4))
         (encoding (uiop:read-little-endian stream 4))
         (compressed-len (uiop:read-little-endian stream 4))
         (array (make-array len :element-type element-type)))
    (case encoding
      (0
       (loop for i from 0 below len
             do (setf (aref array i) (funcall element-reader stream))))
      (1 ;; zip compressed
       ;; TODO:
       (loop repeat compressed-len do (read-byte stream)))
      (t (error "Unsupported FBX array property encoding!")))
    array))

(defun read-property-from-stream (stream)
  (let ((type-code (code-char (read-byte stream))))
    (case type-code
      ;; boolean
      (#\C (if (/= 0 (read-byte stream)) t))
      ;; TODO:
      ;; 2b signed int
      (#\Y (uiop:read-little-endian stream 2))
      ;; 4b signed int
      (#\I (uiop:read-little-endian stream 4))
      ;; 8b signed int
      (#\L (uiop:read-little-endian stream 8))
      ;; 4b float32-ieee-754
      (#\F (decode-float32-iee-754 (uiop:read-little-endian stream 4)))
      ;; 8b float64-ieee-754
      (#\D (decode-float64-iee-754 (uiop:read-little-endian stream 8)))
      ;; array of 1b booleans
      (#\b (read-array-property-from-stream
            stream 'boolean (lambda (s) (if (/= 0 (read-byte s)) t))))
      ;; array of 4b signed ints
      (#\i (read-array-property-from-stream
            stream '(unsigned-byte 32) #'uiop:read-little-endian))
      ;; array of 8b signed ints
      (#\l (read-array-property-from-stream
            stream '(unsigned-byte 64) (lambda (s) (uiop:read-little-endian s 8))))
      ;; array of 4b float32-ieee-754
      (#\f (read-array-property-from-stream
            stream 'single-float
            (lambda (s) (decode-float32-iee-754 (uiop:read-little-endian s 4)))))
      ;; array of 8b float64-ieee-754
      (#\d (read-array-property-from-stream
            stream 'double-float
            (lambda (s) (decode-float64-iee-754 (uiop:read-little-endian s 8)))))
      ;; string
      (#\S
       (let* ((len (uiop:read-little-endian stream 4))
              (string (make-array len :element-type 'character)))
         (loop for i from 0 below len
               do (setf (aref string i) (code-char (read-byte stream))))
         string))
      ;; raw binary data
      (#\R
       (let* ((len (uiop:read-little-endian stream 4))
              (string (make-array len :element-type '(unsigned-byte 8))))
         (loop for i from 0 below len
               do (setf (aref string i) (read-byte stream)))
         string))
      (t (error "Unsupported FBX property type '~A'" type-code)))))

(defun fbx-read-node-header-from-stream (stream)
  (let* ((end-offset (uiop:read-little-endian stream 4))
         (num-properties (uiop:read-little-endian stream 4))
         (property-list-len (uiop:read-little-endian stream 4))
         (name-len (read-byte stream))
         (name (make-array name-len :element-type 'character)))
    (loop for i from 0 below name-len
          do (setf (aref name i) (code-char (read-byte stream))))
    (make-node-header
     :end-offset end-offset
     :num-properties num-properties
     :property-list-len property-list-len
     :name-len name-len
     :name name
     :properties (loop repeat num-properties
                       collect (read-property-from-stream stream)))))

(defun read-fbx-from-file (filespec)
  (with-open-file (fstream filespec :element-type '(unsigned-byte 8))
    (let ((version (fbx-read-header-from-stream fstream)))
      (unless (< 7000 version 7500)
        (error "Unsupported FBX version ~A" version)))
    (labels ((read-node ()
               (let ((node (fbx-read-node-header-from-stream fstream)))
                 (when (zerop (node-header-end-offset node))
                   (return-from read-node :eof))
                 (when (< (file-position fstream) (node-header-end-offset node))
                   ;; Read nested nodes
                   (setf (node-header-nested-nodes node)
                         (loop while (< (file-position fstream)
                                        (- (node-header-end-offset node) 13))
                               collecting (read-node)))
                   (loop repeat 13
                         unless (= #x00 (read-byte fstream))
                           do (error "Not a valid FBX file!")))
                 (unless (= (file-position fstream) (node-header-end-offset node))
                   (error "Not a valid FBX file!"))
                 node)))
      (loop while (< (file-position fstream) (file-length fstream))
            collect (let ((node-or-eof (read-node)))
                      (if (eq :eof node-or-eof)
                          (loop-finish)
                          node-or-eof))))))
