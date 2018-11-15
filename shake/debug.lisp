;;;; Contains various utilities for debugging and profiling.

(in-package #:shake.debug)

(defstruct timer
  (name "<null>" :type string :read-only t)
  (count 0 :type fixnum)
  (total 0d0 :type double-float)
  (new-max 0d0 :type double-float)
  (max 0d0 :type double-float)
  (avg 0d0 :type double-float))

(defun nupdate-timer (timer dt)
  (declare (optimize (speed 3) (space 3)))
  (declare (type timer timer))
  (declare (type double-float dt))
  (incf (timer-count timer))
  (incf (timer-total timer) dt)
  (zap #'max (timer-new-max timer) dt)
  timer)

(defun nreset-timer (timer)
  "Reset the maximum and average calculations of a TIMER."
  (declare (optimize (speed 3) (space 3)))
  (declare (type timer timer))
  (with-struct (timer- total count new-max) timer
    (setf (timer-avg timer) (/ total count)
          (timer-max timer) new-max
          (timer-count timer) 0
          (timer-total timer) 0d0
          (timer-new-max timer) 0d0))
  timer)

(defun performance-delta (start stop)
  "Return the delta in seconds between two performance counters."
  (declare (optimize (speed 3)))
  (declare (type (unsigned-byte 64) start))
  (declare (type (unsigned-byte 64) stop))
  (/ (coerce (the (unsigned-byte 64) (- stop start)) 'double-float)
     (coerce (the (unsigned-byte 64) (sdl2:get-performance-frequency))
             'double-float)))

(defun call-with-timer (timer function &key (reset-every 1d0))
  "Upate the TIMER with the execution time of FUNCTION. If the TIMER TOTAL
  exceeds RESET-EVERY seconds, TIMER is reset."
  (declare (optimize (speed 3)))
  (check-type timer timer)
  (check-type reset-every double-float)
  (check-type function function)
  (let ((start (sdl2:get-performance-counter)))
    (declare (type (unsigned-byte 64) start))
    (multiple-value-prog1 (funcall function)
      (let* ((end (sdl2:get-performance-counter))
             (delta (performance-delta start end)))
        (declare (type (unsigned-byte 64) end))
        (nupdate-timer timer delta)
        (when (>= (timer-total timer) reset-every)
          (nreset-timer timer))))))

(defmacro with-timer ((timer &key (reset-every 1d0)) &body body)
  `(call-with-timer ,timer (lambda () ,@body) :reset-every ,reset-every))
