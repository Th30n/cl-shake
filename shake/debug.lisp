;;;; Contains various utilities for debugging and profiling.

(in-package #:shake.debug)

(defstruct timer
  (name "<null>" :type string :read-only t)
  (count 0 :type fixnum)
  (total 0.0 :type single-float)
  (new-max 0.0 :type single-float)
  (max 0.0 :type single-float)
  (avg 0.0 :type single-float))

(defun nupdate-timer (timer dt)
  (declare (optimize (speed 3) (space 3)))
  (declare (type timer timer))
  (declare (type single-float dt))
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
          (timer-total timer) 0.0
          (timer-new-max timer) 0.0))
  timer)

(defun performance-delta (start stop)
  "Return the delta in seconds between two performance counters."
  (declare (optimize (speed 3)))
  (declare (type (unsigned-byte 64) start))
  (declare (type (unsigned-byte 64) stop))
  (/ (coerce (the (unsigned-byte 64) (- stop start)) 'single-float)
     (coerce (the (unsigned-byte 64) (sdl2:get-performance-frequency))
             'single-float)))

(defun call-with-timer (timer function &key (reset-every 60))
  "Upate the TIMER with the execution time of FUNCTION. If the TIMER-COUNT
  exceeds RESET-EVERY call counts, TIMER is reset."
  (declare (optimize (speed 3)))
  (check-type timer timer)
  (check-type reset-every fixnum)
  (check-type function function)
  (let ((start (sdl2:get-performance-counter)))
    (declare (type (unsigned-byte 64) start))
    (multiple-value-prog1 (funcall function)
      (let ((end (sdl2:get-performance-counter)))
        (declare (type (unsigned-byte 64) end))
        ;; Sometimes END is set to a lower value than START, in such a case
        ;; ignore the update. This usually happens if the performance counter
        ;; overflowed, but it seems rather unlikely to happen as often as it
        ;; does.
        (when (> end start)
          (nupdate-timer timer (performance-delta start end)))
        (when (>= (timer-count timer) reset-every)
          (nreset-timer timer))))))

(defmacro with-timer ((timer &key (reset-every 60)) &body body)
  `(call-with-timer ,timer (lambda () ,@body) :reset-every ,reset-every))
