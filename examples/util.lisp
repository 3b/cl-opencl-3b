(defpackage #:opencl-example-utils
  (:use #:cl))
(in-package #:opencl-example-utils)
(defparameter *frame-count* 0)
(defparameter *last-fps-message-time* 0)
(defparameter *last-fps-message-frame-count* 0)
(defparameter *fps-message-interval* 2.000) ;; in second

(defun update-fps (&key (print t))
  ;; update the frame count
  (incf *frame-count*)
  ;; handle tick count wrapping to 0
  (let ((now (/ (get-internal-real-time) internal-time-units-per-second)))
    (when (< now *last-fps-message-time*)
      (setf *last-fps-message-time* now))
    ;; see if it is time for next message
    (when (and print
               (>= now (+ *last-fps-message-time* *fps-message-interval*)))
      (let ((frames (- *frame-count* *last-fps-message-frame-count*))
            (seconds (- now *last-fps-message-time*)))
        (format t "~s seconds: ~s fps, ~s ms per frame~%"
                (float seconds)
                (if (zerop seconds) "<infinite>" (float (/ frames seconds)))
                (if (zerop frames) "<infinite>" (float (/ seconds frames)))))
      (setf *last-fps-message-time* now)
      (setf *last-fps-message-frame-count* *frame-count*))))

(defmacro with-fps-vars ((&key (interval 2.0)) &body body)
  `(let ((*frame-count* 0)
         (*last-fps-message-time* 0)
         (*last-fps-message-frame-count* 0)
         (*fps-message-interval* ,interval))
     ,@body))