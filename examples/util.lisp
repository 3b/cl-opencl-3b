(defpackage #:opencl-example-utils
  (:use #:cl)
  (:export
   #:with-fps-vars
   #:update-fps
   #:create-shared-context))

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



#+unix
(cffi:defcfun ("glXGetCurrentContext" *-get-current-context) :pointer)
#+unix
(cffi:defcfun ("glXGetCurrentDisplay" *-get-current-display-or-hdc) :pointer)
#+win32
(cffi:defcfun ("wglGetCurrentContext" *-get-current-context) :pointer)
#+win32
(cffi:defcfun ("wglGetCurrentDC" *-get-current-display-or-hdc) :pointer)
#+darwin
(cffi:defcfun ("CLGGetCurrentContext" cgl-get-current-context) :pointer)
#+darwin
(cffi:defcfun ("CGLGetShareGroup" cgl-get-share-group) :pointer
  (context :pointer))


(defun create-shared-context (platform device)
  (cond
    ((ocl:device-extension-present-p device "cl_khr_gl_sharing")
     (ocl:create-context (list device)
                     :platform platform
                     ;; unix, win32, egl use roughly same api:
                     #+ (or unix win32) :gl-context-khr
                     #+ (or unix win32) (*-get-current-context)
                     #+ unix :glx-display-khr
                     #+ win32 :wgl-hdc-khr
                     #+ (or unix win32) (*-get-current-display-or-hdc)
                     ;; but apple is different:
                     #+ darwin :cgl-sharegroup-khr
                     #+ darwin (cgl-get-share-group
                                (cgl-get-current-context))))
    ((ocl:device-extension-present-p device "cl_APPLE_gl_sharing")
     (error "context sharing not implemented yet for cl_APPLE_gl_sharing"))
     (t (error "no context sharing extension found in device?"))))
