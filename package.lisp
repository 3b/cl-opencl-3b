(defpackage #:cl-opencl
  (:use #:common-lisp #:cffi)
  (:nicknames #:opencl #:ocl)
  (:export
   #:get-device-info))