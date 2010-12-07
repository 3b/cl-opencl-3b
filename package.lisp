(defpackage #:cl-opencl
  (:use #:common-lisp #:cffi)
  (:nicknames #:opencl #:ocl)
  (:export
   #:get-device-info
   #:get-platform-ids
   #:get-platform-info
   #:with-context
   #:get-device-ids
   #:with-context-from-type
   #:create-buffer
   #:create-buffer-from-array
   #:create-command-queue
   #:create-program-with-source
   #:release-program
   #:retain-program
   #:build-program
   #:get-program-info
   #:get-program-build-info
   #:create-kernel
   #:create-kernels-in-program
   #:retain-kernel
   #:release-kernel
   #:get-kernel-info
   #:get-kernel-work-group-info
   #:%set-kernel-arg-buffer
   #:%set-kernel-arg-image
   #:%set-kernel-arg-number
   #:retain-mem-object
   #:release-mem-object
   #:get-mem-object-info
   #:enqueue-nd-range-kernel
   #:flush
   #:finish
   #:enqueue-read-buffer))