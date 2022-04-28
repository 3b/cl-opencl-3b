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

   #:svm-alloc
   #:svm-free
   #:enqueue-svm-free
   #:enqueue-svm-memcpy
;;;   #:enqueue-svm-mem-fill
   #:enqueue-svm-map
   #:enqueue-svm-unmap
   

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
   #:%set-kernel-arg-svm-buffer
   #:%set-kernel-arg-image
   #:%set-kernel-arg-number
   #:retain-mem-object
   #:release-mem-object
   #:get-mem-object-info
   #:enqueue-nd-range-kernel
   #:flush
   #:finish
   #:enqueue-read-buffer
   #:enqueue-write-buffer
   #:extension-present-p
   #:release-context
   #:release-command-queue
   #:retain-command-queue
   #:retain-context
   #:create-context
   #:enqueue-map-buffer
   #:with-mapped-buffer
   #:with-mapped-svm
   #:enqueue-unmap-mem-object
   #:create-from-gl-buffer
   #:enqueue-acquire-gl-objects
   #:enqueue-release-gl-objects
   #:device-extension-present-p
   #:create-from-gl-texture-2d
   #:get-device-extension-list
   #:get-extension-list
   #:get-device-ids-with-extensions
   #:%set-kernel-arg
   #:create-image-2d
   ))
