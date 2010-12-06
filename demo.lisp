(in-package #:cl-opencl)


(defun print-info ()
  (loop for i from 0
     for p in (remove-duplicates (get-platform-ids) :test 'cffi:pointer-eq)
     do
     (format t "=====================~%")
     (format t "platform ~s= ~s~%" i p)
     (loop for info in '(:platform-profile :platform-version :platform-name :platform-vendor :platform-extensions)
        for s = (get-platform-info p info)
        do (format t "param ~a = ~s~%" info  s #++(mem-aref s :string))
        )
       #++(loop for info in '(:profile :version :name :vendor :extensions)
        for s = (get-platform-info p info)
        do (format t "param ~a = ~s~%" info  s #++(mem-aref s :string))
        )
     (format t "~%")
     (format t "cpu devices = ~s~%" (ignore-errors (get-device-ids p :cpu)))
     (format t "gpu devices = ~s~%" (ignore-errors (get-device-ids p :gpu)))
     (format t "cpu|gpu devices = ~s~%" (ignore-errors (get-device-ids p :cpu :gpu)))
     (format t "accel devices = ~s~%" (ignore-errors (get-device-ids p :accelerator)))
     (format t "default devices = ~s~%" (ignore-errors (get-device-ids p :default)))
     (format t "all devices = ~s~%" (ignore-errors (get-device-ids p :all)))
     (loop for dev in (ignore-errors (get-device-ids p :all))
        do (format t " ----~% device ~s :~%" dev)
        (loop for info in '(:type
                            :vendor-id
                            :max-compute-units
                            :max-work-item-dimensions
                            :max-work-item-sizes
                            :max-work-group-size
                            :preferred-vector-width-char
                            :preferred-vector-width-short
                            :preferred-vector-width-int
                            :preferred-vector-width-long
                            :preferred-vector-width-float
                            :preferred-vector-width-double
                            :max-clock-frequency
                            :address-bits
                            :max-read-image-args
                            :max-write-image-args
                            :max-mem-alloc-size
                            :image2d-max-width
                            :image2d-max-height
                            :image3d-max-width
                            :image3d-max-height
                            :image3d-max-depth
                            :image-support
                            :max-parameter-size
                            :max-samplers
                            :mem-base-addr-align
                            :min-data-type-align-size
                            :single-fp-config
                            :global-mem-cache-type
                            :global-mem-cacheline-size
                            :global-mem-cache-size
                            :global-mem-size
                            :max-constant-buffer-size
                            :max-constant-args
                            :local-mem-type
                            :local-mem-size
                            :error-correction-support
                            :profiling-timer-resolution
                            :endian-little
                            :available
                            :compiler-available
                            :execution-capabilities
                            :queue-properties
                            :name
                            :vendor
                            :driver-version
                            :profile
                            :version
                            :extensions
                            :platform)
           for s = (get-device-info dev info)
           do (format t "   param ~a = ~s~%" info  s)
           ))))


(let* ((p (car (get-platform-ids)))
       (d (car (get-device-ids p :all))))
  (assert d)
  (with-context (context (list d) :platform p )
    (format t "context = ~s~%" context)
    (loop for info in '(:reference-count :devices :properties)
       do (format t "  param ~s = ~s~%" info (get-context-info context info)))
    (let ((command-queue (create-command-queue context d)))
      (format t "command queue = ~s~%" command-queue)
      (loop for info in '(:context :device :reference-count :properties )
         do (format t "  param ~s = ~s~%" info (get-command-queue-info command-queue info)))
      (format t "set out-of-order prop : old values=~s~%"
              (set-command-queue-property command-queue :out-of-order-exec-mode-enable t :return-old-properties t))
      (loop for info in '(:context :device :reference-count :properties )
         do (format t "  param ~s = ~s~%" info (get-command-queue-info command-queue info)))
      
      (format t "clear out-of-order prop : old values=~s~%"
              (set-command-queue-property command-queue :out-of-order-exec-mode-enable nil :return-old-properties t))
      (loop for info in '(:context :device :reference-count :properties )
         do (format t "  param ~s = ~s~%" info (get-command-queue-info command-queue info))))))


(cffi::canonicalize-foreign-type '%cl:context-properties)