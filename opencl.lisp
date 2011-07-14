(in-package #:cl-opencl)

;; todo: organize these better?

;;; 4.3 contexts

(defgeneric pointer (object))
(defmethod pointer (object)
  ;; fixme: probably should error instead of returning nil for unknown types?
  (when (cffi:pointerp object)
    object))

(defclass device ()
  ((pointer :initarg :pointer :reader pointer)))

(define-finalized-resource-class context %release-context get-context-info)



;; todo: error callback
;;   callback is probably called from arbitrary threads, so need to
;;   figure out how to deal with that first...
(defun create-context (devices &rest properties
                       &key platform
                       ;; should this &allow-other-keys, or drop keys
                       ;; completely rather than enumerating?
                       gl-context-khr glx-display-khr wgl-hdc-khr
                       cgl-sharegroup-khr egl-display-khr
                       context-property-use-cgl-sharegroup-apple)
  (declare (ignore platform gl-context-khr glx-display-khr
                   wgl-hdc-khr cgl-sharegroup-khr egl-display-khr
                   context-property-use-cgl-sharegroup-apple))
  (let ((devices (alexandria:ensure-list devices)))
    (with-opencl-plist (props %cl:context-properties properties)
      (with-foreign-objects ((devs '%cl:device-id (length devices)))
       (loop for i from 0
          for dev in devices
          do (setf (mem-aref devs '%cl:device-id i) (pointer dev)))
       (make-instance 'context
                      :pointer (check-errcode-arg
                                (%cl:create-context props
                                                    (length devices) devs
                                                    ;; todo: error callback
                                                    (cffi:null-pointer)
                                                    (cffi:null-pointer))))))))

(defun create-context-from-type (type &key platform)
  (let ((properties nil))
    (when platform
      (push (pointer-address platform) properties)
      (push :platform properties))
    (with-opencl-plist (props %cl:context-properties properties)
      (make-instance 'context
                     :pointer
                     (check-errcode-arg
                      (%cl:create-context-from-type props type
                                                    ;; todo: error callback
                                                    (cffi:null-pointer)
                                                    (cffi:null-pointer)))))))

(defun %retain-context (context)
  (check-return (%cl:retain-context context))
  ;; not sure if returning context here is useful or if it should just return
  ;; :success fom the low-level call?
  context)

(defun %release-context (context)
  (check-return (%cl:release-context context)))

(defmacro with-context ((context devices &rest properties) &body body)
  `(let ((,context (create-context ,devices ,@properties)))
     (unwind-protect
          (progn
            ,@body)
       (release ,context))))

(defmacro with-context-from-type ((context type &rest properties) &body body)
  `(let ((,context (create-context-from-type ,type ,@properties)))
     (unwind-protect
          (progn
            ,@body)
       (release ,context))))



;;;; 5.1 command queues

(define-finalized-resource-class command-queue %release-command-queue
  get-command-queue-info)

(defun create-command-queue (context device &rest properties
                             &key out-of-order-exec-mode-enable
                             profiling-enable)
  (declare (ignore out-of-order-exec-mode-enable profiling-enable))
  (make-instance 'command-queue
                 :pointer (check-errcode-arg (%cl:create-command-queue
                                              (pointer context)
                                              (pointer device)
                                              properties))))

(defun %retain-command-queue (command-queue)
  (check-return (%cl:retain-command-queue command-queue))
  command-queue)

(defun %release-command-queue (command-queue)
  (check-return (%cl:release-command-queue command-queue)))

(defun set-command-queue-property (command-queue properties enable &key return-old-properties)
  (if return-old-properties
      (with-foreign-object (old '%cl:command-queue-properties)
        (check-return (%cl:set-command-queue-property (pointer command-queue)
                                                      properties enable old))
        (mem-aref old '%cl:command-queue-properties)
        #++(foreign-bitfield-symbols '%cl:command-queue-properties old))

      (check-return (%cl:set-command-queue-property (pointer command-queue)
                                                    properties enable
                                                    (cffi:null-pointer)))))




;;;; 5.2 Memory Objects

;;; 5.2.1 Creating Buffer Objects


(define-finalized-resource-class buffer %release-mem-object get-mem-object-info)

;; fixme: should this support preallocated host memory area?
;; skipping for now, since it exposes ffi level stuff...
;; possibly just support copy-host-ptr mode, with copy from lisp array?
(defun create-buffer (context size &rest flags)
  (make-instance 'buffer
                 :pointer (check-errcode-arg
                           (%cl:create-buffer (pointer context) flags size
                                              (cffi:null-pointer)))))

;; should size/count be implicit from array size?
;; foreign type?
;; switch to keywords instead of using &rest for flags?
(defun create-buffer-from-array (context array count foreign-type &rest flags)
  (with-foreign-object (buf foreign-type count)
    (loop repeat count
       for i below (array-total-size array)
       do (setf (mem-aref buf foreign-type i) (row-major-aref array i)))
    (make-instance 'buffer
                   :pointer
                   (check-errcode-arg
                    (%cl:create-buffer (pointer context)
                                       (adjoin :copy-host-pointer flags)
                                       (* count
                                          (foreign-type-size foreign-type))
                                       buf)))))

(defparameter *lisp-type-map*
  (alexandria:plist-hash-table '(single-float :float
                                 double-float :double
                                 (unsigned-byte 8) :uint8
                                 (signed-byte 8) :int8
                                 (unsigned-byte 16) :uint16
                                 (signed-byte 16) :int16
                                 (unsigned-byte 32) :uint32
                                 (signed-byte 32) :int32)
                               :test 'equal))
;;; 5.2.2 Reading, Writing and Copying Buffer Objects
(defun enqueue-read-buffer (command-queue buffer count
                            &key (blockp t) wait-list event
                            (offset 0) (element-type 'single-float))
  (let* ((foreign-type (gethash element-type *lisp-type-map*))
         (octet-count (* count (foreign-type-size foreign-type)))
         (array (make-array count
                            :element-type element-type)))
    (when (or event wait-list)
      (error "events and wait lists not done yet in enqueue-read-buffer"))
    (unless blockp
      (error "non-blocking enqueue-read-buffer doesn't work yet"))
    ;; w-p-t-v-d won't work with non-blocking read...
    (with-pointer-to-vector-data (p array)
      (check-return (%cl:enqueue-read-buffer (pointer command-queue)
                                             (pointer buffer)
                                             blockp
                                             offset octet-count p
                                             0 (null-pointer) (null-pointer))))
    array))

;;; 5.2.3 Mapping Buffer Objects
(defun enqueue-map-buffer (command-queue buffer count
                           &key (blockp t) wait-list event
                           (offset 0) (element-type 'single-float)
                           read write)
  (let* ((foreign-type (gethash element-type *lisp-type-map*))
         (octet-count (* count (foreign-type-size foreign-type)))
         rw-flags)
    (when read (push :read rw-flags))
    (when write (push :write rw-flags))
    (when (or event wait-list)
      (error "events and wait lists not done yet in enqueue-map-buffer"))
    (unless blockp
      (error "non-blocking enqueue-map-buffer doesn't work yet"))
    (check-errcode-arg
     (%cl:enqueue-map-buffer (pointer command-queue) (pointer buffer)
                             blockp rw-flags
                             offset octet-count
                             0 (null-pointer) (null-pointer)))))

(defmacro with-mapped-buffer ((p command-queue buffer count
                                 &key
                                 (element-type ''single-float)
                                 (offset 0) read write) &body body)
  "Maps the COUNT elements in BUFFER of type ELEMENT-TYPE starting at
 OFFSET, storing the returned pointer in P. The buffer is unmapped
 when execution leaves WITH-MAPPED-BUFFER. READ and/or WRITE should be
 set to specify what access to the mapped data is desired."
  (alexandria:once-only (command-queue buffer)
    `(let ((,p (enqueue-map-buffer ,command-queue ,buffer ,count
                                   :blockp t :offset ,offset
                                   :element-type ,element-type
                                   :read ,read :write ,write)))
       (unwind-protect
            (progn ,@body)
         (enqueue-unmap-mem-object ,command-queue ,buffer ,p)))))

;;; 5.3.1 Creating Image Objects

(define-finalized-resource-class image %release-mem-object get-image-info)

(defmacro with-image-format ((var channel-order channel-type) &body body)
  `(with-foreign-object (,var '%cl:image-format)
     (setf (foreign-slot-value ,var '%cl:image-format '%cl::image-channel-order)
           ,channel-order
           (foreign-slot-value ,var '%cl:image-format
                               '%cl::image-channel-data-type)
           ,channel-type)
     ,@body))

(defun create-image-2d (context channel-order channel-type width height &key flags (row-pitch 0))
  ;; todo: load from array, use host ptr, etc
  (with-image-format (image-format channel-order channel-type)
    (make-instance 'image
                   :pointer (check-errcode-arg
                             (%cl:create-image-2d (pointer context) flags
                                                  image-format width height
                                                  row-pitch
                                                  (cffi:null-pointer))))))

;;; 5.3.3
#++
(defmacro with-size-t-3 ((var source &optional (default 0)) &body body)
  (let ((i (gensym)))
    (alexandria:once-only (source default)
      `(with-foreign-object (,var '%cl:size-t 3)
         (loop for ,i below 3
            for v = (if (< ,i (length ,source))
                        (elt ,source 1)
                        ,default)
            do (setf (mem-aref ,var '%cl:size-t ,i) v))
         ,@body))))

#++
(defmacro with-size-t-3s (bindings &body body)
  (if bindings
      `(with-size-t-3 ,(car bindings)
         (with-size-t-3s ,(cdr bindings)
           ,@body))
      `(progn ,@body)))


#++(defun enqueue-read-image (command-queue image dimensions
                            &key (blockp t) wait-list event
                            (origin '(0 0 0)) (element-type 'single-float)
                            (row-pitch 0) (slice-pitch 0))
  (let* ((foreign-type (ecase element-type
                         (single-float :float)))
         (array (make-array count :element-type element-type)))
    (with-pointer-to-vector-data (p array)
      (with-size-t-3s ((dimensions dimensions 1)
                       (origin origin 0))
        (check-return (%cl:enqueue-read-buffer command-queue buffer blockp
                                               origin )))
      ))
)



;;; 5.4.1 retaining and Releasing Memory Objects

(defun %retain-mem-object (object)
  (check-return (%cl:retain-mem-object object))
  object)

(defun %release-mem-object (object)
  (check-return (%cl:release-mem-object object)))

;; set-mem-object-destructor-callback

;;; 5.4.2 Unmapping mapped memory objects
(defun enqueue-unmap-mem-object (command-queue buffer pointer
                                 &key wait-list event)
  (when (or event wait-list)
    (error "events and wait lists not done yet in enqueue-unmap-mem-object"))
  (check-return
   (%cl:enqueue-unmap-mem-object (pointer command-queue) (pointer buffer)
                                 pointer
                                 0 (null-pointer) (null-pointer))))

;;; 5.4.4 Memory Object Queries - see get.lisp

(define-finalized-resource-class program %release-program get-program-info)

;;; 5.6.1 Creating Program Objects

(defun create-program-with-source (context &rest strings)
  ;; fixme: avoid this extra copy of the string data..
  ;; either pass the concatenated string directly (maybe concatenating
  ;;  into an octet array instead of a string?) or allocate separate
  ;;  buffers for each string
  (let ((string (if (= 1 (length strings))
                    (car strings)
                    (format nil "~{~a~}" strings))))
    (with-foreign-string (cstring string)
      (with-foreign-object (p :pointer)
        (setf (mem-ref p :pointer) cstring)
        (make-instance 'program
                       :pointer
                       (check-errcode-arg
                        (%cl:create-program-with-source (pointer context) 1 p
                                                        (null-pointer))))))))

;; todo: create-program-with-binary

(defun %retain-program (program)
  (check-return (%cl:retain-program program))
  program)

(defun %release-program (program)
  (check-return (%cl:release-program program)))

;;; 5.6.2 Building Program Executables

;; todo: add notify callback support
;;  - requiring callers to pass a cffi callback is a bit ugly
;;  - using an interbal cffi callback and trying to map back to caller's
;;    lisp callback is probably nicer API, but harder to implement
;;  - also need to deal with thread safety stuff... not sure if it might
;;    be called from arbitrary threads or not
;; todo: add keywords for known options?
(defun build-program (program &key devices (options-string "")
                      #++ notify-callback)
  (with-foreign-object (device-list :pointer (length devices))
    (with-foreign-string (options options-string)
     (check-return (%cl:build-program (pointer program) (length devices)
                                      (if devices device-list (null-pointer))
                                      options-string
                                      (null-pointer) (null-pointer))
       ;; nv drivers return :invalid-binary for undefined functions,
       ;; so treat that like build failure for now...
       ((:build-program-failure :invalid-binary)
        (let ((status (loop for i in (get-program-info program :devices)
                         collect (list (get-program-build-info program i :status)
                                       (get-program-build-info program i :log)))))
          (error "Build-program returned :build-program-failure:~:{~&~s : ~s~}" status))))))
)

;;; 5.6.4 Unloading the OpenCL Compiler

(defun unload-compiler ()
  (check-return (%cl:unload-compiler)))

;;; 5.6.5 Program Object Queries - see get.lisp

;;; 5.7.1 Creating Kernel Objects

(define-finalized-resource-class kernel %release-kernel get-kernel-info)

(defun create-kernel (program name)
  (make-instance 'kernel
                 :pointer (check-errcode-arg
                           (%cl:create-kernel (pointer program) name))))

(defun create-kernels-in-program (program)
  ;; fixme: verify calling this twice is the correct way to figure out
  ;; how many kernels are in program...
  (mapcar (lambda (p) (make-instance 'kernel :pointer p))
          (get-counted-list %cl:create-kernels-in-program ((pointer program))
                            '%cl:kernel)))

(defun %retain-kernel (kernel)
  (check-return (%cl:retain-kernel kernel))
  kernel)

(defun %release-kernel (kernel)
  (check-return (%cl:release-kernel kernel)))

;;; 5.7.2 Setting Kernel Arguments

;;; fixme: set-kernel-arg is ugly, since we don't have enough C style
;;;   static type info, or lisp style dynamic info to determine
;;;   anything useful about the arg values...

;;; probably want some combination of wrapping the various low-level
;;;   binding types (buffers, images etc) in clos wrappers so we can
;;;   introspect those for type/size, and statically typed ffi definitions
;;;   for kernels so we can do type conversions for things like numbers

;;; for now, just breaking into a few specific functions, and using function
;;;   name to encode static type info...
(defun %set-kernel-arg-buffer (kernel index buffer)
  (with-foreign-object (p :pointer)
    (setf (mem-ref p :pointer) buffer)
    (check-return (%cl:set-kernel-arg kernel index (foreign-type-size '%cl:mem) p))))

(defun %set-kernel-arg-image (kernel index image)
  (with-foreign-object (p :pointer)
    (setf (mem-ref p :pointer) image)
    (check-return (%cl:set-kernel-arg kernel index (foreign-type-size '%cl:mem) p))))

(defun %set-kernel-arg-sampler (kernel index sampler)
  (with-foreign-object (p :pointer)
    (setf (mem-ref p :pointer) sampler)
    (check-return (%cl:set-kernel-arg kernel index (foreign-type-size '%cl:sampler) sampler))))

(defun %set-kernel-arg-number (kernel index value type)
  (with-foreign-object (p type)
    (setf (mem-ref p type) value)
    (check-return (%cl:set-kernel-arg kernel index (foreign-type-size type) p))))

(defun %set-kernel-arg (kernel index value type)
  (with-foreign-object (p type)
    (setf (mem-ref p type) value)
    (check-return (%cl:set-kernel-arg kernel index (foreign-type-size type) p))))

(defgeneric set-kernel-arg (kernel index value &key &allow-other-keys)
  (:method ((kernel kernel) index (value buffer) &key)
   (%set-kernel-arg-buffer (pointer kernel) index (pointer value)))
  (:method ((kernel kernel) index (value image) &key)
   (%set-kernel-arg-image (pointer kernel) index (pointer value)))
  #++(:method ((kernel kernel) index (value sampler) &key)
   (%set-kernel-arg-sampler (pointer kernel) index (pointer value)))
  (:method ((kernel kernel) index (value number) &key type)
    (unless type
      (error "must specify a type for numeric arguments to SET-KERNEL-ARG"))
    (%set-kernel-arg-number (pointer kernel) index value type)))


;;; 5.8 Executing Kernels

;; not sure about the API here...
;; for now requiring global-size, and getting dimensions from legth of that
(defun enqueue-nd-range-kernel (command-queue kernel global-size
                                &key global-offset local-size
                                wait-list event)
  (when (or event wait-list)
    (error "events and wait lists not done yet in enqueue-nd-range-kernel"))
  (let* ((global-size (alexandria:ensure-list global-size))
         (global-offset (alexandria:ensure-list global-offset))
         (local-size (alexandria:ensure-list local-size))
         (dims (min (length global-size) 3)))
    (with-foreign-arrays ((global-size '%cl:size-t global-size :max 3)
                          (global-offset '%cl:size-t global-offset :max 3 :empty-as-null-p t)
                          (local-size '%cl:size-t local-size :max 3 :empty-as-null-p t))
      (check-return
          (%cl:enqueue-nd-range-kernel (pointer command-queue)
                                       (pointer kernel)
                                       dims global-offset
                                       global-size local-size
                                       0 (null-pointer)
                                       (null-pointer)
 )))
)

)



;;; 5.13 Flush and Finish

(defun flush (command-queue)
  (check-return (%cl:flush (pointer command-queue))))

(defun finish (command-queue)
  (check-return (%cl:finish (pointer command-queue))))

;;; 9.8.2 CL Buffer Objects -> GL Buffer Objects

(defun create-from-gl-buffer (context usage gl-buffer)
  (check-errcode-arg (%cl:create-from-gl-buffer (pointer context) usage gl-buffer)))

;;; 9.8.6 Sharing memory objects that map to GL objects between CL and GL contexts

(defun enqueue-acquire-gl-objects (command-queue objects
                                   &key wait-list event)
  (when (or event wait-list)
    (error "events and wait lists not done yet in enqueue-acquire-gl-objects"))
  (with-counted-foreign-array (c p '%cl:mem objects)
    (check-return
        (%cl:enqueue-acquire-gl-objects (pointer command-queue) c p
                                        0 (null-pointer) (null-pointer)))))

(defun enqueue-release-gl-objects (command-queue objects
                                   &key wait-list event)
  (when (or event wait-list)
    (error "events and wait lists not done yet in enqueue-release-gl-objects"))
  (with-counted-foreign-array (c p '%cl:mem objects)
    (check-return
        (%cl:enqueue-release-gl-objects (pointer command-queue) c p
                                        0 (null-pointer) (null-pointer)))))

;;; 9.12.3 / 9.8.3 CL Image Objects -> GL Textures

(defun create-from-gl-texture-2d (context usage texture-target miplevel texture)
  (check-errcode-arg
   (%cl:create-from-gl-texture-2d (pointer context) usage texture-target
                                  miplevel texture)))


