(in-package #:cl-opencl)

;; todo: organize these better?

;;; 5.9 events (out of order since lots of stuff uses them)

(defun %retain-event (event)
  (check-return (%cl:retain-event event))
  event)

(defun %release-event (event)
  (check-return (%cl:release-event event)))

(define-finalized-resource-class event %release-event get-event-info)

;; should user events get a finalizer that sets status to an error status?
;;   need to make sure we don't have extra instances for the same event
;;   first though

(defun create-user-event (context)
  (make-instance 'event :pointer (check-errcode-arg
                                  (%cl:create-user-event (pointer context)))))

(defun set-user-event-status (event status)
  (check-return (%cl:set-user-event-status (pointer event) status)))

(defun wait-for-events (&rest wait-list)
  (with-counted-foreign-array (wait-count wait-pointer '%cl:event
                                          (mapcar 'pointer wait-list))
    (check-return (%cl:wait-for-events wait-count wait-pointer))))

;; todo: set-event-callback

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

(defun create-command-queue (context device
                             &key out-of-order-exec-mode-enable
                             profiling-enable)
  (let ((properties nil))
    (when out-of-order-exec-mode-enable
      (push :out-of-order-exec-mode-enable properties))
    (when profiling-enable
      (push :profiling-enable properties))
    (make-instance 'command-queue
                   :pointer (check-errcode-arg (%cl:create-command-queue
                                                (pointer context)
                                                (pointer device)
                                                properties)))))

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
;; possibly just support copy-host-pointer mode, with copy from lisp array?
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

;; wrapper for foreign pointer + event, for use with async reads,
;; mapped buffers, etc (see allocated-host-buffer, mapped-host-buffer)
(defclass host-buffer ()
  ((data-pointer :initarg :data-pointer :reader data-pointer)
   ;; :ready (data-pointer is allocated and filled)
   ;; :waiting (check EVENT for status)
   ;; :invalid (uninitialized, unmapped, freed, etc)
   (state :initarg :state :initform :invalid :reader state)
   ;; may be NIL if we blocked on read
   (event :initarg :event :reader event)
   ;; function to call to release resources, called by RELEASE and
   ;; used as finalizer. Call with object to remove finalizer and
   ;; update object state/pointers/etc
   (release-thunk :initarg :release-thunk :reader %release-thunk)))

;; todo: locking, actually do something with the leaked pointers (gc
;; thread, etc)
(defparameter *leaked-resources* nil)

(defun %free-pointer-thunk (event pointer)
  (cond
    (event
     (let ((event-pointer (pointer event)))
       (%retain-event event-pointer)
       (lambda (&optional object)
         (if (<= (get-event-info event-pointer :command-execution-status)
                 (foreign-enum-value '%cl:command-execution-status :complete))
             (progn
               (when object
                 (tg:cancel-finalization object)
                 (setf (slot-value object 'data-pointer) nil
                       (slot-value object 'state) :invalid))
               (%release-event event-pointer)
               (foreign-free pointer))
             (progn
               ;; if we got an object, we can still try again in the
               ;; finalizer, so leave it active but invalidate the
               ;; object to catch attempts to use it after explicit
               ;; release
               (when object
                 (setf (slot-value object 'data-pointer) nil
                       (slot-value object 'state) :invalid))
               ;; If we are in the finalizer, and we can't safely free the
               ;; memory, just complain and save it for later...
               (unless object
                 (push (list :pointer pointer :event-pointer event-pointer)
                       *leaked-resources*)
                 (format *debug-io* "finalizer failed to free memory ~s due to event ~s in state ~s~%"
                         pointer event-pointer
                         (get-event-info event-pointer
                                         :command-execution-status))))))))
    ;; no event, we can just free the pointer directly
    (t
     (lambda (&optional object)
       (when object
        (tg:cancel-finalization object)
        (setf (slot-value object 'data-pointer) nil
              (slot-value object 'state) :invalid))
       (foreign-free pointer)))))

(defun %unmap-buffer-thunk (queue event memobj pointer)
  ;; finalizer needs to remember a command-queue, so it can enqueue an
  ;;   unmap command
  (let ((queue-pointer (pointer queue))
        (event-pointer (when event (pointer event)))
        (memobj-pointer (pointer memobj)))
    (%retain-command-queue queue-pointer)
    (%retain-mem-object memobj-pointer)
    ;; we keep the event around so the unmap can be set to wait on it
    (when event (%retain-event event-pointer))
    ;; should we support RELEASE for mapped buffers, or require
    ;; enqueue-unmap-buffer?
    (lambda (&optional object)
      (when object
        (tg:cancel-finalization object)
        (setf (slot-value object 'data-pointer) nil
              (slot-value object 'state) :invalid))
      (if event-pointer
          (with-counted-foreign-array (wait-count wait-pointer
                                                  '%cl:event
                                                  (list event-pointer))
            (%cl:enqueue-unmap-mem-object queue-pointer memobj-pointer pointer
                                          wait-count wait-pointer
                                          (cffi:null-pointer)))
          (%cl:enqueue-unmap-mem-object queue-pointer memobj-pointer pointer
                                        0 (cffi:null-pointer)
                                        (cffi:null-pointer)))

      (when event-pointer (%release-event event-pointer))
      (%release-mem-object memobj-pointer)
      (%release-command-queue queue-pointer))))

;; ;; host-buffer that should be FOREIGN-FREEd in finalizer
;; (defclass allocated-host-buffer (host-buffer)
;;   ())
;; 
;; ;; host-buffer that needs to be unmap the buffer when finalized,
;; ;; which requires closing over a command-queue and context
;; (defclass mapped-host-buffer (host-buffer)
;;   ())


(defmethod release ((buffer host-buffer))
  (funcall (%release-thunk buffer) buffer)
  (when (event buffer)
    (release (event buffer))))

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
(defun %enqueue-read-buffer (command-queue buffer count
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

(defun enqueue-read-buffer (command-queue buffer count
                            &key (blockp t) wait-list eventp
                            (offset 0) (element-type 'single-float))
  (let* ((foreign-type (gethash element-type *lisp-type-map*))
         (octet-count (* count (foreign-type-size foreign-type)))
         (host-pointer (cffi:foreign-alloc foreign-type :count count))
         (event (check-return+events (wait-list (or eventp (not blockp)))
                    (%cl:enqueue-read-buffer (pointer command-queue)
                                             (pointer buffer)
                                             blockp
                                             offset octet-count host-pointer))))
    (make-instance 'host-buffer
                   :event event
                   :data-pointer host-pointer
                   :state (if blockp :ready :waiting)
                   :release-thunk (%free-pointer-thunk event host-pointer))))


(defun enqueue-copy-buffer (command-queue source-buffer dest-buffer
                            ;; fixme: should this accept an element-type and count instead of raw octet count?
                            octet-count
                            &key wait-list eventp
                            (source-offset 0)
                            (dest-offset 0))
  (check-return+events (wait-list eventp)
      (%cl:enqueue-copy-buffer (pointer command-queue)
                               (pointer source-buffer)
                               (pointer dest-buffer)
                               source-offset dest-offset
                               octet-count)))



;;; 5.2.3 Mapping Buffer Objects
(defun %enqueue-map-buffer (command-queue buffer count
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

(defun enqueue-map-buffer (command-queue buffer count
                           &key (blockp t) wait-list eventp
                           (offset 0) (element-type 'single-float)
                           read write)
  (let* ((foreign-type (gethash element-type *lisp-type-map*))
         (octet-count (* count (foreign-type-size foreign-type)))
         rw-flags)
    (when read (push :read rw-flags))
    (when write (push :write rw-flags))
    (with-foreign-object (event-pointer '%cl:event)
      (with-counted-foreign-array (wait-count wait-pointer
                                              '%cl:event
                                              wait-list)
        (let* ((pointer (check-errcode-arg
                         (%cl:enqueue-map-buffer (pointer command-queue)
                                                 (pointer buffer)
                                                 blockp rw-flags
                                                 offset octet-count
                                                 wait-count wait-pointer
                                                 (if (or eventp (not blockp))
                                                     event-pointer
                                                     (cffi:null-pointer)))))
               (event (when (or eventp (not blockp))
                        (make-instance 'event :pointer
                                       (mem-aref event-pointer
                                                 '%cl:event)))))
          (make-instance 'read/map-buffer
                         :event event
                         :data-pointer pointer
                         :state (if blockp :ready :waiting)
                         :release-thunk (%unmap-buffer-thunk command-queue
                                                             event
                                                             buffer
                                                             pointer)))))))

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
  (tg:cancel-finalization pointer)
  (check-return+events (wait-list event)
   (%cl:enqueue-unmap-mem-object (pointer command-queue) (pointer buffer)
                                 (data-pointer pointer))))

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
  (let* ((global-size (alexandria:ensure-list global-size))
         (global-offset (alexandria:ensure-list global-offset))
         (local-size (alexandria:ensure-list local-size))
         (dims (min (length global-size) 3)))
    (with-foreign-arrays ((global-size '%cl:size-t global-size :max 3)
                          (global-offset '%cl:size-t global-offset :max 3 :empty-as-null-p t)
                          (local-size '%cl:size-t local-size :max 3 :empty-as-null-p t))
      (check-return+events (wait-list event)
          (%cl:enqueue-nd-range-kernel (pointer command-queue)
                                       (pointer kernel)
                                       dims global-offset
                                       global-size local-size)))))



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
  (with-counted-foreign-array (c p '%cl:mem objects)
    (check-return+events (wait-list event)
        (%cl:enqueue-acquire-gl-objects (pointer command-queue) c p))))

(defun enqueue-release-gl-objects (command-queue objects
                                   &key wait-list event)
  (with-counted-foreign-array (c p '%cl:mem objects)
    (check-return+events (wait-list event)
        (%cl:enqueue-release-gl-objects (pointer command-queue) c p))))

;;; 9.12.3 / 9.8.3 CL Image Objects -> GL Textures

(defun create-from-gl-texture-2d (context usage texture-target miplevel texture)
  (check-errcode-arg
   (%cl:create-from-gl-texture-2d (pointer context) usage texture-target
                                  miplevel texture)))


