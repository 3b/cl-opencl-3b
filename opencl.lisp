(in-package #:cl-opencl)

;; todo: organize these better?

;;; 4.3 contexts

(defmacro check-errcode-arg (form)
  (let ((error-code (gensym))
        (ret (gensym)))
    `(with-foreign-object (,error-code '%cl::error-code)
       (let ((,ret (,@form ,error-code)))
         (setf ,error-code (mem-aref ,error-code '%cl::error-code))
         (if (eq :success ,error-code)
             ,ret
             (error "OpenCL error ~s from ~s" ,error-code ',form))))))

;; todo: error callback, better handling of properties stuff
;; properties arg is ugly since it mixes pointers with enums
;; only one option though, so handling it explicitly for now
(defun create-context (devices &key platform)
  (let ((properties nil))
    (when platform
      (push (pointer-address platform) properties)
      (push :platform properties))
    (with-foreign-objects ((props '%cl:intptr-t (* 2 (1+ (length properties))))
                           (devs '%cl:device-id (length devices)))
      (loop for i from 0
         for dev in devices
         do (setf (mem-aref devs '%cl:device-id i) dev))
     (loop
        for i from 0 by 2 ;; step before list so FINALLY sees correct values
        for (p v) on properties by #'cddr
        do
        (setf (mem-aref props '%cl:context-properties i) p)
        (setf (mem-aref props '%cl:intptr-t (1+ i)) v)
        finally (progn
                  (setf (mem-aref props '%cl:intptr-t i) 0)
                  (setf (mem-aref props '%cl:intptr-t (1+ i)) 0)))
     (check-errcode-arg
      (%cl:create-context props (length devices) devs
                             ;; todo: error callback
                             (cffi:null-pointer) (cffi:null-pointer))))))

(defmacro with-opencl-plist ((var type properties) &body body)
  (let ((base-type (cffi::canonicalize-foreign-type type)))
    `(with-foreign-object (,var ',base-type (* 2 (1+ (length ,properties))))
       (loop
          for i from 0 by 2 ;; step before list so FINALLY sees correct values
          for (p v) on ,properties by #'cddr
          do
            (setf (mem-aref ,var ',type i) p)
            (setf (mem-aref ,var ',base-type (1+ i)) v)
          finally (progn
                    (setf (mem-aref ,var ',base-type i) 0)
                    (setf (mem-aref ,var ',base-type (1+ i)) 0)))
       ,@body)))

(defun create-context-from-type (type &key platform)
  (let ((properties nil))
    (when platform
      (push (pointer-address platform) properties)
      (push :platform properties))
    (with-opencl-plist (props %cl:context-properties properties)
      (check-errcode-arg
       (%cl:create-context-from-type props type
                                     ;; todo: error callback
                                     (cffi:null-pointer) (cffi:null-pointer))))))

(defun retain-context (context)
  (check-return (%cl:retain-context context))
  ;; not sure if returning context here is useful or if it should just return
  ;; :success fom the low-level call?
  context)

(defun release-context (context)
  (check-return (%cl:release-context context)))

(defmacro with-context ((context devices &rest properties) &body body)
  `(let ((,context (create-context ,devices ,@properties)))
     (unwind-protect
          (progn
            ,@body)
       (release-context ,context))))

(defmacro with-context-from-type ((context type &rest properties) &body body)
  `(let ((,context (create-context-from-type ,type ,@properties)))
     (unwind-protect
          (progn
            ,@body)
       (release-context ,context))))



;;;; 5.1 command queues

(defun create-command-queue (context device &rest properties
                             &key out-of-order-exec-mode-enable
                             profiling-enable)
  (declare (ignore out-of-order-exec-mode-enable profiling-enable))
  (check-errcode-arg (%cl:create-command-queue context device properties)))

(defun retain-command-queue (command-queue)
  (check-return (%cl:retain-command-queue command-queue))
  command-queue)

(defun release-command-queue (command-queue)
  (check-return (%cl:release-command-queue command-queue)))

(defun set-command-queue-property (command-queue properties enable &key return-old-properties)
  (if return-old-properties
      (with-foreign-object (old '%cl:command-queue-properties)
        (check-return (%cl:set-command-queue-property command-queue
                                                      properties enable old))
        (mem-aref old '%cl:command-queue-properties)
        #++(foreign-bitfield-symbols '%cl:command-queue-properties old))

      (check-return (%cl:set-command-queue-property command-queue
                                                    properties enable
                                                    (cffi:null-pointer)))))




;;;; 5.2 Memory Objects

;;; 5.2.1 Creating Buffer Objects

;; fixme: should this support preallocated host memory area?
;; skipping for now, since it exposes ffi level stuff...
;; possibly just support copy-host-ptr mode, with copy from lisp array?
(defun create-buffer (context size &rest flags)
  (check-errcode-arg (%cl:create-buffer context flags size (cffi:null-pointer))))

;; should size/count be implicit from array size?
;; foreign type?
;; switch to keywords instead of using &rest for flags?
(defun create-buffer-from-array (context array count foreign-type &rest flags)
  (with-foreign-object (buf foreign-type count)
    (loop repeat count
       for i below (array-total-size array)
       do (setf (mem-aref buf foreign-type i) (row-major-aref array i)))
    (check-errcode-arg
     (%cl:create-buffer context (adjoin :copy-host-pointer flags)
                        (* count (foreign-type-size foreign-type))
                        buf))))

;;; 5.2.2 Reading, Writing and Copying Buffer Objects
#++
(defun enqueue-read-buffer (command-queue buffer block offset count element-type &key wait-list event)
  (let* ((foreign-type (ecase element-type
                         (single-float)))
         (array (make-array count :element-type )))
   (with-pointer-to-vector-dat () a))
)