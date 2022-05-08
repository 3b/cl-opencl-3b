;; svm-test.lisp
;; basic svm functionality test.

(in-package #:opencl-opengl-examples)

(defparameter *program-source* "
#pragma OPENCL EXTENSION cl_khr_byte_addressable_store : enable
__constant char hw[] = \"Svm_Test World\";
__kernel void svm_test(__global float * out)
{
    size_t tid = get_global_id(0);
    out[tid] = (float) tid;
}")

(defmacro set-kernel-args (kernel args)
  "Set args of kernel. If element of args is a list use
%set-kernel-svm-buffer in case the second arg is :svm, else
%set-kernel-number, else use %set-kernel-buffer."
  `(progn ,@(loop
               for i from 0
               for arg in args
               collect (if (consp arg)
                           (if (eql (second arg) :svm)
                               `(%set-kernel-arg-svm-buffer ,kernel ,i ,(first arg))    
                               `,(append `(%set-kernel-arg-number ,kernel ,i) arg))
                           `(%set-kernel-arg-buffer ,kernel ,i ,arg)))))

(defun ensure-platform ()
  (or (car (get-platform-ids))
      (error "no openCL platform found")))

(defun svmtest ()
  (let* ((platform (ensure-platform))
         (device (car (get-device-ids platform :all))))
        (declare ((unsigned-byte 64) buffer-size))
    (format t "using ~s / ~s, version ~s~%"
            (get-platform-info platform :vendor)
            (get-platform-info platform :name)
            (get-platform-info platform :version))
    (format t "device id = ~s (~s)~%" device (get-device-info device :type))
    (format t "device svm capabilities = ~s~%" (get-device-info device :svm-capabilities))
    (with-context (context (list device) :platform platform)
      (format t "context = ~s~%" context)
      (let* ((buffer-size 32)
             (buffer (svm-alloc context :float buffer-size :write-only)) ;;; allocate a buffer in shared virtual memory
             (command-queue (create-command-queue context device))
             (program (create-program-with-source context *program-source*)))
        (format t "program = ~s~%" program)
        (format t "buffer = ~s~%" buffer)
        (format t "command-queue = ~s~%" command-queue)
        (loop for i in '(:reference-count :context :num-devices :devices
                         :source :binary-sizes :binaries)
           do (format t "program info: ~s = ~s~%" i
                      (get-program-info program i)))
        (build-program program)
        (loop for i in '(:status :options :log)
           do (format t "program build info: ~s = ~s~%" i
                      (get-program-build-info program device i)))
        (let* ((kernels (create-kernels-in-program program))
               (svm_test (car kernels)))
          (loop for k in kernels
             do (format t "created kernel from program :~%")
               (loop for i in '(:function-name :num-args :reference-count :context :program)
                     do (format t "  kernel info: ~s = ~s~%" i (get-kernel-info k i))))
          ;;; set values of buffer using the host ptr
          (enqueue-svm-map command-queue buffer buffer-size :write t :read t)
          (loop for i below buffer-size do (setf (cffi:mem-aref buffer :float i) 1.0))
          (format t "~&buffer-contents before applying opencl kernel: ~a~%"
                  (loop for i below buffer-size collect (cffi:mem-aref buffer :float i)))
          (enqueue-svm-unmap command-queue buffer)
          (finish command-queue)
          ;;; change buffer values on gpu using opencl
;;;          (%set-kernel-arg-svm-buffer svm_test 0 buffer)
          (set-kernel-args svm_test ((buffer :svm)))
          (enqueue-nd-range-kernel command-queue svm_test `(,buffer-size))
          (finish command-queue)
          ;;; read updated values from host ptr
          (enqueue-svm-map command-queue buffer buffer-size :read t :element-type :float)
          (format t "~&buffer-contents after applying opencl kernel: ~a~%"
                  (loop for i below buffer-size collect (cffi:mem-aref buffer :float i)))
          (enqueue-svm-unmap command-queue buffer)
          (finish command-queue)
          (mapc 'release-kernel kernels))
        (release-program program)
        (svm-free context buffer)))))



#++
(svmtest)
