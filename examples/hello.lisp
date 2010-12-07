;; 'hello world'
;;  roughly based on
;;  http://developer.amd.com/gpu/ATIStreamSDK/pages/TutorialOpenCL.aspx ,
;;  but the c++ api doesn't really translate to the current API very well

(defpackage #:hello-opencl
  (:use :cl :ocl))

(in-package #:hello-opencl)

(defparameter *program-source* "
#pragma OPENCL EXTENSION cl_khr_byte_addressable_store : enable
__constant char hw[] = \"Hello World\";
__kernel void hello(__global char * out)
{
    size_t tid = get_global_id(0);
    out[tid] = hw[tid % (sizeof(hw) - 1)];
}")

(defun ensure-platform ()
  (or (car (get-platform-ids))
      (error "no openCL platform found")))

(defun hello ()
  (let* ((platform (ensure-platform))
         (device (car (get-device-ids platform :all))))
    (format t "using ~s / ~s, version ~s~%"
            (get-platform-info platform :platform-vendor)
            (get-platform-info platform :platform-name)
            (get-platform-info platform :platform-version))
    (format t "device id = ~s (~s)~%" device (get-device-info device :type))
    (with-context (context (list device) :platform platform)
      (format t "context = ~s~%" context)
      (let* ((buffer-size 32)
             (buffer (create-buffer context buffer-size :write-only))
             (command-queue (create-command-queue context device))
             (program (create-program-with-source context *program-source*)))
        (format t "buffer = ~s~%" buffer)
        (loop for i in '(:type :flags :size :host-ptr :map-count
                         :reference-count :context
                         #++ :associated-memobject #++ :offset)
           do (format t " buffer (memobj) info: ~s = ~s~%" i
                      (get-mem-object-info buffer i)))
        (loop for i in '(:reference-count :context :num-devices :devices
                         :source :binary-sizes :binaries)
           do (format t "program info: ~s = ~s~%" i
                      (get-program-info program i)))
        (build-program program)
        (loop for i in '(:status :options :log)
           do (format t "program build info: ~s = ~s~%" i
                      (get-program-build-info program device i)))
        (let* ((kernels (create-kernels-in-program program))
               (hello (car kernels)))
          (loop for k in kernels
             do (format t "created kernel from program :~%")
               (loop for i in '(:function-name :num-args :reference-count :context :program)
                  do (format t "  kernel info: ~s = ~s~%" i (get-kernel-info k i))))
          (%set-kernel-arg-buffer hello 0 buffer)
          (enqueue-nd-range-kernel command-queue hello '(32))
          (finish command-queue)
          (print (babel:octets-to-string (print (enqueue-read-buffer command-queue buffer 32 :element-type '(unsigned-byte 8)))))

          (mapc 'release-kernel kernels))
        (release-program program)
        (release-mem-object buffer)))))

#++
(hello)
