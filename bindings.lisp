;;;; based on files with following copyright:
;;;;
;;;; * Copyright (c) 2008-2009 The Khronos Group Inc.
;;;; *
;;;; * Permission is hereby granted, free of charge, to any person obtaining a
;;;; * copy of this software and/or associated documentation files (the
;;;; * "Materials"), to deal in the Materials without restriction, including
;;;; * without limitation the rights to use, copy, modify, merge, publish,
;;;; * distribute, sublicense, and/or sell copies of the Materials, and to
;;;; * permit persons to whom the Materials are furnished to do so, subject to
;;;; * the following conditions:
;;;; *
;;;; * The above copyright notice and this permission notice shall be included
;;;; * in all copies or substantial portions of the Materials.
;;;; *
;;;; * THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; * MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.

(cl:in-package #:cl-opencl-bindings)

(define-foreign-library opencl
  (:darwin (:framework "OpenCL")) ;; ?
  ;; ?(:windows ".dll" :calling-convention :stdcall)
  (:unix (:or "libOpenCL.so" "libOpenCL.so.1" "libOpenCL.so.1.0" )))

(use-foreign-library opencl)

(cffi:defcfun ("clCreateKernelsInProgram" create-kernels-in-program) error-code
  (program program)
  (num_kernels uint)
  (kernels (:pointer kernel))
  (num-kernels-ret (:pointer uint)))

(cffi:defcfun ("clCreateContext" create-context) context
  (properties (:pointer context-properties))
  (num-devices uint)
  (devices (:pointer device-id))
  (pfn-notify :pointer) ;; fixme: full type?
  (user-data (:pointer :void))
  (errcode-ret (:pointer error-code)))

(cffi:defcfun ("clGetImageInfo" get-image-info) error-code
  (image mem)
  (param-name image-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

(cffi:defcfun ("clGetProgramBuildInfo" get-program-build-info) error-code
  (program program)
  (device device-id)
  (param-name program-build-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

(cffi:defcfun ("clCreateContextFromType" create-context-from-type) context
  (properties (:pointer context-properties))
  (device-type device-type)
  (pfn-notify :pointer) ;; type?
  (user-data (:pointer :void))
  (errcode-ret (:pointer error-code)))

(cffi:defcfun ("clRetainMemObject" retain-mem-object) error-code
  (memobj mem))

(cffi:defcfun ("clEnqueueMapBuffer" enqueue-map-buffer) (:pointer :void)
  (command-queue command-queue)
  (buffer mem)
  (blocking-map-p bool)
  (map-flags map-flags)
  (offset size-t)
  (cb size-t)
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event))
  (errcode-ret (:pointer error-code)))


(cffi:defcfun ("clSetCommandQueueProperty" set-command-queue-property)
    error-code
  (command-queue command-queue)
  (properties command-queue-properties)
  (enable bool)
  (old-properties (:pointer command-queue-properties)))

(cffi:defcfun ("clSetKernelArg" set-kernel-arg) error-code
  (kernel kernel)
  (arg-index uint)
  (arg-size size-t)
  (arg-value (:pointer :void)))

(cffi:defcfun ("clEnqueueMarker" enqueue-marker) error-code
  (command-queue command-queue)
  (event (:pointer event)))

(cffi:defcfun ("clEnqueueBarrier" enqueue-barrier) error-code
  (command-queue command-queue))

(cffi:defcfun ("clReleaseSampler" release-sampler) error-code
  (sampler sampler))

(cffi:defcfun ("clEnqueueReadBuffer" enqueue-read-buffer) error-code
  (command-queue command-queue)
  (buffer mem)
  (blocking-read-p bool)
  (offset size-t)
  (cb size-t)
  (ptr (:pointer :void))
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

(cffi:defcfun ("clGetDeviceIDs" get-device-ids) error-code
  (platform platform-id)
  (device-type device-type)
  (num-entries uint)
  (devices (:pointer device-id))
  (num-devices (:pointer uint)))

(cffi:defcfun ("clFlush" flush) error-code
  (command-queue command-queue))

(cffi:defcfun ("clCreateCommandQueue" create-command-queue) command-queue
  (context context)
  (device device-id)
  (properties command-queue-properties)
  (errcode-ret (:pointer error-code)))

(cffi:defcfun ("clCreateBuffer" create-buffer) mem
  (context context)
  (flags mem-flags)
  (size size-t)
  (host-ptr (:pointer :void))
  (errcode-ret (:pointer error-code)))

(cffi:defcfun ("clGetProgramInfo" get-program-info) error-code
  (program program)
  (param-name program-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

(cffi:defcfun ("clReleaseContext" release-context) error-code
  (context context))

(cffi:defcfun ("clEnqueueTask" enqueue-task) error-code
  (command-queue command-queue)
  (kernel kernel)
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

(cffi:defcfun ("clEnqueueCopyBuffer" enqueue-copy-buffer) error-code
  (command-queue command-queue)
  (src-buffer mem)
  (dst-buffer mem)
  (src-offset size-t)
  (dst-offset size-t)
  (cb size-t)
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

(cffi:defcfun ("clCreateProgramWithSource" create-program-with-source) program
  (context context)
  (count uint)
  (strings (:pointer :string))
  (lengths (:pointer size-t))
  (errcode-ret (:pointer error-code)))

(cffi:defcfun ("clEnqueueWriteImage" enqueue-write-image) error-code
  (command-queue command-queue)
  (image mem)
  (blocking-write bool)
  (origin (:pointer size-t)) ;; todo: special type for these size_t[3] args?
  (region (:pointer size-t))
  (input-row-pitch size-t)
  (input-slice-pitch size-t)
  (ptr (:pointer :void))
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))


(cffi:defcfun ("clEnqueueWriteBuffer" enqueue-write-buffer) error-code
  (command-queue command-queue)
  (buffer mem)
  (blocking-write-p bool)
  (offset size-t)
  (cb size-t)
  (ptr (:pointer :void))
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

(cffi:defcfun ("clFinish" finish) error-code
  (command-queue command-queue))

(cffi:defcfun ("clEnqueueCopyImageToBuffer" enqueue-copy-image-to-buffer)
    error-code
  (command-queue command-queue)
  (src-image mem)
  (dst-buffer mem)
  (src-origin (:pointer size-t))
  (region (:pointer size-t))
  (dst-offset size-t)
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

(cffi:defcfun ("clBuildProgram" build-program) error-code
  (program program)
  (num-devices uint)
  (device-list (:pointer device-id))
  (potions :string)
  (pfn-notify :pointer) ;; type
  (user-data (:pointer :void)))

(cffi:defcfun ("clEnqueueMapImage" enqueue-map-image) (:pointer :void)
  (command-queue command-queue)
  (image mem)
  (blocking-map bool)
  (map-flags map-flags)
  (origin (:pointer size-t)) ;; [3]
  (region (:pointer size-t)) ;; [3]
  (image-row-pitch (:pointer size-t))
  (image-slice-pitch (:pointer size-t))
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event))
  (errcode-ret (:pointer error-code)))

(cffi:defcfun ("clRetainSampler" retain-sampler) error-code
  (sampler sampler))

(cffi:defcfun ("clGetDeviceInfo" get-device-info) error-code
  (device device-id)
  (param-name device-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

(cffi:defcfun ("clRetainContext" retain-context) error-code
  (context context))

(cffi:defcfun ("clReleaseCommandQueue" release-command-queue) error-code
  (command-queue command-queue))

(cffi:defcfun ("clReleaseEvent" release-event) error-code
  (event event))

(cffi:defcfun ("clEnqueueReadImage" enqueue-read-image) error-code
  (command-queue command-queue)
  (image mem)
  (blocking-read bool)
  (origin (:pointer size-t)) ;;[3]
  (region (:pointer size-t)) ;;[3]
  (row-pitch size-t)
  (slice-pitch size-t)
  (ptr (:pointer :void))
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))


(cffi:defcfun ("clEnqueueCopyBufferToImage" enqueue-copy-buffer-to-image)
    error-code
  (command-queue command-queue)
  (src-buffer mem)
  (dst-image mem)
  (src-offset size-t)
  (dst-origin (:pointer size-t)) ;;[3]
  (region (:pointer size-t)) ;;[3]
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

(cffi:defcfun ("clGetMemObjectInfo" get-mem-object-info) error-code
  (memobj mem)
  (param-name mem-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

(cffi:defcfun ("clGetContextInfo" get-context-info) error-code
  (context context)
  (param-name context-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

(cffi:defcfun ("clEnqueueUnmapMemObject" enqueue-unmap-mem-object) error-code
  (command-queue command-queue)
  (memobj mem)
  (mapped-ptr (:pointer :void))
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

(cffi:defcfun ("clGetCommandQueueInfo" get-command-queue-info) error-code
  (command-queue command-queue)
  (param-name command-queue-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

(cffi:defcfun ("clRetainKernel" retain-kernel) error-code
  (kernel kernel))

(cffi:defcfun ("clGetKernelWorkGroupInfo" get-kernel-work-group-info) error-code
  (kernel kernel)
  (device device-id)
  (param-name kernel-work-group-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

(cffi:defcfun ("clRetainEvent" retain-event) error-code
  (event event))

(cffi:defcfun ("clEnqueueNDRangeKernel" enqueue-nd-range-kernel) error-code
  (command-queue-info command-queue)
  (kernel-work-group-info kernel)
  (work-dim uint)
  (global-work-offset (:pointer size-t))
  (global-work-size (:pointer size-t))
  (local-work-size (:pointer size-t))
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

(cffi:defcfun ("clGetSamplerInfo" get-sampler-info) error-code
  (sampler sampler)
  (param-name sampler-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))


(cffi:defcfun ("clReleaseKernel" release-kernel) error-code
  (kernel kernel))

(cffi:defcfun ("clReleaseMemObject" release-mem-object) error-code
  (memobj mem))

(cffi:defcfun ("clCreateKernel" create-kernel) kernel
  (program program)
  (kernel-name :string)
  (errcode-ret (:pointer error-code)))

(cffi:defcfun ("clGetPlatformIDs" get-platform-ids) error-code
  (num-entries uint)
  (platforms (:pointer platform-id))
  (num-platforms (:pointer uint)))

(cffi:defcfun ("clGetSupportedImageFormats" get-supported-image-formats)
    error-code
  (context context)
  (flags mem-flags)
  (image-type mem-object-type)
  (num-entries uint)
  (image-formats (:pointer image-format))
  (num-image-format (:pointer uint)))

(cffi:defcfun ("clCreateProgramWithBinary" create-program-with-binary) program
  (context context)
  (num-devices uint)
  (device-list (:pointer device-id))
  (lengths (:pointer size-t))
  (binaries (:pointer (:pointer uint8-t)))
  (binary-status (:pointer int))
  (errcode-ret (:pointer error-code)))

(cffi:defcfun ("clRetainProgram" retain-program) error-code
  (program program))

(cffi:defcfun ("clEnqueueCopyImage" enqueue-copy-image) error-code
  (command-queue command-queue)
  (src-image mem)
  (dst-image mem)
  (src-origin (:pointer size-t)) ;;[3]
  (dst-origin (:pointer size-t)) ;;[3]
  (region (:pointer size-t)) ;;[3]
  (num-events-in-wait-list uint)
  (event-wait-list (:pointer event))
  (event (:pointer event)))

(cffi:defcfun ("clGetExtensionFunctionAddress" get-extension-function-address)
    (:pointer :void)
  (function-name :string))

(cffi:defcfun ("clRetainCommandQueue" retain-command-queue) error-code
  (command-queue command-queue))

(cffi:defcfun ("clCreateImage2D" create-image-2d) mem
  (context context)
  (flags mem-flags)
  (image-format (:pointer image-format))
  (image-width size-t)
  (image-height size-t)
  (image-row-pitch size-t)
  (host-ptr (:pointer :void))
  (errcode-ret (:pointer error-code)))

(cffi:defcfun ("clCreateImage3D" create-image-3d) mem
  (context context)
  (flags mem-flags)
  (image-format :pointer)
  (image-width size-t)
  (image-height size-t)
  (image-depth size-t)
  (image-row-pitch size-t)
  (image-slice-pitch size-t)
  (host-ptr (:pointer :void))
  (error-code-ret (:pointer error-code)))

(cffi:defcfun ("clWaitForEvents" wait-for-events) error-code
  (num-events uint)
  (event-list (:pointer event)))

(cffi:defcfun ("clGetEventProfilingInfo" get-event-profiling-info) error-code
  (event event)
  (param-name profiling-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

(cffi:defcfun ("clReleaseProgram" release-program) error-code
  (program program))

(cffi:defcfun ("clEnqueueWaitForEvents" enqueue-wait-for-events) error-code
  (command-queue command-queue)
  (num-events uint)
  (event-list (:pointer event)))

(cffi:defcfun ("clGetPlatformInfo" get-platform-info) error-code
  (platform platform-id)
  (param-name platform-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

(cffi:defcfun ("clGetEventInfo" get-event-info) error-code
  (event event)
  (param-name event-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

(cffi:defcfun ("clGetKernelInfo" get-kernel-info) error-code
  (kernel-name kernel)
  (param-name kernel-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret :pointer))

(cffi:defcfun ("clEnqueueNativeKernel" enqueue-native-kernel) error-code
  (command-queue command-queue)
  (user-func :pointer)
  (args (:pointer :void))
  (cb-args size-t)
  (num-mem-objects uint)
  (mem-list (:pointer mem))
  (args-mem-loc (:pointer (:pointer :void)))
  (num-events-in-wait-list uint)
  (event-wait-list :pointer)
  (event (:pointer event)))

(cffi:defcfun ("clCreateSampler" create-sampler) sampler
  (context context)
  (normalized-coords bool)
  (addressing-mode addressing-mode)
  (filter-mode filter-mode)
  (errcode-ret (:pointer error-code)))

(cffi:defcfun ("clUnloadCompiler" unload-compiler) error-code)
