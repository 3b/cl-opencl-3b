(in-package #:opencl-opengl-examples)

;;; cl-glut:window with extra stuff for managing cl resources

(defclass loaded-program-data ()
  ((id :accessor id :initarg :program-id)
   (kernels :initform (make-hash-table :test 'equal) :accessor kernels)))

;;; separate class, so we can get :around method order right for
;;; glut:display-window, and hopefully at some point reuse some for a
;;; glop version as well
(defclass opencl-window-mixin ()
  ;; list of strings, symbols, or functions denoting program sources
  ;; strings are literal source
  ;; symbols are assumed to have source in symbol-value
  ;; functions should return source
  ;; (todo: pathnames = files to load for source)
  ((program-sources :accessor program-sources)
   ;; user-visible resource tracking stuff
   (platform :initform (or (car (ocl:get-platform-ids))
                           (error "no OpenCL platform found" ))
             :accessor platform
             :documentation "OpenCL platform object to use for window.
 Defaults to first available, can be changed before opening window if needed.
 Behavior is undefined if changed while window is open.")
   (context :initform nil :reader context :accessor %context)
   (device :initform nil :reader device :accessor %device)
   (command-queue :initform nil :reader command-queue :accessor %command-queue)
   ;; internal resource tracking stuff
   (program-data :initform (make-hash-table) :accessor %program-data
                 :documentation "stores either a loaded-program-data
                 instance for a successfully loaded/compiled/etc
                 program, or :failed for a program which failed to
                 load (so we do demand loading, but not retry if it
                 failed and hasn't changed)")
   (mem-objects :initform nil :accessor %mem-objects)
   (required-extensions :initarg :required-extensions :accessor required-extensions)
   (share-contexts :initform nil :initarg :share-gl-contexts :reader share-gl-contexts))
  (:default-initargs :share-gl-contexts t :cl-device-type :all
                     :required-extensions nil))


(defclass opencl-glut-window (glut:window opencl-window-mixin)
  ())

;;; order of operations for using opencl + gl stuff...
;; check for opencl / get platform
;;  - pick from multiple platforms if available?
;;    add some option to request minimum version, vendor, maybe extensions?
;; create window class instance
;; display window (glut:display-window :before)
;;   select device matching requested device type, extensions
;;     set up some extension requirement dsl, ex:
;;       (and (or "cl_APPLE_gl_sharing" "cl_khr_gl_sharing")
;;            "cl_khr_global_int32_base_atomics") or similar
;;   create cl context, enable sharing if requested
;;     if sharing requested
;; runtime
;;   reload programs
;; close window (glut:close)
;;   free mem-objects, command queues, context, ...
;; cleanup
;;
;; -- reinitialize-instance?
;;    probably need to figure out if window is open, clean up and recreate
;;      cl stuff as needed if so?
;;    ignoring for now...

#++
(defmethod initialize-instance :after ((window opencl-glut-window) &key share-gl-contexts &allow-other-keys)

  )

(defgeneric release-opencl-resources (container))

(defmethod release-opencl-resources ((lpd loaded-program-data))
  (loop for k being the hash-keys of (kernels lpd) using (hash-value v)
     do
       (release-kernel v)
       (remhash k (kernels lpd)))
  (setf (kernels lpd) :deleted)
  (release-program (id lpd))
  (setf (id lpd) :deleted))

(defmethod release-opencl-resources ((window opencl-window-mixin))
  (loop for k being the hash-keys of (%program-data window) using (hash-value v)
     do
       (when (typep v 'loaded-program-data)
         (release-opencl-resources v))
       (remhash k (%program-data window)))
  (loop while (%mem-objects window)
     do (release-mem-object (pop (%mem-objects window))))
  #++(loop while (%programs window)
     do (release-program (pop (%programs window))))
  (when (command-queue window)
    (release-command-queue (command-queue window))
    (setf (%command-queue window) nil))
  (when (context window)
    (release-context (context window))
    (setf (%context window) nil)))

;; when window is opened, we search for a device that matches the
;; desired extensions, etc, create a context
(defmethod glut:display-window :around ((window opencl-window-mixin))
  ;; at this point, we should have a valid platform slot, and there
  ;; should be a usable GL context
  (with-accessors ((platform platform) (device %device) (context %context)
                   (command-queue %command-queue)) window
    (let* ((rules (if (share-gl-contexts window)
                      (list 'and
                            '(or "cl_APPLE_gl_sharing" "cl_khr_gl_sharing")
                            (required-extensions window))
                      (required-extensions window)))
           ;; fixme: make device types configurable
           (devices (get-device-ids-with-extensions platform rules
                                                    :all)))
      (assert devices nil "no devices found matching device type (~s) and extension requirements (~s)" :all rules)
      ;; just using first matching device if more than one...
      (setf device (car devices))
      (setf context
            (opencl-example-utils:create-shared-context platform device))
      (setf command-queue (create-command-queue context device))
      (reload-programs window)
      ))
  (call-next-method))

(defmethod glut:close :around ((window opencl-window-mixin))
    ;; fixme: make sure this stuff happens before gl context is
    ;; destroyed if possible
    (format t "opencl window closed, cleaning up...~%")
    ;; nv drivers don't seem to like cleaning up resources while GL
    ;; context is still valid...
    #++
    (release-opencl-resources window)
    (call-next-method))



;;; todo: define-kernel macro
;; (defkernel ("openclName" lisp-name)  (arg1-name arg1-type) ...)
;; creates genfun (lisp-name (win opencl-glut-window) arg1-name ... &key event-list event)
;;  and (lisp-name (k %cl:kernel) arg1-name ... &key event-list event)
;;   -- assuming we can specialize on a cffi:pointer portably?



;;; program junk:


;;; (defprogram progname "foo" "bar")
(defparameter *program-namespace* (make-hash-table))
(defparameter *modified-programs* ())

(defmacro defprogram (name &body source)
  `(progn
     (setf (gethash ',name *program-namespace*)
           ,(cons 'list source))
     (pushnew ',name *modified-programs*)))

(defun program (name)
  (gethash name *program-namespace*))

(defmethod try-load-program ((window opencl-window-mixin) program-name
                             &key error (verbose t) force)
  (when (or force (member program-name *modified-programs*)
            (not (gethash program-name (%program-data window))))
    (when verbose
      (format t "reloading program ~s...~%" program-name))
    (setf *modified-programs* (delete program-name *modified-programs*))
   (with-accessors ((platform platform) (device %device) (context %context)
                    (command-queue %command-queue)) window
     (finish command-queue)
     (let* ((program-source (program program-name))
            (program (apply 'create-program-with-source context program-source)))
       (handler-case
           (build-program program)
         (simple-condition (e)
           (release-program program)
           (unless (gethash program-name (%program-data window))
             (setf (gethash program-name (%program-data window)) :failed))
           (if error
               ;; fixme: don't unwind stack if we want errors
               (error e)
               (when verbose
                 (format t "try-reload-programs failed: ~&~?~%"
                         (simple-condition-format-control e)
                         (simple-condition-format-arguments e))))
           (return-from try-load-program nil)))
       (when verbose
         (format t "built program ~s: ~s, log = ~s~%" program-name
                 (get-program-build-info program device :status)
                 (get-program-build-info program device :log)))
       (let ((kernels (create-kernels-in-program program))
             (program-data (make-instance 'loaded-program-data
                                          :program-id program )))
         (loop for k in kernels
            for kernel-name = (get-kernel-info k :function-name)
            ;; todo: store number of args and verify before calling?
            do (setf (gethash kernel-name (kernels program-data))
                     k))
         (let ((old (gethash program-name (%program-data window))))
           (when (and old (typep old 'loaded-program-data))
            (release-opencl-resources old)))
         (setf (gethash program-name (%program-data window))
               program-data)
         (finish command-queue)
         ;; precding setf would return this anyway, but being specific...
         program-data)))))

;;; (defkernel ("kernelName" lisp-fun progname) (arg1...) (arg2...))
;;;   define genfun(s) lisp-fun
;;;   (or maybe normal fun, that calls genfun to get kernel from
;;;     specified object?)

(defmethod find-kernel ((window opencl-window-mixin) program-name kernel-name)
  (let ((program (gethash program-name (%program-data window))))
    (unless program
      (setf program (try-load-program window program-name :error nil)))
    (when (and program (typep program 'loaded-program-data))
      (gethash kernel-name (kernels program)))))


(defmethod reload-programs ((window opencl-window-mixin)
                            &key error force (verbose t))
  (loop for k being the hash-keys of (%program-data window)
     do (try-load-program window k :force force :error error :verbose verbose)))

(defmacro defkernel (program-name name &rest args)
  ;; todo: if name is a symbol, buld an OpenCL usable name out of symbol-name
  (let ((ocl-name (first name))
        (lisp-name (second name)))
    (alexandria:with-gensyms (target kernel queue dimensions wait-list event program)
     `(defun ,lisp-name (,target ,dimensions ,@(mapcar 'first args) &key ((:wait-list ,wait-list)) ((:event ,event)) ((:program ,program) ',program-name))
        (let ((,queue (command-queue ,target))
              (,kernel (find-kernel ,target ,program ,ocl-name)))
          ;; fixme: usefful error when no command-queue
          ;; fixme: do something useful when kernel not found/loaded/?
          (when (and ,queue ,kernel)
            ,@(loop for (arg type) in args
                for index from 0
                 collect `(%set-kernel-arg ,kernel ,index ,arg ',type))
            (enqueue-nd-range-kernel ,queue ,kernel ,dimensions :wait-list ,wait-list :event ,event)))))))

;;; (lispfun window arg arg arg)
;;;   - look up progname in window
;;;     if not found, compile/link?
;;;     if found, look up "kernelName" in kernels for that program
;;;       if not found, error
;;;       if found, call

(defmethod create-buffer* ((window opencl-window-mixin) size &rest flags)
  (let ((buf (apply 'create-buffer (context window) size flags)))
    (push buf (%mem-objects window))
    buf))

(defmethod create-image-2d* ((window opencl-window-mixin)
                             channel-order channel-type width height
                              &rest rest &key flags (row-pitch 0))
  (declare (ignorable channel-order channel-type width height flags row-pitch))
  (let ((img (apply 'create-image-2d (context window)
                    channel-order channel-type width height rest)))
    (push img (%mem-objects window))
    img))

(defmethod create-from-gl-texture-2d* ((window opencl-window-mixin)
                                       usage texture-target miplevel texture)
  (let ((img (create-from-gl-texture-2d (context window) usage texture-target miplevel texture)))
        (push img (%mem-objects window))
    img))
