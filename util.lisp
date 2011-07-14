(cl:in-package #:cl-opencl-bindings)

(cffi:defcfun ("clGetExtensionFunctionAddress" get-extension-function-address)
    (:pointer :void)
  (function-name :string))

(cl:eval-when (:load-toplevel :execute)
  #+clisp (cl:pushnew 'reset-cl-pointers custom:*fini-hooks*)
  #+sbcl (cl:pushnew 'reset-cl-pointers sb-ext:*save-hooks*)
  #+cmu (cl:pushnew 'reset-cl-pointers ext:*before-save-initializations*)
  #-(or clisp sbcl cmu)
  (cl:warn "Don't know how to setup a hook before saving cores on this Lisp."))

(cl:defparameter *cl-extension-resetter-list* cl:nil)
;;; FIXME? There's a possible race condition here, but this function
;;; is intended to be called while saving an image, so if someone is
;;; still calling CL functions we lose anyway...
(cl:defun reset-cl-pointers ()
  (cl:format cl:t "~&;; resetting OpenCL extension pointers...~%")
  (cl:mapc #'cl:funcall *cl-extension-resetter-list*)
  (cl:setf *cl-extension-resetter-list* cl:nil))


(cl:defmacro defclfun (name return-type cl:&body args)
  `(cffi:defcfun ,name ,return-type ,@args))
#++
(cl:defmacro defclfun (name return-type cl:&body args)
  (cl:let ((n (cl:gensym (cl:second name))))
    `(cl:progn
       (cffi:defcfun (,(cl:car name) ,n) ,return-type ,@args)
       (cl:defun ,(cl:second name) ,(cl:mapcar 'cl:first args)
         #++(cl:format cl:t "call ~s:~%   ~s~%" ',name
                    (cl:list ,@(cl:loop for (i cl:nil) in args
                                        collect (cl:format cl:nil "~s:" i)
                                        collect i)))
         (,n ,@(cl:mapcar 'cl:first args))
         ))))


(cl:defmacro defclextfun ((cname lname) return-type cl:&body args)
  (alexandria:with-unique-names (pointer)
    `(cl:let ((,pointer (null-pointer)))
       (cl:defun ,lname ,(cl:mapcar #'cl:car args)
         #++(cl:format cl:t "call ext ~s: ~s~%" ',lname (cl:list ,@(cl:mapcar 'cl:first args)))
         (cl:when (null-pointer-p ,pointer)
           (cl:setf ,pointer (get-extension-function-address ,cname))
           (cl:assert (cl:not (null-pointer-p ,pointer)) ()
                   "Couldn't load symbol ~A~%" ,cname)
           (cl:format cl:t "Loaded function pointer for ~A: ~A~%" ,cname ,pointer)
           (cl:push (cl:lambda () (cl:setf ,pointer (null-pointer)))
                 *cl-extension-resetter-list*))
         (foreign-funcall-pointer
          ,pointer
          (:library opencl)
          ,@(cl:loop for arg in args
                     collect (cl:second arg)
                     collect (cl:first arg))
          ,return-type)))))


(defmacro with-foreign-array ((name type source &key max empty-as-null-p) &body body)
  (let ((p (gensym)))
    (alexandria:once-only (type source)
      `(with-foreign-object (,p ,type ,@(if max
                                            `((min ,max (length ,source)))
                                            `((length ,source))))
         (let ((i 0))
           (map 'nil (lambda (v)
                       (setf (mem-aref ,p ,type i) v)
                       (incf i))
                ,source))
         (let ((,name ,@(if empty-as-null-p
                            `((if (zerop (length ,source))
                                 (null-pointer)
                                 ,p))
                            `(,p))))
           ,@body)))))

(defmacro with-foreign-arrays (bindings &body body)
  (if bindings
      `(with-foreign-array ,(car bindings)
         (with-foreign-arrays ,(cdr bindings)
           ,@body))
      `(progn ,@body)))

(defmacro with-counted-foreign-array ((count-var pointer-var type sequence)
                                      &body body)
  (let ((i (gensym))
        (v (gensym)))
    (alexandria:once-only (sequence type)
      `(let ((,count-var (length ,sequence))
             (,i 0))
         (with-foreign-object (,pointer-var ,type ,count-var)
           (map nil (lambda (,v)
                      (setf (mem-aref ,pointer-var ,type ,i) ,v)
                      (incf ,i))
                ,sequence)
           ,@body)))))

(defmacro check-return (form &body handlers)
  (let ((error-code (gensym)))
    `(restart-case
         (let ((,error-code ,form))
           (case ,error-code
             (:success ,error-code)
             ,@handlers
             (t (error "OpenCL error ~s from ~s" ,error-code ',(car form)))))
       (continue () :report "Continue" ))))

(defmacro get-counted-list (fun (&rest args) type)
  (let ((fcount (gensym))
        (count (gensym))
        (buffer (gensym))
        (i (gensym)))
    `(with-foreign-object (,fcount '%cl:uint)
       (check-return (,fun ,@args 0 (cffi:null-pointer) ,fcount))
       (let ((,count (mem-aref ,fcount '%cl:uint)))
         (when (> ,count 0)
           (with-foreign-object (,buffer ,type (1+ ,count))
             (check-return (,fun ,@args (1+ ,count) ,buffer ,fcount))
             (loop for ,i below ,count
                collect (mem-aref ,buffer ,type ,i))))))))

(defmacro check-errcode-arg (form)
  (let ((error-code (gensym))
        (ret (gensym)))
    `(with-foreign-object (,error-code '%cl::error-code)
       (let ((,ret (,@form ,error-code)))
         (setf ,error-code (mem-aref ,error-code '%cl::error-code))
         (if (eq :success ,error-code)
             ,ret
             (error "OpenCL error ~s from ~s" ,error-code ',form))))))


(defmacro with-opencl-plist ((var type properties) &body body)
  (let ((base-type (cffi::canonicalize-foreign-type type)))
    `(with-foreign-object (,var ',base-type (* 2 (1+ (length ,properties))))
       (loop
          for i from 0 by 2 ;; step before list so FINALLY sees correct values
          for (p v) on ,properties by #'cddr
          do
            (setf (mem-aref ,var ',type i) p)
            (setf (mem-aref ,var ',base-type (1+ i))
                  (if (pointerp v)
                      (pointer-address v)
                      v))
          finally (progn
                    (setf (mem-aref ,var ',base-type i) 0)
                    (setf (mem-aref ,var ',base-type (1+ i)) 0)))
       ,@body)))

(defmacro define-finalized-resource-class (name release info)
  (let ((pointer (gensym)))
    `(progn
       (defclass ,name ()
         ((pointer :initarg :pointer :reader pointer)))

       (defmethod initialize-instance :after ((,name ,name)
                                              &key &allow-other-keys)
         ;; todo: add to scoped resource manager
         (let ((,pointer (when (slot-boundp ,name 'pointer)
                           (pointer ,name))))
           (when ,pointer
             (tg:finalize ,name
                          (lambda ()
                            (format *debug-io* "finalizer releasing ~a with ~s references"
                                    ',name
                                    (,info ,pointer :reference-count))
                            (,release ,pointer))))))

       (defmethod release ((,name ,name))
         (let ((,pointer (pointer ,name)))
           (setf (slot-value ,name 'pointer) nil)
           (tg:cancel-finalization ,name)
           (when ,pointer
             (,release ,pointer)))))))

