(in-package #:cl-opencl)

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

(defmacro check-return+events ((wait-list event-p) form &body handlers)
  (alexandria:with-gensyms (wait-count wait-pointer event)
    (let ((form `(with-counted-foreign-array (,wait-count ,wait-pointer
                                                          '%cl:event
                                                          (mapcar 'pointer
                                                                  ,wait-list))
                   (check-return
                       ,(append form (list wait-count
                                           `(if ,wait-list
                                                ,wait-pointer
                                                (cffi:null-pointer))
                                           event))
                     ,@handlers))))
      `(if ,event-p
           (with-foreign-object (,event '%cl:event)
             ,form
             (make-instance 'event :pointer (mem-aref ,event '%cl:event)))
           (let ((,event (null-pointer)))
             ,form)))))

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
                            (format *debug-io* "finalizer releasing ~a ~s with ~s references~%"
                                    ',name
                                    ,pointer
                                    (,info ,pointer :reference-count))
                            (,release ,pointer))))))

       (defmethod release ((,name ,name))
         (let ((,pointer (pointer ,name)))
           (setf (slot-value ,name 'pointer) nil)
           (tg:cancel-finalization ,name)
           (when ,pointer
             (,release ,pointer)))))))

