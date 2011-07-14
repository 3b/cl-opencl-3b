(in-package #:cl-opencl)

#++(defun get-platform-ids ()
  "returns a list of available OpenCL Platform IDs (opaque, don't need to be
manually released)"
  (with-foreign-object (num-platforms '%cl:uint)
    (check-return (%cl:get-platform-ids 0 (cffi:null-pointer) num-platforms))
    (let ((n (mem-aref num-platforms '%cl:uint)))
      (format t "got count ~s~%" n)
      (with-foreign-object (platforms '%cl:platform-id (1+ n))
        (check-return (%cl:get-platform-ids (1+ n) platforms num-platforms))
        (loop for i below n
           for p = (mem-aref platforms '%cl:platform-id i)
           collect p)))))

(defmacro without-fp-traps (&body body)
  #+(and sbcl (or x86 x86-64))
  `(sb-int:with-float-traps-masked (:invalid :divide-by-zero)
     ,@body)
  #-(and sbcl (or x86 x86-64))
  `(progn ,@body))

(defun get-platform-ids ()
  "returns a list of available OpenCL Platform IDs (opaque, don't need to be
manually released)"
  ;; fixme: figure out if returning same pointer twice is correct,
  ;; possibly remove-duplicates on it if so?
  (without-fp-traps
    (get-counted-list %cl:get-platform-ids () '%cl:platform-id)))

;; fixme: rewrite using define-info-getter
(defun get-platform-info (platform-id param)
  "returns specified info about a platform-id
 param = :profile, :version, :name, :vendor, :extensions"
  ;; space used and parsing depend on the param, so we error if we get
  ;; one we don't know about...
  ;; todo: more specific error?
  (assert (member param '(:profile :version :name :vendor :extensions)))
  (with-foreign-object (fsize '%cl:uint)
    (check-return (%cl:get-platform-info platform-id param
                                         0 (cffi::null-pointer) fsize))
    (let ((size (mem-aref fsize '%cl:uint)))
      (with-foreign-object (s :uchar (1+ size))
        (check-return (%cl:get-platform-info platform-id param (1+ size) s
                                             (cffi::null-pointer)))
       (foreign-string-to-lisp s)))))

(defun split-extension-string (extension-string)
  (loop
     for start = 0 then (1+ end)
     for end = (position #\Space extension-string :start start)
     for ext = (subseq extension-string start end)
     unless (string= ext "")
     collect ext
     while end))

(defun get-extension-list (platform-id)
  (split-extension-string (get-platform-info platform-id :extensions)))

(defun extension-present-p (platform-id name)
  "Check for presence of extension NAME in cl extension list, not
currently implemented for speed, so avoid in inner loops"
  ;; we can't just search here, since we don't want to match substrings.
  ;; it would possibly be cleaner to split up the extension list and
  ;; cache it if there is a good way to tell when to invalidate the
  ;; cache
  (unless (find #\space name)
    (let* ((extension-string (get-platform-info platform-id :extensions))
           (start (search name extension-string))
           (end (when start (+ start (length name)))))
      (and start
           (or (zerop start)
               (char= #\space (char extension-string (1- start))))
           (or (= end (length extension-string))
               (and (< end (length extension-string))
                    (char= #\space (char extension-string end))))))))

(defun get-device-extension-list (device-id)
  (split-extension-string (get-device-info device-id :extensions)))

(defun device-extension-present-p (device-id name)
  "Check for presence of extension NAME in cl extension list, not
currently implemented for speed, so avoid in inner loops"
  ;; we can't just search here, since we don't want to match substrings.
  ;; it would possibly be cleaner to split up the extension list and
  ;; cache it if there is a good way to tell when to invalidate the
  ;; cache
  (unless (find #\space name)
    (let* ((extension-string (get-device-info device-id :extensions))
           (start (search name extension-string))
           (end (when start (+ start (length name)))))
      (and start
           (or (zerop start)
               (char= #\space (char extension-string (1- start))))
           (or (= end (length extension-string))
               (and (< end (length extension-string))
                    (char= #\space (char extension-string end))))))))

(defun get-device-ids (platform-id &rest device-types)
  "get a list of device IDs on specified platform matching any of DEVICE-TYPES
\(:cpu, :gpu, :accelerator, :default, :all)"
  ;; fixme: probably should catch not-found error and just return NIL
  ;; in that case?
  (get-counted-list %cl:get-device-ids (platform-id device-types)
                    '%cl:device-id))

(defun device-filter (rules)
  (lambda (device)
    (let ((extensions (ocl:get-device-extension-list device)))
      (labels ((eval-rule (r)
                 (cond
                   ((null r)
                    ;; we treat empty rule as always matching, since
                    ;; it simplifies things, and not matching anything
                    ;; doesn't seem very useful
                    t)
                   ((stringp r)
                    (not (not (member r extensions :test 'string=))))
                   ((and (consp r) (eq (car r) 'or))
                    (if (cdr r)
                        (some 'identity (mapcar #'eval-rule (cdr r)))
                        nil)
                    #++(let ((a (mapcar #'eval-rule (cdr r))))
                      (format t "eval rule ~s (~s)" r a)
                      (format t " -> ~s~%"  (some 'identity a))
                      (some 'identity a)))
                   ((and (consp r) (eq (car r) 'and))
                    (if (cdr r)
                        (every 'identity (mapcar #'eval-rule (cdr r)))
                        t)
                    #++(let ((a (mapcar #'eval-rule (cdr r))))
                      (format t "eval rule ~s (~s)" r a)
                      (format t " => ~s~%" (every 'identity a))
                      (every 'identity a)))
                   (t (error "don't know how to evaluate device-filter rule ~s?" r)))))
        (eval-rule rules)))))

;; todo: add keywords for extension names in addition to just strings?
(defun get-device-ids-with-extensions (platform-id rules &rest device-types)
  "like get-device-ids, but limit to devices with a specific set of extensions,
specified in a format similar to feature expressions:
  A string specifies a required extension.
  A list starting with OR specifies a list of alternatives, at least 1 of
    which must match
  A list starting with AND specifies a list of requirements, all of which
    must match"
  ;; should NIL always match or never match for rules?
  (remove-if-not (device-filter rules)
                 (apply 'get-device-ids platform-id device-types)))

(defmacro define-info-getter (name (&rest args) fun &body body)
  (let ((count-temp (gensym))
        (foreign-value (gensym))
        (fsize (gensym))
        (s (gensym))
        ;(object (car args))
        (param (car (last args)))
        (call-args (list* `(pointer ,(car args)) (cdr args))))
    `(defun ,name (,@args)
      (ecase ,param
        ,@(loop
             for form in body
             for (pname type count-param flag) = form
             for base-type = (if (eq flag :plist)
                                 ;; fixme: any exported way to do this?
                                 (cffi::canonicalize-foreign-type type)
                                 type)
             when (and type count-param)
             collect `(,pname
                       (let ((,count-temp ,(if (numberp count-param)
                                               count-param
                                               `(,name ,@(butlast args 1) ,count-param))))
                         (with-foreign-object (,foreign-value ',base-type ,count-temp)
                           (check-return
                            (,fun ,@call-args
                                  (* ,(foreign-type-size type)
                                     ,count-temp)
                                  ,foreign-value
                                  (cffi:null-pointer)))
                           (loop for i below ,count-temp
                              for v = (mem-aref ,foreign-value ',type i)
                              ,@ (when (eq flag :plist)
                                   `(for prop = t then (not prop)
                                         when (and prop (not (zerop v)))
                                         collect (foreign-enum-keyword ',type v)
                                         else))
                              collect v))))
             ;; using NIL to indicate params that return an array of
             ;; unknown length, detecting it this way is a bit ugly,
             ;; possibly should just move all the ELSE/COLLECT into
             ;; COLLECT (destructuring-bind ... (cond ...)) so we can check
             ;; for presence of 3rd arg directly?
             else if (and (> (length form) 2)
                          (eq (third form) nil))
             collect `(,pname
                       (with-foreign-object (,fsize '%cl:uint)
                         (check-return (,fun ,@call-args
                                             0 (cffi::null-pointer)
                                             ,fsize))
                         (let ((,count-temp (floor (mem-aref ,fsize '%cl:uint)
                                                   ,(foreign-type-size type))))
                           (with-foreign-object (,foreign-value ',base-type ,count-temp)
                             (check-return
                              (,fun ,@call-args
                                    (* ,(foreign-type-size type)
                                       ,count-temp)
                                    ,foreign-value
                                    (cffi:null-pointer)))
                             (loop for i below ,count-temp
                                for v = (mem-aref ,foreign-value ',base-type i)
                                ,@ (when (eq flag :plist)
                                     `(for prop = t then (not prop)
                                           when (and prop (not (zerop v)))
                                           collect (foreign-enum-keyword ',type v)
                                           else))
                                collect v)))))
             ;; special case for remaining exceptions, just embed some
             ;; code directly...
             else if (> (length form) 2)
             collect `(,pname ,(third form))
             ;; strings could (should?) probably be implemented in terms of the
             ;; unknown-length array stuff...
             else if (eq type :string)
             collect `(,pname
                       (with-foreign-object (,fsize '%cl:uint)
                         (check-return (,fun ,@call-args
                                             0 (cffi::null-pointer)
                                             ,fsize))
                         (let ((,count-temp (mem-aref  ,fsize'%cl:uint)))
                           (with-foreign-object (,s :uchar (1+ ,count-temp))
                             (check-return (,fun ,@call-args (1+ ,count-temp)
                                                 ,s
                                                 (cffi::null-pointer)))
                             (foreign-string-to-lisp ,s)))))
             else
             collect `(,pname
                       (with-foreign-object (,foreign-value ',type)
                         (check-return
                          (,fun ,@call-args
                                ,(foreign-type-size type)
                                ,foreign-value
                                (cffi:null-pointer)))
                         (mem-aref ,foreign-value ',type))))))))


(define-info-getter get-device-info (device-id param) %cl:get-device-info
  (:type %cl:device-type)
  (:vendor-id %cl:uint)
  (:max-compute-units %cl:uint)
  (:max-work-item-dimensions %cl:uint)
  (:max-work-item-sizes %cl:size-t :max-work-item-dimensions)
  (:max-work-group-size %cl:size-t)
  (:preferred-vector-width-char %cl:uint)
  (:preferred-vector-width-short %cl:uint)
  (:preferred-vector-width-int %cl:uint)
  (:preferred-vector-width-long %cl:uint)
  (:preferred-vector-width-float %cl:uint)
  (:preferred-vector-width-double %cl:uint)
  (:max-clock-frequency %cl:uint)
  (:address-bits %cl:uint)
  (:max-read-image-args %cl:uint)
  (:max-write-image-args %cl:uint)
  (:max-mem-alloc-size %cl:ulong)
  (:image2d-max-width %cl:size-t)
  (:image2d-max-height %cl:size-t)
  (:image3d-max-width %cl:size-t)
  (:image3d-max-height %cl:size-t)
  (:image3d-max-depth %cl:size-t)
  (:image-support %cl:bool)
  (:max-parameter-size %cl:size-t)
  (:max-samplers %cl:uint)
  (:mem-base-addr-align %cl:uint)
  (:min-data-type-align-size %cl:uint)
  (:single-fp-config %cl:device-fp-config)
  (:global-mem-cache-type %cl:device-mem-cache-type)
  (:global-mem-cacheline-size %cl:uint)
  (:global-mem-cache-size %cl:ulong)
  (:global-mem-size %cl:ulong)
  (:max-constant-buffer-size %cl:ulong)
  (:max-constant-args %cl:uint)
  (:local-mem-type %cl:device-local-mem-type)
  (:local-mem-size %cl:ulong)
  (:error-correction-support %cl:bool)
  (:profiling-timer-resolution %cl:size-t)
  (:endian-little %cl:bool)
  (:available %cl:bool)
  (:compiler-available %cl:bool)
  (:execution-capabilities %cl:device-exec-capabilities)
  (:queue-properties %cl:command-queue-properties)
  (:name :string)
  (:vendor :string)
  (:driver-version :string)
  (:profile :string)
  (:version :string)
  (:extensions :string)
  (:platform %cl:platform-id)
  ;; opencl 1.1
  (:host-unified-memory %cl:bool)
  (:preferred-vector-width-half %cl:uint)
  (:native-vector-width-half %cl:uint)
  (:native-vector-width-char %cl:uint)
  (:native-vector-width-short %cl:uint)
  (:native-vector-width-int %cl:uint)
  (:native-vector-width-long %cl:uint)
  (:native-vector-width-float %cl:uint)
  (:native-vector-width-double %cl:uint)
  (:opencl-c-version :string))

(define-info-getter get-context-info (context param) %cl:get-context-info
  (:reference-count %cl:uint)
  (:devices %cl:device-id ())
  ;; fixme: support plist stuff: alternating enum/value pairs terminated by a single 0
  (:properties %cl:context-properties () :plist))

(define-info-getter get-command-queue-info (command-queue param) %cl:get-command-queue-info
  (:context %cl:context)
  (:device %cl:device-id)
  (:reference-count %cl:uint)
  (:properties %cl:command-queue-properties))

(defun get-supported-image-formats (context flags image-type)
  (get-counted-list %cl:get-supported-image-formats (context flags image-type)
                    '%cl:image-format))

(define-info-getter get-mem-object-info (memobj param) %cl:get-mem-object-info
  (:type %cl:mem-object-type)
  (:flags %cl:mem-flags)
  (:size %cl:size-t)
  (:host-ptr (:pointer :void))
  (:map-count %cl:uint)
  (:reference-count %cl:uint)
  (:context %cl:context)
  ;; 1.1
  (:associated-memobject %cl:mem)
  (:offset %cl:size-t))


(define-info-getter get-image-info (image param) %cl:get-image-info
  (:format %cl:image-format)
  (:element-size %cl:size-t)
  (:row-pitch %cl:size-t)
  (:slice-pitch %cl:size-t)
  (:width %cl:size-t)
  (:height %cl:size-t)
  (:depth %cl:size-t))


(define-info-getter get-sampler-info (sampler param) %cl:get-sampler-info
  (:reference-count %cl:uint)
  (:context %cl:context)
  (:normalized-coords %cl:bool)
  (:addressing-mode %cl:addressing-mode)
  (:filter-mode %cl:filter-mode))


(define-info-getter get-program-info (program param) %cl:get-program-info
  (:reference-count %cl:uint)
  (:context %cl:context)
  (:num-devices %cl:uint)
  (:devices %cl:device-id ())
  (:source :string)
  (:binary-sizes %cl:size-t ())
  (:binaries
   nil
   ;; fixme: test this...
   (let* ((sizes (get-program-info program :binary-sizes))
          (total-size (reduce '+ sizes)))
     (with-foreign-pointer (buffer total-size)
       (with-foreign-object (pointers '(:pointer :void) (length sizes))
         (loop for j = 0 then (+ size j)
            for size in sizes
            for i from 0
            do (setf (mem-aref pointers :pointer i) (inc-pointer buffer j)))
         (check-return
          (%cl:get-program-info program :binaries
                                (* (foreign-type-size :pointer) (length sizes))
                                pointers
                                (cffi:null-pointer)))
         (loop for i from 0
            for size in sizes
            collect
              (loop with array = (make-array size
                                             :element-type '(unsigned-byte 8))
                 for j below size
                 do (setf (aref array j)
                          (mem-aref (mem-aref pointers :pointer i)
                                    :uchar j))
                 finally (return array))))))))


(define-info-getter get-program-build-info (program device param)
    %cl:get-program-build-info
  (:status %cl:build-status)
  (:options :string)
  (:log :string))

(define-info-getter get-kernel-info (kernel param) %cl:get-kernel-info
  (:function-name :string)
  (:num-args %cl:uint)
  (:reference-count %cl:uint)
  (:context %cl:context)
  (:program %cl:program))

(define-info-getter get-kernel-work-group-info (kernel device param)
    %cl:get-kernel-work-group-info
  (:work-group-size %cl:size-t)
  (:compile-work-group-size %cl:size-t 3)
  (:local-mem-size %cl:ulong))

(define-info-getter get-event-info (event param) %cl:get-event-info
  (:command-queue %cl:command-queue)
  (:command-type %cl:command-type)
  (:command-execution-status %cl:int)
  (:reference-count %cl:uint))

(define-info-getter get-event-profiling-info (event param)
    %cl:get-event-profiling-info
  (:queued %cl:ulong)
  (:submit %cl:ulong)
  (:start %cl:ulong)
  (:end %cl:ulong))




