;;; particle system using OpenCL and OpenGL

;;; run opencl-opengl-examples:particles, click to add particle
;;; systems, space to clear running systems, esc to quit
;;; might need to adjust *particles-per-click*, or point size in
;;;   glut:display method depending on hardware performance

;;; not really finished, and only slightly optimized...
;;; todo:
;;;   be smarter about picking device when there are more than one: pick
;;;     based on presence of extensions, etc
;;;   reduce pipeline stalls
;;;     use wait events rather than finish between lock/update/unlock
;;;     use optional 1.1 clCreateEventFromGLsyncKHR if available
;;;   more interesting particles, benchmark mode, etc
;;;   reduce memory leaks / clean up resource management stuff

(in-package #:opencl-opengl-examples)

(defparameter *program-source* "
__kernel void particles(__global float4* pos, __global float4* v, float dt, float x, float y,float z)
{
    size_t tid = get_global_id(0);
    // fixme: move the centering stuff to GL...
    float4 center = (float4)(x, y, z, 0.0);
    float4 t = (pos[tid] - center) + v[tid] * dt;
    float r = length((float4)(t.xy,0.0,0.0));
    if (r > 1.0) {
      // probably shouldn't reset to exactly 0.0, since it clumps up all
      // the particles that went past 1 this time step...
      t = (float4)(0.0,0.0,0.0,1.0);
    }
    pos[tid] = center + t;
}")


(defparameter *show-fps* t)

(defclass opencl-particles-window (glut:window)
  ((device :initarg :device :reader device)
   (kernel :initform nil :accessor kernel)
   (systems :initform nil :accessor systems))
  (:default-initargs :width 640 :height 480 :title "OpenCL Particles"
                     :mode '(:double :rgb :depth :multisample)))


(defmacro restartable (&body body)
  `(restart-case
      (progn ,@body)
    (continue () :report "Continue")))


(defparameter *platform* nil)
(defparameter *context* nil)
(defparameter *programs* nil)
(defparameter *kernels* nil)
(defparameter *command-queues* nil)
(defparameter *buffers* nil)
(defparameter *particles-per-click* 100000)


#++(defun now ()
  (float (+ (get-universal-time)
            (nth-value 1 (floor (/ (get-internal-real-time)
                                   internal-time-units-per-second))))
         1d0))
(defun now ()
  (float (/ (get-internal-real-time) internal-time-units-per-second)
         1d0))


(defclass particle-system ()
  ((vbo :initarg :vbo :accessor vbo)
   (position-buffer :initarg :position-buffer :accessor position-buffer)
   (velocity-buffer :initarg :velocity-buffer :accessor velocity-buffer)
   (count :initarg :count :accessor particle-count)
   (start-time :initform (now) :reader start-time)
   (last-update :initform () :reader last-update-time)
   (x :initform 0.0 :initarg :x :reader x)
   (y :initform 0.0 :initarg :y :reader y)
   (z :initform 0.0 :initarg :z :reader z)))

(defconstant +float3-octets+ (* 3 4))
(defconstant +float4-octets+ (* 4 4))

(defun make-particle-system (x y z count)
  (declare (ignorable x y z))
  (let* ((vbo (car (gl:gen-buffers 1)))
         ;; 4 single-floats per particle for velocity
         (vel (create-buffer *context* (* +float4-octets+ count) :read-write))
         (ps (make-instance 'particle-system :vbo vbo :count count
                            :velocity-buffer vel
                            :x x :y y :z z)))

    (gl:bind-buffer :array-buffer vbo)
    (%gl:buffer-data :array-buffer (* +float4-octets+ count) (cffi:null-pointer)
                     :dynamic-draw)
    (setf (position-buffer ps)
          (create-from-gl-buffer *context* :read-write vbo))
    (gl:with-mapped-buffer (p :array-buffer :write-only)
      (loop repeat count
         for i from 0 by 4
         do (setf (cffi:mem-aref p :float (+ i 0)) (float x 1.0)
                  (cffi:mem-aref p :float (+ i 1)) (float y 1.0)
                  (cffi:mem-aref p :float (+ i 2)) (float z 1.0)
                  (cffi:mem-aref p :float (+ i 3)) 1.0)))
    (ocl:with-mapped-buffer (p (car *command-queues*) vel (* 4 count) :write t)
      (loop repeat count
         for i from 0 by 4
         for a = (float (random (* 2 pi)) 1.0)
         for v = (+ 0.2 (random 1.0))
         do (setf (cffi:mem-aref p :float (+ i 0)) (* v (sin a))
                  (cffi:mem-aref p :float (+ i 1)) (* v (cos a))
                  (cffi:mem-aref p :float (+ i 2)) 0.0
                  (cffi:mem-aref p :float (+ i 3)) 0.0)))
    ps))

(defun unmake-particle-system (ps)
  (gl:delete-buffers (list (vbo ps)))
  (setf (vbo ps) nil)
  (when (velocity-buffer ps)
    (release-mem-object (velocity-buffer ps))
    (setf *buffers*
          (delete (velocity-buffer ps) *buffers* :test 'cffi:pointer-eq))
    (setf (velocity-buffer ps) nil))
  (when (position-buffer ps)
    (release-mem-object (position-buffer ps))
    (setf *buffers*
          (delete (position-buffer ps) *buffers* :test 'cffi:pointer-eq))
    (setf (particle-count ps) nil)))

(defun clear-systems (window)
  (format t "clearing ~s particle systems from window~%" (length (systems window)))
  (loop while (systems window)
     do (unmake-particle-system (pop (systems window)))))

(defun draw-system (ps)
  (when (vbo ps)
    (gl:color 0.1 0.1 0.2 0.05)
    (gl:bind-buffer :array-buffer (vbo ps))
    (%gl:vertex-pointer 4 :float 0 (cffi:null-pointer))
    (gl:draw-arrays :points 0 (particle-count ps))
    (gl:bind-buffer :array-buffer 0)))

(defun draw-systems (window)
  (gl:enable-client-state :vertex-array)
  (mapc 'draw-system (systems window))
  (gl:disable-client-state :vertex-array))

(defun update-system (window ps)
  (let ((command-queue (car *command-queues*))
        (pos (position-buffer ps))
        (vel (velocity-buffer ps))
        (kernel (kernel window)))
    (enqueue-acquire-gl-objects command-queue (list pos))
    (finish command-queue)
    (%set-kernel-arg-buffer kernel 0 pos)
    (%set-kernel-arg-buffer kernel 1 vel)
    (%set-kernel-arg-number kernel 2 0.002 :float)
    (%set-kernel-arg-number kernel 3 (x ps) :float)
    (%set-kernel-arg-number kernel 4 (y ps) :float)
    (%set-kernel-arg-number kernel 5 (z ps) :float)
    (enqueue-nd-range-kernel command-queue kernel (particle-count ps))
    (finish command-queue)
    (enqueue-release-gl-objects command-queue (list pos))
    (finish command-queue)))

(defun %lock-system-buffers (ps)
  (let ((command-queue (car *command-queues*))
        (pos (position-buffer ps)))
    (enqueue-acquire-gl-objects command-queue (list pos))))

(defun %update-system (window ps)
  (let ((command-queue (car *command-queues*))
        (pos (position-buffer ps))
        (vel (velocity-buffer ps))
        (kernel (kernel window)))
    (%set-kernel-arg-buffer kernel 0 pos)
    (%set-kernel-arg-buffer kernel 1 vel)
    (%set-kernel-arg-number kernel 2 0.002 :float)
    (%set-kernel-arg-number kernel 3 (x ps) :float)
    (%set-kernel-arg-number kernel 4 (y ps) :float)
    (%set-kernel-arg-number kernel 5 (z ps) :float)
    (enqueue-nd-range-kernel command-queue kernel (particle-count ps))))

(defun %unlock-system-buffers (ps)
  (let ((command-queue (car *command-queues*))
        (pos (position-buffer ps)))
    (enqueue-release-gl-objects command-queue (list pos))))

(defun update-systems (window)
  ;; fixme: switch back to something like this once we can implement
  ;;        UPDATE-SYSTEM with event waits instead of FINISH
  #++(mapcar (lambda (a) (update-system window a)) (systems window))
  (let ((command-queue (car *command-queues*)))
    ;; since we can't wait on events, break the update into pieces
    ;; so we only need 3 FINISH stalls per frame
    (mapcar '%lock-system-buffers (systems window))
    (finish command-queue)
    (mapcar (lambda (a) (%update-system window a)) (systems window))
    (finish command-queue)
    (mapcar '%unlock-system-buffers (systems window))
    (finish command-queue)))

(defmethod reload-programs ((w opencl-particles-window) &key &allow-other-keys)
  (loop while *programs*
     do (release-program (pop *programs*)))
  (let ((program (create-program-with-source *context* *program-source*)))
    (push program *programs*)
    (build-program program)
    (format t "built program: ~s, log = ~s~%"
            (get-program-build-info program (device w) :status)
            (get-program-build-info program (device w) :log))
    (let* ((kernel (create-kernel program "particles")))
      (push kernel *kernels*)
      (setf (kernel w) kernel))))

(defmethod glut:display-window :before ((w opencl-particles-window))
  (setf *context*
        (opencl-example-utils:create-shared-context *platform* (device w)))
  (push (create-command-queue *context* (device w)) *command-queues*)
  (reload-programs w)
  (gl:enable :depth-test :multisample))

(defmethod glut:display ((window opencl-particles-window))
  (opencl-example-utils::update-fps :print *show-fps*)
  (gl:load-identity)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one)
  (gl:point-size 5)
  (gl:disable :depth-test)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  ;; (glu:perspective 50 (/ (glut:width window) (glut:height window)) 0.5 20)
  (gl:matrix-mode :modelview)

  (gl:clear :color-buffer :depth-buffer)
  (restartable
    (draw-systems window))

  (glut:swap-buffers)
  (gl:finish)
  (restartable
    (update-systems window)))

(defmethod glut:idle ((window opencl-particles-window))
  (glut:post-redisplay))

(defmethod glut:reshape ((window opencl-particles-window) width height)
  (format t "reshape ~s ~s~%" width height)
  (setf (glut:width window) width
        (glut:height window) height)
  (gl:viewport 0 0 (glut:width window) (glut:height window)))

(defmethod glut:keyboard ((window opencl-particles-window) key x y)
  (declare (ignore x y))
  (when (eql key #\Esc)
    (glut:destroy-current-window))
  (when (eql key #\r)
    (restartable
      (reload-programs window)))
  (when (eql key #\f)
    (setf *show-fps* (not *show-fps*)))
  (when (eql key #\space)
    (restartable
      (clear-systems window))))

(defmethod glut:mouse ((window opencl-particles-window) button state x y)
  (restartable
    (when (and (eql button :left-button) (eql state :down))
      (push (make-particle-system (1- (* 2 (/ x (float (glut:width window)))))
                                  (- 1.0 (* 2 (/ y (float (glut:height window)))))
                                  0.0
                                  *particles-per-click*)
            (systems window))
      (format t "added particle system, ~s total~%" (length (systems window)))
      (format t " = ~:d particles~%" (reduce #'+ (systems window) :key 'particle-count)))))

(defmethod glut:close ((window opencl-particles-window))
  (format t "window closed, cleaning up...~%")
  (clear-systems window)
  (loop while *buffers*
     do (release-mem-object (pop *buffers*)))
  (loop while *command-queues*
     do (release-command-queue (pop *command-queues*)))
  (when *context* (release-context *context*))
  (setf *context* nil))

(defun particle-window (device)
  (let ((win (make-instance 'opencl-particles-window :device device)))
    (unwind-protect
         (glut:display-window win)
      (clear-systems win))))




(defun ensure-platform ()
  (or (car (get-platform-ids))
      (error "no openCL platform found")))

(defun particles ()
  (let* ((*platform* (ensure-platform))
         (device (car (get-device-ids *platform* :all)))
         (sharing-supported
          (or (device-extension-present-p device "cl_APPLE_gl_sharing")
              (device-extension-present-p device "cl_khr_gl_sharing"))))
    (format t "using ~s / ~s, version ~s~% GL context sharing extension available: ~s~%"
            (get-platform-info *platform* :vendor)
            (get-platform-info *platform* :name)
            (get-platform-info *platform* :version)
            sharing-supported)
    (format t "device id = ~s (~s)~%" device (get-device-info device :type))
    (opencl-example-utils::with-fps-vars ()
      (let ((*programs* nil)
            (*context* nil)
            (*kernels* nil)
            (*command-queues* nil)
            (*buffers* nil))
        (unwind-protect
             (particle-window device)
          (mapc 'release-kernel *kernels*)
          (mapc 'release-program *programs*)
          (mapc 'release-command-queue *command-queues*)
          (mapc 'release-mem-object *buffers*)
          (when *context*
            (release-context *context*))))))
  (format t "done...~%"))

#++
(particles)

#++
(glut:main-loop)