;;; simple example of sharing textures between OpenCL and OpenGL
(in-package #:opencl-opengl-examples)

(defprogram colors
  "
__kernel void render( __write_only image2d_t out,int w, float t)
{
    size_t x = get_global_id(0);
    size_t y = get_global_id(1);
    size_t xy = x + y * w;
    float4 colors = (float4)(xy / 256.0, xy / 65536.0, xy / 16777216.0,1.0);
    colors.x = 1 + 0.5 * sin((x + t * 23) / 12.0)
                 + 0.5 * sin((y + t * 43.1) / 13.1);
//    colors.y += 0.3 * cos((y + t) / 10.0);
    colors.y = 1 + 0.3 * cos((x + t * 13) / 7.0) * sin(t)
                 + 0.3 * cos((x+5*sin(y/10.0) + t * 23.1) / 8.1) * cos(t * 1.1);
    colors.z = 1 + 0.3 * cos((x + t * 3) / 17.0) * sin(t * 11)
                 + 0.2 * cos((y + t * 3.1) / 18.1) * cos(t * 11.1);
    write_imagef(out, (int2)(x,y),colors);
}

")

(defprogram clouds
  "
__kernel void render( __write_only image2d_t out,int w, float t)
{
    const float pi = 3.1415927;
    size_t x = get_global_id(0);
    size_t y = get_global_id(1);
    float4 colors = (float4)(0.0,0.0,0.0,1.0);
    float2 xyoffset = (float2)(13.0,96.0);
    float2 xy = (float2)(x,256-y)*4/32 + xyoffset + (float2)(t,-t/12);
    float offset = 0.5;
    float2 phase = (float2)(0.9,0.7);
    float2 freq = (float2)((2 * pi * 0.023),(2 * pi * 0.021));
    float amplitude = 0.3;
    float f = 0.0;
    for (int i = 0; i < 6; i++) {
      float2 fxy = amplitude * (offset + cos(freq * (xy + phase)));
      f += fxy.x * fxy.y;
      phase = pi/2 * (float2)(0.9,1.1) * cos(freq.yx * xy.yx);
      freq *= (float2)(1.9,2.2) + (float2)(i,-i) * (float2)(0.1,0.01);
      amplitude *= 0.707;
    }
    colors.xyz = f;
    write_imagef(out, (int2)(x,y),colors);
}

")
(defkernel colors ("render" texture.render) (out %cl:mem) (w :int) (time :float) )

(defparameter *program-index* 0)
(defparameter *program-list* '(colors clouds))

(defclass texture-window (opencl-glut-window)
  ((texture :accessor texture-name)
   (image :accessor image)
   (w :initform 256 :accessor w))
  (:default-initargs :width 640 :height 480 :title "texture"
                     :mode '(:double :rgb :depth :multisample)
                     :share-gl-contexts t))

(defmethod glut:display-window :before ((w texture-window))
  ;; initialize window...
  #++(setf (image w) (create-image-2d* w :rgba :snorm-int8 (w w) (w w)))
  (let ((texture (car (gl:gen-textures 1))))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-image-2d :texture-2d 0 :rgba8 (w w) (w w) 0 :rgba :float (cffi:null-pointer))
    (gl:generate-mipmap-ext :texture-2d)
    (setf (image w) (create-from-gl-texture-2d* w :write-only :texture-2d
                                                0 texture)
          (texture-name w) texture))
  (gl:enable :depth-test :multisample))


(defmethod update ((window texture-window))
  (enqueue-acquire-gl-objects (command-queue window) (list (image window)))
  (finish (command-queue window))
  (texture.render window (list (w window) (w window)) (image window)
                  (w window) (float (now) 1.0)
                  :program (elt *program-list* (mod *program-index*
                                                    (length *program-list*))))
  (finish (command-queue window))
  (enqueue-release-gl-objects (command-queue window) (list (image window)))
  (finish (command-queue window)))

(defmethod glut:display ((window texture-window))
  (opencl-example-utils::update-fps :print *show-fps*)
  (gl:load-identity)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  ;;(glu:perspective 50 (/ (glut:width window) (glut:height window)) 0.5 20)
  (gl:matrix-mode :modelview)

  (gl:clear :color-buffer :depth-buffer)

  (when (texture-name window)
    (gl:bind-texture :texture-2d (texture-name window))
    (gl:generate-mipmap :texture-2d))

  (gl:enable :texture-2d)
  (gl:with-primitive :quads
    (gl:tex-coord 0 0)
    (gl:vertex -1 -1)

    (gl:tex-coord 0 1)
    (gl:vertex -1 1)

    (gl:tex-coord 1 1)
    (gl:vertex 1 1)

    (gl:tex-coord 1 0)
    (gl:vertex 1 -1))

  (glut:swap-buffers)
  (gl:finish)
  (restartable
    (when *modified-programs*
      (reload-programs window)))
  (restartable (update window)))

(defmethod glut:idle ((window texture-window))
  (glut:post-redisplay))

(defmethod glut:reshape ((window texture-window) width height)
  (setf (glut:width window) width
        (glut:height window) height)
  (gl:viewport 0 0 (glut:width window) (glut:height window)))

(defmethod glut:keyboard ((window texture-window) key x y)
  (declare (ignore x y))
  (when (eql key #\Esc)
    (glut:destroy-current-window))
  (when (eql key #\f)
    (setf *show-fps* (not *show-fps*)))
  (when (eql key #\s)
    (setf *program-index* (mod (1+ *program-index*) (length *program-list*))))
  (when (eql key #\r)
    (restartable
      (reload-programs window :force t))))

(defun texture ()
  (glut:display-window (make-instance 'texture-window)))


#++
(texture)

#++
(glut:main-loop)