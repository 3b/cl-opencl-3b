
(asdf:defsystem opencl-examples
  :depends-on (cffi cl-opencl cl-opengl cl-glut)
  :components
  ((:file "demo")
   (:module
    "examples"
    :serial t
    :components
    ((:file "opengl-examples-package")
     (:file "util")
     (:file "particle-gl")
     ))))
