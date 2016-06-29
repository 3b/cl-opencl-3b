
(asdf:defsystem cl-opencl
  :depends-on (cffi alexandria)
  :serial t
  :license "MIT"
  :author "Bart Botta <00003b at gmail.com>"
  :description "(unfinished) OpenCL bindings"
  :components
  ((:file "bindings-package")
   (:file "library")
   (:file "util")
   (:file "types")
   (:file "bindings")
   ;; Lispifications.
   (:file "package")
   (:file "get")
   (:file "opencl")
   ;...
   ))
