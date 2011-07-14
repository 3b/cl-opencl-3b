
(asdf:defsystem cl-opencl
  :depends-on (cffi alexandria trivial-garbage)
  :serial t
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
