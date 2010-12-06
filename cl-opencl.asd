
(asdf:defsystem cl-opencl
  :depends-on (cffi)
  :serial t
  :components
  ((:file "bindings-package")
   (:file "types")
   (:file "bindings")
   ;; Lispifications.
   (:file "package")
   (:file "get")
   (:file "opencl")
   ;...
   ))
