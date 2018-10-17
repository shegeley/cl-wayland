;;;; cl-wayland.asd

(asdf:defsystem #:cl-wayland
  :description "libwayland bindings for Common Lisp"
  :author ("Malcolm Still" "Stuart Dilts")
  :license "BSD 3-Clause"
  :depends-on (#:cffi #:closer-mop)
  :serial t
  :components ((:file "wayland-interface")
	       (:file "wayland-server-core")))
