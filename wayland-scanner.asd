
(asdf:defsystem #:wayland-scanner
  :description "Wayland protocol generation similar to wayland-scanner with asdf integration"
  :author ("Malcolm Still" "Stuart Dilts")
  :license "BSD 3-Clause"
  :depends-on (#:xmls #:split-sequence #:cl-wayland)
  :serial t
  :components ((:module "gen-protocol"
	        	:serial t
	        	:components
	        	((:file "package")
			 (:file "generate-bindings")
			 (:file "asdf")))))
