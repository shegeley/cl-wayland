
(in-package #:wayland-scanner)

(defclass wl-scanner-op (asdf:downward-operation)
  ()
  (:documentation "This ASDF operation generates files
   from a wayland protocol xml file"))

(defclass wl-scanner (cl-source-file)
  ((protocol-type :reader protocol-type
		 :initform :server
		 :initarg :protocol-type
		 :documentation
		 "The type of wayland protocol generated: either :server or :client")
   (protocol-name :reader protocol-name
		  :initarg :protocol-name)
   (gen-interfaces-p :initarg :gen-interfaces-p
		     :accessor gen-interfaces-p)
   (protocol-path :reader protocol-path
		  :initarg :protocol-path
		  :initform (make-pathname :directory
					   (list :absolute "usr" "share" "wayland-protocols"))))
  (:default-initargs
   :protocol-name (error "Must supply a protocol name")
    :gen-interfaces-p nil)
  (:documentation
   "This ASDF component generates wayland protocol bindings and C headers from XML files"))

;; add the wl-scanner-op operation when you see wl-scanner:
(defmethod component-depends-on ((op compile-op) (file wl-scanner))
  `((wl-scanner-op ,file) ,@(call-next-method)))

(defmethod component-depends-on ((op load-source-op) (file wl-scanner))
  `((wl-scanner-op ,file) ,@(call-next-method)))

(defun find-file (file-name path)
  "finds all of the files in path with the name file-name"
  (declare (type string file-name))
  (let* ((wild-path (make-pathname :directory (if (wild-pathname-p path)
						  path
						  (append (pathname-directory path)
							  (list :wild-inferiors)))))
	 (files (uiop:directory-files wild-path)))
    (remove-if-not (lambda (file)
		     (string= (file-namestring file)
			      file-name))
		   files)))

(defmethod input-files ((op compile-op) (file wl-scanner))
  (list (first (output-files 'wl-scanner-op file))))

(defmethod input-files ((op wl-scanner-op) (scanner wl-scanner))
  (let* ((file-name (concatenate 'string (protocol-name scanner) ".xml"))
	(protocol-file-list (find-file file-name (protocol-path scanner))))
    (when (cdr protocol-file-list)
      (error (format nil "More than one file named ~S found." file-name)))
    (list (first protocol-file-list))))

(defmethod output-files ((op wl-scanner-op) (scanner wl-scanner))
  (list (component-pathname scanner)))

(defmethod perform ((op wl-scanner-op) (protocol-file wl-scanner))
  (let ((input-file (first (input-files op protocol-file)))
	(output-file (first  (output-files op protocol-file))))
    (generate-bindings:generate-bindings (protocol-name protocol-file)
    					 input-file
    					 (protocol-type protocol-file)
    					 :output-file output-file)))

(setf (find-class 'asdf::wl-scanner) (find-class 'wl-scanner))
