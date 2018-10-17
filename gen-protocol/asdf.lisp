
(in-package #:wayland-scanner)

(defclass wl-scanner-op (asdf:downward-operation)
  ()
  (:documentation "This ASDF operation generates files
   from a wayland protocol xml file"))

(defclass wl-scanner (cl-source-file)
  ((binding-type :reader binding-types
		 :initform (list :server)
		 :initarg :binding-types
		 :documentation
		 "The type of wayland protocol generated: either :server or :client")
   (gen-interfaces-p :initarg :gen-interfaces-p
		     :accessor gen-interfaces-p)
   (gen-c-header :reader gen-c-header-p
		 :initarg :gen-c-header
		 :initform nil)
   (protocol-path :reader protocol-path
		  :initarg :protocol-path
		  :initform (make-pathname :directory '(:absolute "usr" "share" "wayland-protocols")
					   )))
  (:documentation
   "This ASDF component generates wayland protocol bindings and C headers from XML files"))

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
  (let* ((file-name (concatenate 'string (component-name scanner) ".xml"))
	(protocol-file-list (find-file file-name (protocol-path scanner))))
    (when (cdr protocol-file-list)
      (error (format nil "More than one file named ~S found." file-name)))
    (list (first protocol-file-list))))

(defmethod output-files ((op wl-scanner-op) (scanner wl-scanner))
  (let ((output-files (append
		       (when (member :client (binding-types scanner))
			 (list (make-pathname :name (concatenate 'string (component-name scanner)
								 "-client-protocol")
  					      :type "lisp"
  					      :defaults (component-pathname scanner))))
		       (when (member :server (binding-types scanner))
			 (list (make-pathname :name (concatenate 'string (component-name scanner)
								 "-protocol")
  					      :type "lisp"
  					      :defaults (component-pathname scanner)))))))
    output-files))

(defmethod perform ((op wl-scanner-op) (protocol-file wl-scanner))
  (let ((input-file (first (input-files op protocol-file)))
	(output-files (output-files op protocol-file)))
    (dolist (file output-files)
      (generate-bindings:generate-bindings (component-name protocol-file)
    					   input-file
    					   (when (search "client" (file-namestring file)))
    					   :output-file file))))


(setf (find-class 'asdf::wl-scanner) (find-class 'wl-scanner))
