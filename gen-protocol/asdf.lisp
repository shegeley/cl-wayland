
(in-package #:wayland-scanner)

(defun get-wl-data-dir-from-pkg-config (package-name)
  (let ((output (uiop:run-program (concatenate 'string "pkg-config --variable=pkgdatadir "
					       (string-trim "\n " package-name))
				  :output '(:string :stripped t))))
    (make-pathname :directory (append (list :absolute) (split-sequence #\/ output :remove-empty-subseqs t)))))

(defun get-wl-proto-data-dir ()
  (get-wl-data-dir-from-pkg-config "wayland-protocols"))

(defun get-wl-data-dir ()
  (get-wl-data-dir-from-pkg-config "wayland-server"))

(defclass wl-scanner-op (asdf:downward-operation)
  ()
  (:documentation "This ASDF operation generates files
   from a wayland protocol xml file"))

(defclass base-wl-scanner ()
  ((protocol-type :reader protocol-type
		 :initform :server
		 :initarg :protocol-type
		 :documentation
		 "The type of wayland protocol generated: either :server or :client")
   (protocol-name :reader protocol-name
		  :initarg :protocol-name)
   (protocol-path :reader protocol-path
		  :initarg :protocol-path))
  (:default-initargs
      :protocol-name (error "Must supply a protocol name"))
  (:documentation "This class allows an ASDF operation to find wayland protocol files"))

(defmethod initialize-instance :after ((instance base-wl-scanner) &key (protocol-source :wl-protos) &allow-other-keys)
  ;; if protocol-path isn't explicity set or nil, use the protocol-source keyword:
  (unless (and (slot-boundp instance 'protocol-path) (not (slot-value instance 'protocol-path)))
    (setf (slot-value instance 'protocol-path)
	  (etypecase protocol-source
	    (keyword (ecase protocol-source
		       (:wl-protos (get-wl-proto-data-dir))
		       ((:wl :wl-server :wl-client) (get-wl-data-dir))))
	    (string protocol-source)))))

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

(defmethod input-files ((op wl-scanner-op) (scanner base-wl-scanner))
  (let* ((file-name (concatenate 'string (protocol-name scanner) ".xml"))
	(protocol-file-list (find-file file-name (protocol-path scanner))))
    (when (cdr protocol-file-list)
      (error (format nil "More than one file named ~S found." file-name)))
    (list (first protocol-file-list))))

(defmethod output-files ((op wl-scanner-op) (scanner base-wl-scanner))
  (list (component-pathname scanner)))

(defclass wl-scanner (base-wl-scanner cl-source-file)
  ((gen-interfaces-p :initarg :gen-interfaces-p
		     :accessor gen-interfaces-p))
  (:default-initargs
    :gen-interfaces-p nil)
  (:documentation
   "This ASDF component generates wayland protocol lisp bindings from XML files"))

;; add the wl-scanner-op operation when you see wl-scanner:
(defmethod component-depends-on ((op compile-op) (file wl-scanner))
  `((wl-scanner-op ,file) ,@(call-next-method)))

(defmethod component-depends-on ((op load-source-op) (file wl-scanner))
  `((wl-scanner-op ,file) ,@(call-next-method)))

(defmethod input-files ((op compile-op) (file wl-scanner))
  (list (first (output-files 'wl-scanner-op file))))

(defmethod perform ((op wl-scanner-op) (protocol-file wl-scanner))
  (let ((input-file (first (input-files op protocol-file)))
	(output-file (first  (output-files op protocol-file))))
    (format t "~%Generating file ~A~%" (file-namestring output-file))
    (generate-bindings:generate-bindings (protocol-name protocol-file)
    					 input-file
    					 (protocol-type protocol-file)
    					 :output-file output-file)))

(defclass c-wl-scanner (base-wl-scanner static-file)
  ()
  (:documentation "This ASDF component generates c header files from wayland XML files"))

(defmethod component-depends-on ((op prepare-op) (file c-wl-scanner))
  `((wl-scanner-op ,file) ,@(call-next-method)))

(defmethod output-files ((op wl-scanner-op) (scanner c-wl-scanner))
  (values (call-next-method) t))

(defmethod perform ((op wl-scanner-op) (component c-wl-scanner))
  (let ((input-file (first (input-files op component)))
	(output-file (first  (output-files op component)))
	(type-string (if (eql :server (protocol-type component))
			 "server-header"
			 "client-header")))
    (format t "~%Generating c header file ~A~%" (file-namestring output-file))
    (uiop:run-program (list "wayland-scanner"
			    type-string
			    (namestring input-file)
			    (namestring output-file)))))

(setf (find-class 'asdf::wl-scanner) (find-class 'wl-scanner))
(setf (find-class 'asdf::c-wl-scanner) (find-class 'c-wl-scanner))
