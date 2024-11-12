;;;; Filename: system-parser.lisp

;;; System-level dependency analysis functionality.
;;; Provides functionality for parsing ASDF system definitions, 
;;; resolving dependencies between components, and coordinating the 
;;; parsing of individual source files.

(in-package #:dep)


(defclass system-parser ()
  ((tracker 
    :initarg :tracker 
    :reader tracker
    :documentation "The dependency tracker for recording findings")
   (system 
    :initarg :system 
    :reader system
    :documentation "The ASDF system being analyzed")
   (parsing-systems
    :initform nil
    :accessor parsing-systems
    :documentation "Stack of systems being parsed for cycle reporting"))
  (:documentation
   "Parser for analyzing an ASDF system and its components."))


(defun record-dependency-cycle (parser system-name)
  "Record a system dependency cycle in the tracker."
  (let ((position (member system-name (parsing-systems parser) :test #'string=)))
    (when position
      (let* ((cycle (cons system-name (ldiff (parsing-systems parser) position)))
             (chain (format nil "~{~A~^ -> ~}" (reverse cycle))))
        ;; Record the cycle in the dependency tracker
        (record-system-cycle (tracker parser) chain)))))


(defmethod parse-system ((parser system-parser))
  "Parse the system associated with the parser."
  (parse-system-for parser (system parser)))


(defun parse-system-for (parser system)
  "Parse the given system using the parser."
  (let* ((system-name (asdf:component-name system))
         (components (asdf:component-children system)))
    (handler-case
        (progn
          ;; Check for cycle before proceeding
          (when (member system-name (parsing-systems parser) :test #'string=)
            (record-dependency-cycle parser system-name)
            (return-from parse-system-for nil))
          ;; Add system to parsing stack and proceed
          (push system-name (parsing-systems parser))
          (unwind-protect
              (progn
                ;; Parse dependent systems first, reusing the same parser instance
                (dolist (dep (asdf:system-depends-on system))
                  (let* ((dep-name (if (listp dep) (second dep) dep))
                         (dep-sys (asdf:find-system dep-name nil)))
                    (when dep-sys
                      (parse-system-for parser dep-sys))))
                ;; Then parse this system's components
                (mapc (lambda (c) (parse-component c parser)) components))
            ;; Always remove system from parsing stack
            (pop (parsing-systems parser))))
      (error (e)
        ;; Pop system-name from parsing-systems
        (pop (parsing-systems parser))
        (error 'system-parse-error 
               :system-name system-name
               :reason e)))))


(defmethod parse-component ((component t) parser)
  "Parse a single ASDF system component based on its type."
  (typecase component
    (asdf:cl-source-file
      (parse-source-file parser component))
    (asdf:module
      (mapc (lambda (c) (parse-component c parser))
            (asdf:component-children component)))
    (t
      (warn "Skipping unknown component type: ~A" component))))


(defmethod parse-source-file ((parser system-parser) source-file)
  "Parse a source file component using the file parser."
  (let ((file (asdf:component-pathname source-file)))
    (when (probe-file file)
      (let ((file-parser (make-instance 'file-parser 
                                      :tracker (tracker parser)
                                      :file file)))
        (parse-file file-parser)))))


(defun find-system-files (system-name)
  "Get a list of all source files in an ASDF system."
  (let ((system (asdf:find-system system-name)))
    (unless system
      (error 'system-parse-error 
             :system-name system-name
             :reason "System not found"))
    (collect-source-files system)))


(defmethod collect-source-files ((component asdf:component))
  "Recursively collect all source files from an ASDF component."
  (typecase component
    (asdf:cl-source-file
      (list (asdf:component-pathname component)))
    (asdf:module
      (mapcan #'collect-source-files 
              (asdf:component-children component)))
    (t
      nil)))


(defun create-system-parser (tracker system-name)
  "Create a system parser for the named ASDF system."
  (let ((system (asdf:find-system system-name)))
    (unless system
      (error 'system-parse-error 
             :system-name system-name
             :reason "System not found"))
    (make-instance 'system-parser :tracker tracker :system system)))


;; Update the initial call to parse-system
(defun analyze-system (system-name)
  "Analyze an ASDF system and return a dependency tracker with results."
  (let* ((tracker (make-instance 'dependency-tracker :system-name system-name))
         (system (asdf:find-system system-name))
         (parser (make-instance 'system-parser :tracker tracker)))
    (parse-system parser system)
    tracker))