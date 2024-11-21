;;;; Filename: system-parser.lisp
;;;
;;; System-level dependency analysis functionality.
;;; Provides functionality for parsing ASDF system definitions, 
;;; resolving dependencies between components, and coordinating the 
;;; parsing of individual source files.


(in-package #:dep)


(defun record-dependency-cycle (parser system-name)
  "Record a system dependency cycle in the tracker."
  (let ((position (member system-name (parsing-systems parser) :test #'string=)))
    (when position
      (let* ((cycle (cons system-name (ldiff (parsing-systems parser) position)))
             (chain (format nil "~{~A~^ -> ~}" (reverse cycle))))
        ;; Record the cycle in the dependency tracker
        (record-system-cycle chain)))))


(defmethod parse-system ((parser system-parser))
  "Parse the system associated with the parser."
  (parse-system-for parser (system parser)))


(defun parse-system-for (parser system)
  "Parse the given system using the parser."
  (let* ((system-name (asdf:component-name system))
         (components (asdf:component-children system)))
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
      (pop (parsing-systems parser)))))


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
      (let ((file-parser (make-instance 'file-parser :file file)))
        (parse-file file-parser)))))


(defun create-system-parser (system-name)
  "Create a system parser for the named ASDF system."
  (let ((system (asdf:find-system system-name)))
    (unless system
      (error 'system-parse-error 
             :system-name system-name
             :reason "System not found"))
    (make-instance 'system-parser :system system)))


(defun analyze-system (system-name)
  "Analyze an ASDF system and create a dependency tracker with results.
   Returns the dependency tracker containing the analysis results.
   Example: (analyze-system \"my-system\")"
  (with-dependency-tracker ((make-instance 'dependency-tracker :system-name system-name))
    (let ((system (asdf:find-system system-name)))
      (let ((parser (make-instance 'system-parser :system system)))
        (parse-system parser)
        *current-tracker*))))


(defun analyze-directory (directory &optional (filespecs '("*.lisp" "*.asd")))
  "Analyze source files in a directory structure and its subdirectories.
   DIRECTORY - The root directory to analyze
   FILESPECS - Optional file patterns to match (default: '(\"*.lisp\" \"*.asd\"))
   Returns the dependency tracker containing the analysis results.
   Example: (analyze-directory #P\"/path/to/project/\")"
  (with-dependency-tracker ((make-instance 'dependency-tracker 
                                         :project-name (format nil "DIR:~A" directory)
                                         :project-root (pathname directory)))
    (labels ((collect-files-for-pattern (dir pattern)
               (let ((files nil))
                 (dolist (file (directory (merge-pathnames pattern dir)))
                   (when (probe-file file)
                     (push file files)))
                 (dolist (subdir (directory (merge-pathnames "*.*" dir)))
                   (when (and (probe-file subdir)
                            (directory-pathname-p subdir))
                     (setf files (nconc files (collect-files-for-pattern subdir pattern)))))
                 files))
             (collect-all-files (dir patterns)
               (let ((files nil))
                 (dolist (pattern (if (listp patterns) patterns (list patterns)))
                   (setf files (nunion files 
                                     (collect-files-for-pattern dir pattern)
                                     :test #'equal)))
                 files))
             (analyze-asd-file (file)
               (with-open-file (stream file :direction :input)
                 (loop for form = (read stream nil nil)
                       while form
                       when (and (listp form)
                                (eq (first form) 'asdf:defsystem)
                                (stringp (second form)))
                       do (let* ((sys-name (second form))
                                (depends-on (getf (cddr form) :depends-on)))
                            (record-system-dependencies *current-tracker* sys-name depends-on))))))
      (let ((files (collect-all-files (pathname directory) filespecs)))
        (dolist (file files)
          (cond ((string-equal (pathname-type file) "asd")
                 (analyze-asd-file file))
                (t (let ((file-parser (make-instance 'file-parser :file file)))
                     (parse-file file-parser)))))
        (detect-system-cycles *current-tracker*)
        *current-tracker*)))))