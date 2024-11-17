;;;; Filename: project-parser.lisp
;;;
;;; Project-level dependency analysis functionality.
;;; Provides functionality for parsing ASDF project definitions, 
;;; resolving dependencies between components, and coordinating the 
;;; parsing of individual source files.


(in-package #:dep)


(defun record-dependency-cycle (parser project-name)
  "Record a project dependency cycle in the tracker.
   A cycle exists if we encounter a project that is already being parsed."
  (let ((position (member project-name (parsing-projects parser) :test #'string=)))
    (when position
      (let* ((cycle (cons project-name (ldiff (parsing-projects parser) position)))
             (chain (format nil "~{~A~^ -> ~}" cycle)))
        ;; Record the cycle and create an anomaly
        (record-project-cycle chain)
        (record-anomaly *current-tracker*
                       :project-cycle
                       :error
                       project-name
                       (format nil "Project dependency cycle detected: ~A" chain)
                       cycle)))))


(defmethod parse-project ((parser project-parser))
  "Parse the project associated with the parser."
  (parse-project-for parser (project parser)))


(defun parse-project-for (parser project)
  "Parse the given project using the parser.
   Records any dependency cycles encountered during parsing."
  (let* ((project-name (asdf:component-name project))
         (components (asdf:component-children project)))
    (handler-case
        (progn
          ;; Check for cycle before proceeding
          (when (member project-name (parsing-projects parser) :test #'string=)
            (record-dependency-cycle parser project-name)
            (return-from parse-project-for nil))
          ;; Add project to parsing stack and proceed
          (push project-name (parsing-projects parser))
          (unwind-protect
              (progn
                ;; Parse dependent projects first
                (dolist (dep (asdf:system-depends-on project))
                  (let* ((dep-name (if (listp dep) (second dep) dep))
                         (dep-sys (asdf:find-system dep-name nil)))
                    (when dep-sys
                      (parse-project-for parser dep-sys))))
                ;; Then parse this project's components
                (mapc (lambda (c) (parse-component c parser)) components))
            ;; Always remove project from parsing stack
            (pop (parsing-projects parser))))
      (error (e)
        (pop (parsing-projects parser))
        (error 'project-parse-error 
               :project-name project-name
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


(defmethod parse-source-file ((parser project-parser) source-file)
  "Parse a source file component using the file parser."
  (let ((file (asdf:component-pathname source-file)))
    (when (probe-file file)
      (let ((file-parser (make-instance 'file-parser :file file)))
        (parse-file file-parser)))))


(defun create-project-parser (project-name)
  "Create a project parser for the named ASDF system."
  (let ((system (asdf:find-system project-name)))
    (unless system
      (error 'project-parse-error 
             :project-name project-name
             :reason "System not found"))
    (make-instance 'project-parser :project system)))


(defun analyze-project (project-name)
  "Analyze an ASDF project and create a dependency tracker with results.
   Returns the dependency tracker containing the analysis results.
   Example: (analyze-project \"my-project\")"
  (with-dependency-tracker ((make-instance 'dependency-tracker :project-name project-name))
    (let* ((system (asdf:find-system project-name)))
      (unless system
        (error 'project-parse-error 
               :project-name project-name
               :reason "System not found"))
      (let ((parser (make-instance 'project-parser :project system)))
        (parse-project parser)
        *current-tracker*))))


(defun analyze-directory (directory &optional (filespecs '("*.lisp")))
  "Analyze source files in a directory structure and its subdirectories.
   DIRECTORY - The root directory to analyze
   FILESPECS - Optional file patterns to match (default: '(\"*.lisp\"))
               Can be a single string pattern or list of patterns
   Returns the dependency tracker containing the analysis results.
   Example: (analyze-directory #P\"/path/to/project/\")
            (analyze-directory #P\"/path/to/project/\" '(\"*.lisp\" \"*.cl\" \"*.lsp\"))"
  (with-dependency-tracker ((make-instance 'dependency-tracker 
                                         :project-name (format nil "DIR:~A" directory)))
    (labels ((collect-files-for-pattern (dir pattern)
               (let ((files nil))
                 ;; Get matching files in current directory
                 (dolist (file (directory (merge-pathnames pattern dir)))
                   (when (probe-file file)
                     (push file files)))
                 ;; Always process subdirectories
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
                 files)))
      ;; Collect all matching files
      (let ((files (collect-all-files (pathname directory) filespecs)))
        ;; Parse each file
        (dolist (file files)
          (handler-case
              (let ((file-parser (make-instance 'file-parser :file file)))
                (parse-file file-parser))
            (error (e)
              (warn "Error parsing file ~A: ~A" file e))))
        *current-tracker*))))