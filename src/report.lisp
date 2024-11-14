;;;; Filename: report.lisp
;;;
;;; Report generation functionality for dependency analysis results.
;;; Provides multiple report formats for visualizing dependencies between
;;; files, packages, and symbols. Includes robust error handling for
;;; file operations.


(in-package #:dep)


;;; Error Conditions

(define-condition report-error (analyzer-error)
  ((path 
    :initarg :path 
    :reader report-path
    :documentation "Path that caused the error")
   (reason 
    :initarg :reason 
    :reader reason
    :documentation "Underlying cause of the error"))
  (:documentation 
   "Signaled when a report cannot be generated or saved.")
  (:report (lambda (condition stream)
             (format stream "Error generating report for path ~A: ~A"
                     (report-path condition)
                     (reason condition)))))


;;; Utility Functions

(defun pathname-to-string (pathname)
  "Convert a pathname to a string representation."
  (if pathname
      (namestring pathname)
      nil))


(defun simplify-path (pathname)
  "Convert a pathname to a simplified string representation."
  (if pathname
      (let ((name (enough-namestring pathname)))
        (if (char= (char name 0) #\/)
            (subseq name 1)
            name))
      nil))


(defun source-file-name (pathname)
  "Extract just the source file name from a pathname."
  (if pathname
      (file-namestring pathname)
      nil))


(defun string-to-dot-id (string)
  "Convert a string to a valid DOT graph identifier."
  (string-downcase
   (with-output-to-string (s)
     (loop for char across (or string "")
           do (case char
                ((#\Space #\/ #\\ #\. #\-)
                 (write-char #\_ s))
                (otherwise
                 (write-char char s)))))))


(defun abbreviate-path (path)
  "Create an abbreviated version of a path string."
  (let ((path-str (simplify-path path)))
    (if (> (length path-str) 50)
        (concatenate 'string "..." 
                    (subseq path-str (max 0 (- (length path-str) 47))))
        path-str)))


(defun ensure-directory-exists (pathname)
  "Ensure the directory component of pathname exists. Returns pathname if successful."
  (handler-case
      (let* ((namestring (namestring pathname))
             (dir (directory-namestring pathname))
             (normalized-dir (if (uiop:os-windows-p)
                               ;; Convert forward slashes to backslashes on Windows
                               (uiop:native-namestring dir)
                               dir)))
        ;; Print diagnostic info
        (format *error-output* "~&Attempting to access directory: ~A~%" normalized-dir)
        (ensure-directories-exist normalized-dir)
        pathname)
    (error (e)
      (error 'report-error
             :path pathname
             :reason (format nil "Could not access directory: ~A~%Error details: ~A" 
                           pathname e)))))


(defun verify-writable (pathname)
  "Verify the specified path is writable. Returns pathname if writable."
  (handler-case 
      (with-open-file (test pathname
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
        (write-char #\Space test)
        pathname)
    (error (e)
      (error 'report-error
             :path pathname
             :reason (format nil "Path is not writable: ~A" e)))))


;;; Utility Functions for ASCII Tree Visualization

(defun build-file-dependency-tree (tracker)
  "Build a tree structure representing file dependencies."
  (let ((nodes (make-hash-table :test 'equal))
        (roots nil))
    ;; First pass: Create nodes for all files that are either defined in or referenced
    (maphash (lambda (file definitions)
               (declare (ignore definitions))
               ;; Create node for the defined file
               (setf (gethash file nodes)
                     (list :name (source-file-name file)
                           :full-name file
                           :children nil 
                           :parents nil))
               ;; Create nodes for all files that reference this one
               (dolist (dependent (file-dependents tracker file))
                 (unless (gethash dependent nodes)
                   (setf (gethash dependent nodes)
                         (list :name (source-file-name dependent)
                               :full-name dependent
                               :children nil
                               :parents nil)))))
             (slot-value tracker 'file-map))
    
    ;; Second pass: Connect dependencies based on dependencies and dependents
    (maphash (lambda (file node)
               ;; Connect based on file dependencies
               (dolist (dep (file-dependencies tracker file))
                 (when-let ((dep-node (gethash dep nodes)))
                   (pushnew node (getf dep-node :children)
                           :test #'equal
                           :key (lambda (n) (getf n :full-name)))
                   (pushnew dep-node (getf node :parents)
                           :test #'equal
                           :key (lambda (n) (getf n :full-name)))))
               ;; Connect based on file dependents
               (dolist (dep (file-dependents tracker file))
                 (when-let ((dep-node (gethash dep nodes)))
                   (pushnew dep-node (getf node :children)
                           :test #'equal
                           :key (lambda (n) (getf n :full-name)))
                   (pushnew node (getf dep-node :parents)
                           :test #'equal
                           :key (lambda (n) (getf n :full-name))))))
             nodes)
    
    ;; Find root nodes (files that are not depended on by any other files)
    (maphash (lambda (file node)
               (when (null (getf node :parents))
                 (push node roots)))
             nodes)
    (sort roots #'string< :key (lambda (node) (getf node :name)))))


(defun build-package-dependency-tree (tracker)
  "Build a tree structure representing package dependencies.
   Returns two values:
   1. List of root package nodes
   2. List of circular dependencies"
  (let ((nodes (make-hash-table :test 'equal))
        (roots nil)
        (cycles nil))
    ;; First pass: Create nodes for all packages including CL
    (maphash (lambda (pkg used-pkgs)
               (declare (ignore used-pkgs))
               ;; Create node for the package
               (setf (gethash pkg nodes)
                     (list :name pkg
                           :children nil
                           :parents nil))
               ;; Create nodes for used packages including COMMON-LISP
               (dolist (used (get-package-uses tracker pkg))
                 (unless (gethash used nodes)
                   (setf (gethash used nodes)
                         (list :name used
                               :children nil
                               :parents nil)))))
             (slot-value tracker 'package-uses))
    
    ;; Ensure COMMON-LISP package exists if any package uses it
    (maphash (lambda (pkg used-pkgs)
               (when (member "COMMON-LISP" used-pkgs :test #'string=)
                 (unless (gethash "COMMON-LISP" nodes)
                   (setf (gethash "COMMON-LISP" nodes)
                         (list :name "COMMON-LISP"
                               :children nil
                               :parents nil)))))
             (slot-value tracker 'package-uses))
    
    ;; Second pass: Connect dependencies
    (maphash (lambda (pkg used-pkgs)
               (let ((pkg-node (gethash pkg nodes)))
                 (dolist (used used-pkgs)
                   (when-let ((used-node (gethash used nodes)))
                     ;; Check for circular dependency
                     (when (member pkg (get-package-uses tracker used))
                       (pushnew (format nil "~A ↔ ~A" pkg used)
                               cycles :test #'string=))
                     ;; Only add child/parent relationship if not already circular
                     (unless (member pkg (get-package-uses tracker used))
                       (pushnew pkg-node (getf used-node :children)
                               :test #'equal
                               :key (lambda (n) (getf n :name)))
                       (pushnew used-node (getf pkg-node :parents)
                               :test #'equal
                               :key (lambda (n) (getf n :name))))))))
             (slot-value tracker 'package-uses))
    
    ;; Find root packages (those with no parents)
    (maphash (lambda (pkg node)
               (when (null (getf node :parents))
                 (push node roots)))
             nodes)
    (values (sort roots #'string< :key (lambda (node) (getf node :name)))
            (sort cycles #'string<))))


(defun print-ascii-tree (stream roots &optional (prefix "") (last-child-p nil))
  "Print an ASCII representation of a dependency tree."
  (dolist (node (butlast roots))
    (format stream "~&~A├── ~A~%" prefix (getf node :name))
    (print-ascii-tree stream 
                      (sort (getf node :children) #'string< 
                            :key (lambda (n) (getf n :name)))
                      (concatenate 'string prefix "│   ")
                      nil))
  (when-let ((last-node (car (last roots))))
    (format stream "~&~A└── ~A~%" prefix (getf last-node :name))
    (print-ascii-tree stream 
                      (sort (getf last-node :children) #'string< 
                            :key (lambda (n) (getf n :name)))
                      (concatenate 'string prefix "    ")
                      t)))


;;; JSON Building Functions

(defun build-file-dependency-json (tracker)
  "Build JSON structure for file dependencies."
  (let ((result (make-hash-table :test 'equal)))
    (maphash (lambda (file definitions)
               (declare (ignore definitions))
               (let* ((file-str (pathname-to-string file))
                      (deps (remove nil (mapcar #'pathname-to-string 
                                              (file-dependencies tracker file))))
                      (required-by (remove nil (mapcar #'pathname-to-string 
                                                     (file-dependents tracker file)))))
                 (when (or deps required-by)
                   (setf (gethash file-str result)
                         (alexandria:alist-hash-table
                          (remove nil
                                 `(,@(when deps `(("depends_on" . ,deps)))
                                   ,@(when required-by `(("required_by" . ,required-by)))))
                          :test 'equal)))))
             (slot-value tracker 'file-map))
    result))


(defun build-package-dependency-json (tracker)
  "Build JSON structure for package dependencies."
  (let ((result (make-hash-table :test 'equal)))
    (maphash (lambda (pkg used-pkgs)
               (let ((exports (mapcar #'symbol-name (get-package-exports tracker pkg))))
                 (when (or used-pkgs exports)
                   (setf (gethash pkg result)
                         (alexandria:alist-hash-table
                          (remove nil
                                 `(,@(when used-pkgs `(("uses" . ,used-pkgs)))
                                   ,@(when exports `(("exports" . ,exports)))))
                          :test 'equal)))))
             (slot-value tracker 'package-uses))
    result))


;;; Report Generation Methods

(defgeneric generate-report (format tracker &key stream)
  (:documentation "Generate a dependency report in the specified format."))


(defmethod generate-report ((format (eql :text)) tracker &key (stream *standard-output*))
  (let ((system-name (system.name tracker)))
    ;; Header
    (format stream "~&Dependency Analysis Report for System: ~A~%" system-name)
    (format stream "~&================================================~%")
    
    ;; Cycle warnings - only show sections if cycles exist
    (alexandria:when-let ((cycles (get-system-cycles tracker)))
      (format stream "~&System Dependency Cycles:~%")
      (dolist (cycle cycles)
        (format stream "  ~A~%" cycle)))
    
    (alexandria:when-let ((cycles (get-file-cycles tracker)))
      (format stream "~&File Dependency Cycles:~%")
      (dolist (cycle cycles)
        (format stream "  ~A~%" cycle)))
    
    (alexandria:when-let ((cycles (get-package-cycles tracker)))
      (format stream "~&Package Dependency Cycles:~%")
      (dolist (cycle cycles)
        (format stream "  ~A~%" cycle)))
    
    ;; File dependencies
    (format stream "~&~%File Dependencies:~%")
    (maphash (lambda (file definitions)
               (declare (ignore definitions))
               (format stream "~&File: ~A~%" (pathname-to-string file))
               (let ((deps (file-dependencies tracker file)))
                 (when deps
                   (format stream "  Depends on:~%")
                   (dolist (dep deps)
                     (format stream "    ~A~%" (pathname-to-string dep)))))
               (let ((deps (file-dependents tracker file)))
                 (when deps
                   (format stream "  Required by:~%")
                   (dolist (dep deps)
                     (format stream "    ~A~%" (pathname-to-string dep))))))
             (slot-value tracker 'file-map))
    
    ;; Package relationships
    (format stream "~&~%Package Dependencies:~%")
    (maphash (lambda (pkg used-pkgs)
               (format stream "~&Package: ~A~%" pkg)
               (when used-pkgs
                 (format stream "  Uses packages:~%")
                 (dolist (used used-pkgs)
                   (format stream "    ~A~%" used)))
               (let ((exports (get-package-exports tracker pkg)))
                 (when exports
                   (format stream "  Exports:~%")
                   (dolist (sym exports)
                     (format stream "    ~A~%" sym)))))
             (slot-value tracker 'package-uses))))


(defmethod generate-report ((format (eql :json)) tracker &key (stream *standard-output*))
  (let ((*print-pretty* t)
        (yason:*symbol-key-encoder* #'string-downcase)
        (yason:*symbol-encoder* #'string-downcase))
    (yason:with-output (stream :indent t)
      (yason:with-object ()
        ;; System name
        (yason:encode-object-element "system" (system.name tracker))
        
        ;; Cycles (if any exist)
        (let ((system-cycles (get-system-cycles tracker))
              (file-cycles (get-file-cycles tracker))
              (package-cycles (get-package-cycles tracker)))
          (when (or system-cycles file-cycles package-cycles)
            (yason:with-object-element ("cycles")
              (yason:with-object ()
                (when system-cycles
                  (yason:encode-object-element "system" system-cycles))
                (when file-cycles
                  (yason:encode-object-element "files" file-cycles))
                (when package-cycles
                  (yason:encode-object-element "packages" package-cycles))))))
        
        ;; File dependencies
        (let ((files (build-file-dependency-json tracker)))
          (when files
            (yason:with-object-element ("files")
              (yason:encode files))))
        
        ;; Package dependencies
        (let ((packages (build-package-dependency-json tracker)))
          (when packages
            (yason:with-object-element ("packages")
              (yason:encode packages))))))))


(defmethod generate-report ((format (eql :dot)) tracker &key (stream *standard-output*))
  (format stream "digraph Dependencies {~%")
  (format stream "  rankdir=LR;~%")
  (format stream "  compound=true;~%")
  (format stream "  node [shape=box, fontname=\"Arial\"];~%")
  (format stream "  edge [fontname=\"Arial\"];~%")
  
  ;; Create package subgraph
  (format stream "  subgraph cluster_packages {~%")
  (format stream "    label=\"Package Dependencies\";~%")
  (format stream "    style=dashed;~%")
  (format stream "    node [style=filled,fillcolor=lightgrey];~%")
  
  ;; Add package nodes and edges
  (maphash (lambda (pkg used-pkgs)
             (let ((pkg-id (string-to-dot-id pkg)))
               (format stream "    \"pkg_~A\" [label=\"~A\"];~%" pkg-id pkg)
               (dolist (used used-pkgs)
                 (let ((used-id (string-to-dot-id used)))
                   (format stream "    \"pkg_~A\" [label=\"~A\"];~%" used-id used)
                   (format stream "    \"pkg_~A\" -> \"pkg_~A\";~%"
                           pkg-id used-id)))))
           (slot-value tracker 'package-uses))
  (format stream "  }~%~%")
  
  ;; Create file subgraph
  (format stream "  subgraph cluster_files {~%")
  (format stream "    label=\"File Dependencies\";~%")
  (format stream "    style=dashed;~%")
  
  ;; Track all files we need nodes for
  (let ((all-files (make-hash-table :test 'equal)))
    ;; Collect files from file-map and dependents
    (maphash (lambda (file definitions)
               (declare (ignore definitions))
               (setf (gethash file all-files) t)
               (dolist (dependent (file-dependents tracker file))
                 (setf (gethash dependent all-files) t)))
             (slot-value tracker 'file-map))
    
    ;; Create file nodes
    (maphash (lambda (file _)
               (declare (ignore _))
               (let* ((file-name (source-file-name file))
                      (file-path (abbreviate-path file))
                      (file-id (string-to-dot-id file-name)))
                 (format stream "    \"~A\" [label=\"~A\"];~%" 
                         file-id file-path)))
             all-files)
    
    ;; Create file dependency edges
    (maphash (lambda (file definitions)
               (declare (ignore definitions))
               (let* ((file-name (source-file-name file))
                      (file-id (string-to-dot-id file-name)))
                 (dolist (dependent (file-dependents tracker file))
                   (let* ((dep-name (source-file-name dependent))
                          (dep-id (string-to-dot-id dep-name)))
                     (format stream "    \"~A\" -> \"~A\";~%"
                             dep-id file-id)))))
             (slot-value tracker 'file-map)))
  
  (format stream "  }~%")
  (format stream "}~%"))


;;; Main Report Function

(defun report (&optional filename)
  "Generate a comprehensive dependency report for the current system analysis.
   If FILENAME is provided, saves the report to that file. Handles file system
   errors gracefully."
  (unless *current-tracker*
    (error "No analysis results available. Please run (dep:analyze-system \"system-name\") first."))
  
  (flet ((generate-all-reports (stream)
           ;; Header
           (format stream "~&~V,,,'-<~>" 70 "")
           (format stream "~&Dependency Analysis Report~%")
           (format stream "System: ~A~%" (system.name *current-tracker*))
           (format stream "Generated: ~A~%" (local-time:now))
           (format stream "~V,,,'-<~>~%" 70 "")
           
           ;; File Dependency Tree
           (format stream "~%File Dependency Hierarchy:~%")
           ;(format stream "~V,,,'-<~>~%" 70 "")
           (let ((roots (build-file-dependency-tree *current-tracker*)))
             (if roots
                 (print-ascii-tree stream roots)
                 (format stream "No file dependencies found.~%")))
           (format stream "~%")

           ;; Package Dependency Tree
           (format stream "~%Package Dependency Hierarchy:~%")
           ;(format stream "~V,,,'-<~>~%" 70 "")
           (multiple-value-bind (roots cycles) 
               (build-package-dependency-tree *current-tracker*)
             (if roots
                 (progn
                   (print-ascii-tree stream roots)
                   (when cycles
                     (format stream "~%Circular Package Dependencies:~%")
                     (dolist (cycle cycles)
                       (format stream "  ~A~%" cycle))))
                 (format stream "No package dependencies found.~%")))
           (format stream "~%")
           
           ;; Core report in text format
           (generate-report :text *current-tracker* :stream stream)
           
           ;; Add DOT graph as appendix
           (format stream "~%~%APPENDIX A: Graphviz DOT Format~%")
           (format stream "~V,,,'-<~>~%" 70 "")
           (format stream "Save the following content to a .dot file and process with Graphviz:~%~%")
           (generate-report :dot *current-tracker* :stream stream)
           
           ;; Add JSON as appendix
           (format stream "~%~%APPENDIX B: JSON Format~%")
           (format stream "~V,,,'-<~>~%" 70 "")
           (format stream "~%")
           (generate-report :json *current-tracker* :stream stream)))
    
    (if filename
        ;; Save to file with error handling
        (handler-case
            (let ((pathname (pathname filename)))
              ;; Verify directory exists and is writable
              (ensure-directory-exists pathname)
              (verify-writable pathname)
              ;; Generate report
              (with-open-file (out pathname
                               :direction :output 
                               :if-exists :supersede
                               :if-does-not-exist :create)
                (generate-all-reports out)
                (format t "~&Report saved to: ~A~%" pathname)))
          (report-error (e)
            (format *error-output* "~&Failed to save report: ~A~%" e))
          (error (e)
            (format *error-output* "~&Unexpected error saving report: ~A~%" e)))
        ;; Display to standard output
        (generate-all-reports *standard-output*)))
  
  ;; Return the tracker to allow for chaining
  *current-tracker*)