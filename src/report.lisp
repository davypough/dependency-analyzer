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


(defun format-anomalies (stream tracker)
  "Format all anomalies in a consistent way, grouped by type and severity."
  (let ((found-anomalies nil))
    ;; First get all anomaly types
    (maphash (lambda (type anomaly-list)
               (when anomaly-list
                 (setf found-anomalies t)
                 (format stream "~&~%~A Anomalies:~%" 
                         (string-capitalize (symbol-name type)))
                 ;; Group by severity within each type
                 (loop for severity in '(:error :warning :info)
                       for severity-anomalies = (remove severity anomaly-list 
                                                      :key #'anomaly.severity 
                                                      :test-not #'eq)
                       when severity-anomalies do
                         (format stream "  ~A:~%" (string-upcase (symbol-name severity)))
                         (dolist (a (reverse severity-anomalies)) ; Reverse to show in detection order
                           (format stream "    ~A~%" (anomaly.description a))
                           (format stream "      Location: ~A~%" (anomaly.location a))))))
             (anomalies tracker))
    ;; Return whether we found any anomalies
    found-anomalies))


;;; Report Generation Methods

(defgeneric generate-report (format tracker &key stream)
  (:documentation "Generate a dependency report in the specified format."))


(defmethod generate-report ((format (eql :text)) tracker &key (stream *standard-output*))
  ;; Header
  (format stream "~&Dependency Analysis Report for Project: ~A~%" (project.name tracker))
  (format stream "~&================================================~%")
  
  ;; Anomalies section first
  (when (format-anomalies stream tracker)
    (format stream "~&------------------------------------------------~%"))
  
  ;; Cycle warnings - only show sections if cycles exist
  (alexandria:when-let ((cycles (get-project-cycles tracker)))
    (format stream "~&Project Dependency Cycles:~%")
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
           (slot-value tracker 'package-uses)))


(defmethod generate-report ((format (eql :json)) tracker &key (stream *standard-output*))
  (let ((*print-pretty* t)
        (yason:*symbol-key-encoder* #'string-downcase)
        (yason:*symbol-encoder* #'string-downcase))
    (yason:with-output (stream :indent t)
      (yason:with-object ()
        ;; Project name
        (yason:encode-object-element "project" (project.name tracker))
        
        ;; Anomalies (new section)
        (let ((anomaly-map (make-hash-table :test 'equal)))
          (maphash (lambda (type anomaly-list)
                    (when anomaly-list
                      (setf (gethash (string-downcase (symbol-name type)) anomaly-map)
                            (mapcar (lambda (a)
                                    (alexandria:alist-hash-table
                                     `(("severity" . ,(string-downcase 
                                                     (symbol-name (anomaly.severity a))))
                                       ("location" . ,(pathname-to-string (anomaly.location a)))
                                       ("description" . ,(anomaly.description a)))
                                     :test 'equal))
                                  (reverse anomaly-list)))))
                  (anomalies tracker))
          (unless (zerop (hash-table-count anomaly-map))
            (yason:with-object-element ("anomalies")
              (yason:encode anomaly-map))))
        
        ;; Cycles (if any exist)
        (let ((project-cycles (get-project-cycles tracker))
              (file-cycles (get-file-cycles tracker))
              (package-cycles (get-package-cycles tracker)))
          (when (or project-cycles file-cycles package-cycles)
            (yason:with-object-element ("cycles")
              (yason:with-object ()
                (when project-cycles
                  (yason:encode-object-element "project" project-cycles))
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
  "Generate a comprehensive dependency report for the current project analysis.
   If FILENAME is provided, saves the report to that file. Handles file project
   errors gracefully."
  (unless *current-tracker*
    (error "No analysis results available. Please run (dep:analyze-project \"project-name\") first."))
  
  (flet ((generate-all-reports (stream)
           ;; Header
           (format stream "~&~V,,,'-<~>" 70 "")
           (format stream "~&Dependency Analysis Report~%")
           (format stream "Project: ~A~%" (project.name *current-tracker*))
           (format stream "Generated: ~A~%" (local-time:now))
           (format stream "~V,,,'-<~>~%" 70 "")
           
           ;; File Dependency Tree
           (format stream "~%File Dependency Hierarchy:~%")
           (let ((roots (build-file-dependency-tree *current-tracker*)))
             (if roots
                 (print-ascii-tree stream roots)
                 (format stream "No file dependencies found.~%")))
           (format stream "~%")

           ;; Package Dependency Tree
           (format stream "~%Package Dependency Hierarchy:~%")
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