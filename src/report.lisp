;;;; Filename: report.lisp
;;;
;;; Report generation functionality for dependency analysis results.
;;; Provides multiple report formats for visualizing dependencies between
;;; files, packages, and symbols. Includes robust error handling for
;;; file operations.


(in-package #:dep)


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
  
  ;; Enhanced file dependencies section with symbol references
  (format stream "~&~%File Dependencies:~%")
  (format stream "------------------------------------------------~%")
  (maphash (lambda (file definitions)
             (declare (ignore definitions))
             (let ((deps (file-dependencies tracker file)))
               (when deps
                 (format stream "~&File: ~A~%" (pathname-to-string file))
                 (format stream "      Depends on:~%")
                 (dolist (dep deps)
                   (format stream "        ~A~%" (pathname-to-string dep))
                   ;; Add symbol references that create this dependency
                   (let ((refs (collect-file-references tracker file dep)))
                     (when refs
                       (format stream "      References: ~{~A~^, ~}~%" refs)))))))
           (slot-value tracker 'file-map))
  
  ;; Package relationships
  (format stream "~&~%Package Dependencies:~%")
  (format stream "------------------------------------------------~%")
  (maphash (lambda (pkg used-pkgs)
             (format stream "~&Package: ~A~%" pkg)
             (when used-pkgs
               (format stream "  Uses packages:~%")
               (dolist (used used-pkgs)
                 (format stream "    ~A~%" used)))
             (let ((exports (get-package-exports tracker pkg)))
               (when exports
                 (format stream "  Exports:~%")
                 (dolist (sym (sort exports #'string< :key #'symbol-name))
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
        
        ;; File dependencies with references
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
  (format stream "    node [style=filled,fillcolor=lightblue];~%")
  
  ;; Track all files and their dependencies
  (let ((all-files (make-hash-table :test 'equal))
        (is-dependency (make-hash-table :test 'equal)))
    
    ;; First identify all dependencies
    (maphash (lambda (file definitions)
               (declare (ignore definitions))
               (setf (gethash file all-files) t)
               (dolist (dep (file-dependencies tracker file))
                 (setf (gethash dep all-files) t)
                 (setf (gethash dep is-dependency) t)))
             (slot-value tracker 'file-map))
    
    ;; Create nodes, marking root nodes differently
    (maphash (lambda (file _)
               (declare (ignore _))
               (let* ((file-name (source-file-name file))
                      (file-path (abbreviate-path file))
                      (file-id (string-to-dot-id file-name)))
                 (format stream "    \"~A\" [label=\"~A\"~A];~%" 
                         file-id file-path
                         (if (not (gethash file is-dependency))
                             ",fillcolor=lightgreen"  ; Root nodes in different color
                             ""))))
             all-files)
    
    ;; Create file dependency edges with reference labels
    (maphash (lambda (file definitions)
               (declare (ignore definitions))
               (let* ((file-name (source-file-name file))
                      (file-id (string-to-dot-id file-name)))
                 (dolist (dep (file-dependencies tracker file))
                   (let* ((dep-name (source-file-name dep))
                          (dep-id (string-to-dot-id dep-name))
                          (refs (collect-file-references tracker file dep))
                          ;; Create abbreviated label with "References: " prefix
                          (label (format nil "References: ~A"
                                       (if (> (length refs) 3)
                                           (format nil "~{~A~^, ~}, ..." 
                                                 (subseq (mapcar #'symbol-name refs) 0 3))
                                           (format nil "~{~A~^, ~}" 
                                                 (mapcar #'symbol-name refs))))))
                     (format stream "    \"~A\" -> \"~A\" [label=\"~A\"];~%"
                             file-id dep-id label)))))
             (slot-value tracker 'file-map)))
  (format stream "  }~%~%")
  
  ;; Add any dependency cycles found
  (alexandria:when-let ((cycles (get-package-cycles tracker)))
    (format stream "  subgraph cluster_cycles {~%")
    (format stream "    label=\"Dependency Cycles\";~%")
    (format stream "    style=dashed;~%")
    (format stream "    node [style=filled,fillcolor=pink];~%")
    (loop for cycle in cycles
          for i from 0
          do (format stream "    \"cycle~D\" [label=\"~A\",shape=note];~%"
                    i cycle))
    (format stream "  }~%"))
  
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