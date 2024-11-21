;;;; Filename: report.lisp
;;;
;;; Report generation functionality for dependency analysis results.
;;; Organized into major sections: Executive Summary, Architectural Overview,
;;; Critical Components, Maintenance Hotspots, and Detailed References.


(in-package #:dep)


(defgeneric generate-report (format tracker &key stream)
 (:documentation "Generate a dependency report in the specified format."))


(defmethod generate-report ((format (eql :text)) tracker &key (stream *standard-output*))
 ;; Top Banner
 (format stream "~&~V,,,'-<~>" 70 "")
 (format stream "~&Dependency Analysis Report~%")
 (format stream "Project: ~A~%" (project.name tracker))
 (format stream "Generated: ~A~%" (local-time:now))
 (format stream "~V,,,'-<~>~2%" 70 "")

 ;; 1. Executive Summary
 (format stream "EXECUTIVE SUMMARY~%")
 (format stream "~V,,,'-<~>~%" 30 "")
 (format stream "Project Statistics:~%")
 (format stream "  Files: ~A~%" (hash-table-count (slot-value tracker 'file-map)))
 (format stream "  Packages: ~A~%" (hash-table-count (slot-value tracker 'package-uses)))
 (format stream "  Definitions: ~A~%" (hash-table-count (slot-value tracker 'definitions)))
 ;; TODO: Add coupling metrics when implemented
 (format stream "~%Key Findings:~%")
 (when (or (get-file-cycles tracker)
           (get-package-cycles tracker))
   (format stream "  * Circular dependencies detected~%"))
 (format stream "~2%")

;; 2. Architectural Overview
(format stream "ARCHITECTURAL OVERVIEW~%")
(format stream "~V,,,'-<~>~%" 30 "")
(format stream "Package Hierarchy:~%")
(multiple-value-bind (roots cycles) 
    (build-package-dependency-tree tracker)
  (if roots
      (print-ascii-tree stream roots)
      (format stream "  No package dependencies found.~%")))
(format stream "~%File Hierarchy:~%")
(let ((file-roots (build-file-dependency-tree tracker)))
  (if file-roots
      (print-ascii-tree stream file-roots)
      (format stream "  No file dependencies found.~%")))
;; TODO: Add layering analysis when implemented
(format stream "~2%")

 ;; 3. Critical Components
 (format stream "CRITICAL COMPONENTS~%")
 (format stream "~V,,,'-<~>~%" 30 "")
 (format stream "Most Referenced Files: (Not Yet Implemented)~%")
 (format stream "Most Referenced Packages: (Not Yet Implemented)~%")
 (format stream "~2%")

 ;; 4. Maintenance Hotspots
 (format stream "MAINTENANCE HOTSPOTS~%")
 (format stream "~V,,,'-<~>~%" 30 "")
 
 ;; Display any cycles found
 (alexandria:when-let ((cycles (get-project-cycles tracker)))
   (format stream "Project Dependency Cycles:~%")
   (dolist (cycle cycles)
     (format stream "  ~A~%" cycle)))
 
 (alexandria:when-let ((cycles (get-file-cycles tracker)))
   (format stream "File Dependency Cycles:~%")
   (dolist (cycle cycles)
     (format stream "  ~A~%" cycle)))
 
 (alexandria:when-let ((cycles (get-package-cycles tracker)))
   (format stream "Package Dependency Cycles:~%")
   (dolist (cycle cycles)
     (format stream "  ~A~%" cycle)))
 
 ;; TODO: Add complexity metrics when implemented
 (format stream "~2%")

 ;; 5. Detailed References
 (format stream "DETAILED REFERENCES~%")
 (format stream "~V,,,'-<~>~%" 30 "")
 
 ;; File Dependencies
 (format stream "File Dependencies:~%")
 (maphash (lambda (file definitions)
            (declare (ignore definitions))
            (let ((deps (file-dependencies tracker file)))
              (when deps
                (format stream "~&File: ~A~%" (project-pathname file))
                (format stream "      Depends on:~%")
                (dolist (dep deps)
                  (format stream "        ~A~%" (project-pathname dep))
                  ;; Symbol references creating the dependency
                  (let ((refs (collect-file-references tracker file dep)))
                    (when refs
                      (format stream "      References: ~{~A~^, ~}~%" refs)))))))
          (slot-value tracker 'file-map))
 
 ;; Package Dependencies
 (format stream "~%Package Dependencies:~%")
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
          (slot-value tracker 'package-uses))
 (format stream "~2%"))


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
    (maphash (lambda (file definitions)
               (declare (ignore definitions))
               (let* ((file-name (source-file-name file))
                      (file-id (string-to-dot-id file-name))
                      (file-path (project-pathname file)))
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


(defun report (&optional filename)
 "Generate a comprehensive dependency report for the current project analysis.
  If FILENAME is provided, saves the report to that file."
 (unless *current-tracker*
   (return-from report nil))
 
 (flet ((generate-all-reports (stream)
          ;; Main text report
          (generate-report :text *current-tracker* :stream stream)
          
          ;; Appendices
          (format stream "~%APPENDIX A: Graphviz DOT Format~%")
          (format stream "~V,,,'-<~>~%" 70 "")
          (format stream "Save the following content to a .dot file and process with Graphviz:~%~%")
          (generate-report :dot *current-tracker* :stream stream)
          
          (format stream "~%APPENDIX B: JSON Format~%")
          (format stream "~V,,,'-<~>~%" 70 "")
          (format stream "~%")
          (generate-report :json *current-tracker* :stream stream)))
   
   (if filename
       ;; Save to file
       (let ((pathname (pathname filename)))
         (ensure-directory-exists pathname)
         (verify-writable pathname)
         (with-open-file (out pathname
                          :direction :output 
                          :if-exists :supersede
                          :if-does-not-exist :create)
           (generate-all-reports out)
           (format t "~&Report saved to: ~A~%" pathname)))
       ;; Display to standard output
       (generate-all-reports *standard-output*)))
 
 ;; Return the tracker to allow for chaining
 *current-tracker*)