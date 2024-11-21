;;;; Filename: report.lisp
;;;
;;; Report generation functionality for dependency analysis results.
;;; Organized into major sections: Executive Summary, Architectural Overview,
;;; Anomalies and Analysis, Maintenance Considerations, and Detailed References.


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
 (format stream "  Systems: ~A~%" (hash-table-count (slot-value tracker 'subsystems)))
 (format stream "  Files: ~A~%" (hash-table-count (slot-value tracker 'file-map)))
 (format stream "  Packages: ~A~%" (hash-table-count (slot-value tracker 'package-uses)))
 (format stream "  Definitions: ~A~%" (hash-table-count (slot-value tracker 'definitions)))
 (let ((ref-count 0))
   (maphash (lambda (key refs)
              (declare (ignore key))
              (incf ref-count (length refs)))
            (slot-value tracker 'references))
   (format stream "  References: ~A~%" ref-count))
 (let ((error-count 0)
       (warning-count 0)
       (info-count 0))
   (maphash (lambda (type anomaly-list)
              (declare (ignore type))
              (dolist (a anomaly-list)
                (case (anomaly.severity a)
                  (:error (incf error-count))
                  (:warning (incf warning-count))
                  (:info (incf info-count)))))
            (anomalies tracker))
   (format stream "  Anomalies: ~A~%" (+ error-count warning-count info-count)))
 (format stream "~2%")
 (format stream "SYSTEM DETAILS~%")
 (format stream "~V,,,'-<~>~%" 30 "")
 (maphash (lambda (sys-name deps)
            (format stream "~&System: ~A~%" sys-name)
            (when deps
              (format stream "  Depends on:~%")
              (dolist (dep deps)
                (format stream "    ~A~%" dep))))
          (slot-value tracker 'subsystems))
 (format stream "~2%")
;; 2. Architectural Overview
 (format stream "ARCHITECTURAL OVERVIEW~%")
 (format stream "~V,,,'-<~>~%" 30 "")
 ;; System Overview
 (format stream "SYSTEM OVERVIEW~%")
 (format stream "~V,,,'-<~>~%" 30 "")
 (maphash (lambda (sys-name deps)
            (format stream "~&System: ~A~%" sys-name)
            (when deps
              (format stream "  Depends on:~%")
              (dolist (dep deps)
                (format stream "    ~A~%" dep))))
          (slot-value tracker 'subsystems))
 (alexandria:when-let ((cycles (get-system-cycles tracker)))
   (format stream "~%System Dependency Cycles:~%")
   (format stream "The following systems have circular dependencies:~%")
   (dolist (cycle cycles)
     (format stream "  ~A~%" cycle)))
 (format stream "~2%")
 (format stream "Package Hierarchy:~%")
 (multiple-value-bind (roots cycles) 
     (build-package-dependency-tree tracker)
   (if roots
       (print-ascii-tree stream roots)
       (format stream "  No package dependencies found.~%"))
   (when cycles
     (format stream "~%Note: Package relationships contain cyclical dependencies.~%"
             "     This may be intentional but could affect maintainability.~%")))
 (format stream "~%File Hierarchy:~%")
 (let ((file-roots (build-file-dependency-tree tracker)))
   (if file-roots
       (print-ascii-tree stream file-roots)
       (format stream "  No file dependencies found.~%")))
 (format stream "~2%")
;; 3. Anomalies and Analysis
(format stream "ANOMALIES AND ANALYSIS~%")
(format stream "~V,,,'-<~>~%" 30 "")
(flet ((print-anomaly (a indent)
         (format stream "~A~A~%" indent (anomaly.description a))
         (format stream "~A  Location: ~A~%" indent (anomaly.location a))
         (when (anomaly.context a)
           (format stream "~A  Context: ~A~%" indent (anomaly.context a)))
         (format stream "~%")))
  (dolist (severity '(:error :warning :info))
    (let ((found-severity nil))
      ;; Check if we have any anomalies of this severity
      (maphash (lambda (type anomaly-list)
                 (declare (ignore type))
                 (when (find severity anomaly-list :key #'anomaly.severity)
                   (setf found-severity t)))
               (anomalies tracker))
      (when found-severity
        (format stream "~A Level Findings:~%" 
                (string-capitalize (symbol-name severity)))
        ;; System-level anomalies
        (let ((system-anomalies nil))
          (maphash (lambda (type anomaly-list)
                    (when (member type '(:system-cycle :system-dependency))
                      (setf system-anomalies 
                            (nconc system-anomalies
                                  (remove severity anomaly-list 
                                         :key #'anomaly.severity 
                                         :test-not #'eq)))))
                  (anomalies tracker))
          (when system-anomalies
            (format stream "~%  System Level:~%")
            (dolist (a system-anomalies)
              (print-anomaly a "    "))))
        ;; Package-level anomalies
        (let ((package-anomalies nil))
          (maphash (lambda (type anomaly-list)
                    (when (member type '(:package-cycle :package-dependency 
                                       :unused-import :undefined-package))
                      (setf package-anomalies 
                            (nconc package-anomalies
                                  (remove severity anomaly-list 
                                         :key #'anomaly.severity 
                                         :test-not #'eq)))))
                  (anomalies tracker))
          (when package-anomalies
            (format stream "~%  Package Level:~%")
            (dolist (a package-anomalies)
              (print-anomaly a "    "))))
        ;; File-level anomalies
        (let ((file-anomalies nil))
          (maphash (lambda (type anomaly-list)
                    (when (member type '(:file-cycle :file-dependency))
                      (setf file-anomalies 
                            (nconc file-anomalies
                                  (remove severity anomaly-list 
                                         :key #'anomaly.severity 
                                         :test-not #'eq)))))
                  (anomalies tracker))
          (when file-anomalies
            (format stream "~%  File Level:~%")
            (dolist (a file-anomalies)
              (print-anomaly a "    "))))
        ;; Definition-level anomalies
        (let ((def-anomalies nil))
          (maphash (lambda (type anomaly-list)
                    (when (member type '(:undefined-reference :duplicate-definition
                                       :shadowed-definition :unused-definition))
                      (setf def-anomalies 
                            (nconc def-anomalies
                                  (remove severity anomaly-list 
                                         :key #'anomaly.severity 
                                         :test-not #'eq)))))
                  (anomalies tracker))
          (when def-anomalies
            (format stream "~%  Definition Level:~%")
            (dolist (a def-anomalies)
              (print-anomaly a "    "))))))))
 ;; 4. Maintenance Considerations
 (format stream "MAINTENANCE CONSIDERATIONS~%")
 (format stream "~V,,,'-<~>~%" 30 "")
 ;; Display any cycles detected
 (alexandria:when-let ((cycles (get-project-cycles tracker)))
   (format stream "Project Dependencies with Cycles:~%")
   (format stream "The following project dependencies form cycles.~%")
   (format stream "While this may be intentional, consider if restructuring is possible:~%~%")
   (dolist (cycle cycles)
     (format stream "  ~A~%" cycle)))
 (alexandria:when-let ((cycles (get-file-cycles tracker)))
   (format stream "~%File Dependencies with Cycles:~%")
   (format stream "The following files have circular dependencies.~%")
   (format stream "This may indicate tightly coupled components:~%~%")
   (dolist (cycle cycles)
     (format stream "  ~A~%" cycle)))
 (alexandria:when-let ((cycles (get-package-cycles tracker)))
   (format stream "~%Package Dependencies with Cycles:~%")
   (format stream "The following packages have mutual dependencies.~%")
   (format stream "While sometimes necessary, this can make the system harder to understand:~%~%")
   (dolist (cycle cycles)
     (format stream "  ~A~%" cycle)))
 (format stream "~2%")
;; 5. Detailed References
(format stream "DETAILED REFERENCES~%")
(format stream "~V,,,'-<~>~%" 30 "")
;; System Dependencies
(format stream "System Dependencies:~%")
(let ((printed-systems (make-hash-table :test 'equal)))
  (maphash (lambda (sys-name deps)
             (unless (gethash sys-name printed-systems)
               (format stream "~&System: ~A~%" sys-name)
               (setf (gethash sys-name printed-systems) t)
               ;; Direct dependencies
               (when deps
                 (format stream "  Direct Dependencies:~%")
                 (dolist (dep deps)
                   (format stream "    ~A~%" dep)
                   (setf (gethash dep printed-systems) t)))
               ;; Component packages
               (let ((sys-packages nil))
                 (maphash (lambda (pkg uses)
                           (declare (ignore uses))
                           (when (search sys-name pkg)
                             (push pkg sys-packages)))
                         (slot-value tracker 'package-uses))
                 (when sys-packages
                   (format stream "  Packages:~%")
                   (dolist (pkg (sort sys-packages #'string<))
                     (format stream "    ~A~%" pkg))))))
           (slot-value tracker 'subsystems)))
(format stream "~2%")
;; Package Dependencies
(format stream "Package Dependencies:~%")
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
(format stream "~2%")
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
                     (format stream "          References: ~{~A~^, ~}~%" refs)))))))
         (slot-value tracker 'file-map)))


(defmethod generate-report ((format (eql :json)) tracker &key (stream *standard-output*))
  (let ((*print-pretty* t)
        (yason:*symbol-key-encoder* #'string-downcase)
        (yason:*symbol-encoder* #'string-downcase))
    (yason:with-output (stream :indent t)
      (yason:with-object ()
        ;; Project name and systems
        (yason:encode-object-element "project" (project.name tracker))
        (yason:with-object-element ("systems")
          (yason:encode (slot-value tracker 'subsystems)))
        ;; Anomalies
        (yason:with-object-element ("anomalies")
          (yason:with-object ()
            (dolist (severity '(:error :warning :info))
              (yason:with-object-element ((string-downcase (symbol-name severity)))
                (yason:with-array ()
                  (maphash (lambda (type anomaly-list)
                            (dolist (a (remove severity anomaly-list 
                                             :key #'anomaly.severity 
                                             :test-not #'eq))
                              (yason:with-object ()
                                (yason:encode-object-element "type" 
                                                          (string-downcase (symbol-name type)))
                                (yason:encode-object-element "location" 
                                                          (anomaly.location a))
                                (yason:encode-object-element "description" 
                                                          (anomaly.description a))
                                (when (anomaly.context a)
                                  (yason:encode-object-element "context" 
                                                            (anomaly.context a))))))
                          (anomalies tracker)))))))
        ;; System dependencies
        (yason:with-object-element ("systems")
          (let ((systems (make-hash-table :test 'equal)))
            (maphash (lambda (sys-name deps)
                      (setf (gethash sys-name systems)
                            (alexandria:alist-hash-table
                             `(("depends_on" . ,deps))
                             :test 'equal)))
                    (slot-value tracker 'subsystems))
            (yason:encode systems)))
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
  
  ;; Create systems subgraph
  (format stream "  subgraph cluster_systems {~%")
  (format stream "    label=\"System Dependencies\";~%")
  (format stream "    style=dashed;~%")
  (format stream "    node [style=filled,fillcolor=lightpink];~%")
  (maphash (lambda (sys-name deps)
             (let ((sys-id (string-to-dot-id sys-name)))
               (format stream "    \"sys_~A\" [label=\"~A\"];~%" sys-id sys-name)
               (dolist (dep deps)
                 (let ((dep-id (string-to-dot-id dep)))
                   (format stream "    \"sys_~A\" [label=\"~A\"];~%" dep-id dep)
                   (format stream "    \"sys_~A\" -> \"sys_~A\";~%"
                           sys-id dep-id)))))
           (slot-value tracker 'subsystems))
  (format stream "  }~%~%")
  
  ;; Create package subgraph
  (format stream "  subgraph cluster_packages {~%")
  (format stream "    label=\"Package Dependencies\";~%")
  (format stream "    style=dashed;~%")
  (format stream "    node [style=filled,fillcolor=lightgrey];~%")
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
  (let ((all-files (make-hash-table :test 'equal))
        (is-dependency (make-hash-table :test 'equal)))
    (maphash (lambda (file definitions)
               (declare (ignore definitions))
               (setf (gethash file all-files) t)
               (dolist (dep (file-dependencies tracker file))
                 (setf (gethash dep all-files) t)
                 (setf (gethash dep is-dependency) t)))
             (slot-value tracker 'file-map))
    (maphash (lambda (file definitions)
               (declare (ignore definitions))
               (let* ((file-name (source-file-name file))
                      (file-id (string-to-dot-id file-name))
                      (file-path (project-pathname file))
                      (has-errors (block check-errors
                                  (maphash (lambda (type anomaly-list)
                                           (declare (ignore type))
                                           (when (find file anomaly-list 
                                                     :key #'anomaly.location 
                                                     :test #'equal)
                                             (return-from check-errors t)))
                                          (anomalies tracker))
                                  nil)))
                 (format stream "    \"~A\" [label=\"~A\"~A];~%" 
                         file-id file-path
                         (cond (has-errors ",fillcolor=pink")
                               ((not (gethash file is-dependency)) ",fillcolor=lightgreen")
                               (t "")))))
             all-files)
    (maphash (lambda (file definitions)
               (declare (ignore definitions))
               (let* ((file-name (source-file-name file))
                      (file-id (string-to-dot-id file-name)))
                 (dolist (dep (file-dependencies tracker file))
                   (let* ((dep-name (source-file-name dep))
                          (dep-id (string-to-dot-id dep-name))
                          (refs (collect-file-references tracker file dep))
                          (label (format nil "References: ~A"
                                       (if (> (length refs) 3)
                                           (format nil "~{~A~^, ~}, ..." 
                                                 (subseq (mapcar #'symbol-name refs) 0 3))
                                           (format nil "~{~A~^, ~}" 
                                                 (mapcar #'symbol-name refs))))))
                     (format stream "    \"~A\" -> \"~A\" [label=\"~A\"];~%"
                             file-id dep-id label)))))
             (slot-value tracker 'file-map)))
  (format stream "  }~%")
  (format stream "}~%"))


(defun report (&optional filename)
 "Generate a comprehensive dependency report for the current project analysis.
  If FILENAME is provided, saves the report to that file. Returns the tracker
  to allow for chaining."
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
