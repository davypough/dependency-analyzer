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
; (format stream "  Systems: ~A~%" (hash-table-count (slot-value tracker 'subsystems)))
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
                  (:ERROR (incf error-count))
                  (:WARNING (incf warning-count))
                  (:INFO (incf info-count)))))
            (anomalies tracker))
   (format stream "  Anomalies: ~A~%" (+ error-count warning-count info-count)))
 (format stream "~2%")
 (format stream "SYSTEM DETAILS~%")
 (format stream "~V,,,'-<~>~%" 30 "")
 (format stream "~2%")
 ;; 2. Architectural Overview
 (format stream "ARCHITECTURAL OVERVIEW~%")
 (format stream "~V,,,'-<~>~%" 30 "")
 ;; System Overview
 (format stream "SYSTEM OVERVIEW~%")
 (format stream "~V,,,'-<~>~%" 30 "")
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
  (dolist (severity '(:ERROR :WARNING :INFO))
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
 (format stream "~2%")
 ;; Package Dependencies
 (format stream "Package Dependencies:~%")
 (maphash (lambda (pkg used-pkgs)
           (format stream "~&~A " pkg)
           (when used-pkgs
             (format stream "uses")
             (dolist (used used-pkgs)
               (format stream " ~A" used)))
           (let ((exports (get-package-exports tracker pkg)))
             (when exports
               (format stream "  Exports:~%")
               (dolist (sym (sort exports #'string< :key #'symbol-name))
                 (format stream "    ~A~%" sym)))))
         (slot-value tracker 'package-uses))
 (format stream "~2%")
 ;; File Dependencies
 (format stream "File Dependencies:~%")
 (let ((deps-table (make-hash-table :test 'equal))
       (depends-on-pos nil))
  ;; First find position of "depends on" label
  (let ((sample-line (format nil "X depends on Y")))
    (setf depends-on-pos (search "depends on" sample-line)))
  ;; Output dependencies with aligned references
  (maphash (lambda (file definitions)
             (declare (ignore definitions))
             (let ((deps (file-dependencies tracker file)))
               (when deps
                 (dolist (dep deps)
                   (let* ((dep-line (format nil "~A depends on ~A" 
                                          (project-pathname file)
                                          (project-pathname dep)))
                          (refs (collect-file-references tracker file dep)))
                     (format stream "~A" dep-line)
                     (when refs
                       (format stream "~%~A"
                               (format-references-list refs 
                                                     :depends-pos depends-on-pos
                                                     :max-width *print-width*)))
                     (format stream "~%"))))))
           (slot-value tracker 'file-map))))


(defmethod generate-report ((format (eql :json)) tracker &key (stream *standard-output*))
  "Generate a JSON format dependency report.
   Handles pathnames by converting them to project-relative paths before encoding."
  (let ((*print-pretty* t)
        (yason:*symbol-key-encoder* #'string-downcase)
        (yason:*symbol-encoder* #'string-downcase))
    (yason:with-output (stream :indent t)
      (yason:with-object ()
        ;; Project name and systems
        (yason:encode-object-element "project" (project.name tracker))
        #+ignore (yason:with-object-element ("systems")
          (yason:encode (slot-value tracker 'subsystems)))
        ;; Anomalies
        (yason:with-object-element ("anomalies")
          (yason:with-object ()
            (dolist (severity '(:ERROR :WARNING :INFO))
              (yason:with-object-element ((string-downcase (symbol-name severity)))
                (yason:with-array ()
                  (maphash (lambda (type anomaly-list)
                            (dolist (a (remove severity anomaly-list 
                                             :key #'anomaly.severity 
                                             :test-not #'eq))
                              (yason:with-object ()
                                (yason:encode-object-element "type" 
                                                          (string-downcase (symbol-name type)))
                                ;; Convert pathname locations to strings before encoding
                                (yason:encode-object-element "location" 
                                                          (let ((loc (anomaly.location a)))
                                                            (if (pathnamep loc)
                                                                (project-pathname loc)
                                                                loc)))
                                (yason:encode-object-element "description" 
                                                          (anomaly.description a))
                                (when (anomaly.context a)
                                  (yason:encode-object-element "context" 
                                                            (anomaly.context a))))))
                          (anomalies tracker)))))))
        ;; System dependencies
        #+ignore (yason:with-object-element ("systems")
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
  #+ignore (maphash (lambda (sys-name deps)
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
  "Generate comprehensive dependency reports for the current project.
   If FILENAME is provided, saves text report to that file.
   Otherwise writes to reports/ directory with project-based filenames."
  (unless *current-tracker*
    (return-from report nil))
  
  ;; Always output text report to terminal
  (generate-report :text *current-tracker* :stream *standard-output*)
  
  (let* ((project-name (project.name *current-tracker*))
         (text-path (if filename
                       (parse-namestring filename)
                       (make-report-path project-name :text)))
         (json-path (make-report-path project-name :json))
         (dot-path (make-report-path project-name :dot)))
    
    ;; Generate each report format
    (report-to-file text-path :text *current-tracker*)
    (report-to-file json-path :json *current-tracker*)
    (report-to-file dot-path :dot *current-tracker*)
    
    (format t "~2%Reports saved to:~%")
    (format t "  Text: ~A~%" (project-pathname text-path))
    (format t "  JSON: ~A~%" (project-pathname json-path))
    (format t "  Graphviz DOT: ~A~%" (project-pathname dot-path)))
  
  ;; Return tracker for chaining
  *current-tracker*)
