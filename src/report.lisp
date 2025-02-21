;;;; Filename: report.lisp
;;;
;;; Report generation functionality for dependency analysis results.
;;; Organized into major sections: Executive Summary, Architectural Overview,
;;; Anomalies and Analysis, Maintenance Considerations, and Detailed References.


(in-package #:dep)


(defun report (&optional filename)
  "Generate comprehensive dependency reports for the current project.
   If FILENAME is provided, saves text report to that file.
   Otherwise writes to reports/ directory with project-based filenames."
  (unless *current-tracker*
    (return-from report nil))
  
  ;; Always output text report to terminal
  (generate-report :text *current-tracker* :stream *standard-output*)
  
  (let* ((project-name (slot-value *current-tracker* 'project-name))
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


(defgeneric generate-report (format tracker &key stream)
 (:documentation "Generate a dependency report in the specified format."))


(defmethod generate-report ((format (eql :text)) tracker &key (stream *standard-output*))
  "Generate comprehensive text report including package analysis metrics."
  ;; Top Banner
  (format stream "~&~V,,,'-<~A~>" 70 "")
  (format stream "~&Dependency Analysis Report~%")
  (format stream "Project: ~A~%" (slot-value tracker 'project-name))
  (format stream "Generated: ~A~%" (local-time:now))
  (format stream "~V,,,'-<~A~>~2%" 70 "")

  ;; Executive Summary with enhanced package metrics
  (format stream "EXECUTIVE SUMMARY~%")
  (format stream "~V,,,'-<~A~>~%" 30 "")
  (format stream "Project Statistics:~%")
  (let ((total-packages 0)
        (total-symbols 0)
        (total-dependencies 0)
        (total-files (hash-table-count (slot-value tracker 'file-map)))
        (total-defs (hash-table-count (slot-value tracker 'definitions)))
        (ref-count 0)
        (error-count 0)
        (warning-count 0)
        (info-count 0))
    ;; Calculate package metrics using package objects
    (maphash (lambda (pkg metrics)  ; pkg is now package object
               (declare (ignore pkg))
               (incf total-packages)
               (incf total-symbols 
                     (+ (getf metrics :local-symbols)
                        (getf metrics :inherited-symbols)))
               (incf total-dependencies
                     (getf metrics :used-packages)))
             (slot-value tracker 'package-metrics))
    ;; Calculate reference count
    (maphash (lambda (key refs)
               (declare (ignore key))
               (incf ref-count (length refs)))
             (slot-value tracker 'references))
    ;; Calculate anomaly counts
    (maphash (lambda (type anomaly-list)
               (declare (ignore type))
               (dolist (a anomaly-list)
                 (case (anomaly.severity a)
                   (:ERROR (incf error-count))
                   (:WARNING (incf warning-count))
                   (:INFO (incf info-count)))))
             (slot-value tracker 'anomalies))
    ;; Output all metrics
    (format stream "  Files: ~A~%" total-files)
    (format stream "  Packages: ~A~%" total-packages)
    (format stream "  Definitions: ~A~%" total-defs)
    (format stream "  References: ~A~%" ref-count)
    (format stream "  Total Symbols: ~A~%" total-symbols)
    (format stream "  Package Dependencies: ~A~%" total-dependencies)
    (format stream "  Anomalies: ~A~%" (+ error-count warning-count info-count)))

  (format stream "~2%")
  (format stream "SYSTEM DETAILS~%")
  (format stream "~V,,,'-<~A~>~%" 30 "")
  (format stream "~2%")

  ;; Architectural Overview with package analysis
  (format stream "ARCHITECTURAL OVERVIEW~%")
  (format stream "~V,,,'-<~A~>~%" 30 "")
  
  ;; System Overview
  (format stream "SYSTEM OVERVIEW~%")
  (format stream "~V,,,'-<~A~>~%" 30 "")
  (alexandria:when-let ((cycles (get-system-cycles tracker)))
    (format stream "~%System Dependency Cycles:~%")
    (format stream "The following systems have circular dependencies:~%")
    (dolist (cycle cycles)
      (format stream "  ~A~%" cycle)))
  (format stream "~2%")
  
  ;; Package Architecture
  (format stream "Package Hierarchy:~%")
  (multiple-value-bind (roots cycles) 
      (build-package-dependency-tree tracker)
    (if roots
        (print-ascii-tree stream roots)
        (format stream "  No package dependencies found.~%"))
    (when cycles
      (format stream "~%Note: Package relationships contain cyclical dependencies.~%
                   This may be intentional but could affect maintainability.~%")))
  
  ;; File Architecture
  (format stream "~%File Hierarchy:~%")
  (if-let (file-roots (build-file-dependency-tree tracker))
    (print-ascii-tree stream file-roots)
    (format stream "  No file dependencies found.~%"))
  (format stream "~2%")

  ;; Anomalies Section
  (format stream "ANOMALIES AND ANALYSIS~%")
  (format stream "~V,,,'-<~A~>~%" 30 "")
  (flet ((print-anomaly (a indent)
          (format stream "~A~A~%" indent (anomaly.description a))
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
                 (slot-value tracker 'anomalies))
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
                    (slot-value tracker 'anomalies))
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
                    (slot-value tracker 'anomalies))
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
                    (slot-value tracker 'anomalies))
            (when file-anomalies
              (format stream "~%  File Level:~%")
              (dolist (a file-anomalies)
                (print-anomaly a "    "))))
          ;; Definition-level anomalies
          (let ((def-anomalies nil))
            (maphash (lambda (type anomaly-list)
                      (when (member type '(:duplicate-definition :complex-type-dependency
                                         :shadowed-definition :unused-definition))
                        (setf def-anomalies 
                              (nconc def-anomalies
                                    (remove severity anomaly-list 
                                           :key #'anomaly.severity 
                                           :test-not #'eq)))))
                    (slot-value tracker 'anomalies))
            (when def-anomalies
              (format stream "~%  Definition Level:~%")
              (dolist (a def-anomalies)
                (print-anomaly a "    "))))))))

  ;; Maintenance Section
  (format stream "MAINTENANCE CONSIDERATIONS~%")
  (format stream "~V,,,'-<~A~>~%" 30 "")
  ;; Display any cycles detected
  (when-let (cycles (slot-value tracker 'file-cycles))
    (format stream "~%File Dependencies with Cycles:~%")
    (format stream "The following files have circular dependencies.~%")
    (format stream "This may indicate tightly coupled components:~%~%")
    (dolist (cycle cycles)
      (format stream "  ~A~%" cycle)))
  (when-let (cycles (slot-value tracker 'package-cycles))
    (format stream "~%Package Dependencies with Cycles:~%")
    (format stream "The following packages have mutual dependencies.~%")
    (format stream "While sometimes necessary, this can make the system harder to understand:~%~%")
    (dolist (cycle cycles)
      (format stream "  ~A~%" cycle)))
  (format stream "~2%")

  ;; Detailed References Section
  (format stream "DETAILED REFERENCES~%")
  (format stream "~V,,,'-<~A~>~%" 30 "")
  
  ;; System Dependencies
  (format stream "System Dependencies:~%")
  (format stream "~2%")
  
  ;; Package Dependencies
  (format stream "Package Dependencies:~%")
  (maphash (lambda (pkg used-pkgs)  ; pkg is package object
             (format stream "~&~A " (package-name pkg))
             (when used-pkgs
               (format stream "uses")
               (dolist (used used-pkgs)
                 (format stream " ~A" (package-name used))))
             (let ((metrics (gethash pkg (slot-value tracker 'package-metrics))))
               (when metrics
                 (format stream "~%  Metrics:")
                 (format stream "~%    Exports: ~D symbols"
                         (getf metrics :exported-symbols))
                 (when (plusp (getf metrics :export-users 0))
                   (format stream "~%    Used by: ~D packages (~D references)"
                           (getf metrics :export-users)
                           (getf metrics :export-references)))))
             (when-let (exports (get-package-exports tracker pkg))  ; Updated to take package object
               (format stream "~%  Exports:~%")
               (dolist (sym (sort exports #'string< :key #'symbol-name))
                 (format stream "    ~A~%" sym))))
           (slot-value tracker 'package-uses))
  
  ;; File Dependencies
  (format stream "File Dependencies:~%")
  (maphash (lambda (file definitions)
             (declare (ignore definitions))
             (when-let (deps (file-dependencies tracker file))
               (dolist (dep deps)
                 (let* ((dep-line (format nil "~A depends on ~A" 
                                          (project-pathname file)
                                          (project-pathname dep)))
                        (refs (collect-file-references tracker file dep)))
                   (format stream "~A~%" dep-line)
                   (when refs
                     (dolist (ref refs)
                       (let ((name (reference.name ref))
                             (quals (reference.qualifiers ref))
                             (args (reference.arguments ref)))
                         (format stream "    ~A~@[ ~{~A~^ ~}~]~@[ ~{~S ~S~^ ~}~]~%" 
                                 name quals args))))
                   (format stream "~%")))))
           (slot-value tracker 'file-map))
  
  ;; Return value
  tracker)


(defmethod generate-report ((format (eql :json)) tracker &key (stream *standard-output*))
  "Generate a JSON format dependency report.
   Handles pathnames by converting them to project-relative paths before encoding."
  (let ((*print-pretty* t)
        (yason:*symbol-key-encoder* #'string-downcase)
        (yason:*symbol-encoder* #'string-downcase))
    (yason:with-output (stream :indent t)
      (yason:with-object ()
        ;; Project metadata section
        (yason:encode-object-element "project" (slot-value tracker 'project-name))
        (yason:encode-object-element "systems" (slot-value tracker 'subsystems))
        
        ;; Package metrics section
        (yason:with-object-element ("package_metrics")
          (yason:with-object ()
            (maphash #'(lambda (pkg metrics)  ; pkg is package object
                         (yason:with-object-element ((package-name pkg))
                           (yason:with-object ()
                             (yason:encode-object-element "local_symbols" 
                                                          (getf metrics :local-symbols))
                             (yason:encode-object-element "inherited_symbols" 
                                                          (getf metrics :inherited-symbols))
                             (yason:encode-object-element "used_packages"
                                                          (getf metrics :used-packages)))))
                     (slot-value tracker 'package-metrics))))
        
        ;; Anomalies section
        (yason:with-object-element ("anomalies")
          (yason:with-object ()
            (dolist (severity '(:ERROR :WARNING :INFO))
              (yason:with-object-element ((string-downcase (symbol-name severity)))
                (yason:with-array ()
                  (maphash #'(lambda (type anomaly-list)
                              (dolist (a (remove severity anomaly-list 
                                               :key #'anomaly.severity 
                                               :test-not #'eq))
                                (yason:with-object ()
                                  (yason:encode-object-element "type" 
                                    (string-downcase (symbol-name type)))
                                  (yason:encode-object-element "description" 
                                    (anomaly.description a))
                                  (when (anomaly.context a)
                                    (yason:encode-object-element "context" 
                                      (anomaly.context a)))
                                  (when (anomaly.file a)
                                    (yason:encode-object-element "file"
                                      (mapcar #'project-pathname 
                                             (alexandria:ensure-list (anomaly.file a)))))
                                  (when (anomaly.package a)
                                    (yason:encode-object-element "package"
                                      (package-designator-to-string (anomaly.package a)))))))
                           (slot-value tracker 'anomalies)))))))

        ;; Type metrics section
        (yason:with-object-element ("type_metrics")
          (yason:with-object ()
            (maphash (lambda (type metrics)
                       (yason:with-object-element ((string type))
                         (yason:with-object ()
                           (dolist (metric metrics)
                             (yason:encode-object-element 
                               (string-downcase (string (car metric)))
                               (cdr metric))))))
                     (slot-value tracker 'type-metrics))))
        
        ;; Dependencies sections
        (yason:with-object-element ("dependencies")
          (yason:with-object ()
            ;; Package dependencies
            (yason:with-object-element ("packages")
              (yason:with-object ()
                (maphash #'(lambda (pkg deps)
                            (yason:with-object-element (pkg)
                              (yason:with-object ()
                                (yason:encode-object-element "uses" deps)
                                (when-let (exports (get-package-exports tracker pkg))
                                  (yason:encode-object-element "exports"
                                    (mapcar #'symbol-name exports))))))
                        (slot-value tracker 'package-uses))))
            
            ;; File dependencies
            (yason:with-object-element ("files")
              (yason:with-object ()
                (maphash #'(lambda (file definitions)
                            (declare (ignore definitions))
                            (when-let (deps (file-dependencies tracker file))
                              (yason:with-object-element 
                                  ((project-pathname file))
                                (yason:with-object ()
                                  (yason:encode-object-element "depends_on"
                                    (mapcar #'project-pathname deps))
                                  (yason:with-object-element ("references")
                                    (yason:with-array ()
                                      (dolist (dep deps)
                                        (when-let (refs (collect-file-references 
                                                       tracker file dep))
                                          (dolist (ref refs)
                                            (yason:with-object ()
                                              (yason:encode-object-element "name" 
                                                (reference.name ref))
                                              (when (reference.qualifiers ref)
                                                (yason:encode-object-element "qualifiers"
                                                  (reference.qualifiers ref)))
                                              (when (reference.arguments ref)
                                                (yason:encode-object-element "arguments"
                                                  (reference.arguments ref)))))))))))))
                        (slot-value tracker 'file-map))))
            
            ;; System dependencies
            (yason:with-object-element ("systems")
              (yason:encode (slot-value tracker 'subsystems)))))))))


(defmethod generate-report ((format (eql :dot)) tracker &key (stream *standard-output*))
  "Generate a GraphViz DOT format dependency report.
   Shows file, package and system dependencies with reference labels."
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
  
  (let* ((systems-table (slot-value tracker 'subsystems))
         (system-names (when (hash-table-p systems-table)
                        (sort (alexandria:hash-table-keys systems-table)
                              #'string<))))
    (when system-names  ; Only proceed if we have systems
      ;; Create nodes for all systems
      (dolist (sys system-names)
        (format stream "    \"sys_~A\" [label=\"~A\"];~%"
                (string-to-dot-id sys) sys))
      ;; Create dependency edges
      (dolist (sys system-names)
        (when-let (deps (gethash sys systems-table))
          (dolist (dep deps)
            (format stream "    \"sys_~A\" -> \"sys_~A\";~%"
                    (string-to-dot-id sys)
                    (string-to-dot-id dep)))))))
  
  (format stream "  }~%~%")
  
  ;; Create package subgraph
  (format stream "  subgraph cluster_packages {~%")
  (format stream "    label=\"Package Dependencies\";~%")
  (format stream "    style=dashed;~%")
  (format stream "    node [style=filled,fillcolor=lightgrey];~%")
  (maphash (lambda (pkg used-pkgs)
             (let ((pkg-id (string-to-dot-id pkg)))
               ;; Create node for package with metrics
               (let ((metrics (gethash pkg (slot-value tracker 'package-metrics))))
                 (format stream "    \"pkg_~A\" [label=\"~A~A\"];~%"
                         pkg-id pkg
                         (if metrics
                             (format nil "\\n(~D symbols, ~D deps)"
                                     (+ (getf metrics :local-symbols)
                                        (getf metrics :inherited-symbols))
                                     (getf metrics :used-packages))
                             "")))
               ;; Create edges for dependencies
               (dolist (used used-pkgs)
                 (let ((used-id (string-to-dot-id used)))
                   (format stream "    \"pkg_~A\" -> \"pkg_~A\"~A;~%"
                           pkg-id used-id
                           (if (member pkg 
                                     (gethash used (slot-value tracker 'package-uses)))
                               " [dir=both,color=red]" ; Cycle indicator
                               ""))))))
           (slot-value tracker 'package-uses))
  (format stream "  }~%~%")

  ;; Type relationships subgraph
  (format stream "  subgraph cluster_types {~%")
  (format stream "    label=\"Type Dependencies\";~%")
  (format stream "    style=dashed;~%")
  (format stream "    node [style=filled,fillcolor=lightyellow];~%")
  (maphash (lambda (type deps)
             (let ((type-id (string-to-dot-id (string type))))
               ;; Create node for this type
               (format stream "    \"type_~A\" [label=\"~A\"];~%" 
                       type-id type)
               ;; Create edges to each dependency
               (dolist (dep deps)
                 (let ((dep-id (string-to-dot-id (string dep))))
                   ;; Create node for dependency if not yet seen
                   (format stream "    \"type_~A\" [label=\"~A\"];~%"
                           dep-id dep)
                   ;; Create edge showing dependency
                   (format stream "    \"type_~A\" -> \"type_~A\";~%"
                           type-id dep-id)))))
           (slot-value tracker 'type-graph))
  (format stream "  }~%~%")
  
  ;; Create file subgraph
  (format stream "  subgraph cluster_files {~%")
  (format stream "    label=\"File Dependencies\";~%")
  (format stream "    style=dashed;~%")
  (format stream "    node [style=filled,fillcolor=lightblue];~%")
  (let ((all-files (make-hash-table :test 'equal))
        (is-dependency (make-hash-table :test 'equal)))
    ;; First collect all files and mark dependencies
    (maphash (lambda (file definitions)
               (declare (ignore definitions))
               (setf (gethash file all-files) t)
               (dolist (dep (file-dependencies tracker file))
                 (setf (gethash dep all-files) t)
                 (setf (gethash dep is-dependency) t)))
             (slot-value tracker 'file-map))
    
    ;; Create nodes for all files
    (maphash (lambda (file definitions)
               (declare (ignore definitions))
               (let* ((file-name (source-file-name file))
                      (file-id (string-to-dot-id file-name))
                      (file-path (project-pathname file))
                      (has-errors (block check-errors
                                  (maphash 
                                   (lambda (type anomaly-list)
                                     (declare (ignore type))
                                     (when (find file anomaly-list 
                                               :key #'anomaly.file 
                                               :test #'equal)
                                       (return-from check-errors t)))
                                   (slot-value tracker 'anomalies))
                                  nil)))
                 (format stream "    \"~A\" [label=\"~A\"~A];~%" 
                         file-id file-path
                         (cond (has-errors ",fillcolor=pink")
                               ((not (gethash file is-dependency)) 
                                ",fillcolor=lightgreen")
                               (t "")))))
             all-files)
    
    ;; Create edges with reference labels
    (maphash (lambda (file definitions)
               (declare (ignore definitions))
               (let* ((file-name (source-file-name file))
                      (file-id (string-to-dot-id file-name)))
                 (dolist (dep (file-dependencies tracker file))
                   (let* ((dep-name (source-file-name dep))
                          (dep-id (string-to-dot-id dep-name))
                          (refs (collect-file-references tracker file dep))
                          (label (when refs
                                 (format nil "~{~A~@[ ~{~A~^ ~}~]~@[ ~{~S ~S~^ ~}~]~^,\\n~}"
                                         (mapcar (lambda (r)
                                                 (list (reference.name r)
                                                       (reference.qualifiers r)
                                                       (reference.arguments r)))
                                                refs)))))
                     (format stream "    \"~A\" -> \"~A\"~@[ [label=\"~A\"]~];~%"
                             file-id dep-id 
                             (when label 
                               (with-output-to-string (s)
                                 (loop for c across label do
                                   (case c
                                     ((#\" #\\) (write-char #\\ s))
                                     (otherwise (write-char c s)))))))))))
             (slot-value tracker 'file-map)))
  (format stream "  }~%")
  (format stream "}~%"))
