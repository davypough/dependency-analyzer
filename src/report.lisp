;;;; Filename: report.lisp
;;;
;;; Report generation functionality for dependency analysis results.
;;; Organized into major sections: Executive Summary, Architectural Overview,
;;; Anomalies and Analysis, Maintenance Considerations, and Detailed References.


(in-package #:dep)


(defun report ()
  "Generate a comprehensive dependency report for the current project.
   Displays the analysis results directly to *standard-output*."
  (unless *current-tracker*
    (return-from report nil))
  
  ;; Top Banner
  (format t "~&~V,,,'-<~A~>" 70 "")
  (format t "~&Dependency Analysis Report~%")
  (format t "Project: ~A~%" (slot-value *current-tracker* 'project-name))
  (format t "Generated: ~A~%" (local-time:now))
  (format t "~V,,,'-<~A~>~2%" 70 "")

  ;; Executive Summary with enhanced package metrics
  (format t "EXECUTIVE SUMMARY~%")
  (format t "~V,,,'-<~A~>~%" 30 "")
  (format t "Project Statistics:~%")
  (let ((total-packages 0)
        (total-symbols 0)
        (total-dependencies 0)
        (total-files (hash-table-count (slot-value *current-tracker* 'file-map)))
        (total-defs (hash-table-count (slot-value *current-tracker* 'definitions)))
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
             (slot-value *current-tracker* 'package-metrics))
    ;; Calculate reference count
    (maphash (lambda (key refs)
               (declare (ignore key))
               (incf ref-count (length refs)))
             (slot-value *current-tracker* 'references))
    ;; Calculate anomaly counts
    (maphash (lambda (type anomaly-list)
               (declare (ignore type))
               (dolist (a anomaly-list)
                 (case (anomaly.severity a)
                   (:ERROR (incf error-count))
                   (:WARNING (incf warning-count))
                   (:INFO (incf info-count)))))
             (slot-value *current-tracker* 'anomalies))
    ;; Output all metrics
    (format t "  Files: ~A~%" total-files)
    (format t "  Packages: ~A~%" total-packages)
    (format t "  Definitions: ~A~%" total-defs)
    (format t "  References: ~A~%" ref-count)
    (format t "  Total Symbols: ~A~%" total-symbols)
    (format t "  Package Dependencies: ~A~%" total-dependencies)
    (format t "  Anomalies: ~A~%" (+ error-count warning-count info-count)))

  (format t "~2%")
  (format t "SYSTEM DETAILS~%")
  (format t "~V,,,'-<~A~>~%" 30 "")
  (format t "~2%")

  ;; Clear section divider for architectural overview
  (format t "~2%ARCHITECTURAL ANALYSIS~%")
  (format t "~V,,,'-<~A~>~2%" 30 "")

  ;; System Hierarchy Section
  (format t "System Hierarchy:~%")
  (let ((system-name (slot-value *current-tracker* 'project-name)))
    (let ((dependencies (gethash system-name (slot-value *current-tracker* 'subsystems))))
      (if dependencies
          (progn
            (format t "~&-- ~A~%" system-name)
            (dolist (dep (sort (remove system-name dependencies :test #'string=) #'string<))
              (format t "   -- ~A~%" dep)))
          (format t "  No system dependencies found.~%"))))

  ;; Package Hierarchy Section
  (format t "~2%Package Hierarchy:~%")
  (multiple-value-bind (roots cycles) 
      (build-package-dependency-tree *current-tracker*)
    (if roots
        (print-ascii-tree *standard-output* roots)
        (format t "  No package dependencies found.~%"))
    (when cycles
      (format t "~%Note: Package relationships contain cyclical dependencies.~%")))

  ;; File Hierarchy Section  
  (format t "~2%File Hierarchy:~%")
  (if-let (file-roots (build-file-dependency-tree *current-tracker*))
    (print-ascii-tree *standard-output* file-roots)
    (format t "  No file dependencies found.~%"))

  ;; Class Hierarchy section - modified to use recorded cycles
  (format t "~2%Class Hierarchy:~%")
  (multiple-value-bind (roots _) ; We'll ignore the second return value
      (build-class-dependency-tree *current-tracker*)
    (declare (ignore _))
    (if roots
        (print-ascii-tree *standard-output* roots)
        (format t "  No CLOS class hierarchy found.~%"))
    ;; Now use the pre-recorded cycles instead of detecting them again
    (when-let ((cycles (slot-value *current-tracker* 'class-cycles)))
      (format t "~%Note: Class relationships contain cyclical dependencies:~%")
      (dolist (cycle cycles)
        (format t "  ~A~%" cycle))))

  ;; Structure Hierarchy section - modified to use recorded cycles
  (format t "~2%Structure Hierarchy:~%")
  (multiple-value-bind (roots _) ; Ignore second return value
      (build-structure-dependency-tree *current-tracker*)
    (declare (ignore _))
    (if roots
        (print-ascii-tree *standard-output* roots)
        (format t "  No structure hierarchy found.~%"))
    ;; Structures typically don't have cycles, but we'll check anyway
    (when-let ((cycles (slot-value *current-tracker* 'structure-cycles)))
      (format t "~%Note: Structure relationships contain cyclical dependencies:~%")
      (dolist (cycle cycles)
        (format t "  ~A~%" cycle))))

  ;; Condition Hierarchy section - modified to use recorded cycles
  (format t "~2%Condition Hierarchy:~%")
  (multiple-value-bind (roots _) ; Ignore second return value
      (build-condition-dependency-tree *current-tracker*)
    (declare (ignore _))
    (if roots
        (print-ascii-tree *standard-output* roots)
        (format t "  No condition hierarchy found.~%"))
    ;; Use the pre-recorded condition cycles
    (when-let ((cycles (slot-value *current-tracker* 'condition-cycles)))
      (format t "~%Note: Condition relationships contain cyclical dependencies:~%")
      (dolist (cycle cycles)
        (format t "  ~A~%" cycle))))

  ;; Type Hierarchy section - modified to use recorded cycles
  (format t "~2%Type Hierarchy:~%")
  (multiple-value-bind (roots _) ; Ignore second return value
      (build-deftype-dependency-tree *current-tracker*)
    (declare (ignore _))
    (if roots
        (print-ascii-tree *standard-output* roots)
        (format t "  No user-defined type relationships found.~%"))
    ;; Use the pre-recorded type cycles
    (when-let ((cycles (slot-value *current-tracker* 'type-cycles)))
      (format t "~%Note: Type definitions contain interdependencies:~%")
      (dolist (cycle cycles)
        (format t "  ~A~%" cycle))))

  ;; Anomalies Section
  (format t "~%ANOMALIES AND ANALYSIS~2%")
  (format t "~V,,,'-<~A~>~%" 30 "")
  (flet ((print-anomaly (a indent)
           (format t "~A~A~%" indent (anomaly.description a))
           (when (anomaly.context a)
             (format t "~A  Context: ~A~%" indent (anomaly.context a)))
           (format t "~%")))
    (dolist (severity '(:ERROR :WARNING :INFO))
      (let ((found-severity nil))
        ;; Check if we have any anomalies of this severity
        (maphash (lambda (type anomaly-list)
                   (declare (ignore type))
                   (when (find severity anomaly-list :key #'anomaly.severity)
                     (setf found-severity t)))
                 (slot-value *current-tracker* 'anomalies))
        (when found-severity
          (format t "~A Level Findings:~%" 
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
                    (slot-value *current-tracker* 'anomalies))
            (when system-anomalies
              (format t "~%  System Level:~%")
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
                    (slot-value *current-tracker* 'anomalies))
            (when package-anomalies
              (format t "~%  Package Level:~%")
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
                    (slot-value *current-tracker* 'anomalies))
            (when file-anomalies
              (format t "~%  File Level:~%")
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
                    (slot-value *current-tracker* 'anomalies))
            (when def-anomalies
              (format t "~%  Definition Level:~%")
              (dolist (a def-anomalies)
                (print-anomaly a "    "))))))))

  ;; Maintenance Section
  (format t "MAINTENANCE CONSIDERATIONS~%")
  (format t "~V,,,'-<~A~>~%" 30 "")
  ;; Display any cycles detected
  (when-let (cycles (slot-value *current-tracker* 'file-cycles))
    (format t "~%File Dependencies with Cycles:~%")
    (format t "The following files have circular dependencies.~%")
    (format t "This may indicate tightly coupled components:~%~%")
    (dolist (cycle cycles)
      (format t "  ~A~%" cycle)))
  (when-let (cycles (slot-value *current-tracker* 'package-cycles))
    (format t "~%Package Dependencies with Cycles:~%")
    (format t "The following packages have mutual dependencies.~%")
    (format t "While sometimes necessary, this can make the system harder to understand:~%~%")
    (dolist (cycle cycles)
      (format t "  ~A~%" cycle)))
  (format t "~2%")

  ;; Detailed References Section
  (format t "DETAILED REFERENCES~%")
  (format t "~V,,,'-<~A~>~%" 30 "")
  
  ;; System Dependencies
  (format t "System Dependencies:~%")
  (format t "~2%")
  
  ;; File Dependencies
  (format t "File Dependencies:~%")
  (maphash (lambda (file definitions)
             (declare (ignore definitions))
             (when-let (deps (file-dependencies *current-tracker* file))
               (dolist (dep deps)
                 (let* ((dep-line (format nil "~A depends on ~A" 
                                        (project-pathname file)
                                        (project-pathname dep)))
                        (refs (collect-file-references *current-tracker* file dep)))
                   (format t "~A~%" dep-line)
                   (when refs
                     ;; Start a new line with indentation
                     (format t "    ")
                     ;; Track current line length
                     (let ((line-length 4)  ; Starting after indentation
                           (max-line-length 70))
                       (loop for ref in refs
                             for first-ref = t then nil
                             for ref-text = (let ((name (reference.name ref))
                                                (quals (reference.qualifiers ref))
                                                (args (reference.arguments ref)))
                                            (format nil "~A~@[ ~{~A~^ ~}~]~@[ ~{~S ~S~^ ~}~]"
                                                    name quals args))
                             do (let ((ref-length (length ref-text)))
                                  ;; If this ref would exceed line length, start new line
                                  (when (and (not first-ref)
                                           (> (+ line-length ref-length 1) max-line-length))
                                    (format t "~%    ")
                                    (setf line-length 4))
                                  ;; Add space between refs unless first on line
                                  (unless (= line-length 4)
                                    (write-char #\Space *standard-output*)
                                    (incf line-length))
                                  ;; Write the reference
                                  (write-string ref-text *standard-output*)
                                  (incf line-length ref-length)))))
                   (format t "~2%")))))
           (slot-value *current-tracker* 'file-map))
  
  ;; Return value
  *current-tracker*)


#+ignore (defun report (&optional filename)
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


#+ignore (defgeneric generate-report (format tracker &key stream)
 (:documentation "Generate a dependency report in the specified format."))


#+ignore (defmethod generate-report ((format (eql :text)) tracker &key (stream *standard-output*))
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

  ;; Clear section divider for architectural overview
  (format stream "~2%ARCHITECTURAL ANALYSIS~%")
  (format stream "~V,,,'-<~A~>~2%" 30 "")

  ;; System Hierarchy Section
  (format stream "System Hierarchy:~%")
  (let ((system-name (slot-value tracker 'project-name)))
    (let ((dependencies (gethash system-name (slot-value tracker 'subsystems))))
      (if dependencies
          (progn
            (format stream "~&-- ~A~%" system-name)
            (dolist (dep (sort (remove system-name dependencies :test #'string=) #'string<))
              (format stream "   -- ~A~%" dep)))
          (format stream "  No system dependencies found.~%"))))

  ;; Package Hierarchy Section
  (format stream "~2%Package Hierarchy:~%")
  (multiple-value-bind (roots cycles) 
      (build-package-dependency-tree tracker)
    (if roots
        (print-ascii-tree stream roots)
        (format stream "  No package dependencies found.~%"))
    (when cycles
      (format stream "~%Note: Package relationships contain cyclical dependencies.~%")))

  ;; File Hierarchy Section  
  (format stream "~2%File Hierarchy:~%")
  (if-let (file-roots (build-file-dependency-tree tracker))
    (print-ascii-tree stream file-roots)
    (format stream "  No file dependencies found.~%"))

  ;; Add new Class Hierarchy section
  (format stream "~2%Class Hierarchy:~%")
  (multiple-value-bind (roots cycles) 
      (build-class-dependency-tree tracker)
    (if roots
        (print-ascii-tree stream roots)
        (format stream "  No CLOS class hierarchy found.~%"))
    (when cycles
      (format stream "~%Note: Class relationships contain cyclical dependencies.~%")))

  ;; Add new Structure Hierarchy section after Class Hierarchy
  (format stream "~2%Structure Hierarchy:~%")
  (multiple-value-bind (roots cycles)
      (build-structure-dependency-tree tracker)
    (declare (ignore cycles))
    (if roots
        (print-ascii-tree stream roots)
        (format stream "  No structure hierarchy found.~%")))

  ;; Add new Condition Hierarchy section after Structure Hierarchy
  (format stream "~2%Condition Hierarchy:~%")
  (multiple-value-bind (roots cycles) 
      (build-condition-dependency-tree tracker)
    (if roots
        (print-ascii-tree stream roots)
        (format stream "  No condition hierarchy found.~%"))
    ;; Conditions can have cycles due to multiple inheritance
    (when cycles
      (format stream "~%Note: Condition relationships contain cyclical dependencies.~%")))

  ;; Replace the original Type Hierarchy section with one focused on deftype relationships
  (format stream "~2%Type Hierarchy:~%")
  (multiple-value-bind (roots cycles) 
      (build-deftype-dependency-tree tracker)
    (if roots
        (print-ascii-tree stream roots)
        (format stream "  No user-defined type relationships found.~%"))
    (when cycles
      (format stream "~%Note: Type definitions contain interdependencies.~%")))

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
                     ;; Start a new line with indentation
                     (format stream "    ")
                     ;; Track current line length
                     (let ((line-length 4)  ; Starting after indentation
                           (max-line-length 70))
                       (loop for ref in refs
                           for first-ref = t then nil
                           for ref-text = (let ((name (reference.name ref))
                                              (quals (reference.qualifiers ref))
                                              (args (reference.arguments ref)))
                                          (format nil "~A~@[ ~{~A~^ ~}~]~@[ ~{~S ~S~^ ~}~]"
                                                  name quals args))
                           do (let ((ref-length (length ref-text)))
                                ;; If this ref would exceed line length, start new line
                                (when (and (not first-ref)
                                         (> (+ line-length ref-length 1) max-line-length))
                                  (format stream "~%    ")
                                  (setf line-length 4))
                                ;; Add space between refs unless first on line
                                (unless (= line-length 4)
                                  (write-char #\Space stream)
                                  (incf line-length))
                                ;; Write the reference
                                (write-string ref-text stream)
                                (incf line-length ref-length)))))
                   (format stream "~2%")))))
           (slot-value tracker 'file-map))
  
  ;; Return value
  tracker)
