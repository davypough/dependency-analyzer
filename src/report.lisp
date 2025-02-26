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
  (format t "~V,,,'-<~A~>" 70 "")

  ;; Executive Summary with enhanced package metrics
  (format t "~2%EXECUTIVE SUMMARY~%")
  (format t "~V,,,'-<~A~>~%" 30 "")
  (format t "Project Statistics:~%")
  (let ((total-packages 0)
        (total-symbols 0)
        (total-package-dependencies 0)
        (total-files (hash-table-count (slot-value *current-tracker* 'file-map)))
        (total-defs (hash-table-count (slot-value *current-tracker* 'definitions)))
        (ref-count 0)
        (total-anomalies (length (slot-value *current-tracker* 'anomalies))))
    ;; Calculate package metrics using package objects
    (maphash (lambda (pkg metrics)  ; pkg is now package object
               (declare (ignore pkg))
               (incf total-packages)
               (incf total-symbols 
                     (+ (getf metrics :local-symbols)
                        (getf metrics :inherited-symbols)))
               (incf total-package-dependencies
                     (getf metrics :used-packages)))
             (slot-value *current-tracker* 'package-metrics))
    ;; Calculate reference count
    (maphash (lambda (key refs)
               (declare (ignore key))
               (incf ref-count (length refs)))
             (slot-value *current-tracker* 'references))
    ;; Output all metrics
    (format t "  Files: ~A~%" total-files)
    (format t "  Packages: ~A~%" total-packages)
    (format t "  Definitions: ~A~%" total-defs)
    (format t "  References: ~A~%" ref-count)
    (format t "  Total Symbols: ~A~%" total-symbols)
    (format t "  Package Dependencies: ~A~%" total-package-dependencies)
    (format t "  Anomalies: ~A~%" total-anomalies))

  (format t "~2%SYSTEM DETAILS~%")
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
  (format t "~2%ANOMALIES AND ANALYSIS~%")
  (format t "~V,,,'-<~A~>~%" 30 "")
  ;; Use the anomalies list directly
  (let ((all-anomalies (slot-value *current-tracker* 'anomalies)))
    ;; Define severity ordering for sorting
    (let ((severity-order '(:ERROR :WARNING :INFO)))
      ;; Sort by severity (preserving the ERROR, WARNING, INFO order)
      ;; and then by description for consistent ordering
      (setf all-anomalies 
            (sort (copy-list all-anomalies)
                  (lambda (a b)
                    (let ((a-pos (position (anomaly.severity a) severity-order))
                          (b-pos (position (anomaly.severity b) severity-order)))
                      (or (< a-pos b-pos)
                          (and (= a-pos b-pos)
                               (string< (anomaly.description a) (anomaly.description b)))))))))
  
    ;; Display all anomalies with full details
    (if all-anomalies
      (dolist (a all-anomalies)
        (format t "ANOMALY>~%")
        (format t "    :Description ~S~%" (anomaly.description a))
        (format t "    :Type ~S~%" (anomaly.type a))
        (format t "    :Severity ~S~%" (anomaly.severity a))
        (when (anomaly.context a)
          (format t "    :Context ~S~%" (anomaly.context a)))
        (when (anomaly.file a)
          (format t "    :File ~A~%" (anomaly.file a)))
        (when (anomaly.package a)
          (format t "    :Package ~S~%" (anomaly.package a)))
        (format t "~%"))
      (format t "No anomalies found.~%")))

  ;; Maintenance Section
  (format t "~2%MAINTENANCE CONSIDERATIONS~%")
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

  ;; Detailed References Section
  (format t "~2%DETAILED REFERENCES~%")
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
