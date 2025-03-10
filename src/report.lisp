;;;; Filename: report.lisp
;;;
;;; Report generation functionality for dependency analysis results.
;;; Organized into major sections: Executive Summary, Architectural Overview,
;;; Anomalies and Analysis, Maintenance Considerations, and Detailed References.


(in-package #:dep)


(defun report (system-designator)
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
  (let ((sys-info (get-system-details system-designator)))
    (format t "Name: ~A~%" (getf sys-info :name))
    (when (getf sys-info :version)
      (format t "Version: ~A~%" (getf sys-info :version)))
    (when (getf sys-info :author)
      (format t "Author: ~A~%" (getf sys-info :author)))
    (when (getf sys-info :license)
      (format t "License: ~A~%" (getf sys-info :license)))
    (when (getf sys-info :description)
      (format t "Description: ~A~%" (getf sys-info :description)))
    (format t "Dependencies:~%")
    (let ((deps (getf sys-info :dependencies)))
      (if deps
        (dolist (dep deps)
          (format t "  - ~A~%" dep))
        (format t "  No dependencies~%"))))

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
  ;(format t "~2%ANOMALIES~%")
  ;(format t "~V,,,'-<~A~>~%" 30 "")
  ;; Use the anomalies list directly
  #+ignore (let ((all-anomalies (slot-value *current-tracker* 'anomalies)))
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
          (format t "    :Context ~A~%" (anomaly.context a)))
        (when (anomaly.file a)
          (format t "    :File ~A~%" (anomaly.file a)))
        (when (anomaly.package a)
          (format t "    :Package ~S~%" (anomaly.package a)))
        (format t "~%"))
      (format t "No anomalies found.~%")))

  ;; Maintenance Section
  (format t "~2%MAINTENANCE CONSIDERATIONS (reference logs/anomalies.log for details)~%")
  (format t "~V,,,'-<~A~>~%" 30 "")

  ;; Get all anomalies
  (let ((all-anomalies (slot-value *current-tracker* 'anomalies)))
    
    ;; 1. Dependency Hot Spots
    (multiple-value-bind (file-deps pkg-deps) 
        (count-dependencies *current-tracker*)
      (let ((hotspots (find-hotspots file-deps pkg-deps)))
        (when hotspots
          (format t "~%Dependency Hot Spots:~%")
          (dolist (item hotspots)
            (format t "    • ~A~%" item))
          (format t "~%    These components have unusually high coupling and may benefit from refactoring.~%"))))
    
    ;; 2. Duplicate Definitions
    (let ((dupes (remove-if-not (lambda (a) (eq (anomaly.type a) :duplicate-definition))
                                all-anomalies)))
      (when dupes
        (format t "~%Duplicate Definitions (~D):~%" (length dupes))
        (format t "    The following symbols are defined in multiple files:~%")
        (let ((dupe-names (mapcar (lambda (a) 
                                    (let ((name (subseq (anomaly.description a) 
                                                 0 
                                                (position #\Space (anomaly.description a)))))
                                      (string-trim "()" name)))
                                  dupes)))
          (format t "    • ~{~A~^, ~}~%" (sort dupe-names #'string<))
          (format t "~%    Duplicate definitions overwrite previous definitions making bugs difficult to trace.~%"))))
    
    ;; 3. Type System Issues
    (let ((cycles (remove-if-not (lambda (a) (eq (anomaly.type a) :complex-type-cycle))
                                all-anomalies))
          (shadowing (remove-if-not (lambda (a) (eq (anomaly.type a) :shadowed-definition))
                                    all-anomalies)))
      
      ;; 3a. Type Dependency Cycles
      (when cycles
        (format t "~%Type Dependency Cycles (~D):~%" (length cycles))
        (dolist (cycle cycles)
          (format t "    • ~A~%" (anomaly.description cycle)))
        (format t "~%    Cyclical type relationships become harder to reason about, increasing the likelihood of subtle semantic errors.~%"))
      
      ;; 3b. Shadowed Definitions
      (when shadowing
        (format t "~%Shadowed Definitions (~D):~%" (length shadowing))
        (dolist (shadow shadowing)
          (let ((desc (anomaly.description shadow)))
            (format t "    • ~A~%" desc)))
        (format t "~%    Shadowing can create subtle runtime behavior based on which definition is active in a given context.~%")))
    
    ;; 4. Naming Convention Issues
    (let ((const-issues (remove-if-not (lambda (a) (eq (anomaly.type a) :constant-naming))
                                      all-anomalies))
          (var-issues (remove-if-not (lambda (a) (eq (anomaly.type a) :special-var-naming))
                                    all-anomalies)))
      
      ;; 4a. Constant Naming
      (when const-issues
        (format t "~%Constant Naming Issues (~D):~%" (length const-issues))
        (dolist (issue const-issues)
          (let ((desc (anomaly.description issue)))
            (format t "    • ~A~%" desc)))
        (format t "~%    Constants typically follow the +CONST+ naming convention.~%"))
      
      ;; 4b. Special Variable Naming
      (when var-issues
        (format t "~%Special Variable Naming Issues (~D):~%" (length var-issues))
        (dolist (issue var-issues)
          (let ((desc (anomaly.description issue)))
            (format t "    • ~A~%" desc)))
        (format t "~%    Special variables typically follow the *VAR* naming convention.~%")))
    
    ;; 5. Package Structure Issues
    (let ((cohesion (remove-if-not (lambda (a) (eq (anomaly.type a) :package-core-provider))
                                  all-anomalies))
          (fragmentation (remove-if-not (lambda (a) (eq (anomaly.type a) :package-fragmentation))
                                        all-anomalies))
          (interface (remove-if-not (lambda (a) (eq (anomaly.type a) :interface-segregation))
                                  all-anomalies))
          (external-deps (remove-if-not (lambda (a) (eq (anomaly.type a) :external-dependency))
                                      all-anomalies))
          (unused (remove-if-not (lambda (a) (eq (anomaly.type a) :unused-import))
                            all-anomalies)))
  
      ;; 5a. Package Cohesion
      (when cohesion
        (format t "~%Package Cohesion Issues (~D):~%" (length cohesion))
        (dolist (issue cohesion)
          (let ((desc (anomaly.description issue)))
            (format t "    • ~A~%" desc)))
        (format t "~%    Such packages may have too many responsibilities, becoming hard to reason about systematically.~%"))
  
      ;; 5b. Package Fragmentation
      (when fragmentation
        (format t "~%Package Fragmentation Issues (~D):~%" (length fragmentation))
        (dolist (issue fragmentation)
          (let ((desc (anomaly.description issue)))
            (format t "    • ~A~%" desc)))
        (format t "~%    Fragmented packages contain disconnected functional groups and may benefit from being split.~%"))
  
      ;; 5c. Interface Segregation
      (when interface
        (format t "~%Interface Segregation Issues (~D):~%" (length interface))
        (dolist (issue interface)
          (let ((desc (anomaly.description issue)))
            (format t "    • ~A~%" desc)))
        (format t "~%    Consider separating interfaces that are used by disjoint client groups.~%"))
  
      ;; 5d. External Dependencies
      (when external-deps
        (format t "~%External Dependency Issues (~D):~%" (length external-deps))
        (dolist (issue external-deps)
          (let ((desc (anomaly.description issue)))
            (format t "    • ~A~%" desc)))
        (format t "~%    Packages with low symbol ownership may have unclear boundaries or excessive coupling.~%"))
     
      ;; 5e. Unused Imports
      (when unused
        (format t "~%Unused Package Imports (~D):~%" (length unused))
        (dolist (issue unused)
          (let ((desc (anomaly.description issue)))
            (format t "    • ~A~%" desc)))
        (format t "~%    Consider removing unnecessary imports to clarify dependencies.~%")))
    
    ;; 6. Symbol Visibility Issues
    (let ((vis-issues (remove-if-not (lambda (a) (eq (anomaly.type a) :qualified-internal-reference))
                                    all-anomalies)))
      (when vis-issues
        (format t "~%Symbol Visibility Issues (~D):~%" (length vis-issues))
        (format t "    The following internal symbols are referenced with package qualification:~%")
        (let ((internal-refs (remove-duplicates 
                              (mapcar (lambda (a) 
                                        (let ((desc (anomaly.description a)))
                                          (subseq desc (+ (position #\Space desc) 1))))
                                      vis-issues)
                              :test #'string=)))
          (dolist (ref (sort internal-refs #'string<))
            (format t "    • ~A~%" ref)))
        (format t "~%    Consider exporting these symbols or restructuring the packages.~%")))
    
    ;; 7. File Dependency Cycles
    (when-let (cycles (slot-value *current-tracker* 'file-cycles))
      (format t "~%File Dependency Cycles (~D):~%" (length cycles))
      (format t "    The following files have circular dependencies:~%")
      (dolist (cycle cycles)
        (format t "    • ~A~%" cycle))
      (format t "~%    Circular dependencies make code harder to understand and maintain.~%"))
    
    ;; 8. Package Dependency Cycles
    (when-let (cycles (slot-value *current-tracker* 'package-cycles))
      (format t "~%Package Dependency Cycles (~D):~%" (length cycles))
      (format t "    The following packages have circular dependencies:~%")
      (dolist (cycle cycles)
        (format t "    • ~A~%" cycle))
      (format t "~%    Consider refactoring to establish a clearer package hierarchy.~%")))

  ;; Detailed References Section
  (format t "~2%DETAILED REFERENCES~%")
  (format t "~V,,,'-<~A~>~%" 30 "")
  
  ;; System Dependencies
  (format t "System Dependencies:~%")
  (format t "~2%")
  
  ;; File Dependencies
  (format t "File Dependencies:~%")
  ;; File Dependencies
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
                             for ref-text = (format nil "~A" (reference.name ref))
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


(defun categorize-anomalies (tracker)
  "Group anomalies by category for maintenance reporting."
  (let ((categories (make-hash-table :test 'eq))
        (all-anomalies (slot-value tracker 'anomalies)))
    
    ;; Create category groupings
    (dolist (anomaly all-anomalies)
      (let ((type (anomaly.type anomaly)))
            ; (severity (anomaly.severity anomaly)))
        
        ;; Group by higher-level category
        (let ((category (cond
                          ((member type '(:duplicate-definition))
                           :duplication)
                          ((member type '(:complex-type-cycle :shadowed-definition))
                           :type-system)
                          ((member type '(:constant-naming :special-var-naming))
                           :naming-conventions)
                          ((member type '(:qualified-internal-reference))
                           :visibility)
                          ((member type '(:package-cohesion :unused-import))
                           :package-structure)
                          (t :other))))
          
          ;; Add to appropriate category list
          (push anomaly (gethash category categories nil)))))
    
    categories))


(defun count-dependencies (tracker)
  "Count incoming and outgoing dependencies for files and packages."
  (let ((file-deps (make-hash-table :test 'equal))
        (pkg-deps (make-hash-table :test 'equal)))
    
    ;; Calculate file dependencies
    (maphash (lambda (file definitions)
               (declare (ignore definitions))
               (let ((out-deps (length (file-dependencies tracker file)))
                     (in-deps 0))
                 
                 ;; Count incoming dependencies (other files that depend on this one)
                 (maphash (lambda (other-file _)
                            (declare (ignore _))
                            (when (member file (file-dependencies tracker other-file)
                                         :test #'equal)
                              (incf in-deps)))
                          (slot-value tracker 'file-map))
                 
                 ;; Record stats
                 (setf (gethash file file-deps)
                       (list :in in-deps :out out-deps :total (+ in-deps out-deps)))))
             (slot-value tracker 'file-map))
    
    ;; Calculate package dependencies
    (maphash (lambda (pkg-name metrics)
               (let (;(pkg (find-package pkg-name))
                     (in-deps (getf metrics :export-users 0))
                     (out-deps (getf metrics :used-packages 0)))
                 (setf (gethash pkg-name pkg-deps)
                       (list :in in-deps :out out-deps 
                             :total (+ in-deps out-deps)))))
             (slot-value tracker 'package-metrics))
    
    (values file-deps pkg-deps)))


(defun find-hotspots (file-deps pkg-deps)
  "Find components with unusually high coupling."
  (let ((hotspots nil)
        (file-threshold 5)   ; Threshold for high coupling
        (pkg-threshold 3))
    
    ;; Check files with many dependencies
    (maphash (lambda (file stats)
               (let ((in-deps (getf stats :in))
                     (out-deps (getf stats :out)))
                 (when (> (+ in-deps out-deps) file-threshold)
                   (push (format nil "File ~A has ~D incoming and ~D outgoing dependencies"
                                (project-pathname file) in-deps out-deps)
                         hotspots))))
             file-deps)
    
    ;; Check packages with many dependencies
    (maphash (lambda (pkg-name stats)
               (let ((in-deps (getf stats :in))
                     (out-deps (getf stats :out)))
                 (when (> (+ in-deps out-deps) pkg-threshold)
                   (push (format nil "Package ~A has ~D incoming and ~D outgoing dependencies"
                                pkg-name in-deps out-deps)
                         hotspots))))
             pkg-deps)
    
    (sort hotspots #'string<)))