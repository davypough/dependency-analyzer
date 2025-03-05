;;;; Filename:  main.lisp

;;;; Entry point for analyzing the inter-file dependencies in a user's project files.


(in-package #:dep)


(defmacro with-dependency-tracker ((&optional (tracker-form '(make-instance 'dependency-tracker :project-name "test-project")))
                                    &body body)
  "Execute BODY with *CURRENT-TRACKER* bound to the result of TRACKER-FORM.
   If TRACKER-FORM is not provided, creates a new tracker instance."
  `(let ((tracker ,tracker-form))
     (setf *current-tracker* tracker)
     (let ((*current-tracker* tracker))
       ,@body)))


(defun analyze (system-designator)
  "Analyze a system for dependencies.
   System must be loaded first for reliable analysis.
   SYSTEM-DESIGNATOR can be a keyword, string, or symbol
   as accepted by asdf:find-system.
   Returns tracker instance with analysis results."
  (unless (asdf:find-system system-designator nil)
    (format t "~2%System ~S not found. Please ensure the system is correctly defined in ASDF.~2%" 
            system-designator)
    (return-from analyze))
  
  ;; Load the system to ensure all runtime information is available
  (delete-project-fasls system-designator)
  (asdf:load-system system-designator)
  
  ;; Get system information from ASDF
  (let* ((system (asdf:find-system system-designator))
         (system-name (asdf:component-name system))
         (source-pathname (asdf:system-source-directory system))
         ;; Define these variables for compatibility with existing code
         (parent-pathname source-pathname)
         (parent-dir-name system-name)
         (logs-dir (merge-pathnames "logs/" (asdf:system-source-directory :dependency-analyzer)))
         ;; Collect source files from system components
         (source-files (collect-system-source-files system)))
    
    (unless source-files
      (format t "~2%Can't find lisp source files in system ~A." system-name)
      (format t "~%Please verify the system definition includes valid source components.~2%")
      (return-from analyze))
    
    (format t "~2%Found source files:~%~{  ~A~%~}~%" source-files)
    
    ;; Begin analysis
    (with-dependency-tracker ((make-instance 'dependency-tracker
                                           :project-name parent-dir-name
                                           :project-root parent-pathname))
      
      ;; Initialization
      (ensure-project-loaded system-designator)  ;modified to use system designator
      (dolist (file source-files)  
        (setf (gethash file (slot-value *current-tracker* 'file-map)) nil))  ;initialize file-map

      ;; First pass: analyze definitions
      (format t "~%First Pass - Collecting Definitions...~%")
      (dolist (file source-files)
        (let ((file-parser (make-instance 'file-parser :file file)))
          (parse-definitions-in-file file-parser)))

      ;; Second pass: analyze references  
      (format t "~%Second Pass - Collecting References...~%") 
      (dolist (file source-files)
        (let ((file-parser (make-instance 'file-parser :file file)))
          (parse-references-in-file file-parser)))

      ;; Third pass: package-symbol analysis
      (format t "~%Third Pass - Analyzing Package/Symbol Consistency...~%")
      (dolist (file source-files)
        (let ((file-parser (make-instance 'file-parser :file file)))
          (parse-package-symbols-in-file file-parser)))

      ;; Post-pass quality analysis
      (format t "~%Quality Review - Analyzing Code Dependencies...~2%")
      (analyze-package-dependencies *current-tracker*)
      (analyze-package-exports *current-tracker*)
      (analyze-unused-imports *current-tracker*)
      (analyze-type-relationships *current-tracker*)
      ;; Add our new hierarchy cycle analysis functions
      (analyze-class-hierarchies *current-tracker*)
      (analyze-condition-hierarchies *current-tracker*)
      (analyze-type-hierarchies *current-tracker*)
      (analyze-structure-hierarchies *current-tracker*)

      ;; Log final definitions, references, anomalies
      (let ((*print-circle* nil) ;disable circular notation
            (*print-length* 10)  ;limit list length
            (*print-level* 5))  ;limit depth
        (with-open-file (log-stream (merge-pathnames "definitions.log" logs-dir)
                                    :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
          (declare (special log-stream))
          (log-definitions))
        (with-open-file (log-stream (merge-pathnames "references.log" logs-dir)
                                    :direction :output 
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
          (declare (special log-stream))
          (log-references))
        (with-open-file (log-stream (merge-pathnames "anomalies.log" logs-dir)
                                    :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
          (declare (special log-stream))
          (log-anomalies)))
      (in-package :dep)
          
      ;; Generate final report to terminal
      (report system-designator)
          
      ;; Return tracker for chaining
      *current-tracker*)))


(defun collect-system-source-files (system)
  "Extract all source file pathnames from an ASDF system."
  (let ((files '()))
    (labels ((collect-component-files (component)
               (typecase component
                 (asdf:source-file 
                  (when (member (pathname-type (asdf:component-pathname component))
                                '("lisp" "lsp" "cl") :test #'string-equal)
                    (push (asdf:component-pathname component) files)))
                 (asdf:module
                  (map nil #'collect-component-files (asdf:component-children component))))))
      (collect-component-files system))
    (reverse files)))


(defun ensure-project-loaded (system-designator)
  "Loads the project to ensure its runtime state is available for analysis.
   Cleans up any old compiled files first to ensure fresh compilation."
  (delete-project-fasls system-designator)
  (asdf:load-system system-designator :force t)
  (setf (slot-value *current-tracker* 'subsystems)
        (get-runtime-dependencies system-designator)))


(defun get-system-details (system-designator)
  "Gather comprehensive information about an ASDF system for reporting."
  (let* ((system (asdf:find-system system-designator))
         (author (asdf:system-author system))
         (description (asdf:system-description system))
         (version (asdf:component-version system))
         (license (asdf:system-license system))
         (dependencies (asdf:system-depends-on system)))
    
    (list :name (asdf:component-name system)
          :author author
          :description description
          :version version
          :license license
          :dependencies dependencies)))


(defun get-environment-info ()
  "Get information about the current Lisp environment for reporting."
  (list :implementation (lisp-implementation-type)
        :version (lisp-implementation-version)
        :machine-type (machine-type)
        :machine-version (machine-version)
        :features *features*
        :pathname-separator (pathname-separator)))


(defun pathname-separator ()
  "Return the pathname separator character for the current platform."
  #+windows "\\"
  #-windows "/")


(defun get-runtime-dependencies (system-designator)
  "Get runtime dependencies as a hash table mapping systems to dependency lists"
  (let ((result (make-hash-table :test 'equal))
        (sys (asdf:find-system system-designator)))
    (setf (gethash (asdf:primary-system-name system-designator) result)
          (remove-duplicates 
           (mapcar #'(lambda (dep)
                      (asdf:primary-system-name (second dep)))
                  (asdf:component-depends-on 'asdf:load-op sys))
           :test #'string=))
    result))


;;; Compute the full transitive closure of dependencies.
(defun all-package-dependencies (package &optional (acc (make-hash-table :test 'eq)))
  "Return a list of all packages that PACKAGE depends on, directly or indirectly,
   including PACKAGE itself."
  (let ((pkg (find-package package)))
    (when pkg
      (unless (gethash pkg acc)
        (setf (gethash pkg acc) t)
        ;; For each direct dependency, recurse.
        (dolist (dep (direct-package-dependencies pkg))
          (all-package-dependencies dep acc)))))
  ;; Return the final set of packages as a list
  (loop for k being the hash-keys in acc collect k))


;;; Combine dependencies to get all direct dependencies (via :use or :import-from).
(defun direct-package-dependencies (package)
  "Return a list of packages that PACKAGE depends on, either via :use
   or via (shadowing-)import-from. This does NOT recursively descend."
  (let ((pkg (find-package package)))
    (if pkg
      (remove-duplicates (append (used-packages pkg)
                                 (imported-from-packages pkg))
                         :test #'eq)
      '())))


;;; See which packages provide the symbols that PACKAGE has imported,
;;; whether import-from or shadowing-import-from.
(defun imported-from-packages (package)
  "Return a list of distinct 'home' packages for any symbols
   present in PACKAGE but whose home package is not PACKAGE."
  (let* ((pkg (find-package package))
         (seen (make-hash-table :test 'eq)))
    (do-symbols (sym pkg)
      (let ((home (symbol-package sym)))
        ;; If HOME is neither NIL nor the same as PKG, it means
        ;; sym is 'imported' (or inherited) from another package.
        (when (and home (not (eq home pkg)))
          (setf (gethash home seen) t))))
    (loop for k being the hash-keys of seen
          collect k)))


;;; Get which packages a given package inherits via :USE
(defun used-packages (package)
  "Return a list of packages that PACKAGE uses (directly)."
  (package-use-list (find-package package)))


(defun log-definitions ()
  (declare (special log-stream))
  (format log-stream "Filename: DEFINITIONS.LOG")
  (format log-stream "~2%The list of all definitions identified in the ~A project.~2%"
          (slot-value *current-tracker* 'project-name))
  (maphash (lambda (key def-list)
             (declare (ignore key))
             (dolist (def def-list)
               (print-object def log-stream)
               (terpri log-stream)))
           (slot-value *current-tracker* 'definitions)))


(defun log-references ()
  (declare (special log-stream))
  (format log-stream "Filename: REFERENCES.LOG")
  (format log-stream "~2%The list of all references to definitions in other files for the ~A project.~%"
          (slot-value *current-tracker* 'project-name))
  (maphash (lambda (key ref-list)
             (declare (ignore key))
             (dolist (ref ref-list)  ;(sort ref-list #'string< 
                                     ;   :key (lambda (r)
                                     ;           (format nil "~A:~A" (reference.file r) (reference.name r)))))
               (print-object ref log-stream)
               (terpri log-stream)))
           (slot-value *current-tracker* 'references)))
    

(defun log-anomalies ()
  "Log all anomalies grouped by type to the specified stream."
  (declare (special log-stream))
  (format log-stream "Filename: ANOMALIES.LOG")
  (format log-stream "~2%The list of all anomalies detected during dependency analysis of the ~A project.~2%"
          (slot-value *current-tracker* 'project-name))
  
  ;; Group anomalies by type for consistent output
  (let ((anomalies-by-type (make-hash-table)))
    ;; Group all anomalies by their type
    (dolist (anomaly (slot-value *current-tracker* 'anomalies))
      (push anomaly (gethash (anomaly.type anomaly) anomalies-by-type)))
    
    ;; Get all anomaly types for sorting
    (let ((anomaly-types nil))
      (maphash (lambda (type anomaly-list)
                 (declare (ignore anomaly-list))
                 (push type anomaly-types))
               anomalies-by-type)
      
      ;; Process each type in sorted order
      (dolist (type (sort anomaly-types #'string< :key #'symbol-name))
        (when-let (anomalies-of-type (gethash type anomalies-by-type))
          (dolist (anomaly (sort anomalies-of-type #'string< 
                                 :key #'anomaly.description))
            (print-anomaly anomaly log-stream 0))
          (terpri log-stream))))))
