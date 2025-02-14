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


(defun analyze (source-dir)
  "Analyze source files in a directory for dependencies.
   Project must be loaded first for reliable analysis.
   Returns tracker instance with analysis results."
  (unless (stringp source-dir)
    (format t "~2%Note that the source directory must be a string, got ~S instead.~2%" source-dir)
    (return-from analyze))
  (let ((source-pathname (pathname source-dir)))
    (unless (cdr (pathname-directory source-pathname))  ; Check for parent dir
      (format t "~2%Note that the source directory must have a parent directory, got a root directory ~S instead.~2%" source-dir)
      (return-from analyze))
    (let ((parent-pathname (make-pathname :directory (if (pathname-name source-pathname)
                                                       (pathname-directory source-pathname)
                                                       (butlast (pathname-directory source-pathname)))
                                          :name nil
                                          :type nil))
          (parent-dir-name (car (last (pathname-directory source-pathname))))
          (logs-dir (merge-pathnames "logs/" (asdf:system-source-directory :dependency-analyzer))))
    
      ;; Verify source directory exists  
      (unless (ignore-errors (truename source-pathname))
        (format t "~2%The provided source directory ~A does not exist." source-dir)
        (format t "~%Please verify the directory containing the source files for the project.~2%")
        (return-from analyze))

      ;; Collect all source files
      (let ((source-files
              (mapcan (lambda (ext)
                         (directory (make-pathname :defaults source-pathname
                                                   :directory (append (pathname-directory source-pathname)
                                                                      '(:wild-inferiors))
                                                   :name :wild
                                                   :type ext)))
                      '("lisp" "lsp" "cl"))))
        (unless source-files
          (format t "~2%Can't find lisp source files (with extension .lisp, .lsp, or .cl) in ~A." source-dir)
          (format t "~%Please verify the source directory and valid file extensions for analysis.~2%")
          (return-from analyze))

        (format t "~2%Found source files:~%~{  ~A~%~}~%" source-files)

        ;; Begin analysis
        (with-dependency-tracker ((make-instance 'dependency-tracker
                                                 :project-name parent-dir-name
                                                 :project-root parent-pathname))

          ;; Initialization: get all project-related packages
          (setf (slot-value *current-tracker* 'project-packages)
            (load-project-and-get-all-related-packages))

          ;; First pass: analyze definitions
          (format t "~%First Pass - Collecting Definitions...~%")
          (dolist (file source-files)
            (let ((file-parser (make-instance 'file-parser :file file)))
              (parse-definitions-in-file file-parser)))

          ;; Second pass: analyze references  
          (format t "~%Second Pass - Analyzing References...~%") 
          (dolist (file source-files)
            (let ((file-parser (make-instance 'file-parser :file file)))
              (parse-references-in-file file-parser)))

          ;; Third pass: package-symbol analysis
          (format t "~%Third Pass - Analyzing Package/Symbol Consistency...~2%")
          (dolist (file source-files)
            (let ((file-parser (make-instance 'file-parser :file file)))
              (parse-package-symbols-in-file file-parser)))

          ;; Post-pass analysis
          ;(detect-unused-definitions *current-tracker*)  ;maybe enhance later
          (detect-redundant-package-uses *current-tracker*)
          (detect-suboptimal-package-placement *current-tracker*)
          (detect-shadowed-definitions *current-tracker*)
          (detect-qualified-internal-references *current-tracker*)
          (detect-circular-type-dependencies *current-tracker*)
          (detect-inline-package-references *current-tracker*)
          (detect-indirect-slot-access *current-tracker*)

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
          (report)
          *current-tracker*)))))


;;; Do a runtime snapshot, load the project,
;;; and gather the newly introduced packages + their dependencies.
(defun load-project-and-get-all-related-packages ()  ;(loader-fn)
  "1) Record all packages before loading.
   2) Call LOADER-FN (a function that loads/compiles your project).
   3) Record all packages after loading.
   4) Identify new packages introduced by the load.
   5) Gather the transitive closure of dependencies from those packages.
   Returns a list of all 'project-related' packages."
  (let ((before (list-all-packages)))
    ;; Delete FASL files before loading to ensure fresh compilation of user's project files
    (delete-project-fasls :test-project)
    (asdf:load-system :test-project)
    ;; Analyze runtime :depends-on system dependencies
    (setf (slot-value *current-tracker* 'subsystems)
          (get-runtime-dependencies :test-project))
    (let ((after (list-all-packages))
          ;; Store all project-related packages in PROJECT-PACKAGES
          (project-packages '()))
      (let ((new-packages (set-difference after before :test #'eq)))
        ;; Store just the new packages
        (setf (slot-value *current-tracker* 'project-owned-packages) new-packages)
        ;; For each newly introduced package, gather its full dependency closure.
        (dolist (pkg new-packages)
          (setq project-packages
                (union project-packages
                       (all-package-dependencies pkg)
                       :test #'eq))))
      project-packages)))


(defun get-runtime-dependencies (system-name)
  "Get runtime dependencies as a list of system names"
  (let ((sys (asdf:find-system system-name)))
    (remove-duplicates 
     (mapcar #'(lambda (dep)
                 (asdf:primary-system-name (second dep)))
             (asdf:component-depends-on 'asdf:load-op sys))
     :test #'string=)))


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
  (let ((anomaly-types nil))
    ;; Collect all anomaly types
    (maphash (lambda (type anomaly-list)
               (declare (ignore anomaly-list))
               (push type anomaly-types))
             (slot-value *current-tracker* 'anomalies))
    ;; Process each type in sorted order
    (dolist (type (sort anomaly-types #'string< :key #'symbol-name))
      (when-let (anomalies-of-type (gethash type (slot-value *current-tracker* 'anomalies)))
        (dolist (anomaly (sort anomalies-of-type #'string< 
                               :key #'anomaly.description))
          (print-anomaly anomaly log-stream 0))
        (terpri log-stream)))))
