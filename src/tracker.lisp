;;;; Filename: tracker.lisp
;;;
;;; Core dependency tracking functionality.
;;; Provides the main data structures and operations for recording and querying
;;; dependencies between files, symbols, and packages.


(in-package #:dep)


(defun analyze (source-dir)  ; package-designator)
  "Analyze source files in a directory for dependencies.
   Project must be loaded first for reliable analysis.
   Returns tracker instance with analysis results."
  (unless (stringp source-dir)
    (format t "~2%Note that the source directory must be a string, got ~S instead.~2%" source-dir)
    (return-from analyze))
  ;(unless (typep package-designator '(or character string package symbol))
  ;  (format t "~2%Note that the package-designator must be a character, string, package, or symbol, got ~S instead.~2%" 
  ;          package-designator)
  ;  (return-from analyze))
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
          ;(project-package (find-package package-designator))
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

        (format t "~2%Found source files:~%~{  ~A~%~}" source-files)

        ;; Begin analysis
        (with-dependency-tracker ((make-instance 'dependency-tracker
                                                 :project-name parent-dir-name
                                                 ;:project-package project-package
                                                 :project-root parent-pathname))

          ;; Initialization: get all project-related packages
          (setf (slot-value *current-tracker* 'project-packages)
            (load-project-and-get-all-related-packages))

          ;; First pass: analyze definitions
          (format t "~%First Pass - Collecting Definitions...~%")
          (with-open-file (log-stream (merge-pathnames "definitions-trace.log" logs-dir)
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
            (declare (special log-stream))
            (dolist (file source-files)
              (format log-stream "~%Definitions Analysis Trace for ~A~2%" file)
              (let ((file-parser (make-instance 'file-parser :file file)))
                (parse-definitions-in-file file-parser))))

          ;; Second pass: analyze references  
          (format t "~%Second Pass - Analyzing References...~%") 
          (with-open-file (log-stream (merge-pathnames "references-trace.log" logs-dir)
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
            (declare (special log-stream))
            (dolist (file source-files)
              (format log-stream "~%Reference Analysis Trace for ~A~2%" file)
              (let ((file-parser (make-instance 'file-parser :file file)))
                (parse-references-in-file file-parser))))

          ;; Third pass: package-symbol analysis
          (format t "~%Third Pass - Analyzing Package/Symbol Consistency...~2%")
          (with-open-file (log-stream (merge-pathnames "package-symbols-trace.log" logs-dir)
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
            (declare (special log-stream))
            (dolist (file source-files)
              (format log-stream "~%Package/Symbol Analysis Trace for ~A~2%" file)
              (let ((file-parser (make-instance 'file-parser :file file)))
                (parse-package-symbols-in-file file-parser))))

          ;; Post-pass analysis
          ;(detect-unused-definitions *current-tracker*)  ;maybe enhance later
          (detect-redundant-package-uses *current-tracker*)
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
          *current-tracker*)))))


;;;;;;;;;;;;;;;;;; Find all project-related packages ;;;;;;;;;;;;;;;;;


;;; 1) A helper to see which packages a given package inherits via :USE
(defun used-packages (package)
  "Return a list of packages that PACKAGE uses (directly)."
  (package-use-list (find-package package)))


;;; 2) A helper to see which packages provide the symbols that PACKAGE has imported
;;;    (whether import-from or shadowing-import-from).
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


;;; 3) Combine them to get all direct dependencies (via :USE or import).
(defun direct-package-dependencies (package)
  "Return a list of packages that PACKAGE depends on, either via :use
   or via (shadowing-)import-from. This does NOT recursively descend."
  (let ((pkg (find-package package)))
    (if pkg
        (remove-duplicates
         (append (used-packages pkg)
                 (imported-from-packages pkg))
         :test #'eq)
      '())))


;;; 4) Compute the FULL transitive closure of dependencies.
(defun all-package-dependencies (package
                                 &optional (acc (make-hash-table :test 'eq)))
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


;;; 5) Now the top-level function to do a runtime snapshot, load the project,
;;;    and gather the newly introduced packages + their dependencies.
(defun load-project-and-get-all-related-packages ()  ;(loader-fn)
  "1) Record all packages before loading.
   2) Call LOADER-FN (a function that loads/compiles your project).
   3) Record all packages after loading.
   4) Identify new packages introduced by the load.
   5) Gather the transitive closure of dependencies from those packages.
   Returns a list of all 'project-related' packages."
  (let ((before (list-all-packages)))
    ;; Execute the user-supplied loader, e.g., (funcall loader-fn)
    (asdf:load-system :test-project)
    (let ((after (list-all-packages))
          ;; We’ll store all project-related packages in PROJECT-PACKAGES
          (project-packages '()))
      (let ((new-packages (set-difference after before :test #'eq)))
        ;; For each newly introduced package, gather its full dependency closure.
        (dolist (pkg new-packages)
          (setq project-packages
                (union project-packages
                       (all-package-dependencies pkg)
                       :test #'eq))))
      project-packages)))


