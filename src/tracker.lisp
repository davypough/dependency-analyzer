;;;; Filename: tracker.lisp
;;;
;;; Core dependency tracking functionality.
;;; Provides the main data structures and operations for recording and querying
;;; dependencies between files, symbols, and packages.


(in-package #:dep)


(defun analyze (source-dir package-designator)
  "Analyze source files in a directory for dependencies.
   Project must be loaded first for reliable analysis.
   Returns tracker instance with analysis results."
  (let* ((source-pathname (pathname source-dir))
         (parent-pathname (make-pathname :directory (if (pathname-name source-pathname)
                                                      (pathname-directory source-pathname)
                                                      (butlast (pathname-directory source-pathname)))
                                       :name nil
                                       :type nil))
         (parent-dir-name (car (last (pathname-directory source-pathname))))
         (project-package (find-package package-designator))
         (logs-dir (merge-pathnames "logs/" (asdf:system-source-directory :dependency-analyzer))))
    
    ;; Verify source directory exists  
    (unless (ignore-errors (truename source-pathname))
      (error "~2%Error: The directory ~A does not exist.~%" source-dir))

    ;; Verify package exists
    (unless project-package
      (error "~2%Error: The project package ~A was not found. Project must be loaded before analysis.~%" 
             package-designator))

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
        (error "~2%There are no lisp source files in ~A." source-dir))
      (format t "~2%Found source files:~%~{  ~A~%~}" source-files)

      (with-dependency-tracker ((make-instance 'dependency-tracker :project-name parent-dir-name
                                             :project-root parent-pathname))
        ;; First pass: analyze definitions
        (format t "~%First Pass - Collecting Definitions...~%")
        (with-open-file (log-stream (merge-pathnames "definitions-trace.log" logs-dir) :direction :output
                                   :if-exists :supersede :if-does-not-exist :create)
          (declare (special log-stream))
          (dolist (file source-files)
            (format log-stream "~%Definitions Analysis Trace for ~A~2%" file)
            (let ((file-parser (make-instance 'file-parser :file file)))
              (parse-definitions-in-file file-parser))))

        ;; Second pass: analyze references  
        (format t "~%Second Pass - Analyzing References...~2%") 
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
        (with-open-file (log-stream (merge-pathnames "definitions.log" logs-dir) :direction :output
                                   :if-exists :supersede :if-does-not-exist :create)
          (declare (special log-stream))
          (log-definitions))
        (with-open-file (log-stream (merge-pathnames "references.log" logs-dir) :direction :output 
                                   :if-exists :supersede :if-does-not-exist :create)
          (declare (special log-stream))
          (log-references))
        (with-open-file (log-stream (merge-pathnames "anomalies.log" logs-dir) :direction :output
                                   :if-exists :supersede :if-does-not-exist :create)
          (declare (special log-stream))
          (log-anomalies))
        (in-package :dep)
        *current-tracker*))))

