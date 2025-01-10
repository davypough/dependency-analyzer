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
  (unless (stringp source-dir)
    (format t "~2%Note that the source directory must be a string, got ~S instead.~2%" source-dir)
    (return-from analyze))
  (unless (typep package-designator '(or character string package symbol))
    (format t "~2%Note that the package-designator must be a character, string, package, or symbol, got ~S instead.~2%" 
            package-designator)
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
          (project-package (find-package package-designator))
          (logs-dir (merge-pathnames "logs/" (asdf:system-source-directory :dependency-analyzer))))
    
      ;; Verify source directory exists  
      (unless (ignore-errors (truename source-pathname))
        (format t "~2%The provided source directory ~A does not exist." source-dir)
        (format t "~%Please verify the directory containing the source files for the project.~2%")
        (return-from analyze))

      ;; Verify package exists
      (unless project-package
        (format t "~2%The project package ~A was not found." package-designator)
        (format t "~%Note that the project's source files must be successfully compiled & loaded before analysis.~2%")
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
                                                 :project-package project-package
                                                 :project-root parent-pathname))
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

