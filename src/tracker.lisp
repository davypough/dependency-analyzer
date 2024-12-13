;;;; Filename: tracker.lisp
;;;
;;; Core dependency tracking functionality.
;;; Provides the main data structures and operations for recording and querying
;;; dependencies between files, symbols, and packages.


(in-package #:dep)


(defun analyze (source-dir)
  "Analyze source files in a directory and its subdirectories in three passes:
   1. Initialize all packages across all files by:
      a) Collecting all package forms
      b) Creating base packages
      c) Evaluating full package forms
   2. Collect all definitions across all files
   3. Analyze references to those definitions  
   Each pass logs its form traversal analysis to a separate log file."
  (let* ((source-pathname (pathname source-dir))
         (parent-pathname (make-pathname :directory (if (pathname-name source-pathname)
                                                      (pathname-directory source-pathname)
                                                      (butlast (pathname-directory source-pathname)))
                                       :name nil
                                       :type nil))
         (parent-dir-name (car (last (pathname-directory source-pathname))))
         (logs-dir (merge-pathnames "logs/" (asdf:system-source-directory :dependency-analyzer))))
    ;; Verify source directory exists  
    (unless (ignore-errors (truename source-pathname))
      (error "~2%Error: The directory ~A does not exist.~%" source-dir))
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
        ;; First pass: initialize all packages
        (format t "~%First Pass - Initializing Packages...~%")
        (with-open-file (log-stream (merge-pathnames "packages-trace.log" logs-dir) :direction :output
                                   :if-exists :supersede :if-does-not-exist :create)
          (unless (initialize-packages source-files log-stream)
            (error "~2%Package initialization failed. Cannot continue reliable analysis. See packages-trace.log file.")))
        
        ;; Second pass: analyze definitions
        (format t "~%Second Pass - Collecting Definitions...~%")
        (with-open-file (log-stream (merge-pathnames "definitions-trace.log" logs-dir) :direction :output
                                   :if-exists :supersede :if-does-not-exist :create)
          (dolist (file source-files)
            (format log-stream "~%Definitions Analysis Trace for ~A~2%" file)
            (let ((file-parser (make-instance 'file-parser :file file)))
              (parse-definitions-in-file file-parser log-stream))))
        
        ;; Third pass: analyze references  
        (format t "~%Third Pass - Analyzing References...~2%") 
        (with-open-file (log-stream (merge-pathnames "references-trace.log" logs-dir) :direction :output
                                   :if-exists :supersede :if-does-not-exist :create)
          (dolist (file source-files)
            (format log-stream "~%Reference Analysis Trace for ~A~2%" file)
            (let ((file-parser (make-instance 'file-parser :file file)))
              (parse-references-in-file file-parser log-stream))))

        ;; Log final definitions, references, anomalies
        (with-open-file (log-stream (merge-pathnames "definitions.log" logs-dir) :direction :output
                                   :if-exists :supersede :if-does-not-exist :create)
          (log-definitions log-stream))
        (with-open-file (log-stream (merge-pathnames "references.log" logs-dir) :direction :output 
                                   :if-exists :supersede :if-does-not-exist :create)
          (log-references log-stream))
        (with-open-file (log-stream (merge-pathnames "anomalies.log" logs-dir) :direction :output
                                   :if-exists :supersede :if-does-not-exist :create)
          (log-anomalies log-stream))
        (in-package :dep)
        *current-tracker*))))


(defun initialize-packages (source-files log-stream)
  "Initialize all packages in source files using a staged approach:
   1. Collect all package definition forms
   2. Create base packages with just names
   3. Evaluate full package definitions  
   Returns T if successful, NIL if errors encountered."
  (let ((package-forms nil))
    ;; Stage 1: Collect all package forms
    (format log-stream "~&Stage 1: Collecting package forms...~%")
    (dolist (file source-files)
      (with-open-file (stream file :direction :input)
        (format log-stream "~&Scanning file: ~A~%" file)
        (loop for form = (read stream nil :eof)
              until (eq form :eof)
              when (and (consp form) 
                       (member (car form) '(defpackage make-package)))
              do (progn 
                   (format log-stream "~&Found package form: ~S" form)
                   (push (list form file) package-forms))
              finally (terpri log-stream))))
    
    ;; Stage 2: Create base packages
    (format log-stream "~&Stage 2: Creating base packages...~%")
    (dolist (form-and-file package-forms)
      (let* ((form (first form-and-file))
             (file (second form-and-file))
             (raw-name (cond
                        ((eq (car form) 'defpackage) 
                         (second form))
                        ((eq (car form) 'make-package)
                         (if (stringp (second form))
                             (second form)
                             (second form)))))
             (package-name (normalize-designator raw-name)))
        (format log-stream "~&Creating base package: ~A~%" package-name)
        (handler-case
            (eval `(make-package ,package-name :use nil))
          (error (e)
            ;; Skip if package already exists
            (unless (typep e 'package-error)
              (format log-stream "~&Error creating package ~A in ~A: ~A~%" 
                      package-name file e)
              (return-from initialize-packages nil))))))
    
    ;; Stage 3: Evaluate full package definitions
    (format log-stream "~%Stage 3: Evaluating package definitions...~%")
    (handler-case
        (dolist (form-and-file package-forms)
          (let ((form (first form-and-file))
                (file (second form-and-file)))
            (format log-stream "~&Evaluating package form in ~A:~% ~S~%" file form)
            (handler-case (eval form)
              (error (e)
                (format log-stream "~&Error evaluating package form in ~A:~% ~A~%" 
                        file e)
                (format log-stream "Form: ~S~%" form)
                (return-from initialize-packages nil)))))
      (error (e)
        (format log-stream "~&Unexpected error during package initialization: ~A~%" e)
        (return-from initialize-packages nil)))
    
    t))