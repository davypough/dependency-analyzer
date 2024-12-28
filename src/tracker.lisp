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


(defun parse-packages-in-file (parser project-package)
  "First pass: Validate package form ordering and track package creations.
   Records anomalies for:
   1. Missing in-package as first form (unless proper exceptions)
   2. Package creation forms spread across multiple files"
  (declare (special log-stream))
  (with-slots (file) parser
    ;; For validating in-package requirement
    (let ((first-form-package-p nil)
          (second-form-in-package-p nil)
          (found-non-package-form nil)
          (package-creation-forms nil))
      
      ;; Scan all forms in file
      (with-open-file (stream file :direction :input)
        (loop for form = (read stream nil :eof)
              for form-count from 1
              until (eq form :eof)
              do (when (and (consp form) (symbolp (car form)))
                   (let ((op (car form)))
                     ;; Track package creations for multi-file check
                     (when (member op '(defpackage make-package))
                       (push (list op (second form) file) package-creation-forms))
                     
                     ;; Track form positions for in-package validation
                     (cond ((= form-count 1)
                            (when (member op '(defpackage make-package in-package))
                              (setf first-form-package-p t)))
                           ((= form-count 2)
                            (when (eq op 'in-package)
                              (setf second-form-in-package-p t)))
                           (t (unless (member op '(defpackage make-package in-package))
                                (setf found-non-package-form t))))))))
      
      ;; Record anomaly if package definitions spread over multiple files
      (when package-creation-forms
        (let* ((files (mapcar #'third package-creation-forms))
               (unique-files (remove-duplicates files :test #'equal))
               (project-files (mapcar #'project-pathname unique-files)))
          (when (> (length unique-files) 1)
            (record-anomaly *current-tracker*
                       :name (current-package-name parser)
                       :type :dispersed-package-forms
                       :severity :INFO
                       :files project-files
                       :description "Packages creation not consolidated into one file; potential for subtle package bugs")
            (let ((anomalies (gethash (current-package-name parser) 
                                    (slot-value *current-tracker* 'anomalies))))
              (when anomalies
                (format log-stream "~A~%" anomalies))))))

      ;; Check for missing/improper in-package
      (unless (or (eq project-package (find-package :cl-user))
                  (and first-form-package-p second-form-in-package-p)
                  (and (not found-non-package-form) first-form-package-p))
        (record-anomaly *current-tracker*
                       :name (current-package-name parser)
                       :type :missing-in-package
                       :severity :WARNING
                       :files (list file)
                       :description "File should begin with in-package form")
        (let ((anomalies (gethash (current-package-name parser) 
                                (slot-value *current-tracker* 'anomalies))))
          (when anomalies
            (format log-stream "~A~%" anomalies)))))))
