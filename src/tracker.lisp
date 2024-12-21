;;;; Filename: tracker.lisp
;;;
;;; Core dependency tracking functionality.
;;; Provides the main data structures and operations for recording and querying
;;; dependencies between files, symbols, and packages.


(in-package #:dep)


(defun analyze (source-dir)
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
        ;; First pass: parse packages
        (format t "~%First Pass - Parsing Packages...~%")
        (with-open-file (log-stream (merge-pathnames "packages-trace.log" logs-dir) :direction :output
                                   :if-exists :supersede :if-does-not-exist :create)
          (declare (special log-stream))
          (dolist (file source-files)
            (format log-stream "~%Package Analysis Trace for ~A~2%" file)
            (let ((file-parser (make-instance 'file-parser :file file)))
              (parse-packages-in-file file-parser))))
        
        ;; Second pass: analyze definitions
        (format t "~%Second Pass - Collecting Definitions...~%")
        (with-open-file (log-stream (merge-pathnames "definitions-trace.log" logs-dir) :direction :output
                                   :if-exists :supersede :if-does-not-exist :create)
          (declare (special log-stream))
          (dolist (file source-files)
            (format log-stream "~%Definitions Analysis Trace for ~A~2%" file)
            (let ((file-parser (make-instance 'file-parser :file file)))
              (parse-definitions-in-file file-parser))))
        
        ;; Third pass: analyze references  
        (format t "~%Third Pass - Analyzing References...~2%") 
        (with-open-file (log-stream (merge-pathnames "references-trace.log" logs-dir) 
                           :direction :output
                           :if-exists :supersede 
                           :if-does-not-exist :create)
           (declare (special log-stream))
           (dolist (file source-files)
             (format log-stream "~%Reference Analysis Trace for ~A~2%" file)
             (let ((file-parser (make-instance 'file-parser :file file)))
               (parse-references-in-file file-parser))))

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


(defun verify-package-state (pkg-name file context operation)
 "Verify package state matches the operation found in source.
  Records anomaly if package state doesn't match expected."
 (let ((pkg (find-package pkg-name)))
   (case operation 
     ((:defpackage :make-package :in-package)
      (unless pkg
        (record-anomaly *current-tracker*
                       :name pkg-name
                       :type :package-not-loaded
                       :severity :error
                       :file file
                       :description (format nil "Package ~A not found in environment - project may not be loaded" pkg-name)
                       :context context)))
     (:delete-package
      (when pkg
        (record-anomaly *current-tracker*
                       :name pkg-name 
                       :type :package-state-mismatch
                       :severity :error
                       :file file
                       :description (format nil "Package ~A exists but source contains delete-package" pkg-name)
                       :context context)))
     (:rename-package 
      (unless pkg
        (record-anomaly *current-tracker*
                       :name pkg-name
                       :type :package-state-mismatch 
                       :severity :error
                       :file file
                       :description (format nil "Cannot rename package ~A - package not found" pkg-name)
                       :context context))))))


(defun parse-packages-in-file (parser)
 "Parse package definitions, validating against loaded environment.
  Records package definitions and validates against current Lisp image 
  since project should already be loaded."
 (declare (special log-stream))
 (with-slots (file) parser
   (format log-stream "~&Scanning file: ~A~%" file)
   (with-open-file (stream file :direction :input)
     (loop for form = (read stream nil :eof)
           until (eq form :eof)
           when (and (consp form) 
                    (symbolp (car form))
                    (member (car form) '(defpackage make-package rename-package delete-package in-package)))
           do (let* ((raw-name (second form))
                    (pkg-name (normalize-designator raw-name))
                    (op (car form))
                    (context (limit-form-size form pkg-name)))

                (format log-stream "~&Found package form: ~S~%" form)
                
                ;; Validate package state matches operation
                (verify-package-state pkg-name file context op)

                (case op
                  (defpackage
                   ;; Record definition and process options 
                   (record-definition *current-tracker*
                                    :name pkg-name
                                    :type :PACKAGE
                                    :file file
                                    :package pkg-name 
                                    :exported-p t
                                    :context context)
                   
                   (let ((options (cddr form)))
                     ;; Process :use options
                     (dolist (option options)
                       (when (and (listp option)
                                (eq (car option) :use))
                         (dolist (used-pkg (cdr option))
                           (let ((used-name (normalize-designator used-pkg)))
                             (record-package-use *current-tracker* pkg-name used-name)))))
                     
                     ;; Process :export options
                     (dolist (option options)
                       (when (and (listp option)
                                (eq (car option) :export))
                         (dolist (sym (cdr option))
                           (record-export *current-tracker* pkg-name sym))))))
                  
                  ((make-package rename-package delete-package in-package)
                   ;; Record package references for other operations
                   (record-reference *current-tracker*
                                   :name pkg-name
                                   :file file 
                                   :package pkg-name
                                   :visibility :LOCAL
                                   :context context))))))))