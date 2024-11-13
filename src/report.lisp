;;;; Filename: report.lisp
;;;
;;; Report generation functionality for dependency analysis results.
;;; Provides multiple report formats for visualizing dependencies between
;;; files, packages, and symbols.


(in-package #:dep)


(defun pathname-to-string (pathname)
  "Convert a pathname to a string representation."
  (if pathname
      (namestring pathname)
      nil))


(defun simplify-path (pathname)
  "Convert a pathname to a simplified string representation."
  (if pathname
      (let ((name (enough-namestring pathname)))
        (if (char= (char name 0) #\/)
            (subseq name 1)
            name))
      nil))


(defun source-file-name (pathname)
  "Extract just the source file name from a pathname."
  (if pathname
      (file-namestring pathname)
      nil))


(defgeneric generate-report (format tracker &key stream)
  (:documentation "Generate a dependency report in the specified format.")
  (:method ((format (eql :text)) tracker &key (stream *standard-output*))
    (let ((system-name (system.name tracker)))
      ;; Header
      (format stream "~&Dependency Analysis Report for System: ~A~%" system-name)
      (format stream "~&================================================~%")
      
      ;; Cycle warnings - only show sections if cycles exist
      (alexandria:when-let ((cycles (get-system-cycles tracker)))
        (format stream "~&System Dependency Cycles:~%")
        (dolist (cycle cycles)
          (format stream "  ~A~%" cycle)))
      
      (alexandria:when-let ((cycles (get-file-cycles tracker)))
        (format stream "~&File Dependency Cycles:~%")
        (dolist (cycle cycles)
          (format stream "  ~A~%" cycle)))
      
      (alexandria:when-let ((cycles (get-package-cycles tracker)))
        (format stream "~&Package Dependency Cycles:~%")
        (dolist (cycle cycles)
          (format stream "  ~A~%" cycle)))
      
      ;; File dependencies
      (format stream "~&~%File Dependencies:~%")
      (maphash (lambda (file definitions)
                 (declare (ignore definitions))
                 (format stream "~&File: ~A~%" (pathname-to-string file))
                 (let ((deps (file-dependencies tracker file)))
                   (when deps
                     (format stream "  Depends on:~%")
                     (dolist (dep deps)
                       (format stream "    ~A~%" (pathname-to-string dep)))))
                 (let ((deps (file-dependents tracker file)))
                   (when deps
                     (format stream "  Required by:~%")
                     (dolist (dep deps)
                       (format stream "    ~A~%" (pathname-to-string dep))))))
               (slot-value tracker 'file-map))
      
      ;; Package relationships
      (format stream "~&~%Package Dependencies:~%")
      (maphash (lambda (pkg used-pkgs)
                 (format stream "~&Package: ~A~%" pkg)
                 (when used-pkgs
                   (format stream "  Uses packages:~%")
                   (dolist (used used-pkgs)
                     (format stream "    ~A~%" used)))
                 (let ((exports (get-package-exports tracker pkg)))
                   (when exports
                     (format stream "  Exports:~%")
                     (dolist (sym exports)
                       (format stream "    ~A~%" sym)))))
               (slot-value tracker 'package-uses))))

  (:method ((format (eql :json)) tracker &key (stream *standard-output*))
    (let* ((system-cycles (get-system-cycles tracker))
           (file-cycles (get-file-cycles tracker))
           (package-cycles (get-package-cycles tracker))
           (files (build-file-dependency-json tracker))
           (packages (build-package-dependency-json tracker))
           (report-data
             (alexandria:alist-hash-table
              (remove nil
                `(("system" . ,(system.name tracker))
                  ,@(when (or system-cycles file-cycles package-cycles)
                      `(("cycles" . ,(remove nil
                                           `(,@(when system-cycles `(("system" . ,system-cycles)))
                                             ,@(when file-cycles `(("files" . ,file-cycles)))
                                             ,@(when package-cycles `(("packages" . ,package-cycles))))))))
                  ,@(when files `(("files" . ,files)))
                  ,@(when packages `(("packages" . ,packages)))))
              :test 'equal)))
      (yason:with-output (stream)
        (yason:encode report-data))))

  (:method ((format (eql :dot)) tracker &key (stream *standard-output*))
    (format stream "digraph Dependencies {~%")
    (format stream "  rankdir=LR;~%")
    (format stream "  node [shape=box, fontname=\"Arial\"];~%")
    (format stream "  edge [fontname=\"Arial\"];~%")
    
    ;; Generate file nodes and edges
    (maphash (lambda (file definitions)
               (declare (ignore definitions))
               (let* ((file-name (source-file-name file))
                      (file-path (simplify-path file))
                      (file-id (string-to-dot-id file-name)))
                 (format stream "  \"~A\" [label=\"~A\"];~%" 
                         file-id file-path)
                 (dolist (dep (file-dependencies tracker file))
                   (let* ((dep-name (source-file-name dep))
                          (dep-id (string-to-dot-id dep-name)))
                     (format stream "  \"~A\" -> \"~A\";~%"
                             file-id dep-id)))))
             (slot-value tracker 'file-map))
    
    (format stream "}~%")))


(defun string-to-dot-id (string)
  "Convert a string to a valid DOT graph identifier."
  (string-downcase
   (with-output-to-string (s)
     (loop for char across (or string "")
           do (case char
                ((#\Space #\/ #\\ #\. #\-)
                 (write-char #\_ s))
                (otherwise
                 (write-char char s)))))))


(defun build-file-dependency-json (tracker)
  "Build JSON structure for file dependencies."
  (let ((result (make-hash-table :test 'equal)))
    (maphash (lambda (file definitions)
               (declare (ignore definitions))
               (let* ((file-str (pathname-to-string file))
                      (deps (remove nil (mapcar #'pathname-to-string 
                                              (file-dependencies tracker file))))
                      (required-by (remove nil (mapcar #'pathname-to-string 
                                                     (file-dependents tracker file)))))
                 (when (or deps required-by)
                   (setf (gethash file-str result)
                         (alexandria:alist-hash-table
                          (remove nil
                                 `(,@(when deps `(("depends_on" . ,deps)))
                                   ,@(when required-by `(("required_by" . ,required-by)))))
                          :test 'equal)))))
             (slot-value tracker 'file-map))
    result))


(defun build-package-dependency-json (tracker)
  "Build JSON structure for package dependencies."
  (let ((result (make-hash-table :test 'equal)))
    (maphash (lambda (pkg used-pkgs)
               (let ((exports (mapcar #'symbol-name (get-package-exports tracker pkg))))
                 (when (or used-pkgs exports)
                   (setf (gethash pkg result)
                         (alexandria:alist-hash-table
                          (remove nil
                                 `(,@(when used-pkgs `(("uses" . ,used-pkgs)))
                                   ,@(when exports `(("exports" . ,exports)))))
                          :test 'equal)))))
             (slot-value tracker 'package-uses))
    result))


(defun report (&optional filename)
  "Generate a comprehensive dependency report for the current system analysis.
   If FILENAME is provided, saves the report to that file."
  (unless *current-tracker*
    (error "No analysis results available. Please run (dep:analyze-system \"system-name\") first."))
  (flet ((generate-all-reports (stream)
           ;; Header
           (format stream "~&~V,,,'-<~>" 70 "")
           (format stream "~&Dependency Analysis Report~%")
           (format stream "System: ~A~%" (system.name *current-tracker*))
           (format stream "Generated: ~A~%" (local-time:now))
           (format stream "~V,,,'-<~>~%" 70 "")
           ;; Core report in text format
           (generate-report :text *current-tracker* :stream stream)
           ;; Add DOT graph as appendix
           (format stream "~%~%APPENDIX A: Graphviz DOT Format~%")
           (format stream "~V,,,'-<~>~%" 70 "")
           (format stream "Save the following content to a .dot file and process with Graphviz:~%~%")
           (generate-report :dot *current-tracker* :stream stream)
           ;; Add JSON as appendix
           (format stream "~%~%APPENDIX B: JSON Format~%")
           (format stream "~V,,,'-<~>~%" 70 "")
           (format stream "~%")
           (generate-report :json *current-tracker* :stream stream)))
    (if filename
        ;; Save to file
        (with-open-file (out filename 
                         :direction :output 
                         :if-exists :supersede
                         :if-does-not-exist :create)
          (generate-all-reports out)
          (format t "~&Report saved to: ~A~%" filename))
        ;; Display to standard output
        (generate-all-reports *standard-output*)))
  ;; Return the tracker to allow for chaining
  *current-tracker*)