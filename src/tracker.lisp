;;;; Filename: tracker.lisp
;;;
;;; Core dependency tracking functionality.
;;; Provides the main data structures and operations for recording and querying
;;; dependencies between files, symbols, and packages.


(in-package #:dep)


(defun get-definitions (&optional tracker symbol)
  "Get all recorded definitions of a symbol or string.
   If only one arg provided, treat it as the symbol/string and use the current tracker."
  (when (and tracker (null symbol))
    ;; If only one arg provided, it's the symbol/string
    (setf symbol tracker
          tracker nil))
  (let* ((actual-tracker (or tracker (ensure-tracker)))
         (pkg-name (etypecase symbol
                    (string "COMMON-LISP-USER")  
                    (symbol (if (symbol-package symbol)
                              (package-name (symbol-package symbol))
                              "COMMON-LISP-USER"))))
         (key (make-tracking-key symbol pkg-name)))
    (or (gethash key (slot-value actual-tracker 'definitions))
        ;; Try without package context as fallback
        (gethash (make-tracking-key symbol) (slot-value actual-tracker 'definitions)))))


(defun get-references (&optional (tracker nil tracker-provided-p) symbol)
  "Get all recorded references to a symbol."
  (let* ((actual-tracker (if tracker-provided-p tracker (ensure-tracker)))
         (key (if (symbol-package symbol)
                 (make-tracking-key symbol (package-name (symbol-package symbol)))
                 (make-tracking-key symbol))))
    (gethash key (slot-value actual-tracker 'references))))


(defun get-file-definitions (&optional (tracker nil tracker-provided-p) file)
  "Get all definitions in a file."
  (let ((actual-tracker (if tracker-provided-p tracker (ensure-tracker))))
    (gethash file (slot-value actual-tracker 'file-map))))


(defun package-depends-on-p (&optional (tracker nil tracker-provided-p) package1 package2)
  "Check if package1 depends on package2 (directly or indirectly)."
  (let ((actual-tracker (if tracker-provided-p tracker (ensure-tracker))))
    (labels ((check-deps (pkg visited)
               (when (member pkg visited :test #'string=)
                 (return-from check-deps nil))
               (let ((uses (get-package-uses actual-tracker pkg)))
                 (or (member package2 uses :test #'string=)
                     (some (lambda (p)
                            (check-deps p (cons pkg visited)))
                          uses)))))
      (check-deps package1 nil))))


(defmethod clear-tracker (&optional (tracker nil tracker-provided-p))
  "Clear all recorded information from the tracker."
  (let ((actual-tracker (if tracker-provided-p tracker (ensure-tracker))))
    (with-slots (definitions references file-map package-uses 
                 package-exports macro-bodies) actual-tracker
      (clrhash definitions)
      (clrhash references)
      (clrhash file-map)
      (clrhash package-uses)
      (clrhash package-exports)
      (clrhash macro-bodies))))


(defmethod clear-tracker :after (&optional (tracker nil tracker-provided-p))
  "Clear all recorded information including cycles from the tracker."
  (let ((actual-tracker (if tracker-provided-p tracker (ensure-tracker))))
    (setf (project-cycles actual-tracker) nil)
    (setf (file-cycles actual-tracker) nil)
    (setf (package-cycles actual-tracker) nil)))


(defun analyze (source-dir)
  "Analyze source files in a directory and its subdirectories in two passes:
   1. Collect all definitions across all files
   2. Analyze references to those definitions
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
        ;; First pass: analyze definitions
        (format t "~%First Pass - Collecting Definitions...~%")
        (with-open-file (log-stream (merge-pathnames "definitions-trace.log" logs-dir) :direction :output
                                   :if-exists :supersede :if-does-not-exist :create)
          (dolist (file source-files)
            (format log-stream "~%Definitions Analysis Trace for ~A~2%" file)
            (let ((file-parser (make-instance 'file-parser :file file)))
              (parse-definitions-in-file file-parser log-stream))))
        ;; Second pass: analyze references
        (format t "~%Second Pass - Analyzing References...~2%")
        (with-open-file (log-stream (merge-pathnames "references-trace.log" logs-dir) :direction :output
                                    :if-exists :supersede :if-does-not-exist :create)
          (dolist (file source-files)
            (format log-stream "~%Reference Analysis Trace for ~A~2%" file)
            (let ((file-parser (make-instance 'file-parser :file file)))
              (parse-references-in-file file-parser log-stream))))
        ;; Log final definitions, references, anomalies
        (with-open-file (log-stream (merge-pathnames "definitions.log" logs-dir) :direction :output
                                   :if-exists :supersede :if-does-not-exist :create)
          (print-tracker-slot *current-tracker* 'definitions log-stream))
        (with-open-file (log-stream (merge-pathnames "references.log" logs-dir) :direction :output 
                                    :if-exists :supersede :if-does-not-exist :create)
          (print-tracker-slot *current-tracker* 'references log-stream))
        (with-open-file (log-stream (merge-pathnames "anomalies.log" logs-dir) :direction :output
                                    :if-exists :supersede :if-does-not-exist :create)
          (print-tracker-slot *current-tracker* 'anomalies log-stream))
        (in-package :dep)
        *current-tracker*))))