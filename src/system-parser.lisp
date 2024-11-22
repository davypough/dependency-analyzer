;;;; Filename: system-parser.lisp
;;;
;;; System-level dependency analysis functionality.
;;; Provides functionality for analyzing ASDF system definitions and
;;; parsing arbitrary directories of Lisp files. This is distinct from
;;; project analysis as it can handle standalone systems and directories
;;; outside of a formal ASDF project.


(in-package #:dep)


(defun record-dependency-cycle (parser system-name)
 "Record a system dependency cycle in the tracker."
 (let ((position (member system-name (parsing-systems parser) :test #'string=)))
   (when position
     (let* ((cycle (cons system-name (ldiff (parsing-systems parser) position)))
            (chain (format nil "~{~A~^ -> ~}" (reverse cycle))))
       ;; Record the cycle in the dependency tracker
       (record-system-cycle chain)))))


(defmethod parse-system ((parser system-parser))
 "Parse the system associated with the parser."
 (parse-system-for parser (system parser)))


(defun parse-system-for (parser system)
 "Parse the given system using the parser."
 (let* ((system-name (asdf:component-name system))
        (components (asdf:component-children system)))
   ;; Check for cycle before proceeding
   (when (member system-name (parsing-systems parser) :test #'string=)
     (record-dependency-cycle parser system-name)
     (return-from parse-system-for nil))
   ;; Add system to parsing stack and proceed
   (push system-name (parsing-systems parser))
   (unwind-protect
       (progn
         ;; Parse dependent systems first, reusing the same parser instance
         (dolist (dep (asdf:system-depends-on system))
           (let* ((dep-name (if (listp dep) (second dep) dep))
                  (dep-sys (asdf:find-system dep-name nil)))
             (when dep-sys
               (parse-system-for parser dep-sys))))
         ;; Then parse this system's components
         (mapc (lambda (c) (parse-component c parser)) components))
     ;; Always remove system from parsing stack
     (pop (parsing-systems parser)))))


(defun create-system-parser (system-name)
 "Create a system parser for the named ASDF system."
 (let ((system (asdf:find-system system-name)))
   (unless system
     (error 'system-parse-error 
            :system-name system-name
            :reason "System not found"))
   (make-instance 'system-parser :system system)))


(defun analyze-directory (directory &key (filespecs '("*.lisp")))
  "Analyze source files in a directory structure and its subdirectories in two passes:
   1. Collect all definitions across all files
   2. Analyze references to those definitions"
  (with-dependency-tracker ((make-instance 'dependency-tracker 
                                         :project-name (format nil "DIR:~A" directory)
                                         :project-root (pathname directory)))
    (let ((files (collect-directory-files directory filespecs)))
      ;; First pass: collect all definitions
      (dolist (file files)
        (let ((file-parser (make-instance 'file-parser :file file)))
          (parse-definitions-in-file file-parser)))
      
      ;; Second pass: analyze references
      (dolist (file files)
        (let ((file-parser (make-instance 'file-parser :file file)))
          (parse-references-in-file file-parser)))
      
      *current-tracker*)))