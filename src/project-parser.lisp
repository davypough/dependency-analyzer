;;;; Filename: project-parser.lisp
;;;
;;; Project-level dependency analysis functionality.
;;; Provides functionality for parsing ASDF project definitions and 
;;; resolving dependencies between projects. The project parser handles
;;; complete ASDF projects including their subsystems and dependencies.


(in-package #:dep)


(defun record-dependency-cycle (parser project-name)
 "Record a project dependency cycle in the tracker.
  A cycle exists if we encounter a project that is already being parsed."
 (let ((position (member project-name (parsing-projects parser) :test #'string=)))
   (when position
     (let* ((cycle (cons project-name (ldiff (parsing-projects parser) position)))
            (chain (format nil "~{~A~^ -> ~}" cycle)))
       ;; Record the cycle and create an anomaly
       (record-project-cycle chain)
       (record-anomaly *current-tracker*
                      :project-cycle
                      :ERROR
                      project-name
                      (format nil "Project dependency cycle detected: ~A" chain)
                      cycle)))))


(defmethod parse-project ((parser project-parser))
 "Parse the project associated with the parser."
 (parse-project-for parser (project parser)))


(defun parse-project-for (parser project)
 "Parse the given project using the parser.
  Records any dependency cycles encountered during parsing."
 (let* ((project-name (asdf:component-name project))
        (components (asdf:component-children project)))
   ;; Check for cycle before proceeding
   (when (member project-name (parsing-projects parser) :test #'string=)
     (record-dependency-cycle parser project-name)
     (return-from parse-project-for nil))
   ;; Add project to parsing stack and proceed
   (push project-name (parsing-projects parser))
   (unwind-protect
       (progn
         ;; Parse dependent projects first
         (dolist (dep (asdf:system-depends-on project))
           (let* ((dep-name (if (listp dep) (second dep) dep))
                  (dep-sys (asdf:find-system dep-name nil)))
             (when dep-sys
               (parse-project-for parser dep-sys))))
         ;; Then parse this project's components
         (mapc (lambda (c) (parse-component c parser)) components))
     ;; Always remove project from parsing stack
     (pop (parsing-projects parser)))))


(defun create-project-parser (project-name)
 "Create a project parser for the named ASDF system."
 (let ((system (asdf:find-system project-name)))
   (unless system 
     (return-from create-project-parser nil))
   (make-instance 'project-parser :project system)))


(defun analyze-project (project-name &key (overwrite t))
 "Analyze an ASDF project and its associated subsystems."
 (let* ((project (asdf:find-system project-name))
        (asd-file (asdf:system-source-file project))
        (systems nil))
   (with-dependency-tracker ((make-instance 'dependency-tracker 
                                        :project-name (string project-name)
                                        :project-root (asdf:system-source-directory project)))
     ;; First parse the asd file for system dependencies
     (with-open-file (stream asd-file :direction :input)
       (loop for form = (read stream nil nil)
             while form
             when (and (listp form)
                      (eq (first form) 'asdf:defsystem)
                      (stringp (second form)))
             do (let* ((sys-name (second form))
                      (sys (asdf:find-system sys-name))
                      (depends-on (getf (cddr form) :depends-on)))
                  (when sys 
                    (push sys systems)
                    (record-system-dependencies *current-tracker* sys-name depends-on)))))
     ;; Then parse all source files
     (let ((source-files (find-project-files project-name)))
       (dolist (file source-files)
         (when (probe-file file)
           (let ((file-parser (make-instance 'file-parser :file file)))
             (parse-file file-parser)))))
     *current-tracker*)))