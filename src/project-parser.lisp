;;;; Filename: project-parser.lisp
;;;
;;; Project-level dependency analysis functionality.
;;; Provides functionality for parsing ASDF project definitions, 
;;; resolving dependencies between components, and coordinating the 
;;; parsing of individual source files.


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
                       :error
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


(defmethod parse-component ((component t) parser)
  "Parse a single ASDF system component based on its type."
  (typecase component
    (asdf:cl-source-file
      (parse-source-file parser component))
    (asdf:module
      (mapc (lambda (c) (parse-component c parser))
            (asdf:component-children component)))
    (t
      (warn "Skipping unknown component type: ~A" component))))


(defmethod parse-source-file ((parser project-parser) source-file)
  "Parse a source file component using the file parser."
  (let ((file (asdf:component-pathname source-file)))
    (when (probe-file file)
      (let ((file-parser (make-instance 'file-parser :file file)))
        (parse-file file-parser)))))


(defun create-project-parser (project-name)
  "Create a project parser for the named ASDF system."
  (let ((system (asdf:find-system project-name)))
    (unless system 
      (return-from create-project-parser nil))
    (make-instance 'project-parser :project system)))


(defun analyze-project (project-name)
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


(defun record-system-dependencies (tracker system-name depends-on)
  "Record dependencies between systems defined in the project."
  (let ((deps (if (listp depends-on) 
                  depends-on 
                  (and depends-on (list depends-on)))))
    (setf (gethash (string system-name) (slot-value tracker 'subsystems))
          (mapcar #'string deps))))


(defun get-system-dependencies (tracker system-name)
  "Get list of systems that a system depends on."
  (gethash (string system-name) (slot-value tracker 'subsystems)))


(defun system-depends-on-p (tracker system1 system2)
  "Check if system1 depends on system2 (directly or indirectly)."
  (labels ((check-deps (sys visited)
             (when (member sys visited :test #'string=)
               (return-from check-deps nil))
             (let ((deps (get-system-dependencies tracker sys)))
               (or (member system2 deps :test #'string=)
                   (some (lambda (dep)
                          (check-deps dep (cons sys visited)))
                        deps)))))
    (check-deps (string system1) nil)))


(defun detect-system-cycles (tracker)
  "Detect cycles in system dependencies."
  (labels ((traverse (sys path)
             (when (member sys path :test #'string=)
               (let* ((cycle (cons sys (ldiff path (member sys path :test #'string=))))
                      (chain (format nil "~{~A~^ -> ~}" (reverse cycle))))
                 (record-anomaly tracker 
                               :system-cycle
                               :warning
                               sys
                               (format nil "System dependency cycle detected: ~A" chain)
                               cycle)))
             (dolist (dep (get-system-dependencies tracker sys))
               (traverse dep (cons sys path)))))
    (maphash (lambda (sys deps)
               (declare (ignore deps))
               (traverse sys nil))
             (slot-value tracker 'subsystems))))
