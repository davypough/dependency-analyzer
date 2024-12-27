;;;; Filename: tracker-utils.lisp
;;;
;;; Utility functions for dependency tracking functionality.
;;; Contains helper functions for working with dependency tracking,
;;; including key generation, tracker management, and common operations.


(in-package #:dep)


(defmacro with-dependency-tracker ((&optional (tracker-form '(make-instance 'dependency-tracker :project-name "test-project"))) &body body)
  "Execute BODY with *CURRENT-TRACKER* bound to the result of TRACKER-FORM.
   If TRACKER-FORM is not provided, creates a new tracker instance."
  `(let ((tracker ,tracker-form))
     (setf *current-tracker* tracker)
     (let ((*current-tracker* tracker))
       ,@body)))


(defun ensure-tracker (&optional tracker)
  "Return TRACKER if provided, otherwise return *CURRENT-TRACKER*.
   Signals an error if no tracker is available."
  (or tracker *current-tracker*
      (error "No tracker is currently bound. Please use 'with-dependency-tracker' to bind one.")))


(defun make-tracking-key (designator &optional package type qualifiers specializers)
  "Create a lookup key for a symbol or package name, with optional type/method info.
   For methods and generic functions, always includes qualifiers and specializers parts.
   DESIGNATOR can be string, symbol, or (setf name) form.
   PACKAGE is package name string (optional).
   TYPE is one of +valid-definition-types+ (optional).
   QUALIFIERS is a list of method qualifiers (optional).
   SPECIALIZERS is a list of specializer type names (optional).
   Returns key like: name::package::type[::()::()].
   Examples:
     PROCESS-DATA::USER-MAIN-PACKAGE::METHOD::()::(STRING)
     PROCESS-DATA::USER-MAIN-PACKAGE::METHOD::(AFTER)::(STRING)
     PROCESS-DATA::USER-MAIN-PACKAGE::GENERIC-FUNCTION::()::()"
  (let* ((name (etypecase designator
                (string designator)
                (symbol (string designator))
                (cons (if (and (eq (car designator) 'setf) (= (length designator) 2))
                         (format nil "(SETF ~A)" (string (cadr designator)))
                         (error "Invalid designator form ~S in make-tracking-key~%  package: ~S~%  type: ~S" 
                               designator package type)))))
         (pkg (when package (or (package-name package) (string package)))))
    
    (when (and type (not (member type +valid-definition-types+)))
      (error "Invalid definition type in make-tracking-key~%  designator: ~S~%  package: ~S~%  type: ~S"
             designator package type))
    
    (let ((key (format nil "~A::~A::~A" 
                      name
                      (or pkg "") 
                      (or type ""))))
      
      (when (or (member type '(:METHOD :GENERIC-FUNCTION))
                qualifiers 
                specializers)
        (let* ((sorted-quals (sort (copy-list (or qualifiers nil)) #'string<)))
          (setf key (format nil "~A::(~{~A~^ ~})" key sorted-quals)))
        
        (let* ((sorted-specs (sort (copy-list (or specializers nil)) #'string<)))
          (setf key (format nil "~A::(~{~A~^ ~})" key sorted-specs))))
      
      key)))


(defun project-pathname (pathname)
  "Convert a pathname to a string representation relative to project root.
   Returns a path starting with / that is relative to the project root.
   E.g., /source/file.lisp instead of /path/to/project/source/file.lisp"
  (when pathname
    (let* ((project-root (project-root *current-tracker*))
           (namestring (namestring pathname)))
      (if project-root
          (let ((relative (enough-namestring pathname project-root)))
            (if (char= (char relative 0) #\/)
                relative
                (concatenate 'string "/" relative)))
          namestring))))


(defun log-definitions ()
  (declare (special log-stream))
  (format log-stream "Filename: DEFINITIONS.LOG")
  (format log-stream "~2%The list of all definitions identified in the ~A project.~2%"
                 (slot-value *current-tracker* 'project-name))
  (let ((def-ht (slot-value *current-tracker* 'definitions)))
    (maphash (lambda (key val)
               (declare (ignore key))
               (dolist (def val)
                 (print-definition def log-stream)
                 (format log-stream "~%")))
             def-ht)))


(defun log-references ()
  (declare (special log-stream))
  (format log-stream "Filename: REFERENCES.LOG")
  (format log-stream "~2%The list of all references to definitions in other files for the ~A project.~%"
                 (slot-value *current-tracker* 'project-name))
  (let ((ref-ht (slot-value *current-tracker* 'references)))
    (maphash (lambda (key ref-list)
               (declare (ignore key))
               (dolist (ref (sort ref-list #'string< 
                                 :key (lambda (r)
                                       (format nil "~A:~A"
                                               (reference.file r)
                                               (reference.name r)))))
                 (terpri log-stream)
                 (print-reference ref log-stream)))
             ref-ht)))
    

(defun log-anomalies ()
  "Log all anomalies grouped by type to the specified stream."
  (declare (special log-stream))
  (format log-stream "Filename: ANOMALIES.LOG")
  (format log-stream "~2%The list of all anomalies detected during dependency analysis of the ~A project.~2%"
                 (slot-value *current-tracker* 'project-name))
  (let ((anomaly-types nil))
    ;; Collect all anomaly types
    (maphash (lambda (type anomaly-list)
               (declare (ignore anomaly-list))
               (push type anomaly-types))
             (anomalies *current-tracker*))
    ;; Process each type in sorted order
    (dolist (type (sort anomaly-types #'string< :key #'symbol-name))
      (let ((anomalies-of-type (gethash type (anomalies *current-tracker*))))
        (when anomalies-of-type
          ;(format stream "~&~A Anomalies:" (string-upcase (symbol-name type)))
          (dolist (anomaly (sort anomalies-of-type #'string< 
                                :key #'anomaly.description))
            (print-anomaly anomaly log-stream 0))
          (terpri log-stream))))))
