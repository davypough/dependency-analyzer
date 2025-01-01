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


(defun detect-unused-definitions (tracker)
  "Find definitions that are never referenced."
  (let ((used-defs (make-hash-table :test 'equal)))
    ;; Mark all referenced definitions
    (maphash (lambda (key refs)
               (declare (ignore key))
               (dolist (ref refs)
                 (dolist (def (reference.definitions ref))
                   (setf (gethash def used-defs) t))))
             (slot-value tracker 'references))
    
    ;; Find unused definitions
    (maphash (lambda (key defs)
               (declare (ignore key))
               (dolist (def defs)
                 (unless (or (gethash def used-defs)
                           (eq (definition.type def) :package))
                   (record-anomaly tracker
                    :name (definition.name def)
                    :type :unused-definition
                    :severity :warning 
                    :file (definition.file def)
                    :package (definition.package def)
                    :description (format nil "~A ~A is never referenced"
                                      (definition.type def)
                                      (definition.name def))))))
             (slot-value tracker 'definitions))))


(defun detect-orphaned-methods (tracker)
  "Find methods defined without corresponding generic functions."
  (maphash (lambda (key defs)
             (let ((name (first (split-sequence:split-sequence key "::"))))
               (dolist (def defs)
                 (when (eq (definition.type def) :method)
                   (let ((gf-key (make-tracking-key name 
                                                  (definition.package def)
                                                  :generic-function)))
                     (unless (gethash gf-key (slot-value tracker 'definitions))
                       (record-anomaly tracker
                         :name name
                         :type :specialized-method-without-generic
                         :severity :error
                         :file (definition.file def)
                         :package (definition.package def)
                         :description (format nil "Method defined for nonexistent generic function ~A"
                                           name))))))))
           (slot-value tracker 'definitions)))


(defun detect-redundant-package-uses (tracker)
  "Find packages that are used but none of their symbols are referenced."
  (maphash (lambda (using-pkg used-pkgs)
             (dolist (used used-pkgs)
               (unless (eq used "CL") ; Skip common-lisp package
                 (let ((used-symbols nil))
                   ;; Check all references from using-pkg
                   (maphash (lambda (key refs)
                             (declare (ignore key))
                             (dolist (ref refs)
                               (when (and (eq (reference.package ref) 
                                            (find-package using-pkg))
                                        (equal (package-name 
                                               (symbol-package (reference.name ref)))
                                              used))
                                 (setf used-symbols t))))
                           (slot-value tracker 'references))
                   (unless used-symbols
                     (record-anomaly tracker
                       :name using-pkg
                       :type :redundant-package-use  
                       :severity :warning
                       :file using-pkg
                       :package using-pkg
                       :description (format nil "Package ~A uses but never references symbols from ~A"
                                         using-pkg used)))))))
           (slot-value tracker 'package-uses)))


(defun detect-shadowed-definitions (tracker)
  "Find definitions that shadow inherited symbols from other packages."
  (maphash (lambda (key defs)
             (declare (ignore key))
             (dolist (def defs)
               (let* ((sym-name (definition.name def))
                     (pkg (definition.package def)))
                 (when (and pkg (symbolp sym-name))
                   (multiple-value-bind (inherited status)
                       (find-symbol (string sym-name) pkg)
                     (when (and inherited 
                               (eq status :inherited)
                               (not (eq (definition.type def) :package)))
                       (record-anomaly tracker
                         :name sym-name
                         :type :shadowed-definition
                         :severity :warning
                         :file (definition.file def)
                         :package pkg
                         :description (format nil "Definition of ~A shadows inherited symbol from ~A"
                                           sym-name 
                                           (package-name (symbol-package inherited))))))))))
           (slot-value tracker 'definitions)))


(defun detect-qualified-internal-references (tracker)
  "Find package-qualified references to internal symbols."
  (maphash (lambda (key refs)
             (declare (ignore key))
             (dolist (ref refs)
               (let ((sym (reference.name ref))
                     (pkg (reference.package ref)))
                 (when (and (symbolp sym) 
                          pkg
                          (not (eq pkg (symbol-package sym))) ; Different package = qualified
                          (eq (nth-value 1 (find-symbol (string sym) 
                                                      (symbol-package sym)))
                              :internal))
                   (record-anomaly tracker
                     :name sym
                     :type :qualified-internal-reference
                     :severity :warning
                     :file (reference.file ref)
                     :package pkg
                     :description (format nil "Package-qualified reference to internal symbol ~A:~A"
                                       (package-name (symbol-package sym))
                                       sym))))))
           (slot-value tracker 'references)))


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
