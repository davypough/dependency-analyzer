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
   DESIGNATOR can be either a symbol, a string, or a (setf name) form.
   PACKAGE is the package name string (optional).
   TYPE is one of +valid-definition-types+ (optional).
   QUALIFIERS is a list of method qualifiers (optional).
   SPECIALIZERS is a list of specializer type names (optional).
   Returns a key string like name::package::type[::qualifiers][::specializers]."
  (let* ((name (etypecase designator
                (string designator)
                (symbol (string designator))
                (cons (if (and (eq (car designator) 'setf) (= (length designator) 2))
                         (format nil "(SETF ~A)" (string (cadr designator)))
                         (error "Invalid designator form ~S in make-tracking-key~%  package: ~S~%  type: ~S" 
                               designator package type)))))
         (pkg (when package (string package))))
    ;; Validate qualifiers/specializers are proper lists when provided
    (when (and qualifiers (not (listp qualifiers)))
      (error "Qualifiers must be a proper list in make-tracking-key~%  designator: ~S~%  package: ~S~%  type: ~S~%  qualifiers: ~S" 
             designator package type qualifiers))
    (when (and specializers (not (listp specializers)))
      (error "Specializers must be a proper list in make-tracking-key~%  designator: ~S~%  package: ~S~%  type: ~S~%  specializers: ~S"
             designator package type specializers))
    ;; Type validation against known types
    (when (and type (not (member type +valid-definition-types+)))
      (error "Invalid definition type in make-tracking-key~%  designator: ~S~%  package: ~S~%  type: ~S"
             designator package type))
    ;; Base key includes name, package, type  
    (let ((key (format nil "~A::~A::~A" 
                      name
                      (or pkg "") 
                      (or type ""))))
      ;; Append sorted qualifiers if provided
      (when qualifiers
        (setf key (format nil "~A::~{~A~^-~}" 
                         key (sort (copy-list qualifiers) #'string<))))
      ;; Append sorted specializers if provided  
      (when specializers
        (setf key (format nil "~A::(~{~A~^ ~})"
                         key (sort (copy-list specializers) #'string<))))
      key)))


(defun log-definitions (stream)
  (format stream "Filename: DEFINITIONS.LOG")
  (format stream "~2%The list of all definitions identified in the ~A project."
                 (slot-value *current-tracker* 'project-name))
  (let ((def-ht (slot-value *current-tracker* 'definitions))
        (defs nil))
    (maphash (lambda (key val)
               (declare (ignore key))
               (setf defs (union val defs :test #'equalp)))
             def-ht)
    (dolist (def (sort defs #'string< :key #'definition.name))
      (format stream "~2%~S" def))))


(defun log-references (stream)
  (format stream "Filename: REFERENCES.LOG")
  (format stream "~2%The list of all references to definitions in other files for the ~A project."
                 (slot-value *current-tracker* 'project-name))
  (let ((def-ht (slot-value *current-tracker* 'definitions))
        (defs nil))
    (maphash (lambda (key val)
               (declare (ignore key))
               (push (first val) defs))
             def-ht)
    (dolist (def (sort defs #'string< :key #'reference.name))
      (format stream "~2%~S" def))))
    

(defun log-anomalies (stream)
  (format stream "Filename: ANOMALIES.LOG")
  (format stream "~2%The list of all anomalies detected during dependency analysis of the ~A project."
                 (slot-value *current-tracker* 'project-name)))


(defun print-tracker-slot (tracker slot-name stream)
  "Print contents of tracker slot to stream with headers and indentation.
   TRACKER is a dependency-tracker instance
   SLOT-NAME is one of 'definitions, 'references, or 'anomalies
   STREAM is where to write the output"
  (let ((table (slot-value tracker slot-name)))
    ;; Print slot header
    (format stream "~&~A.LOG~%" (string-upcase slot-name))
    (format stream "~V,,,'-<~>~2%" 30)
    (case slot-name
      (definitions
       ;; Sort and print definitions directly
       (let ((defs nil))
         (maphash (lambda (key def-list)
                    (declare (ignore key))
                    (dolist (def def-list)
                      (push def defs)))
                  table)
         (dolist (def (sort defs #'string< :key #'definition.name))
           (format stream "~A~2%" def))))

      (references
       ;; Group references by symbol (and package), but print no sub-headers.
       (let ((keys nil))
         (maphash (lambda (key refs)
                    (declare (ignore refs))
                    (push key keys))
                  table)
         (dolist (key (sort keys #'string<))
           (let ((refs (gethash key table)))
             (cond
               ((null refs)
                ;; No references, do nothing.
                )
               ((null (cdr refs))
                ;; Only one reference, print it directly.
                (format stream "~A~2%" (car refs)))
               (t
                ;; Multiple references, print them in a grouped list.
                (format stream "(~%")
                (dolist (ref refs)
                  (format stream "  ~A~%" ref))
                (format stream ")~%")))))))

      (anomalies
       ;; Group anomalies by type
       (let ((types nil))
         (maphash (lambda (type anomaly-list)
                    (declare (ignore anomaly-list))
                    (push type types))
                  table)
         (dolist (type (sort types #'string<))
           (format stream "~&~A ANOMALIES~%"
                   (string-upcase (symbol-name type)))
           (dolist (anomaly (sort (gethash type table)
                                  #'string< :key #'anomaly.description))
             (format stream "  ~A~%" anomaly))
           (format stream "~%")))))))
