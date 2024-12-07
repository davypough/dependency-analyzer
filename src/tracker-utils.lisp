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


(defun make-tracking-key (designator &optional package)
  "Create a lookup key for a symbol or package name, optionally in a specific package context.
   DESIGNATOR can be either a symbol or a string.
   For symbols: Creates a key based on the symbol name and optional package
   For strings: Uses the string directly as a name
   Returns a string key that uniquely identifies the entity."
  (let ((name (etypecase designator
                (string designator)
                (symbol (symbol-name designator)))))
    (if package
        (format nil "~A::~A" package name)
        name)))


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
         (maphash (lambda (key def)
                    (declare (ignore key))
                    (push def defs))
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
             ;; Sort references by the symbol name for consistency
             (setf refs (sort refs
                              (lambda (a b)
                                (string< (symbol-name (reference.symbol a))
                                         (symbol-name (reference.symbol b))))))

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



#+ignore (defun print-tracker-slot (tracker slot-name stream)
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
         (maphash (lambda (key def)
                   (declare (ignore key))
                   (push def defs))
                 table)
         (dolist (def (sort defs #'string< :key #'definition.name))
           (format stream "~A~2%" def))))
      (references
       ;; Group references by symbol and package
       (let ((keys nil))
         (maphash (lambda (key refs)
                    (declare (ignore refs))
                    (push key keys))
                  table)
         (dolist (key (sort keys #'string<))
           (let* ((pos (search "::" key))
                  (header (if pos
                            (format nil "REFERENCES TO SYMBOL: ~A IN PACKAGE: ~A" 
                                    (subseq key (+ 2 pos))  ; symbol after ::
                                    (subseq key 0 pos))     ; package before ::
                            (format nil "REFERENCES TO SYMBOL: ~A" key))))
             (format stream "~&~A~%" header)
             (dolist (ref (sort (gethash key table) #'string< :key #'reference.symbol))
               (format stream "  ~A~%" ref))
             (format stream "~%")))))
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