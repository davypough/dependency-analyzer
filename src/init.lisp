;;;; Filename: init.lisp
;;;
;;; Initialization for the dependency analyzer.
;;; Defines the main package with the minimal public API needed by users.
;;; Includes basic utilities.


(in-package #:cl-user)


(defpackage #:dependency-analyzer
  (:use #:cl #:alexandria)
  (:nicknames #:dep)
  (:export 
    ;; Main analysis function
    #:analyze
    ;; Report generation
    #:report  
    #:generate-report
    ;; Cycle detection
    #:get-package-cycles
    #:get-project-cycles  
    #:get-file-cycles
    ;; Development
    #:prt
    #:defs
    #:refs
    #:anoms))


(in-package :dep)


(defparameter *current-tracker* nil
  "The currently active dependency tracker instance.")


;;; Development utilities


(defmacro prt (&rest forms)
  "Print the names & values of given forms (ie,variables or accessors).
   Can wrap around an expression, returning its value."
  `(progn ,@(loop for form in forms
              collect `(format t "~&  ~S => ~S~%" ',form ,form))
          ,@(last `,forms)))


(defun defs ()
  "Pretty print all definition records from current tracker in symbol name order."
  (let ((defs nil))
    (maphash (lambda (key def)
               (declare (ignore key))
               (push def defs))
             (slot-value *current-tracker* 'definitions))
    (dolist (def (sort defs #'string< :key #'definition.name))
      (format t "~&Name: ~S~%  Type: ~S~%  File: ~A~%  Package: ~S~%  Exported: ~A~%  Spec-Symbols: ~S~%~%"
              (etypecase (definition.name def)
                (string (definition.name def))
                (symbol (symbol-name (definition.name def))))
              (definition.type def)
              (project-pathname (definition.file def))
              (definition.package def)
              (definition.exported-p def)
              (definition.spec-symbols def)))))


(defun refs ()
  "Pretty print all reference records from current tracker in symbol name order."
  (let ((refs nil))
    (maphash (lambda (key ref-list)
               (declare (ignore key))
               (dolist (ref ref-list)
                 (push ref refs)))  
             (slot-value *current-tracker* 'references))
    (dolist (ref (sort refs #'string< :key #'reference.symbol))
      (format t "~&Symbol: ~S~%  Type: ~A~%  File: ~A~%  Package: ~S~%  Visibility: ~A~%  Definition: ~S~%~%"
              (reference.symbol ref)
              (reference.type ref) 
              (project-pathname (reference.file ref))
              (reference.package ref)
              (reference.visibility ref)
              (when (reference.definition ref)
                (definition.name (reference.definition ref)))))))


(defun anoms ()
  "Pretty print all anomaly records from current tracker in type order."
  (let ((anomaly-list nil))
    (maphash (lambda (type anomalies)
               (declare (ignore type))
               (dolist (anomaly anomalies)
                 (push anomaly anomaly-list)))
             (anomalies *current-tracker*))
    (dolist (anomaly (sort anomaly-list #'string< :key #'anomaly.type))
      (format t "~&Type: ~S~%  Severity: ~A~%  Location: ~A~%  Description: ~A~%  Context: ~S~%~%"
              (anomaly.type anomaly)
              (anomaly.severity anomaly)
              (project-pathname (anomaly.location anomaly))
              (anomaly.description anomaly)
              (anomaly.context anomaly)))))


(defgeneric show (object &rest rest)
  (:documentation "Displays an object in a user-friendly format."))


(defmethod show ((table hash-table) &key (sort-by 'key))
  "Displays a hash table line-by-line, sorted either by key or val."
  (declare (type hash-table table))
  (when (= (hash-table-count table) 0)
    (format t "~&~A Empty~%" table)
    (return-from show))
  (let (alist)
    (maphash
      (lambda (key val)
        (push (cons (princ-to-string key) (prin1-to-string val))
          alist))
      table)
    (setf alist 
      (sort (copy-list alist) #'string< :key (ecase sort-by (key #'car) (val #'cdr))))
    (loop for (key . val) in alist
      do (format t "~&~A ->~10T ~A~%" key val)))
  t)


(defmethod show ((fn function) &rest rest)
  (declare (ignore rest))
  (format t "~&~A~%" (function-lambda-expression fn))
  t)


(defmethod show ((lst list) &rest rest)
  (declare (ignore rest))
  (format t "~&~A~%" lst))


(defmethod show ((object t) &rest rest)
  "Prints any basic lisp object."
  (declare (ignore rest))
  (format t "~&~S~%" object)
  t)
