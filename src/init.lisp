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
    #:defs))


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
    (dolist (def (sort defs #'string< :key #'definition.designator))
      (format t "~&Designator: ~S~%  Type: ~S~%  File: ~A~%  Package: ~S~%  Exported: ~A~%  Spec-Symbols: ~S~%~%"
              (etypecase (definition.designator def)
                (string (definition.designator def))
                (symbol (symbol-name (definition.designator def))))
              (definition.type def)
              (project-pathname (definition.file def))
              (definition.package def)
              (definition.exported-p def)
              (definition.spec-symbols def)))))


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
