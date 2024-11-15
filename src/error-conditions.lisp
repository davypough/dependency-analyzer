;;;; Filename: error-conditions.lisp

;;; Condition system for the dependency analyzer.
;;; Defines a hierarchy of error conditions for handling various failure modes
;;; during project and file parsing operations.

(in-package #:dep)


(define-condition analyzer-error (error)
  ()
  (:documentation
   "Base condition for all dependency analyzer errors.
    All specific error types inherit from this condition."))


(define-condition project-parse-error (analyzer-error)
  ((project-name 
    :initarg :project-name 
    :reader project.name
    :documentation "Name of the ASDF project that failed to parse")
   (reason 
    :initarg :reason 
    :reader reason
    :documentation "Underlying cause of the parse failure"))
  (:documentation
   "Signaled when an ASDF project definition cannot be parsed.")
  (:report (lambda (condition stream)
             (format stream "Error parsing project ~A: ~A" 
                     (project.name condition) 
                     (reason condition)))))


(define-condition file-parse-error (analyzer-error)
  ((file 
    :initarg :file 
    :reader file-name
    :documentation "Path of the file that failed to parse")
   (reason 
    :initarg :reason 
    :reader reason
    :documentation "Underlying cause of the parse failure"))
  (:documentation
   "Signaled when a Lisp source file cannot be parsed.")
  (:report (lambda (condition stream)
             (format stream "Error parsing file ~A: ~A"
                     (file-name condition)
                     (reason condition)))))