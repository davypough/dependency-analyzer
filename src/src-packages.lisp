;;;; Filename: src-packages.lisp
;;;
;;; Package definitions for the dependency analyzer.
;;; Defines the main package with the minimal public API needed by users.

(in-package #:cl-user)


(defpackage #:dependency-analyzer
  (:use #:cl #:alexandria)
  (:nicknames #:dep)
  (:export 
   ;; Main analysis functions
   #:analyze-project
   #:analyze-directory
   
   ;; Report generation
   #:report  
   #:generate-report
   
   ;; Error conditions
   #:analyzer-error
   #:project-parse-error
   #:file-parse-error
   
   ;; Cycle detection
   #:get-package-cycles
   #:get-project-cycles  
   #:get-file-cycles))
