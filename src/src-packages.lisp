;;;; Filename: src-packages.lisp

;;; Package definitions for the dependency analyzer.
;;; Defines the main package and test package, including all exported symbols
;;; needed by users of the system.

(in-package #:cl-user)


(defpackage #:dependency-analyzer
  (:use #:cl #:alexandria #:trivia)
  (:nicknames #:dep)
  (:export #:analyze-system
           #:analyze-file
           #:generate-report
           ;; Classes
           #:dependency-tracker
           #:system-parser
           #:file-parser
           ;; Class Accessors
           #:tracker                    
           ;; Condition types
           #:analyzer-error
           #:system-parse-error
           #:file-parse-error
           ;; Parser operations
           #:parse-system
           #:parse-file
           #:create-system-parser
           ;; Structure types
           #:reference        
           #:definition      
           ;; Tracker operations
           #:record-definition
           #:record-reference
           #:record-package-use
           #:record-export
           #:record-macro-body-symbols
           #:get-macro-body-symbols
           #:lookup-macro-definition
           #:clear-tracker
           ;; Accessors
           #:get-definitions
           #:get-references
           #:get-file-definitions
           #:get-package-exports
           #:get-package-uses
           #:definition.type
           #:definition.file
           #:definition.package
           #:definition.exported-p
           #:definition.symbol
           #:reference.type
           #:reference.symbol   
           #:reference.file    
           #:reference.position
           #:reference.context
           #:reference.package
           ;; Dependencies
           #:file-dependencies
           #:file-dependents
           #:package-depends-on-p
           ;; Class accessors
           #:system.name
           ;; Report types
           #:text-report
           #:json-report
           #:graph-report
           ;; Cycle detection
           #:get-package-cycles
           #:get-system-cycles
           #:get-file-cycles))