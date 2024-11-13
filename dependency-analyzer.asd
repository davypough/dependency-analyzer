;;;; Filename: dependency-analyzer.asd

;;; This is a tool for analyzing dependencies between files in Common Lisp ASDF systems.
;;; It parses ASDF system definitions and source files to track relationships between
;;; packages, files, and symbols, helping developers understand and manage their 
;;; project dependencies.

;;; The main system provides:
;;;   - File and system parsing
;;;   - Symbol definition and reference tracking
;;;   - Package dependency analysis
;;;   - Direct and indirect dependency detection
;;;   - Component relationship visualization


(asdf:defsystem #:dependency-analyzer
  :description "Analyzer for inter-file dependencies within ASDF systems"
  :author "Dave Brown <davypough@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:alexandria
               #:asdf
               #:cl-ppcre
               #:closer-mop
               #:trivia
               #:trivial-types
               #:fiveam
               #:yason        ; Added for JSON report generation
               #:local-time)  ; Added for report timestamps
  :serial t
  :components ((:module "src"
                 :components ((:file "src-packages")
                              (:file "conditions")
                              (:file "utils")
                              (:file "tracker")
                              (:file "analyzer")
                              (:file "system-parser")
                              (:file "file-parser")
                              (:file "report")))))