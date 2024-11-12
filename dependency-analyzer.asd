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

;;; The test system provides comprehensive coverage of:
;;;   - Package-level dependencies (direct and inherited)
;;;   - File-level dependencies (explicit and implicit)
;;;   - Symbol-level tracking (definitions and references)
;;;   - System component relationships
;;;   - Cross-system dependencies

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
               #:fiveam)
  :serial t
  :components ((:module "src"
                 :components ((:file "src-packages")
                              (:file "conditions")
                              (:file "utils")
                              (:file "tracker")
                              (:file "analyzer")
                              (:file "system-parser")
                              (:file "file-parser")))))


(asdf:defsystem #:dependency-analyzer/tests
  :description "Test suite for dependency-analyzer"
  :author "Dave Brown <davypough@gmail.com>"
  :license "MIT"
  :depends-on (#:dependency-analyzer
               #:fiveam
               #:alexandria
               #:uiop)
  :serial t
  :components ((:module "tests"
                 :components ((:file "tests-packages")
                              (:file "tests-suite")
                              (:module "helpers"
                                :components ((:file "helpers-utils")
                                             (:file "helpers-data")
                                             (:file "helpers-fixtures")))
                              (:module "data"
                                :components ((:file "data-common")
                                             (:file "data-definitions")
                                             (:file "data-references")))
                              (:file "tracker-tests")    ))))
                              ;(:file "file-parser-tests")
                              ;(:file "system-parser-tests")
                              ;(:module "package-tests"
                              ;  :components ((:file "direct-package-tests")
                              ;               (:file "indirect-package-tests")))
                              ;(:module "file-tests"
                              ;  :components ((:file "direct-file-tests")
                              ;               (:file "indirect-file-tests")))
                              ;(:module "symbol-tests"
                              ;  :components ((:file "function-tests")
                              ;               (:file "macro-tests")))
                              ;(:module "system-tests"
                              ;  :components ((:file "component1-tests")
                              ;               (:file "component2-tests")
                              ;               (:file "intersystem1-tests")
                              ;               (:file "intersystem2-tests")
                              ;               (:file "intersystem3-tests")))))))