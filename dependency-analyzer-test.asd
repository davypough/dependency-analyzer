;;;; Filename: dependency-analyzer-test.asd

;;; Test system for demonstrating dependency tracking.


(asdf:defsystem #:dependency-analyzer-test
  :description "Test data for dependency-analyzer"
  :version "0.1.0"
  :serial t
  :depends-on (#:alexandria #:dependency-analyzer)
  :components ((:module "test"
                 :components ((:file "defs")
                              (:file "refs")))))