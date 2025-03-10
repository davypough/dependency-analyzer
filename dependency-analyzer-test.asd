;;;; Filename: dependency-analyzer-test.asd

;;; Test system for demonstrating dependency tracking.


(asdf:defsystem #:dependency-analyzer-test
  :description "Test data for dependency-analyzer"
  :version "0.1.0"
  :serial t  ;; Important for proper load order
  :depends-on (#:alexandria #:dependency-analyzer)
  :components ((:module "test"
                 :components ((:file "defs")
                              (:file "defs2")
                              (:file "refs")
                              (:file "anoms")
                              (:file "anoms2")
                              (:file "cycle-test")
                              (:file "cohesion-test")
                             ))))
