(asdf:defsystem #:dependency-analyzer
 :description "Analyzer for inter-file dependencies within ASDF projects"
 :author "Dave Brown <davypough@gmail.com>"
 :license "MIT"
 :version "0.1.0"
 :depends-on (#:alexandria
              #:uiop
              #:asdf
              #:closer-mop 
              #:yason
              #:local-time
              #:split-sequence)
 :serial t
 :components ((:module "src"
                :components ((:file "init")
                             (:file "globals")
                             (:file "classes")
                             (:file "utils")
                             (:file "main")
                             (:file "definition-analysis")
                             (:file "reference-analysis")
                             (:file "method-reference-analysis")
                             (:file "package-symbol-analysis")
                             (:file "code-quality-analysis")
                             (:file "report-utils")
                             (:file "report")))))
