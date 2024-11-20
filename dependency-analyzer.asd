(asdf:defsystem #:dependency-analyzer
  :description "Analyzer for inter-file dependencies within ASDF projects"
  :author "Dave Brown <davypough@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:alexandria
               #:asdf
               #:yason        ;JSON report generation
               #:local-time)  ;report timestamps
  :serial t
  :components ((:module "src"
                 :components ((:file "src-packages")
                              (:file "base-definitions")
                              (:file "tracker-utils")
                              (:file "tracker")
                              (:file "project-parser-utils")
                              (:file "project-parser")
                              (:file "file-parser-utils")
                              (:file "file-parser")
                              (:file "report-utils")
                              (:file "report")))))
