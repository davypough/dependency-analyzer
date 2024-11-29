(asdf:defsystem #:dependency-analyzer
  :description "Analyzer for inter-file dependencies within ASDF projects"
  :author "Dave Brown <davypough@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:alexandria
               #:uiop
               #:asdf
               #:yason        ;JSON report generation
               #:local-time)  ;report timestamps
  :serial t
  :components ((:module "src"
                 :components ((:file "init")
                              (:file "structures&classes")
                              (:file "tracker-utils")
                              (:file "tracker")
                              ;(:file "system-parser-utils")
                              ;(:file "system-parser")
                              ;(:file "project-parser-utils")
                              ;(:file "project-parser")
                              (:file "file-parser-utils")
                              (:file "file-parser")
                              (:file "report-utils")
                              (:file "report")))))
