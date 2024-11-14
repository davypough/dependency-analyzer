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
                           (:file "base-definitions")
                           (:file "tracker-utils")
                           (:file "tracker")
                           (:file "system-parser")
                           (:file "file-parser-utils")
                           (:file "file-parser")
                           (:file "report")))))