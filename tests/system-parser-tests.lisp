;;;; Filename: tests-system-parser.lisp

;;; Test suite for ASDF system parsing functionality.
;;; Tests cover system creation, component traversal, dependency tracking,
;;; and error handling.

(in-package #:dep/tests)


(in-suite system-parser-tests)


(defun create-test-system-file (path system-name files)
  "Create a temporary ASDF system file for testing"
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (format stream "(asdf:defsystem ~S~%" system-name)
    (format stream "  :components~%  ((:file ~S)~%" (first files))
    (dolist (file (rest files))
      (format stream "   (:file ~S :depends-on (~S))~%" 
              file (first files)))
    (format stream "))~%")))


(defun create-test-source-file (path &key defines calls)
  "Create a test Lisp source file with given definitions and calls"
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (format stream "(defpackage #:test-package~%  (:use #:cl))~%~%")
    (format stream "(in-package #:test-package)~%~%")
    (dolist (def defines)
      (format stream "(defun ~A ()~%  (declare (ignorable))~%  nil)~%~%" 
              def))
    (dolist (call calls)
      (format stream "(~A)~%~%" call))))


(test test-system-not-found
  "Test error handling for non-existent systems."
  (signals dep:system-parse-error
    (dep:create-system-parser 
     (make-instance 'dep:dependency-tracker :system-name "nonexistent")
     "nonexistent")
    "Should signal system-parse-error for nonexistent system~%System name: nonexistent"))


(test test-parse-simple-system
  "Test parsing a simple system with one file."
  (uiop:with-temporary-file (:pathname asd-path :type "asd")
    (uiop:with-temporary-file (:pathname src-path :type "lisp")
      (create-test-system-file asd-path "test-system" '("test"))
      (create-test-source-file src-path :defines '(foo bar) :calls '(baz))
      (let* ((tracker (make-instance 'dep:dependency-tracker 
                                   :system-name "test-system"))
             (parser (dep:create-system-parser tracker "test-system")))
        (dep:parse-system parser)
        (let ((defs (dep:get-file-definitions tracker src-path)))
          (is (= 2 (length defs))
              "Should find two definitions in source file~%Definitions found: ~{~%  ~A~}"
              defs)
          (is (find 'foo defs :key #'dep:definition.symbol)
              "Should find definition for foo~%Definitions: ~{~%  Symbol: ~A~%  Type: ~A~%  File: ~A~%~}"
              (mapcar (lambda (def)
                       (list (dep:definition.symbol def)
                             (dep:definition.type def)
                             (dep:definition.file def)))
                     defs))
          (is (find 'bar defs :key #'dep:definition.symbol)
              "Should find definition for bar~%Definitions: ~{~%  Symbol: ~A~%  Type: ~A~%  File: ~A~%~}"
              (mapcar (lambda (def)
                       (list (dep:definition.symbol def)
                             (dep:definition.type def)
                             (dep:definition.file def)))
                     defs)))
        (let ((refs (dep:get-references tracker 'baz)))
          (is (= 1 (length refs))
              "Should find one reference to baz~%References found: ~{~%  ~A~}"
              refs)
          (is (eq (dep:reference.type (first refs)) :call)
              "Reference should be of type :call~%Reference details:~%  Type: ~A~%  File: ~A~%  Symbol: ~A"
              (dep:reference.type (first refs))
              (dep:reference.file (first refs))
              (dep:reference.symbol (first refs))))))))


(test test-parse-system-with-dependencies
  "Test parsing a system with inter-file dependencies."
  (uiop:with-temporary-file (:pathname asd-path :type "asd")
    (uiop:with-temporary-file (:pathname src1-path :type "lisp")
      (uiop:with-temporary-file (:pathname src2-path :type "lisp")
        (create-test-system-file asd-path "test-system" 
                                '("base" "dependent"))
        (create-test-source-file src1-path :defines '(base-func))
        (create-test-source-file src2-path :calls '(base-func))
        (let* ((tracker (make-instance 'dep:dependency-tracker 
                                     :system-name "test-system"))
               (parser (dep:create-system-parser tracker "test-system")))
          (dep:parse-system parser)
          (is (member src1-path 
                     (dep:file-dependencies tracker src2-path) 
                     :test #'equal)
              "Dependent file should depend on base file~%Dependencies found: ~{~%  ~A~}"
              (dep:file-dependencies tracker src2-path))
          (is (member src2-path 
                     (dep:file-dependents tracker src1-path) 
                     :test #'equal)
              "Base file should have dependent file as dependent~%Dependents found: ~{~%  ~A~}"
              (dep:file-dependents tracker src1-path)))))))


(test test-parse-invalid-system
  "Test error handling for invalid system definitions."
  (uiop:with-temporary-file (:pathname asd-path :type "asd")
    (with-open-file (stream asd-path :direction :output)
      (format stream "(this is not valid lisp"))
    (signals dep:system-parse-error
      (let* ((tracker (make-instance 'dep:dependency-tracker 
                                   :system-name "test-system"))
             (parser (dep:create-system-parser tracker "test-system")))
        (dep:parse-system parser))
      "Should signal system-parse-error for invalid system definition~%System file: ~A"
      asd-path)))