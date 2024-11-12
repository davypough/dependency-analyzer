;;;; Filename: direct-file-tests.lisp

;;; Tests for direct file dependencies through function calls and symbol references.

(in-package #:dep/tests)


(in-suite file-tests)


(def-fixture with-test-files ()
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (file-a (create-temp-file
                   "(defpackage #:test-a (:use #:cl) (:export #:func-a #:var-a))
                    (in-package #:test-a)
                    (defvar var-a 42)
                    (defun func-a (x) (+ x var-a))"))
         (file-b (create-temp-file
                   "(defpackage #:test-b (:use #:cl #:test-a))
                    (in-package #:test-b)
                    (defun func-b () (func-a var-a))"))
         (parser-a (make-instance 'dep:file-parser
                                  :tracker tracker
                                  :file file-a))
         (parser-b (make-instance 'dep:file-parser
                                  :tracker tracker
                                  :file file-b)))
    (unwind-protect
        (progn
          (dep:parse-file parser-a)
          (dep:parse-file parser-b)
          (&body))
      (delete-temp-file file-a)
      (delete-temp-file file-b)
      (dep:clear-tracker tracker))))


(test test-function-call-dependencies
  "Test dependencies created through direct function calls."
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (file-a (create-temp-file
                   "(defpackage #:test-a 
                      (:use #:cl) 
                      (:export #:func-a #:var-a))
                    (in-package #:test-a)
                    (defvar var-a 42)
                    (defun func-a (x) (+ x var-a))"))
         (file-b (create-temp-file
                   "(defpackage #:test-b 
                      (:use #:cl #:test-a))
                    (in-package #:test-b)
                    (defun func-b () (func-a var-a))"))
         (parser-a (make-instance 'dep:file-parser
                                :tracker tracker
                                :file file-a))
         (parser-b (make-instance 'dep:file-parser
                                :tracker tracker
                                :file file-b)))
    (unwind-protect
        (progn
          ;; Parse both files
          (dep:parse-file parser-a)
          (dep:parse-file parser-b)
          
          ;; Test file dependencies
          (let* ((deps (dep:file-dependencies tracker file-b))
                 (func-def (dep:get-definitions tracker 'test-a::func-a))
                 (func-refs (dep:get-references tracker 'test-a::func-a)))
            ;; Test file dependency
            (is (member file-a deps :test #'equal)
                "File ~A should depend on ~A~%Dependencies found: ~{~%  ~A~}~%All definitions in file-a:~{~%  ~A~}"
                file-b file-a deps
                (mapcar (lambda (def)
                         (format nil "~A (~A in ~A)"
                                 (dep:definition.symbol def)
                                 (dep:definition.type def)
                                 (dep:definition.package def)))
                       (dep:get-file-definitions tracker file-a)))
            
            ;; Test function definition recording
            (is (not (null func-def))
                "Function definition for test-a::func-a should be recorded~%Current definition: ~A~%All definitions in file: ~{~%  ~A~}"
                func-def
                (mapcar (lambda (def)
                         (format nil "~A (~A in ~A)"
                                 (dep:definition.symbol def)
                                 (dep:definition.type def)
                                 (dep:definition.package def)))
                       (dep:get-file-definitions tracker file-a)))
            
            ;; Test function reference recording
            (is (find :call (mapcar #'dep:reference.type func-refs))
                "Function call should be recorded as a reference~%Current references: ~{~%  ~A~}"
                (mapcar (lambda (ref)
                         (format nil "~A (~A in ~A from ~A)"
                                 (dep:reference.symbol ref)
                                 (dep:reference.type ref)
                                 (dep:reference.package ref)
                                 (dep:reference.file ref)))
                       func-refs))))
      ;; Cleanup
      (progn
        ;; Delete temp files
        (delete-temp-file file-a)
        (delete-temp-file file-b)
        ;; Delete test packages
        (when (find-package "TEST-A")
          (delete-package "TEST-A"))
        (when (find-package "TEST-B")
          (delete-package "TEST-B"))
        (dep:clear-tracker tracker)))))


(test test-variable-reference-dependencies
  "Test dependencies created through special variable references."
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (file-a (create-temp-file
                   "(defpackage #:test-a 
                      (:use #:cl)
                      (:export #:func-a #:var-a))
                    (in-package #:test-a)
                    (defvar var-a 42 \"Test variable\")
                    (defun func-a (x) (+ x var-a))"))
         (file-b (create-temp-file
                   "(defpackage #:test-b
                      (:use #:cl #:test-a))
                    (in-package #:test-b)
                    (defun func-b () (list var-a (func-a var-a)))"))
         (parser-a (make-instance 'dep:file-parser
                                :tracker tracker
                                :file file-a))
         (parser-b (make-instance 'dep:file-parser
                                :tracker tracker
                                :file file-b)))
    (unwind-protect
        (progn
          ;; Parse both files
          (dep:parse-file parser-a)
          (dep:parse-file parser-b)
          ;; Test file dependencies
          (let* ((deps (dep:file-dependencies tracker file-b))
                 (var-def (dep:get-definitions tracker 'test-a::var-a))
                 (var-refs (dep:get-references tracker 'test-a::var-a)))
            ;; Test file dependency
            (is (member file-a deps :test #'equal)
                "File ~A should depend on ~A due to variable reference~%Dependencies found: ~{~%  ~A~}~%All definitions in file-a:~{~%  ~A~}"
                file-b file-a deps
                (mapcar (lambda (def)
                         (format nil "~A (~A in ~A)"
                                 (dep:definition.symbol def)
                                 (dep:definition.type def)
                                 (dep:definition.package def)))
                       (dep:get-file-definitions tracker file-a)))
            ;; Test variable definition recording
            (is (not (null var-def))
                "Variable definition for test-a::var-a should be recorded~%Current definition: ~A~%All definitions in file: ~{~%  ~A~}"
                var-def
                (mapcar (lambda (def)
                         (format nil "~A (~A in ~A)"
                                 (dep:definition.symbol def)
                                 (dep:definition.type def)
                                 (dep:definition.package def)))
                       (dep:get-file-definitions tracker file-a)))
            ;; Test variable reference recording
            (is (find :reference (mapcar #'dep:reference.type var-refs))
                "Variable reference should be recorded~%Current references: ~{~%  ~A~}~%Reference details:~{~%  ~A~}"
                var-refs
                (mapcar (lambda (ref)
                         (format nil "~A (~A in ~A from ~A)"
                                 (dep:reference.symbol ref)
                                 (dep:reference.type ref)
                                 (dep:reference.package ref)
                                 (dep:reference.file ref)))
                       var-refs))
            ;; Verify multiple reference types
            (let ((ref-types (mapcar #'dep:reference.type var-refs)))
              (is (> (length ref-types) 1)
                  "Should record multiple references to var-a~%Reference types found: ~{~A~^, ~}~%All references:~{~%  ~A~}"
                  ref-types
                  (mapcar (lambda (ref)
                           (format nil "~A (~A in ~A from ~A)"
                                   (dep:reference.symbol ref)
                                   (dep:reference.type ref)
                                   (dep:reference.package ref)
                                   (dep:reference.file ref)))
                         var-refs)))))
      ;; Cleanup
      (progn
        ;; Delete temp files
        (delete-temp-file file-a)
        (delete-temp-file file-b)
        ;; Delete test packages
        (when (find-package "TEST-A")
          (delete-package "TEST-A"))
        (when (find-package "TEST-B")
          (delete-package "TEST-B"))
        (dep:clear-tracker tracker)))))