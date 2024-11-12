;;;; Filename: direct-package-tests.lisp

;;; Tests for direct package dependencies through :use and :import-from.

(in-package #:dep/tests)


(in-suite package-tests)


(def-fixture with-test-packages ()
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (pkg-def (create-temp-file 
                    "(in-package #:test-base-package)
                     (defun exported-function () :test)
                     (defmacro exported-macro (x) `(+ ,x 1))
                     (defvar exported-variable 42)"))
         (parser (make-instance 'dep:file-parser 
                              :tracker tracker 
                              :file pkg-def)))
    (unwind-protect
        (progn
          (dep:parse-file parser)
          (&body))
      (delete-temp-file pkg-def)
      (dep:clear-tracker tracker))))


(test test-package-use-dependencies
  "Test dependencies created through package :use clauses."
  (with-fixture with-test-packages ()
    (is (dep:package-depends-on-p tracker "TEST-MIDDLE-PACKAGE" "TEST-BASE-PACKAGE")
        "Package TEST-MIDDLE-PACKAGE should depend on TEST-BASE-PACKAGE~%Current package uses: ~{~%  ~A~}"
        (dep:get-package-uses tracker "TEST-MIDDLE-PACKAGE"))
    (let ((uses (dep:get-package-uses tracker "TEST-MIDDLE-PACKAGE")))
      (is (member "TEST-BASE-PACKAGE" uses :test #'string=)
          "Package uses list should include TEST-BASE-PACKAGE~%Actual package uses: ~{~%  ~A~}"
          uses))))


(test test-package-import-dependencies
  "Test dependencies created through :import-from clauses."
  (with-fixture with-test-packages ()
    (is (dep:package-depends-on-p tracker "TEST-END-PACKAGE" "TEST-BASE-PACKAGE")
        "Package TEST-END-PACKAGE should depend on TEST-BASE-PACKAGE~%Current package uses: ~{~%  ~A~}"
        (dep:get-package-uses tracker "TEST-END-PACKAGE"))
    (let ((refs (dep:get-references tracker 'test-base-package::exported-function)))
      (is (find :inherited (mapcar #'dep:reference.type refs))
          "Should record an :inherited reference for imported symbol~%Current references: ~{~%  Type: ~A, Package: ~A~%~}"
          (mapcar (lambda (ref) 
                   (list (dep:reference.type ref)
                         (dep:reference.package ref)))
                 refs)))))


(test test-package-export-consistency
  "Test that exported symbols are properly tracked in dependencies."
  (with-fixture with-test-packages ()
    (let ((exports (dep:get-package-exports tracker "TEST-BASE-PACKAGE")))
      (is (find 'test-base-package::exported-function exports)
          "Exported function should be in package exports~%Current exports: ~{~%  ~A~}"
          exports)
      (is (find 'test-base-package::exported-variable exports)
          "Exported variable should be in package exports~%Current exports: ~{~%  ~A~}"
          exports)
      (is (find 'test-base-package::exported-macro exports)
          "Exported macro should be in package exports~%Current exports: ~{~%  ~A~}"
          exports))))