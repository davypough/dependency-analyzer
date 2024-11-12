;;;; Filename: indirect-package-tests.lisp

;;; Tests for indirect package dependencies through inheritance and symbol usage.

(in-package #:dep/tests)


(in-suite package-tests)


(def-fixture with-package-chain ()
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (pkg-def (create-temp-file
                    "(in-package #:test-base-package)
                     (defun base-function () :base)
                     (defvar base-variable :base-var)
                     (in-package #:test-middle-package)
                     (defun middle-function () (base-function))
                     (in-package #:test-end-package)
                     (defun end-function () (middle-function))"))
         (parser (make-instance 'dep:file-parser
                              :tracker tracker
                              :file pkg-def)))
    (unwind-protect
        (progn
          (dep:parse-file parser)
          (&body))
      (delete-temp-file pkg-def)
      (dep:clear-tracker tracker))))


(test test-inherited-symbol-dependencies
  "Test dependencies created through inherited symbols."
  (with-fixture with-package-chain ()
    (let ((middle-exports (dep:get-package-exports tracker "TEST-MIDDLE-PACKAGE")))
      (is (find 'test-base-package::base-function middle-exports :test #'eq)
          "Middle package should expose inherited base function~%Current middle package exports: ~{~%  ~A~}"
          middle-exports))
    (let ((refs (dep:get-references tracker 'test-base-package::base-function)))
      (is (find :inherited (mapcar #'dep:reference.type refs) :test #'eq)
          "Should record inherited symbol references~%Current references: ~{~%  Type: ~A~%  From package: ~A~%  File: ~A~%~}"
          (mapcar (lambda (ref)
                   (list (dep:reference.type ref)
                         (dep:reference.package ref)
                         (dep:reference.file ref)))
                 refs)))))


(test test-transitive-package-dependencies
  "Test detection of package dependencies through intermediate packages."
  (with-fixture with-package-chain ()
    (let ((base-uses (dep:get-package-uses tracker "TEST-BASE-PACKAGE"))
          (middle-uses (dep:get-package-uses tracker "TEST-MIDDLE-PACKAGE"))
          (end-uses (dep:get-package-uses tracker "TEST-END-PACKAGE")))
      (is (dep:package-depends-on-p tracker "TEST-END-PACKAGE" "TEST-BASE-PACKAGE")
          "End package should depend on base package through middle~%Package dependency chain:~%  TEST-END-PACKAGE uses: ~{~A~^, ~}~%  TEST-MIDDLE-PACKAGE uses: ~{~A~^, ~}~%  TEST-BASE-PACKAGE uses: ~{~A~^, ~}"
          end-uses middle-uses base-uses))
    (is (dep:package-depends-on-p tracker "TEST-END-PACKAGE" "TEST-MIDDLE-PACKAGE")
        "End package should directly depend on middle package~%Current dependencies for TEST-END-PACKAGE: ~{~%  ~A~}"
        (dep:get-package-uses tracker "TEST-END-PACKAGE"))))


(test test-package-dependency-cycle-detection
  "Test detection of circular dependencies between packages."
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (cycle-def (create-temp-file
                     "(in-package #:test-pkg-a)
                      (defun func-a () (test-pkg-b:func-b))
                      (in-package #:test-pkg-b)
                      (defun func-b () (test-pkg-a:func-a))"))
         (parser (make-instance 'dep:file-parser
                              :tracker tracker
                              :file cycle-def)))
    (unwind-protect
        (progn
          (dep:parse-file parser)
          ;; Verify direct dependencies with detailed output
          (let ((a-deps (dep:get-package-uses tracker "TEST-PKG-A"))
                (b-deps (dep:get-package-uses tracker "TEST-PKG-B")))
            (is (member "TEST-PKG-B" a-deps :test #'string=)
                "Package A should record dependency on B~%Package A dependencies: ~{~%  ~A~}"
                a-deps)
            (is (member "TEST-PKG-A" b-deps :test #'string=)
                "Package B should record dependency on A~%Package B dependencies: ~{~%  ~A~}"
                b-deps))
          ;; Verify cycle detection with detailed output
          (let* ((cycles (dep:get-package-cycles tracker))
                 (expected-cycle "TEST-PKG-A -> TEST-PKG-B -> TEST-PKG-A"))
            (is (= 1 (length cycles))
                "Should detect exactly one cycle~%Cycles found (~D): ~{~%  ~A~}"
                (length cycles) cycles)
            (is (find expected-cycle cycles :test #'string=)
                "Should detect the correct package dependency cycle~%Expected: ~A~%Found cycles: ~{~%  ~A~}"
                expected-cycle cycles)))
      (delete-temp-file cycle-def)
      (dep:clear-tracker tracker))))