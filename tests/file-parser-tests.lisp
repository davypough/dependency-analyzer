;;;; Filename: tests-file-parser.lisp

;;; Test suite for file parsing functionality.
;;; Tests cover package definition parsing, symbol tracking, reference resolution,
;;; and dependency analysis between files.

(in-package #:dep/tests)


(in-suite file-parser-tests)


(defvar *test-definitions-path*
  (asdf:system-relative-pathname :dependency-analyzer
                                "tests/data/data-definitions.lisp"))

(defvar *test-references-path*
  (asdf:system-relative-pathname :dependency-analyzer
                                "tests/data/data-references.lisp"))


(defun create-test-parser (file)
  "Helper function to create a parser instance for testing."
  (let ((tracker (make-instance 'dep:dependency-tracker :system-name "test")))
    (make-instance 'dep:file-parser 
                   :tracker tracker 
                   :file file
                   :package (find-package :common-lisp-user)
                   :package-name "COMMON-LISP-USER")))


(defun verify-reference (tracker symbol package-name &key type)
  "Helper function to verify that a reference exists."
  (let* ((pkg (find-package package-name))
         (sym (if pkg
                  (find-symbol (string symbol) pkg)
                  symbol))
         (refs (dep:get-references tracker sym)))
    (and refs 
         (or (find type refs :key #'dep:reference.type :test #'eq)
             (find :inherited refs :key #'dep:reference.type :test #'eq)))))


(test test-package-definitions
  "Test parsing of package definitions and their exports."
  (let* ((parser (create-test-parser *test-definitions-path*))
         (tracker (dep:tracker parser)))
    (unwind-protect
        (progn
          ;; Parse file and verify core package exports
          (dep:parse-file parser)
          (let ((core-exports (dep:get-package-exports tracker "USER-CORE-PACKAGE")))
            (is (member 'user-core-package::exported-function core-exports
                       :test (lambda (a b) 
                              (string= (symbol-name a) (symbol-name b))))
                "Core package should export 'exported-function'~%Current exports: ~{~%  ~A~}"
                core-exports)
            (is (member 'user-core-package::exported-macro core-exports
                       :test (lambda (a b)
                              (string= (symbol-name a) (symbol-name b))))
                "Core package should export 'exported-macro'~%Current exports: ~{~%  ~A~}"
                core-exports)
            (is (member 'user-core-package::internal-condition core-exports
                       :test (lambda (a b)
                              (string= (symbol-name a) (symbol-name b))))
                "Core package should export 'internal-condition'~%Current exports: ~{~%  ~A~}"
                core-exports))
          ;; Verify utils package exports
          (let ((util-exports (dep:get-package-exports tracker "USER-UTILS-PACKAGE")))
            (is (member 'user-utils-package::utility-function util-exports
                       :test (lambda (a b)
                              (string= (symbol-name a) (symbol-name b))))
                "Utils package should export 'utility-function'~%Current exports: ~{~%  ~A~}"
                util-exports)))
      ;; Cleanup
      (progn
        (when (find-package "USER-CORE-PACKAGE")
          (delete-package "USER-CORE-PACKAGE"))
        (when (find-package "USER-UTILS-PACKAGE")
          (delete-package "USER-UTILS-PACKAGE"))))))


(test test-function-definitions
  "Test parsing of function definitions in different packages."
  (let* ((parser (create-test-parser *test-definitions-path*))
         (tracker (dep:tracker parser)))
    (unwind-protect
        (progn
          ;; Parse file
          (dep:parse-file parser)
          
          ;; Test core package function definition
          (let ((def (dep:get-definitions tracker 'user-core-package::exported-function)))
            (is-true def
                    "Should find definition for exported-function~%All definitions in file: ~{~%  ~A~}"
                    (dep:get-file-definitions tracker *test-definitions-path*))
            (is (eq (dep:definition.type def) :function)
                "Should record correct definition type~%Definition: ~A"
                def)
            (is (string= (dep:definition.package def) "USER-CORE-PACKAGE")
                "Should record correct package~%Expected: USER-CORE-PACKAGE~%Got: ~A"
                (dep:definition.package def))
            (is-true (dep:definition.exported-p def)
                    "Should record export status~%Definition details:~%  Symbol: ~A~%  Package: ~A~%  Type: ~A~%  File: ~A"
                    (dep:definition.symbol def)
                    (dep:definition.package def)
                    (dep:definition.type def)
                    (dep:definition.file def)))

          ;; Test utils package function definition
          (let ((def (dep:get-definitions tracker 'user-utils-package::utility-function)))
            (is-true def
                    "Should find definition for utility-function~%All definitions in file: ~{~%  ~A~}"
                    (dep:get-file-definitions tracker *test-definitions-path*))
            (is (eq (dep:definition.type def) :function)
                "Should record correct definition type~%Definition: ~A"
                def)
            (is (string= (dep:definition.package def) "USER-UTILS-PACKAGE")
                "Should record correct package~%Expected: USER-UTILS-PACKAGE~%Got: ~A"
                (dep:definition.package def))
            (is-true (dep:definition.exported-p def)
                    "Should record export status~%Definition details:~%  Symbol: ~A~%  Package: ~A~%  Type: ~A~%  File: ~A"
                    (dep:definition.symbol def)
                    (dep:definition.package def)
                    (dep:definition.type def)
                    (dep:definition.file def))))
      ;; Cleanup
      (progn
        (when (find-package "USER-CORE-PACKAGE")
          (delete-package "USER-CORE-PACKAGE"))
        (when (find-package "USER-UTILS-PACKAGE")
          (delete-package "USER-UTILS-PACKAGE"))))))


(test test-macro-usage
  "Test that macro usages are properly recorded as dependencies."
  (let* ((parser (create-test-parser *test-references-path*))
         (tracker (dep:tracker parser)))
    (unwind-protect
        (progn
          ;; Parse file
          (dep:parse-file parser)
          
          ;; Test macro reference recording
          (let ((refs (dep:get-references tracker 'user-core-package::exported-macro)))
            (is-true refs
                   "Should find references to exported-macro~%All references in file: ~{~%  Type: ~A~%  Package: ~A~%  File: ~A~%~}"
                   (mapcar (lambda (ref)
                           (list (dep:reference.type ref)
                                 (dep:reference.package ref)
                                 (dep:reference.file ref)))
                         refs))
            ;; Verify macro call is recorded
            (let ((call-ref (find :call refs :key #'dep:reference.type)))
              (is-true call-ref
                     "Should record macro usage as :call reference~%References found: ~{~%  Type: ~A~%  Package: ~A~%  File: ~A~%~}"
                     (mapcar (lambda (ref)
                             (list (dep:reference.type ref)
                                   (dep:reference.package ref)
                                   (dep:reference.file ref)))
                           refs))
              (when call-ref
                (is (string= (dep:reference.package call-ref) "USER-CORE-PACKAGE")
                    "Should record correct package for macro reference~%Expected: USER-CORE-PACKAGE~%Got: ~A~%Reference details:~%  Type: ~A~%  File: ~A"
                    (dep:reference.package call-ref)
                    (dep:reference.type call-ref)
                    (dep:reference.file call-ref))))))
      ;; Cleanup
      (progn
        (when (find-package "USER-CORE-PACKAGE")
          (delete-package "USER-CORE-PACKAGE"))
        (when (find-package "USER-DIRECT-PACKAGE")
          (delete-package "USER-DIRECT-PACKAGE"))
        (when (find-package "USER-COMBINED-IMPORT-PACKAGE")
          (delete-package "USER-COMBINED-IMPORT-PACKAGE"))))))


(test test-direct-package-references
  "Test symbol references through package use."
  (let* ((parser (create-test-parser *test-references-path*))
         (tracker (dep:tracker parser)))
    (unwind-protect
        (progn
          ;; Parse file
          (dep:parse-file parser)
          ;; Test exported function reference
          (let ((func-refs (dep:get-references tracker 'user-core-package::exported-function)))
            (is-true (find :call (mapcar #'dep:reference.type func-refs))
                    "Should find :call reference to exported-function~%Current references: ~A~%Reference details:~{~%  Type: ~A~%  Package: ~A~%  File: ~A~}"
                    func-refs
                    (loop for ref in func-refs
                          collect (dep:reference.type ref)
                          collect (dep:reference.package ref)
                          collect (dep:reference.file ref)))
            (let ((call-ref (find :call func-refs :key #'dep:reference.type)))
              (when call-ref
                (is (string= (dep:reference.package call-ref) "USER-CORE-PACKAGE")
                    "Should record correct package for function reference~%Expected: USER-CORE-PACKAGE~%Got: ~A"
                    (dep:reference.package call-ref)))))
          ;; Test exported macro reference
          (let ((macro-refs (dep:get-references tracker 'user-core-package::exported-macro)))
            (is-true (find :call (mapcar #'dep:reference.type macro-refs))
                    "Should find :call reference to exported-macro~%Current references: ~A~%Reference details:~{~%  Type: ~A~%  Package: ~A~%  File: ~A~}"
                    macro-refs
                    (loop for ref in macro-refs
                          collect (dep:reference.type ref)
                          collect (dep:reference.package ref)
                          collect (dep:reference.file ref)))
            (let ((call-ref (find :call macro-refs :key #'dep:reference.type)))
              (when call-ref
                (is (string= (dep:reference.package call-ref) "USER-CORE-PACKAGE")
                    "Should record correct package for macro reference~%Expected: USER-CORE-PACKAGE~%Got: ~A"
                    (dep:reference.package call-ref))))))
      ;; Cleanup - delete packages in dependency order
      (progn
        (when (find-package "USER-COMBINED-IMPORT-PACKAGE")
          (delete-package "USER-COMBINED-IMPORT-PACKAGE"))
        (when (find-package "USER-DIRECT-PACKAGE")
          (delete-package "USER-DIRECT-PACKAGE"))
        (when (find-package "USER-CORE-PACKAGE")
          (delete-package "USER-CORE-PACKAGE"))))))


(test test-combined-package-references
  "Test symbol references through explicit imports."
  (let* ((parser (create-test-parser *test-references-path*))
         (tracker (dep:tracker parser)))
    (dep:parse-file parser)
    (is-true (verify-reference tracker 'exported-function "USER-CORE-PACKAGE" 
                               :type :call)
             "Should find reference to exported-function~%Current references: ~{~%  Type: ~A~%  Package: ~A~%  File: ~A~%~}"
             (mapcar (lambda (ref)
                      (list (dep:reference.type ref)
                            (dep:reference.package ref)
                            (dep:reference.file ref)))
                    (dep:get-references tracker 'user-core-package::exported-function)))
    (is-true (verify-reference tracker 'utility-function "USER-UTILS-PACKAGE" 
                               :type :call)
             "Should find reference to utility-function~%Current references: ~{~%  Type: ~A~%  Package: ~A~%  File: ~A~%~}"
             (mapcar (lambda (ref)
                      (list (dep:reference.type ref)
                            (dep:reference.package ref)
                            (dep:reference.file ref)))
                    (dep:get-references tracker 'user-utils-package::utility-function)))))


(test test-cross-package-dependencies
  "Test detection of package-level dependencies."
  (let ((tracker (make-instance 'dep:dependency-tracker :system-name "test")))
    (unwind-protect
        (progn
          ;; First parse the definitions file to establish packages and symbols
          (dep:parse-file (make-instance 'dep:file-parser 
                                       :tracker tracker 
                                       :file *test-definitions-path*))
          ;; Then parse the references file to establish dependencies
          (dep:parse-file (make-instance 'dep:file-parser 
                                       :tracker tracker 
                                       :file *test-references-path*))
          ;; Test direct package dependencies
          (is (dep:package-depends-on-p tracker "USER-DIRECT-PACKAGE" "USER-CORE-PACKAGE")
              "Direct package should depend on core package~%Package uses: ~{~%  ~A~}~%All package dependencies:~{~%  ~A -> ~A~}"
              (dep:get-package-uses tracker "USER-DIRECT-PACKAGE")
              (loop for pkg in (dep:get-package-uses tracker "USER-DIRECT-PACKAGE")
                    collect (list "USER-DIRECT-PACKAGE" pkg)))
          ;; Test combined import package dependencies
          (let ((combined-pkg "USER-COMBINED-IMPORT-PACKAGE"))
            ;; Test dependency on core package
            (is (dep:package-depends-on-p tracker combined-pkg "USER-CORE-PACKAGE")
                "Combined import package should depend on core package~%Package uses: ~{~%  ~A~}~%All package dependencies:~{~%  ~A -> ~A~}"
                (dep:get-package-uses tracker combined-pkg)
                (loop for pkg in (dep:get-package-uses tracker combined-pkg)
                      collect (list combined-pkg pkg)))
            ;; Test dependency on utils package
            (is (dep:package-depends-on-p tracker combined-pkg "USER-UTILS-PACKAGE")
                "Combined import package should depend on utils package~%Package uses: ~{~%  ~A~}~%All package dependencies:~{~%  ~A -> ~A~}"
                (dep:get-package-uses tracker combined-pkg)
                (loop for pkg in (dep:get-package-uses tracker combined-pkg)
                      collect (list combined-pkg pkg)))
            ;; Verify package exports are properly tracked
            (let ((core-exports (dep:get-package-exports tracker "USER-CORE-PACKAGE")))
              (is (member 'user-core-package::exported-function core-exports)
                  "Core package should export expected functions~%Current exports: ~{~%  ~A~}"
                  core-exports))))
      ;; Cleanup
      (progn
        ;; Delete packages in dependency order - most dependent first
        (when (find-package "USER-COMBINED-IMPORT-PACKAGE")
          (delete-package "USER-COMBINED-IMPORT-PACKAGE"))
        (when (find-package "USER-DIRECT-PACKAGE")
          (delete-package "USER-DIRECT-PACKAGE"))
        (when (find-package "USER-UTILS-PACKAGE")
          (delete-package "USER-UTILS-PACKAGE"))
        (when (find-package "USER-CORE-PACKAGE")
          (delete-package "USER-CORE-PACKAGE"))))))