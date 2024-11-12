;;;; Filename: helpers-fixtures.lisp

;;; Test fixtures for setting up and tearing down test environments.

(in-package #:dep/tests)


(def-fixture clean-tracker ()
  "Basic fixture providing a clean dependency tracker."
  (let ((tracker (make-instance 'dep:dependency-tracker :system-name "test")))
    (unwind-protect
        (&body)
      (dep:clear-tracker tracker))))


(def-fixture with-test-files ()
  "Fixture providing standard test files with cleanup."
  (let* ((files (create-test-files :package t :module t))
         (tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (package-parser (make-instance 'dep:file-parser
                                        :tracker tracker
                                        :file (cdr (assoc :package files))))
         (module-parser (make-instance 'dep:file-parser
                                       :tracker tracker
                                       :file (cdr (assoc :module files)))))
    (unwind-protect
        (progn
          (dep:parse-file package-parser)
          (dep:parse-file module-parser)
          (&body))
      (cleanup-test-files files)
      (dep:clear-tracker tracker))))


(def-fixture with-test-system ()
  "Fixture providing a complete test system with components."
  (let* ((files (create-test-files :system t :package t :module t))
         (tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (system (asdf:find-system "test-system" nil)))
    (unwind-protect
        (let ((parser (make-instance 'dep:system-parser
                                     :tracker tracker
                                     :system system)))
          (dep:parse-system parser)
          (&body))
      (cleanup-test-files files)
      (asdf:clear-system "test-system")
      (dep:clear-tracker tracker))))


(def-fixture with-package-dependencies ()
  "Fixture for testing package-level dependencies."
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (files (create-test-files :package t :module t))
         (pkg-file (cdr (assoc :package files)))
         (mod-file (cdr (assoc :module files))))
    (unwind-protect
        (progn
          (dep:parse-file (make-instance 'dep:file-parser
                                         :tracker tracker
                                         :file pkg-file))
          (dep:parse-file (make-instance 'dep:file-parser
                                         :tracker tracker
                                         :file mod-file))
          (&body))
      (cleanup-test-files files)
      (dep:clear-tracker tracker))))