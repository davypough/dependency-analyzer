;;;; Filename: intersystem2-tests.lisp

;;; Tests for feature-conditional and conflict-detection scenarios in inter-system dependencies.

(in-package #:dep/tests)


(in-suite system-tests)


(test test-system-feature-dependencies
  "Test feature-conditional system dependencies."
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (temp-dir (uiop:ensure-absolute-pathname
                    (uiop:ensure-directory-pathname
                      (asdf:system-relative-pathname :dependency-analyzer/tests "tmp"))
                    :defaults (uiop:getcwd)))
         (base-asd (merge-pathnames "test-base.asd" temp-dir))
         (tests-asd (merge-pathnames "test-base-tests.asd" temp-dir))
         (base-file (merge-pathnames "base.lisp" temp-dir))
         (tests-file (merge-pathnames "tests.lisp" temp-dir)))
    (ensure-directories-exist temp-dir)
    (unwind-protect
        (progn
          ;; Write test-base.asd
          (with-open-file (s base-asd :direction :output :if-exists :supersede)
            (format s "(defsystem \"test-base\"~%")
            (format s "  :description \"Base system with conditional test dependency\"~%")
            (format s "  :pathname ~S~%" (namestring temp-dir))
            (format s "  :components ((:file \"base\"))~%")
            (format s "  :in-order-to ((load-op (load-op \"test-base-tests\"))))~%"))
          ;; Write test-base-tests.asd
          (with-open-file (s tests-asd :direction :output :if-exists :supersede)
            (format s "(defsystem \"test-base-tests\"~%")
            (format s "  :description \"Test suite depending on test-base system\"~%")
            (format s "  :pathname ~S~%" (namestring temp-dir))
            (format s "  :depends-on (\"test-base\")~%")
            (format s "  :components ((:file \"tests\")))~%"))
          ;; Write base.lisp
          (with-open-file (s base-file :direction :output :if-exists :supersede)
            (format s "(defpackage #:test-base~%")
            (format s "  (:use #:cl)~%")
            (format s "  (:export #:my-test-function))~%")
            (format s "(in-package #:test-base)~%")
            (format s "(defun my-test-function () t)~%"))
          ;; Write tests.lisp
          (with-open-file (s tests-file :direction :output :if-exists :supersede)
            (format s "(defpackage #:test-base-tests~%")
            (format s "  (:use #:cl #:test-base))~%")
            (format s "(in-package #:test-base-tests)~%")
            (format s "(defun run-tests ()~%")
            (format s "  (my-test-function))~%"))
          ;; Load the system definitions directly
          (asdf:load-asd base-asd)
          (asdf:load-asd tests-asd)
          (let ((base-sys (asdf:find-system "test-base"))
                (tests-sys (asdf:find-system "test-base-tests")))
            (dep:parse-system (make-instance 'dep:system-parser 
                                           :tracker tracker 
                                           :system base-sys))
            (dep:parse-system (make-instance 'dep:system-parser 
                                           :tracker tracker 
                                           :system tests-sys))
            (let ((test-deps (dep:file-dependencies tracker tests-file)))
              (is (member base-file test-deps :test #'equal)
                  "Test system should depend on base system through :depends-on relationship"))))
      ;; Cleanup
      (when (probe-file base-asd) (delete-file base-asd))
      (when (probe-file tests-asd) (delete-file tests-asd))
      (when (probe-file base-file) (delete-file base-file))
      (when (probe-file tests-file) (delete-file tests-file))
      (when (probe-file temp-dir)
        (uiop:delete-directory-tree temp-dir :validate t))
      ;; Clear ASDF systems
      (asdf:clear-system "test-base")
      (asdf:clear-system "test-base-tests")
      (dep:clear-tracker tracker))))


(test test-system-dependency-conflicts
  "Test detection of conflicting system dependencies."
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (temp-dir (uiop:ensure-absolute-pathname
                    (uiop:ensure-directory-pathname
                      (asdf:system-relative-pathname :dependency-analyzer/tests "tmp"))
                    :defaults (uiop:getcwd)))
         (base-asd (merge-pathnames "test-base.asd" temp-dir))
         (app-a-asd (merge-pathnames "test-app-a.asd" temp-dir))
         (app-b-asd (merge-pathnames "test-app-b.asd" temp-dir))
         (base-file (merge-pathnames "base.lisp" temp-dir))
         (app-a-file (merge-pathnames "app-a.lisp" temp-dir))
         (app-b-file (merge-pathnames "app-b.lisp" temp-dir)))
    (ensure-directories-exist temp-dir)
    (unwind-protect
        (progn
          (with-open-file (s base-asd :direction :output :if-exists :supersede)
            (format s "(defsystem \"test-base\"~%")
            (format s "  :description \"Base system with version for conflict testing\"~%")
            (format s "  :pathname ~S~%" (namestring temp-dir))
            (format s "  :version \"1.0\"~%")
            (format s "  :components ((:file \"base\")))~%"))
          (with-open-file (s app-a-asd :direction :output :if-exists :supersede)
            (format s "(defsystem \"test-app-a\"~%")
            (format s "  :description \"Application A requiring test-base 1.0\"~%")
            (format s "  :pathname ~S~%" (namestring temp-dir))
            (format s "  :depends-on ((:version \"test-base\" \"1.0\"))~%")
            (format s "  :components ((:file \"app-a\")))~%"))
          (with-open-file (s app-b-asd :direction :output :if-exists :supersede)
            (format s "(defsystem \"test-app-b\"~%")
            (format s "  :description \"Application B requiring incompatible test-base 2.0\"~%")
            (format s "  :pathname ~S~%" (namestring temp-dir))
            (format s "  :depends-on ((:version \"test-base\" \"2.0\"))~%")
            (format s "  :components ((:file \"app-b\")))~%"))
          (with-open-file (s base-file :direction :output :if-exists :supersede)
            (format s "(defpackage #:test-base~%")
            (format s "  (:use #:cl)~%")
            (format s "  (:export #:base-op))~%")
            (format s "(in-package #:test-base)~%")
            (format s "(defun base-op () :base)~%"))
          (with-open-file (s app-a-file :direction :output :if-exists :supersede)
            (format s "(defpackage #:test-app-a~%")
            (format s "  (:use #:cl #:test-base)~%")
            (format s "  (:export #:app-a-op))~%")
            (format s "(in-package #:test-app-a)~%")
            (format s "(defun app-a-op () (base-op))~%"))
          (with-open-file (s app-b-file :direction :output :if-exists :supersede)
            (format s "(defpackage #:test-app-b~%")
            (format s "  (:use #:cl #:test-base)~%")
            (format s "  (:export #:app-b-op))~%")
            (format s "(in-package #:test-app-b)~%")
            (format s "(defun app-b-op () (base-op))~%"))
          ;; Load the system definitions directly
          (asdf:load-asd base-asd)
          (asdf:load-asd app-a-asd)
          (asdf:load-asd app-b-asd)
          (signals dep:system-parse-error
            (let ((base-sys (asdf:find-system "test-base"))
                  (app-a-sys (asdf:find-system "test-app-a"))
                  (app-b-sys (asdf:find-system "test-app-b")))
              (dep:parse-system (make-instance 'dep:system-parser 
                                             :tracker tracker 
                                             :system base-sys))
              (dep:parse-system (make-instance 'dep:system-parser 
                                             :tracker tracker 
                                             :system app-a-sys))
              (dep:parse-system (make-instance 'dep:system-parser 
                                             :tracker tracker 
                                             :system app-b-sys)))
            "Should signal system-parse-error due to version conflict between apps A and B"))
      ;; Cleanup
      (when (probe-file base-asd) (delete-file base-asd))
      (when (probe-file app-a-asd) (delete-file app-a-asd))
      (when (probe-file app-b-asd) (delete-file app-b-asd))
      (when (probe-file base-file) (delete-file base-file))
      (when (probe-file app-a-file) (delete-file app-a-file))
      (when (probe-file app-b-file) (delete-file app-b-file))
      (when (probe-file temp-dir)
        (uiop:delete-directory-tree temp-dir :validate t))
      ;; Clear ASDF systems
      (asdf:clear-system "test-base")
      (asdf:clear-system "test-app-a")
      (asdf:clear-system "test-app-b")
      (dep:clear-tracker tracker))))