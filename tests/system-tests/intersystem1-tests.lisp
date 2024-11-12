;;;; Filename: intersystem1-tests.lisp

;;; Tests for dependencies between different ASDF systems.

(in-package #:dep/tests)


(in-suite system-tests)


(def-fixture with-multiple-systems ()
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (base-asd (create-temp-file
                     "(defsystem \"test-base\"
                        :description \"Base system for testing inter-system dependencies\"
                        :components
                        ((:file \"base\")))"
                     :type "asd"))
         (utils-asd (create-temp-file
                      "(defsystem \"test-utils\"
                         :description \"Utility system dependent on test-base\"
                         :depends-on (\"test-base\")
                         :components
                         ((:file \"utils\")))"
                      :type "asd"))
         (base-file (create-temp-file
                      "(defpackage #:test-base
                         (:use #:cl)
                         (:export #:base-function))
                       (in-package #:test-base)
                       (defun base-function (x)
                         (* x 2))"
                      :type "lisp"))
         (utils-file (create-temp-file
                       "(defpackage #:test-utils
                          (:use #:cl #:test-base)
                          (:export #:util-function))
                        (in-package #:test-utils)
                        (defun util-function (x)
                          (base-function (1+ x)))"
                       :type "lisp")))
    (unwind-protect
        (let ((base-sys (asdf:find-system "test-base" nil))
              (utils-sys (asdf:find-system "test-utils" nil)))
          (dep:parse-system (make-instance 'dep:system-parser 
                                           :tracker tracker 
                                           :system base-sys))
          (dep:parse-system (make-instance 'dep:system-parser 
                                           :tracker tracker 
                                           :system utils-sys))
          (&body))
      (delete-temp-file base-asd)
      (delete-temp-file utils-asd)
      (delete-temp-file base-file)
      (delete-temp-file utils-file)
      (asdf:clear-system "test-base")
      (asdf:clear-system "test-utils")
      (dep:clear-tracker tracker))))


(test test-direct-system-dependencies
  "Test direct dependencies between ASDF systems."
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (temp-dir (uiop:ensure-absolute-pathname
                    (uiop:ensure-directory-pathname
                      (asdf:system-relative-pathname :dependency-analyzer/tests "tmp"))
                    :defaults (uiop:getcwd)))
         (base-asd (merge-pathnames "test-base.asd" temp-dir))
         (utils-asd (merge-pathnames "test-utils.asd" temp-dir))
         (base-file (merge-pathnames "base.lisp" temp-dir))
         (utils-file (merge-pathnames "utils.lisp" temp-dir)))
    (ensure-directories-exist temp-dir)
    (unwind-protect
        (progn
          (with-open-file (s base-asd :direction :output :if-exists :supersede)
            (format s "(defsystem \"test-base\"~%")
            (format s "  :description \"Base system for testing inter-system dependencies\"~%")
            (format s "  :pathname ~S~%" (namestring temp-dir))
            (format s "  :components ((:file \"base\")))~%"))
          (with-open-file (s utils-asd :direction :output :if-exists :supersede)
            (format s "(defsystem \"test-utils\"~%")
            (format s "  :description \"Utility system dependent on test-base\"~%")
            (format s "  :pathname ~S~%" (namestring temp-dir))
            (format s "  :depends-on (\"test-base\")~%")
            (format s "  :components ((:file \"utils\")))~%"))
          (with-open-file (s base-file :direction :output :if-exists :supersede)
            (format s "(defpackage #:test-base~%")
            (format s "  (:use #:cl)~%")
            (format s "  (:export #:base-function))~%")
            (format s "(in-package #:test-base)~%")
            (format s "(defun base-function (x) (* x 2))~%"))
          (with-open-file (s utils-file :direction :output :if-exists :supersede)
            (format s "(defpackage #:test-utils~%")
            (format s "  (:use #:cl #:test-base)~%")
            (format s "  (:export #:util-function))~%")
            (format s "(in-package #:test-utils)~%")
            (format s "(defun util-function (x)~%")
            (format s "  (base-function (1+ x)))~%"))
          ;; Load the system definitions directly
          (asdf:load-asd base-asd)
          (asdf:load-asd utils-asd)
          (let ((base-sys (asdf:find-system "test-base"))
                (utils-sys (asdf:find-system "test-utils")))
            (dep:parse-system (make-instance 'dep:system-parser 
                                           :tracker tracker 
                                           :system base-sys))
            (dep:parse-system (make-instance 'dep:system-parser 
                                           :tracker tracker 
                                           :system utils-sys))
            (let ((utils-deps (dep:file-dependencies tracker utils-file)))
              (is (member base-file utils-deps :test #'equal)
                  "Utils system should depend on base system through explicit system dependency"))))
      ;; Cleanup
      (when (probe-file base-asd) (delete-file base-asd))
      (when (probe-file utils-asd) (delete-file utils-asd))
      (when (probe-file base-file) (delete-file base-file))
      (when (probe-file utils-file) (delete-file utils-file))
      (when (probe-file temp-dir)
        (uiop:delete-directory-tree temp-dir :validate t))
      ;; Clear ASDF systems
      (asdf:clear-system "test-base")
      (asdf:clear-system "test-utils")
      (dep:clear-tracker tracker))))


(test test-system-version-dependencies
  "Test version-specific system dependencies."
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (temp-dir (uiop:ensure-absolute-pathname
                    (uiop:ensure-directory-pathname
                      (asdf:system-relative-pathname :dependency-analyzer/tests "tmp"))
                    :defaults (uiop:getcwd)))
         (base-asd (merge-pathnames "test-base.asd" temp-dir))
         (app-asd (merge-pathnames "test-app.asd" temp-dir))
         (base-file (merge-pathnames "base.lisp" temp-dir))
         (app-file (merge-pathnames "app.lisp" temp-dir)))
    (ensure-directories-exist temp-dir)
    (unwind-protect
        (progn
          (with-open-file (s base-asd :direction :output :if-exists :supersede)
            (format s "(defsystem \"test-base\"~%")
            (format s "  :description \"Base system with version for dependency testing\"~%")
            (format s "  :pathname ~S~%" (namestring temp-dir))
            (format s "  :version \"1.0.0\"~%")
            (format s "  :components ((:file \"base\")))~%"))
          (with-open-file (s app-asd :direction :output :if-exists :supersede)
            (format s "(defsystem \"test-app\"~%")
            (format s "  :description \"Application requiring specific version of test-base\"~%")
            (format s "  :pathname ~S~%" (namestring temp-dir))
            (format s "  :depends-on ((:version \"test-base\" \"1.0.0\"))~%")
            (format s "  :components ((:file \"app\")))~%"))
          (with-open-file (s base-file :direction :output :if-exists :supersede)
            (format s "(defpackage #:test-base~%")
            (format s "  (:use #:cl)~%")
            (format s "  (:export #:base-op))~%")
            (format s "(in-package #:test-base)~%")
            (format s "(defun base-op () :v1)~%"))
          (with-open-file (s app-file :direction :output :if-exists :supersede)
            (format s "(defpackage #:test-app~%")
            (format s "  (:use #:cl #:test-base))~%")
            (format s "(in-package #:test-app)~%")
            (format s "(defun app-op ()~%")
            (format s "  (base-op))~%"))
          ;; Load the system definitions directly
          (asdf:load-asd base-asd)
          (asdf:load-asd app-asd)
          (let ((base-sys (asdf:find-system "test-base"))
                (app-sys (asdf:find-system "test-app")))
            (dep:parse-system (make-instance 'dep:system-parser 
                                           :tracker tracker 
                                           :system base-sys))
            (dep:parse-system (make-instance 'dep:system-parser 
                                           :tracker tracker 
                                           :system app-sys))
            (let ((app-deps (dep:file-dependencies tracker app-file)))
              (is (member base-file app-deps :test #'equal)
                  "App should depend on specific version (1.0.0) of base system"))))
      ;; Cleanup
      (when (probe-file base-asd) (delete-file base-asd))
      (when (probe-file app-asd) (delete-file app-asd))
      (when (probe-file base-file) (delete-file base-file))
      (when (probe-file app-file) (delete-file app-file))
      (when (probe-file temp-dir)
        (uiop:delete-directory-tree temp-dir :validate t))
      ;; Clear ASDF systems
      (asdf:clear-system "test-base")
      (asdf:clear-system "test-app")
      (dep:clear-tracker tracker))))