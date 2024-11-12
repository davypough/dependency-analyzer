;;;; Filename: data-common.lisp

;;; Common data structures and setup functions for tests.
;;; Provides reusable test definitions to ensure consistency across test suites.

(in-package #:dep/tests)


(defparameter *test-system-definition*
  "test-system
   :description \"Test system for dependency analysis\"
   :components
   ((:module \"src\"
      :components
      ((:file \"packages\")
       (:file \"base\" :depends-on (\"packages\"))
       (:file \"utils\" :depends-on (\"base\"))
       (:file \"main\" :depends-on (\"utils\")))))"
  "Standard test system definition used across system tests.")


(defparameter *test-package-definition*
  "(defpackage #:test-package
     (:use #:cl)
     (:export #:exported-function
             #:exported-macro
             #:exported-variable))
   (in-package #:test-package)
   (defun exported-function (x)
     (* x 2))
   (defmacro exported-macro (name &body body)
     `(defun ,name ()
        ,@body))
   (defvar exported-variable 42)"
  "Standard package definition used for package dependency tests.")


(defparameter *test-module-definition*
  "(defpackage #:test-module
     (:use #:cl #:test-package)
     (:export #:wrapper-function))
   (in-package #:test-module)
   (defun wrapper-function (x)
     (exported-function (+ x exported-variable)))"
  "Standard module definition for testing dependencies on other packages.")


(defun create-test-files (&key system package module)
  "Create temporary test files with standard definitions.
   Returns pathnames of created files in an alist.
   Options:
     :system  - Create system definition file
     :package - Create package definition file
     :module  - Create module definition file"
  (let (files)
    (when system
      (push (cons :system
                  (create-temp-file
                    (format nil "(defsystem ~A)" *test-system-definition*)
                    :type "asd"))
            files))
    (when package
      (push (cons :package
                  (create-temp-file *test-package-definition*))
            files))
    (when module
      (push (cons :module
                  (create-temp-file *test-module-definition*))
            files))
    files))


(defun cleanup-test-files (files)
  "Clean up temporary test files.
   Parameter files should be alist returned by create-test-files."
  (dolist (file files)
    (delete-temp-file (cdr file))))