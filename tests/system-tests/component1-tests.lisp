;;;; Filename: component1-tests.lisp

;;; Tests for ASDF component dependencies within a single system.

(in-package #:dep/tests)


(in-suite system-tests)


(def-fixture with-test-system ()
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (system-file (create-temp-file
                        "(defsystem \"test-system\"
                           :description \"Test system for dependency analysis\"
                           :components
                           ((:module \"src\"
                              :components
                              ((:file \"base\")
                               (:file \"utils\" :depends-on (\"base\"))
                               (:file \"main\" :depends-on (\"utils\"))))))"
                        :type "asd"))
         (base-file (create-temp-file
                      "(defpackage #:test-system/base
                         (:use #:cl)
                         (:export #:base-function))
                       (in-package #:test-system/base)
                       (defun base-function (x)
                         (* x 2))"
                      :type "lisp"))
         (utils-file (create-temp-file
                       "(defpackage #:test-system/utils
                          (:use #:cl #:test-system/base)
                          (:export #:util-function))
                        (in-package #:test-system/utils)
                        (defun util-function (x)
                          (base-function (1+ x)))"
                       :type "lisp"))
         (main-file (create-temp-file
                      "(defpackage #:test-system/main
                         (:use #:cl #:test-system/utils))
                       (in-package #:test-system/main)
                       (defun main ()
                         (util-function 10))"
                      :type "lisp"))
         (system (asdf:find-system "test-system" nil))
         (parser (make-instance 'dep:system-parser :tracker tracker :system system)))
    (unwind-protect
        (progn
          (dep:parse-system parser)
          (&body))
      (delete-temp-file system-file)
      (delete-temp-file base-file)
      (delete-temp-file utils-file)
      (delete-temp-file main-file)
      (asdf:clear-system "test-system")
      (dep:clear-tracker tracker))))


(test test-file-component-dependencies
  "Test dependencies between ASDF file components."
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (temp-dir (uiop:ensure-directory-pathname
                     (asdf:system-relative-pathname :dependency-analyzer/tests "tmp")))
         (system-file (merge-pathnames "test-system.asd" temp-dir))
         (base-file (merge-pathnames "base.lisp" temp-dir))
         (utils-file (merge-pathnames "utils.lisp" temp-dir))
         (main-file (merge-pathnames "main.lisp" temp-dir)))
    (ensure-directories-exist temp-dir)
    (unwind-protect
        (progn
          (with-open-file (s system-file :direction :output :if-exists :supersede)
            (format s "(defsystem \"test-system\"~%")
            (format s "  :description \"Test system for dependency analysis\"~%")
            (format s "  :components~%")
            (format s "  ((:file \"base\")~%")
            (format s "   (:file \"utils\" :depends-on (\"base\"))~%")
            (format s "   (:file \"main\" :depends-on (\"utils\"))))~%"))
          (with-open-file (s base-file :direction :output :if-exists :supersede)
            (format s "(defpackage #:test-system/base~%")
            (format s "  (:use #:cl)~%")
            (format s "  (:export #:base-function))~%")
            (format s "(in-package #:test-system/base)~%")
            (format s "(defun base-function (x) (* x 2))~%"))
          (with-open-file (s utils-file :direction :output :if-exists :supersede)
            (format s "(defpackage #:test-system/utils~%")
            (format s "  (:use #:cl #:test-system/base)~%")
            (format s "  (:export #:util-function))~%")
            (format s "(in-package #:test-system/utils)~%")
            (format s "(defun util-function (x)~%")
            (format s "  (base-function (1+ x)))~%"))
          (with-open-file (s main-file :direction :output :if-exists :supersede)
            (format s "(defpackage #:test-system/main~%")
            (format s "  (:use #:cl #:test-system/utils))~%")
            (format s "(in-package #:test-system/main)~%")
            (format s "(defun main ()~%")
            (format s "  (util-function 10))~%"))
          (let ((asdf:*central-registry* (cons temp-dir asdf:*central-registry*)))
            (let ((system (asdf:find-system "test-system" nil)))
              (dep:parse-system (make-instance 'dep:system-parser 
                                               :tracker tracker 
                                               :system system))
              (let ((deps-utils (dep:file-dependencies tracker utils-file))
                    (deps-main (dep:file-dependencies tracker main-file)))
                (is (member base-file deps-utils :test #'equal)
                    "Utils file should depend on base file with declared dependency")
                (is (member utils-file deps-main :test #'equal)
                    "Main file should depend on utils file with declared dependency")))))
      (when (probe-file system-file) (delete-file system-file))
      (when (probe-file base-file) (delete-file base-file))
      (when (probe-file utils-file) (delete-file utils-file))
      (when (probe-file main-file) (delete-file main-file))
      (when (probe-file temp-dir)
        (uiop:delete-directory-tree temp-dir :validate t))
      (asdf:clear-system "test-system")
      (dep:clear-tracker tracker))))


(test test-module-component-dependencies
  "Test dependencies between ASDF modules."
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (temp-dir (uiop:ensure-directory-pathname
                     (asdf:system-relative-pathname :dependency-analyzer/tests "tmp")))
         (system-file (merge-pathnames "test-system.asd" temp-dir))
         (core-file (merge-pathnames "core.lisp" temp-dir))
         (helpers-file (merge-pathnames "helpers.lisp" temp-dir))
         (app-file (merge-pathnames "app.lisp" temp-dir)))
    (ensure-directories-exist temp-dir)
    (unwind-protect
        (progn
          (with-open-file (s system-file :direction :output :if-exists :supersede)
            (format s "(defsystem \"test-system\"~%")
            (format s "  :components~%")
            (format s "  ((:module \"base\"~%")
            (format s "     :components ((:file \"core\")))~%")
            (format s "   (:module \"utils\"~%")
            (format s "     :depends-on (\"base\")~%")
            (format s "     :components ((:file \"helpers\")))~%")
            (format s "   (:module \"main\"~%")
            (format s "     :depends-on (\"utils\")~%")
            (format s "     :components ((:file \"app\")))))~%"))
          (with-open-file (s core-file :direction :output :if-exists :supersede)
            (format s "(defpackage #:test-system/core~%")
            (format s "  (:use #:cl)~%")
            (format s "  (:export #:core-function))~%")
            (format s "(in-package #:test-system/core)~%")
            (format s "(defun core-function () :core)~%"))
          (with-open-file (s helpers-file :direction :output :if-exists :supersede)
            (format s "(defpackage #:test-system/helpers~%")
            (format s "  (:use #:cl #:test-system/core)~%")
            (format s "  (:export #:help-function))~%")
            (format s "(in-package #:test-system/helpers)~%")
            (format s "(defun help-function ()~%")
            (format s "  (core-function))~%"))
          (with-open-file (s app-file :direction :output :if-exists :supersede)
            (format s "(defpackage #:test-system/app~%")
            (format s "  (:use #:cl #:test-system/helpers))~%")
            (format s "(in-package #:test-system/app)~%")
            (format s "(defun app-function ()~%")
            (format s "  (help-function))~%"))
          (let ((asdf:*central-registry* (cons temp-dir asdf:*central-registry*)))
            (let ((system (asdf:find-system "test-system" nil)))
              (dep:parse-system (make-instance 'dep:system-parser 
                                               :tracker tracker 
                                               :system system))
              (let ((deps-helpers (dep:file-dependencies tracker helpers-file))
                    (deps-app (dep:file-dependencies tracker app-file)))
                (is (member core-file deps-helpers :test #'equal)
                    "Helpers module should depend on core module through module dependency")
                (is (member helpers-file deps-app :test #'equal)
                    "App module should depend on helpers module through module dependency")))))
      (when (probe-file system-file) (delete-file system-file))
      (when (probe-file core-file) (delete-file core-file))
      (when (probe-file helpers-file) (delete-file helpers-file))
      (when (probe-file app-file) (delete-file app-file))
      (when (probe-file temp-dir)
        (uiop:delete-directory-tree temp-dir :validate t))
      (asdf:clear-system "test-system")
      (dep:clear-tracker tracker))))