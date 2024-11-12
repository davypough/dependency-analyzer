;;;; Filename: intersystem3-tests.lisp

;;; Tests for cycle detection in inter-system dependencies.

(in-package #:dep/tests)


(in-suite system-tests)


(test test-system-dependency-cycles
  "Test detection of circular system dependencies."
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (temp-dir (uiop:ensure-absolute-pathname
                    (uiop:ensure-directory-pathname
                      (asdf:system-relative-pathname :dependency-analyzer/tests "tmp"))
                    :defaults (uiop:getcwd)))
         (sys-a-asd (merge-pathnames "test-sys-a.asd" temp-dir))
         (sys-b-asd (merge-pathnames "test-sys-b.asd" temp-dir))
         (sys-a-file (merge-pathnames "sys-a.lisp" temp-dir))
         (sys-b-file (merge-pathnames "sys-b.lisp" temp-dir)))
    (ensure-directories-exist temp-dir)
    (unwind-protect
        (progn
          (with-open-file (s sys-a-asd :direction :output :if-exists :supersede)
            (format s "(defsystem \"test-sys-a\"~%")
            (format s "  :description \"System A with circular dependency on System B\"~%")
            (format s "  :pathname ~S~%" (namestring temp-dir))
            (format s "  :depends-on (\"test-sys-b\")~%")
            (format s "  :components ((:file \"sys-a\")))~%"))
          (with-open-file (s sys-b-asd :direction :output :if-exists :supersede)
            (format s "(defsystem \"test-sys-b\"~%")
            (format s "  :description \"System B with circular dependency on System A\"~%")
            (format s "  :pathname ~S~%" (namestring temp-dir))
            (format s "  :depends-on (\"test-sys-a\")~%")
            (format s "  :components ((:file \"sys-b\")))~%"))
          (with-open-file (s sys-a-file :direction :output :if-exists :supersede)
            (format s "(defpackage #:test-sys-a~%")
            (format s "  (:use #:cl #:test-sys-b)~%")
            (format s "  (:export #:func-a))~%")
            (format s "(in-package #:test-sys-a)~%")
            (format s "(defun func-a () (func-b))~%"))
          (with-open-file (s sys-b-file :direction :output :if-exists :supersede)
            (format s "(defpackage #:test-sys-b~%")
            (format s "  (:use #:cl #:test-sys-a)~%")
            (format s "  (:export #:func-b))~%")
            (format s "(in-package #:test-sys-b)~%")
            (format s "(defun func-b () (func-a))~%"))
          ;; Load the system definitions directly
          (asdf:load-asd sys-a-asd)
          (asdf:load-asd sys-b-asd)
          (let ((sys-a (asdf:find-system "test-sys-a"))
                (sys-b (asdf:find-system "test-sys-b")))
            ;; Parse both systems
            (dep:parse-system (make-instance 'dep:system-parser 
                                           :tracker tracker 
                                           :system sys-a))
            (dep:parse-system (make-instance 'dep:system-parser 
                                           :tracker tracker 
                                           :system sys-b))
            ;; Verify cycle detection
            (let ((cycles (dep:get-system-cycles tracker)))
              (is (= 1 (length cycles))
                  "Should detect exactly one dependency cycle between systems")
              (is (find "test-sys-a -> test-sys-b -> test-sys-a" 
                       cycles :test #'string=)
                  "Should detect cycle from system A to B and back to A")
              ;; Verify file-level dependencies reflect system dependencies
              (let ((file-a-deps (dep:file-dependencies tracker sys-a-file))
                    (file-b-deps (dep:file-dependencies tracker sys-b-file)))
                (is (member sys-b-file file-a-deps :test #'equal)
                    "System A's file should show dependency on System B's file")
                (is (member sys-a-file file-b-deps :test #'equal)
                    "System B's file should show dependency on System A's file")))))
      ;; Cleanup
      (when (probe-file sys-a-asd) (delete-file sys-a-asd))
      (when (probe-file sys-b-asd) (delete-file sys-b-asd))
      (when (probe-file sys-a-file) (delete-file sys-a-file))
      (when (probe-file sys-b-file) (delete-file sys-b-file))
      (when (probe-file temp-dir)
        (uiop:delete-directory-tree temp-dir :validate t))
      ;; Clear ASDF systems
      (asdf:clear-system "test-sys-a")
      (asdf:clear-system "test-sys-b")
      (dep:clear-tracker tracker))))