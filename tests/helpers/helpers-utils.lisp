;;;; Filename: helpers-utils.lisp

;;; Common utility functions used across the test suites.

(in-package #:dep/tests)

;; Ensure required systems are loaded
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :fiveam)
    (ql:quickload :fiveam))
  (unless (find-package :alexandria)
    (ql:quickload :alexandria)))

(defvar *temp-files* nil
  "List of temporary files created during testing.")

(defun register-temp-file (pathname)
  "Register a temporary file for cleanup."
  (push pathname *temp-files*))

(defun cleanup-temp-files ()
  "Clean up all registered temporary files."
  (dolist (file *temp-files*)
    (when (probe-file file)
      (delete-file file)))
  (setf *temp-files* nil))

(defmacro with-test-env (&body body)
  "Execute tests in a clean environment."
  `(let ((*package* (find-package :dep/tests)))
     (unwind-protect
         (progn ,@body)
       (cleanup-temp-files))))

(defun create-temp-file (content &key (type "lisp"))
  "Create a temporary file with given content for testing.
   Returns pathname of the created file.
   Parameters:
     content - String content to write
     type    - File extension (defaults to \"lisp\")
   Signals error if file creation/writing fails."
  (check-type content string)
  (check-type type string)
  (let ((temp-path (uiop:with-temporary-file (:pathname p
                                             :type type
                                             :keep t)
                     p)))
    (with-open-file (stream temp-path :direction :output
                                     :if-exists :supersede)
      (write-string content stream)
      (finish-output stream))
    (register-temp-file temp-path)
    temp-path))

(defun delete-temp-file (pathname)
  "Delete a temporary file created by create-temp-file.
   Returns true if deleted successfully.
   Parameters:
     pathname - Pathname to delete
   Signals error if deletion fails."
  (check-type pathname pathname)
  (when (probe-file pathname)
    (delete-file pathname))
  (setf *temp-files* (remove pathname *temp-files* :test #'equal)))

(defun verify-definition (tracker symbol type &key package exported-p)
  "Verify that a symbol is defined with expected attributes."
  (let ((def (dep:get-definitions tracker symbol)))
    (and def
         (eq (dep:definition.type def) type)
         (or (null package)
             (string= (dep:definition.package def) package))
         (or (null exported-p)
             (eq (dep:definition.exported-p def) exported-p)))))

(defun verify-reference (tracker symbol type &key package file)
  "Verify that a symbol reference exists with expected attributes."
  (let ((refs (dep:get-references tracker symbol)))
    (find-if (lambda (ref)
               (and (eq (dep:reference.type ref) type)
                    (or (null package)
                        (string= (dep:reference.package ref) package))
                    (or (null file)
                        (equal (dep:reference.file ref) file))))
             refs)))

(defun clear-test-packages (&rest packages)
  "Delete test packages if they exist."
  (dolist (pkg packages)
    (let ((package (find-package pkg)))
      (when package
        (delete-package package)))))