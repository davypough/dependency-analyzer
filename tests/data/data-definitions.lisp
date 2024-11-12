;;;; Filename: data-definitions.lisp

;;; Standard test definitions used across multiple test suites.
;;; Provides sample packages, functions, and variables for testing
;;; dependency analysis.

(in-package #:cl-user)


(defpackage #:user-core-package
  (:use #:cl)
  (:export #:exported-function
           #:exported-macro
           #:exported-variable
           #:internal-condition
           #:complex-function))


(defpackage #:user-utils-package
  (:use #:cl)
  (:export #:utility-function
           #:format-value))


(in-package #:user-core-package)


(defun exported-function (x y)
  "An exported function for testing dependencies."
  (+ x y))


(defun internal-function (x)
  "An internal function to test non-exported references."
  (* x 2))


(defmacro exported-macro (name &body body)
  "An exported macro for testing macro dependencies."
  `(defun ,name ()
     ,@body))


(defvar exported-variable 42
  "An exported special variable for testing.")


(define-condition internal-condition (error)
  ((reason :initarg :reason :reader reason))
  (:documentation "A condition for testing error handling."))


(defun complex-function (x &key (y 0))
  "A function using multiple package features for testing."
  (handler-case
      (if (> x y)
          (internal-function x)
          (error 'internal-condition :reason "Invalid input"))
    (internal-condition (c)
      (format nil "Error: ~A" (reason c)))))


(in-package #:user-utils-package)


(defun utility-function (x)
  "A utility function for testing cross-package dependencies."
  (* x 100))


(defun format-value (value)
  "A formatting function for testing string operations."
  (format nil "Value: ~A" value))