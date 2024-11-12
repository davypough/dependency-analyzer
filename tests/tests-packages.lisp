;;;; Filename: tests-packages.lisp

;;; Package definitions for testing.
;;; Defines test packages needed at compile time to support various test suites,
;;; including those for package dependency analysis.

(in-package #:cl-user)

(defpackage #:dependency-analyzer/tests
  (:use #:cl #:fiveam)
  (:nicknames #:dep/tests)
  (:export #:test-all
           #:test-packages
           #:test-files
           #:test-symbols
           #:test-systems
           #:dependency-analyzer-tests
           #:package-tests
           #:file-tests
           #:symbol-tests
           #:system-tests
           #:tracker-tests))

;; Base test packages for inheritance tests
(defpackage #:test-base-package
  (:use #:cl)
  (:export #:base-function
           #:base-variable))

(defpackage #:test-middle-package
  (:use #:cl #:test-base-package)
  (:export #:base-function
           #:middle-function))

(defpackage #:test-end-package
  (:use #:cl #:test-middle-package))

;; Test packages for direct dependency tests
(defpackage #:test-pkg-a
  (:use #:cl)
  (:export #:func-a))

(defpackage #:test-pkg-b
  (:use #:cl)
  (:export #:func-b))

;; Test packages for file dependency tests
(defpackage #:test-a
  (:use #:cl)
  (:export #:func-a 
           #:var-a))

(defpackage #:test-b
  (:use #:cl #:test-a))

;; Test packages for function tests
(defpackage #:test-funcs
  (:use #:cl)
  (:export #:base-op
           #:wrapped-op
           #:call-ops))

;; Test packages for argument tests
(defpackage #:test-args
  (:use #:cl)
  (:export #:process-data 
           #:data-item
           #:data-item-value))

;; Test packages for method tests
(defpackage #:test-methods
  (:use #:cl)
  (:export #:processor
           #:process))

;; Test packages for return value tests
(defpackage #:test-returns
  (:use #:cl)
  (:export #:make-result
           #:result
           #:get-result))

;; Test packages for macro tests
(defpackage #:test-macros
  (:use #:cl)
  (:export #:with-wrapper
           #:helper-func
           #:call-wrapped))

;; Test packages for nested macro tests
(defpackage #:test-nested 
  (:use #:cl)
  (:export #:outer
           #:inner
           #:use-macros))

;; Test packages for macro helper tests
(defpackage #:test-helpers
  (:use #:cl)
  (:export #:define-wrapper
           #:generate-wrapper-name))

;; Test packages for macro expansion tests
(defpackage #:test-expand
  (:use #:cl)
  (:export #:with-counter))