;;;; Filename: tests-suite.lisp

;;; Main test suite definition and organization.

(in-package #:dep/tests)

;; Ensure FiveAM is ready
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :fiveam)
    (ql:quickload :fiveam))
  (use-package :fiveam))

;; Define the main test suite hierarchy
(def-suite dependency-analyzer-tests
  :description "Main test suite for dependency analyzer")

(def-suite file-parser-tests
  :description "Tests for file parsing functionality"
  :in dependency-analyzer-tests)

(def-suite system-parser-tests
  :description "Tests for ASDF system parsing"
  :in dependency-analyzer-tests)

(def-suite package-tests
  :description "Tests for package dependency analysis"
  :in dependency-analyzer-tests)

(def-suite file-tests
  :description "Tests for file dependency analysis"
  :in dependency-analyzer-tests)

(def-suite symbol-tests
  :description "Tests for symbol-level dependency analysis"
  :in dependency-analyzer-tests)

(def-suite system-tests
  :description "Tests for ASDF system dependency analysis"
  :in dependency-analyzer-tests)

(def-suite tracker-tests
  :description "Tests for dependency tracker functionality"
  :in dependency-analyzer-tests)

;; Ensure we're in the main suite for subsequent test definitions
(in-suite dependency-analyzer-tests)

;; Functions to run test suites
(defun run-test-suite (suite-name)
  "Run a test suite and explain results."
  (format t "~%====================================~%")
  (format t "Running ~A Test Suite~%" suite-name)
  (format t "====================================~%")
  (let ((results (run suite-name)))
    (explain! results)
    (results-status results)))

(defun test-all ()
  "Run all dependency analyzer tests."
  (run! 'dependency-analyzer-tests))

(defun test-file-parser ()
  "Run file parser tests."
  (run! 'file-parser-tests))

(defun test-system-parser ()
  "Run system parser tests."
  (run! 'system-parser-tests))

(defun test-packages ()
  "Run package dependency tests."
  (run! 'package-tests))

(defun test-files ()
  "Run file dependency tests."
  (run! 'file-tests))

(defun test-symbols ()
  "Run symbol dependency tests."
  (run! 'symbol-tests))

(defun test-systems ()
  "Run system dependency tests."
  (run! 'system-tests))

(defun test-tracker ()
  "Run tracker functionality tests."
  (run! 'tracker-tests))