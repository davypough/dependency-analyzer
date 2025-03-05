;;;; File: defs2.lisp

;;;; Test file containing minimal definitions for testing 
;;;; :QUALIFIED-INTERNAL-REFERENCE ANOMALIES
;;;; and :SHADOWED-DEFINITION ANOMALIES


(defpackage #:internal-test-pkg
  (:import-from :cl #:defvar #:defun #:+ #:*)
  (:export #:public-function #:*public-variable*
           #:*shadowed-var* #:shadowed-func) ; Just export variable and function
  (:documentation "Package with both internal and exported symbols for testing"))


;; Create a package that inherits from internal-test-pkg
(defpackage #:shadowing-test-pkg
  (:use :cl #:internal-test-pkg)
  (:export #:test-shadowing)
  (:documentation "Package for testing shadowing of inherited symbols"))


(in-package #:internal-test-pkg)


;; Then define the internal-test-pkg symbols in its context
(defvar *public-variable* 42 "This is exported")
(defvar *internal-var* 100 "This is internal")
(defun public-function (x) (+ x 1))
(defun internal-function (x) (* x 2))


;; Additional definitions to be shadowed in inheriting packages
(defvar *shadowed-var* 50 "Variable that will be shadowed")
(defun shadowed-func (x) (+ x 10) "Function that will be shadowed")
