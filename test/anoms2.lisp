;;;; File: anoms2.lisp

;;;; Test file containing minimal examples of anomalies


;; :SHADOWED-DEFINITION ANOMALIES
;; Switch to shadowing-test-pkg to define symbols that shadow inherited ones
(in-package #:shadowing-test-pkg)


;; This shadows internal-test-pkg:shadowed-var
(defvar *shadowed-var* 100 "Shadows the inherited variable")


;; This shadows internal-test-pkg:shadowed-func  
(defun shadowed-func (x y) (* x y) "Shadows the inherited function")
