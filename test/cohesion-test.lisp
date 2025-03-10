;;;; File: cohesion-test.lisp

;;;; Test file containing minimal examples of package cohesion anomalies


;; :SHADOWED-DEFINITION ANOMALIES
;; Switch to shadowing-test-pkg to define symbols that shadow inherited ones
(in-package #:shadowing-test-pkg)


;; This shadows internal-test-pkg:shadowed-var
(defvar *shadowed-var* 100 "Shadows the inherited variable")


;; This shadows internal-test-pkg:shadowed-func  
(defun shadowed-func (x y) (* x y) "Shadows the inherited function")


;; :EXTERNAL-DEPENDENCY ANOMALIES

(defpackage #:external-heavy-pkg
  (:use :cl)
  (:import-from :alexandria 
                #:curry #:rcurry #:compose 
                #:with-gensyms #:once-only
                #:when-let #:if-let
                #:ensure-list #:ensure-function
                #:proper-list-p #:circular-list-p)
  (:export #:process-data))

(in-package #:external-heavy-pkg)

;; Minimal own symbols, heavy usage of imported ones
(defun process-data (data)
  (let ((normalized (ensure-list data)))
    (when-let ((valid-items (remove-if-not #'proper-list-p normalized)))
      (mapcar (curry #'apply #'compose) valid-items))))

#|
;;;; Test for package-fragmentation anomaly

(defpackage #:fragmented-pkg
  (:use :cl)
  (:export #:group1-func #:group2-func))

(in-package #:fragmented-pkg)

;; No references between Group 1 and Group 2
;; Group 1 - functions that call each other
(defun group1-helper (x)
  (* x x))

(defun group1-func (x)
  (+ (group1-helper x) 10))

;; Group 2 - completely separate function group
(defun group2-helper (str)
  (concatenate 'string str "-suffix"))

(defun group2-func (str)
  (format nil "Result: ~A" (group2-helper str)))


;;;; Test for interface-segregation anomaly

(defpackage #:interface-pkg
  (:use :cl)
  (:export #:numeric-func #:string-func #:list-func #:vector-func))

(in-package #:interface-pkg)

;; Define various exported functions
(defun numeric-func (x) (* x 2))
(defun string-func (s) (concatenate 'string s s))
(defun list-func (lst) (append lst lst))
(defun vector-func (vec) (map 'vector #'1+ vec))

;; Create client packages that use disjoint parts
(defpackage #:numeric-client
  (:use :cl :interface-pkg))

(defpackage #:string-client
  (:use :cl :interface-pkg))

;; Use the exported functions in disjoint patterns
(in-package #:numeric-client)
(defun test-num () (numeric-func 10) (list-func '(1 2 3)))

(in-package #:string-client)
(defun test-str () (string-func "hello") (vector-func #(1 2 3)))
|#
