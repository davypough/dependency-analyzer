;;;; File: anoms.lisp

;;;; Test file containing minimal examples of anomalies
;;;; that can be processed by the dependency analyzer. 


;; First establish package context for remaining expressions
(in-package #:user-pkg)


;; :DUPLICATE-DEFINITION ANOMALIES
;; :variable type
(defparameter *special1* 0)
(setf (symbol-value '*param1*) 101)
(defconstant +const1+ 1)

;; :function type
(defun fn1 (x) (* 2 x))

;; :symbol-macro type
(define-symbol-macro today (get-universal-time))

;; :structure/class/condition type
(defstruct point  ; Same name but simplified constructor
  (x 0 :type number)
  (y 0 :type number))
(defclass named-point ()
  ((name :initarg :name :accessor point-name)
   (u :initarg :u :accessor point-u)
   (v :initarg :v :accessor point-v)))
(define-condition invalid-point (error)
  ((reason :initarg :reason :reader point-error-reason)
   (value :initarg :value :reader point-error-value)))

;; :generic-function type
(defgeneric gf1 (y)
  (:documentation "Basic generic function"))

;; :method type
(defmethod gf2 ((a number) b)
  (list a b))

;; :method-combination type
(define-method-combination my-comb (&key (order :most-specific-first))
  ((methods *))
  `(progn
     ,(case order
        (:most-specific-first `(call-method ,(first methods)))
        (:most-specific-last `(call-method ,(car (last methods)))))))

;; :deftype type
(deftype pos-integer ()
  '(fixnum 1 *))


;; :SPECIAL-VAR-NAMING & :CONSTANT-NAMING ANOMALIES
(defvar my-var)
(defparameter my-param 0)
(defconstant my-const 0)


;; :UNUSED-IMPORT ANOMALIES
;; Test instances of unused imports already in defs.lisp


;; :QUALIFIED-INTERNAL-REFERENCE ANOMALIES
(defun test-public-access ()
  (let ((val internal-test-pkg:*public-variable*))   ;no anomaly
    (internal-test-pkg:public-function val)))   ;no anomaly

;; Test internal symbol access (should trigger 2 anomalies)
(defun test-internal-access ()
  (let ((val internal-test-pkg::*internal-var*))
    (internal-test-pkg::internal-function val)))

;; Function that uses internal symbols (should trigger anomaly)
(defun test-internal-reference (x)
  "This function references internal symbols of another package"
  (+ x (internal-test-pkg::internal-function 10)))

;; Macro using internal symbols (should trigger anomaly)
(defmacro with-internal-test (&body body)
  `(let ((value internal-test-pkg::*internal-var*))
     ,@body))
