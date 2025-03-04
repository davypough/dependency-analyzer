;;;; File: defs.lisp

;;;; Test file containing minimal examples of all Lisp definition forms
;;;; that can be processed by the dependency analyzer. Forms are grouped by their
;;;; definition type as specified in +valid-definition-types+.


;; First establish package context for remaining definitions
(defpackage #:user-pkg
  (:use :cl :alexandria))

(in-package #:user-pkg)


;; VARIABLE DEFINITIONS (:VARIABLE type)
(defvar *special1* 0)  ;standard variable definition
(defvar *special2* "Special variable without initialization")
(defparameter *param1* 100 "Parameter must have initial value")
(defconstant +const1+ 1 "Constants use plus signs")
(setf (symbol-value 'var1) 42)  ;symbol-value definition
(defparameter *param2* (defparameter *param3* 0) "Embedded definition")


;; PACKAGE DEFINITIONS (:PACKAGE type)
(defpackage #:test1-pkg
  (:nicknames :t1pkg)
  (:use :cl)
  (:export #:var1 #:param1 #:+const1+ #:*special1*))
(when (find-package :test2-pkg)
  (delete-package :test2-pkg))
(make-package :test2-pkg
  :nicknames '("T2PKG")
  :use '("COMMON-LISP"))
;; Package that demonstrates import-from relationships
(defpackage #:test3-pkg
  (:nicknames :t3pkg)
  (:use :cl :alexandria)
  (:documentation "Demonstrates import-from packages")
  (:import-from #:test1-pkg #:var1 #:param1)
  (:export #:var1 #:process-data))
;; Package that demonstrates shadowing and conflicts
(defpackage #:test4-pkg
  (:use :cl :test1-pkg)
  (:shadow #:var1)  ; Shadow an imported symbol
  (:shadowing-import-from :alexandria #:when-let)  ; Shadowing import
  (:export #:shadow-test))
;; Package with internal symbol organization
(defpackage #:test5-pkg
  (:use :cl)
  (:intern #:internal-only)
  (:export #:public-api)
  (:documentation "Package for testing comprehensive symbol visibility analysis"))


;; FUNCTION DEFINITIONS (:FUNCTION type)
(defun fn1 (x) x)
(defun (setf fn1) (new-val x) 
  (setf (car x) new-val))
(defun fn2a (x &optional y)
  (declare (type number x))
  (alexandria:proper-list-p (list x y)))
(defun fn2b (x &key z)
  (declare (type number x))
  (list x z))
(define-method-combination my-comb (&key (order :most-specific-first))
  ((methods *))
  `(progn
     ,(case order
        (:most-specific-first `(call-method ,(first methods)))
        (:most-specific-last `(call-method ,(car (last methods)))))))
(setf (symbol-function 'fn3) #'(lambda (x) (* x 2)))


;; MACRO DEFINITIONS (:MACRO type)
(defmacro with-test ((var) &body body)
  `(let ((,var t))
     ,@body))
(define-modify-macro incf-by (n) +)
(defun my-plus (a b)
  (+ a b))
(define-compiler-macro my-plus (&whole form a b)
  (if (and (numberp a) (numberp b))
    (+ a b)
    form))
(setf (macro-function 'my-macro)
      #'(lambda (form env)
          (declare (ignore env))
          `(list ,(second form))))


;; STRUCTURE, CLASS, AND CONDITION DEFINITIONS (:STRUCTURE/CLASS/CONDITION type)
(defstruct (point (:constructor make-point (x y)))
  (x 0 :type number)
  (y 0 :type number))
(defclass named-point ()
  ((name :initarg :name :accessor point-name)
   (u :initarg :u :accessor point-u)
   (v :initarg :v :accessor point-v)))
(define-condition invalid-point (error)
  ((reason :initarg :reason :reader point-error-reason)
   (value :initarg :value :reader point-error-value)))


;; GENERIC FUNCTION DEFINITIONS (:GENERIC-FUNCTION type)
(defgeneric gf1 (x)
  (:documentation "Basic generic function"))
(defgeneric gf2 (x y)
  (:documentation "Generic function with multiple arguments")
  (:method-combination standard)
  (:generic-function-class standard-generic-function))
(defgeneric (setf gf3) (new-value obj)
  (:documentation "Setf generic function"))


;; METHOD DEFINITIONS (:METHOD type)
(defmethod gf1 ((x number))
  (* x 2))
(defmethod gf1 ((x string))
  (concatenate 'string x x))
(defmethod gf1 ((x cons))
  (list 'cons-processed x))
(defmethod gf1 ((x named-point))
  (list 'named-point (point-name x) (point-u x) (point-v x)))
(defmethod gf2 ((x number) y)
  (list x y))
(defmethod gf2 :before ((x number) y)
  (check-type y number))
(defmethod (setf gf3) (new-value (obj cons))
  ;; Implementation that defines what happens when setting gf3 of a cons
  (setf (car obj) new-value)
  new-value)
(defmethod (setf gf3) :around (new-value (obj cons))
  (if (numberp new-value)
    (call-next-method)
    (error "Invalid value")))
;; Define a method with an EQL specializer on *special1*
(defmethod process-var ((value (eql '*special1*)) &optional (limit *param1*))
  "Method specialized to handle specifically the *special1* value"
  (list value limit))
(defmethod test-method ((x string)) x)



;; SYMBOL MACRO DEFINITIONS (:SYMBOL-MACRO type)
(define-symbol-macro today 
    (get-universal-time))
(define-symbol-macro pi/2 
    (/ pi 2))
(define-symbol-macro empty-list 
    nil)


;; TYPE DEFINITIONS (:DEFTYPE type)
(deftype pos-integer ()
  '(integer 1 *))
(deftype sized-vector (element-type size)
  `(vector ,element-type ,size))
(deftype string-or-symbol ()
  '(or string symbol))


;; EMBEDDED DEFINITION EXAMPLES (poor style but valid)
;; First establish some special variables
(defvar *embedded-count* 0 "Counter for embedded functions")
(defvar *last-embedded-point* nil "Storage for last point object")
;; Container function with nested definitions
(defun setup-embedded-functions ()
  ;; Local counter management
  (defun reset-embedded-count ()
    (setf *embedded-count* 0))
  (defun get-embedded-count ()
    *embedded-count*)
  (values #'reset-embedded-count #'get-embedded-count))
;; Class with embedded method definition
(defclass monitored-point (named-point)
  ((timestamp :initform (get-universal-time) 
              :reader point-timestamp)))
(defmethod shared-initialize :after ((point monitored-point) slot-names &key)
  (setf *last-embedded-point* point)
  (unless (fboundp 'get-last-point-timestamp)
    (defun get-last-point-timestamp ()
      (when *last-embedded-point*
        (point-timestamp *last-embedded-point*)))))