;;;; File: cycle-test.lisp
;;;; This file demonstrates a :complex-type-cycle that will
;;;; generate an anomaly when analyzed by the dependency analyzer.


;; :COMPLEX-TYPE-CYCLE ANOMALY (cross-package)

;;;; Define three packages
(defpackage :cycle-pkg-a
  (:use :cl)
  (:export :class-a))

(defpackage :cycle-pkg-b
  (:use :cl)
  (:export :class-b))

(defpackage :cycle-pkg-c
  (:use :cl)
  (:export :class-c))

;;;; Define classes with circular dependencies across packages

(in-package :cycle-pkg-a)

(defclass class-a ()
  ((b-ref :accessor a-b-ref
          :initarg :b-ref
          :type cycle-pkg-b:class-b)))

(in-package :cycle-pkg-b)

(defclass class-b ()
  ((c-ref :accessor b-c-ref
          :initarg :c-ref
          :type cycle-pkg-c:class-c)))

(in-package :cycle-pkg-c)

(defclass class-c ()
  ((a-ref :accessor c-a-ref
          :initarg :a-ref
          :type cycle-pkg-a:class-a)))
