;;;; Filename: data-references.lisp

;;; Standard test references used across multiple test suites.
;;; Demonstrates various ways of creating dependencies between packages,
;;; files, and symbols.

(in-package #:cl-user)


(defpackage #:user-direct-package
  (:use #:cl #:user-core-package))


(defpackage #:user-combined-import-package
  (:use #:cl)
  (:import-from #:user-core-package 
                #:complex-function
                #:internal-condition)
  (:import-from #:user-utils-package
                #:utility-function
                #:format-value))


(in-package #:user-direct-package)


(defun direct-function-caller ()
  "Function demonstrating direct package dependencies."
  (let ((result (exported-function 10 exported-variable)))
    (complex-function result :y 20)))


(exported-macro generated-function
  (format t "Testing macro expansion dependencies"))


(in-package #:user-combined-import-package)


(defun import-function-caller (x)
  "Function demonstrating selective symbol importing."
  (handler-case
      (let ((base (user-core-package:exported-function 
                    x user-core-package:exported-variable)))
        (user-utils-package:format-value 
          (user-utils-package:utility-function base)))
    (internal-condition (c)
      (format nil "Error in processing: ~A" c))))