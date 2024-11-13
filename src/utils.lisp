;;;; Filename: utils.lisp

;;; Utility functions for the dependency analyzer.
;;; This module will simulate providing common user utilities for testing purposes.


(in-package #:dep)


(defun directory-pathname-p (pathname)
  "Returns true if the pathname represents a directory.
   A directory pathname is one whose name component is nil."
  (and pathname
       (let ((name (pathname-name pathname)))
         (or (null name)
             (and (stringp name)
                  (string= name ""))))))