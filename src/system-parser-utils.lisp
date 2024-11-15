;;;; Filename: system-parser-utils.lisp
;;;
;;; Utility functions for the system parser, including system file collection
;;; and directory path handling.


(in-package #:dep)


(defun directory-pathname-p (pathname)
  "Returns true if the pathname represents a directory.
   A directory pathname is one whose name component is nil."
  (and pathname
       (let ((name (pathname-name pathname)))
         (or (null name)
             (and (stringp name)
                  (string= name ""))))))


(defun find-system-files (system-name)
  "Get a list of all source files in an ASDF system."
  (let ((system (asdf:find-system system-name)))
    (unless system
      (error 'system-parse-error 
             :system-name system-name
             :reason "System not found"))
    (collect-source-files system)))


(defmethod collect-source-files ((component asdf:component))
  "Recursively collect all source files from an ASDF component."
  (typecase component
    (asdf:cl-source-file
      (list (asdf:component-pathname component)))
    (asdf:module
      (mapcan #'collect-source-files 
              (asdf:component-children component)))
    (t
      nil)))