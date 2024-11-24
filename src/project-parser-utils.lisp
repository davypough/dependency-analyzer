;;;; Filename: project-parser-utils.lisp
;;;
;;; Utility functions for the project parser, including project file collection
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


(defun find-project-files (project-name)
  "Get a list of all source files in an ASDF project."
  (let ((system (asdf:find-system project-name)))
    (unless system
      (error 'project-parse-error 
             :project-name project-name
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
