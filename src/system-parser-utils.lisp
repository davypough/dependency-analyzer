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


(defun collect-directory-files (directory patterns)
  "Collect all files matching patterns in directory and its subdirectories.
   DIRECTORY - Root directory to search
   PATTERNS - String or list of strings with file patterns (e.g. \"*.lisp\")"
  (labels ((collect-for-pattern (dir pattern)
             (let ((files nil))
               (dolist (file (directory (merge-pathnames pattern dir)))
                 (push file files))
               (dolist (subdir (directory (merge-pathnames "*.*" dir)))
                 (when (directory-pathname-p subdir)
                   (setf files (nconc files (collect-for-pattern subdir pattern)))))
               files)))
    (let ((files nil))
      (dolist (pattern (if (listp patterns) patterns (list patterns)))
        (setf files (nunion files 
                           (collect-for-pattern (pathname directory) pattern)
                           :test #'equal)))
      (sort files #'string< :key #'namestring))))