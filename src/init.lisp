;;;; Filename: init.lisp
;;;
;;; Initialization for the dependency analyzer.
;;; Defines the main package with the minimal public API needed by users.
;;; Includes basic utilities.


(in-package #:cl-user)


(defpackage #:dependency-analyzer
  (:use #:cl #:alexandria)
  (:nicknames #:dep)
  (:export #:analyze #:pushit
     ;; Development
     #:prt
     #:delete-fasl
     #:dep))


(in-package :dep)


(defparameter *current-tracker* nil
  "The currently active dependency tracker instance.")


;;; Development utilities


(defmacro prt (&rest forms)
  "Print the names & values of given forms (ie,variables or accessors).
   Can wrap around an expression, returning its value."
  `(let ((*package* (find-package "DEP")))
     (progn ,@(loop for form in forms
                    collect `(format t "~&  ~S => ~S~%" ',form ,form))
            ,@(last `,forms))))


(defmacro debug-let* (bindings &body body)
  "A LET* macro that prints each binding as it is evaluated,
  showing each variable and its computed value.
  Bindings are evaluated sequentially."
  (let ((debug-bindings
         (mapcar (lambda (binding)
                   (if (symbolp binding)
                       binding  ; leave lone symbols unchanged
                     (destructuring-bind (var init) binding
                       (list var
                             `(let ((result ,init)
                                    (*package* (find-package "DEP")))
                                (format t "~S => ~S~%" ',var result)
                                result)))))
                 bindings)))
    `(let* ,debug-bindings ,@body)))


(defmacro debug-let (bindings &body body)
  "Alias for DEBUG-LET* that provides the same sequential binding behavior."
  `(debug-let* ,bindings ,@body))


(defun flatten-directories (input-directories output-directory &optional (filespecs '("*.lisp")))
  "Collect all files matching FILESPECS from INPUT-DIRECTORIES and their subdirectories,
   and copy them into a new directory named 'flattened-files' in the current working directory.
   INPUT-DIRECTORIES can be a single directory or a list of directories.
   FILESPECS can be either a single string filespec or a list of filespecs."
  (let* ((input-dirs (if (listp input-directories) input-directories (list input-directories)))
         (new-dir (uiop:ensure-directory-pathname
                   (make-pathname :name "flattened-files"
                                  :defaults output-directory)))
         (absolute-new-dir (uiop:native-namestring new-dir))
         ;; Ensure filespecs is a list
         (filespec-list (if (listp filespecs) filespecs (list filespecs))))
    ;; Delete the existing new directory if it exists
    (when (uiop:directory-exists-p new-dir)
      (uiop:delete-directory-tree new-dir :validate t))
    ;; Ensure the new directory exists
    (ensure-directories-exist new-dir)
    ;; Collect and copy files for each filespec from each input directory
    (dolist (input-dir input-dirs)
      (let ((input-dir-path (uiop:ensure-directory-pathname 
                             (uiop:native-namestring input-dir))))
        (dolist (filespec filespec-list)
          (let ((files (uiop:directory-files input-dir-path 
                                             (make-pathname :name :wild 
                                                            :type (pathname-type filespec)
                                                            :directory '(:relative :wild-inferiors)))))
            ;; Copy files to the new directory
            (dolist (file files)
              (let ((dest-file (merge-pathnames (file-namestring file) new-dir)))
                ;; Ensure the directory for the destination file exists
                (ensure-directories-exist dest-file)
                ;; Copy the file
                (uiop:copy-file file dest-file)))))))
    ;; Return the path of the new directory
    absolute-new-dir))


(defun flat ()
  "Collects all the files for uploading to Claude."
  (let ((in-dir1 "D:/quicklisp/local-projects/dependency-analyzer/")
        (in-dir2 "D:/quicklisp/local-projects/test-project/")
        (out-dir "D:/Users Data/Dave/Desktop/"))
    (flatten-directories (list in-dir1 in-dir2) out-dir '("*.lisp" "*.asd" "*.log" "*.text" "*.json" "*.dot"))))


(defun delete-fasl ()
  "Delete FASL files for the dependency-analyzer system from the SBCL Windows cache directory."
  (let ((cache-dir "C:\\Users\\user\\AppData\\Local\\cache\\common-lisp\\sbcl-2.4.9-win-x64\\D\\quicklisp\\local-projects\\dependency-analyzer\\src\\"))
    (let ((fasl-files (directory (merge-pathnames "*.fasl" cache-dir))))
      (dolist (file fasl-files t)
        (ignore-errors
          (delete-file file))))))


(defun delete-project-fasls (system-name)
  "Delete FASL files for SYSTEM-NAME by:
   1. Finding the system's source module
   2. Translating its path to the FASL cache location
   3. Deleting all FASL files in that directory"
  (let* ((sys (asdf:find-system system-name))
         (source-module (first (asdf:component-children sys)))
         (source-dir (when (typep source-module 'asdf:module)
                      (asdf:component-pathname source-module)))
         (fasl-dir (when source-dir 
                    (asdf:apply-output-translations source-dir))))
    (if fasl-dir
        (let ((fasl-files (directory (make-pathname :defaults fasl-dir
                                                   :name :wild
                                                   :type "fasl"))))
          (format t "~&FASL directory: ~A~%" fasl-dir)
          (if fasl-files
              (dolist (fasl fasl-files t)
                (format t "~&Deleting: ~A~%" fasl)
                (delete-file fasl))
              (format t "~&No FASL files found.~%")))
        (format t "~&Could not determine FASL directory for system ~A~%" 
                system-name))))


(defun dep ()
  "Fresh reloads the dependency-analyzer system to continue testing."
  ;(asdf:load-system :test-project)
  (unwind-protect 
    (progn (delete-fasl)
           (ql:quickload :dependency-analyzer :verbose t)
           (funcall (symbol-function (read-from-string "DEP:ANALYZE"))
                    "d:/quicklisp/local-projects/test-project/src"))
                    ;:test-project-package))
    (in-package :dep)))


(defgeneric show (object &rest rest)
  (:documentation "Displays an object in a user-friendly format."))


(defmethod show ((table hash-table) &key (sort-by 'key))
  "Displays a hash table line-by-line, sorted either by key or val."
  (declare (type hash-table table))
  (when (= (hash-table-count table) 0)
    (format t "~&~A Empty~%" table)
    (return-from show))
  (let (alist)
    (maphash
      (lambda (key val)
        (push (cons (princ-to-string key) (prin1-to-string val))
          alist))
      table)
    (setf alist 
      (sort (copy-list alist) #'string< :key (ecase sort-by (key #'car) (val #'cdr))))
    (loop for (key . val) in alist
      do (format t "~%~A ->~%~A~2%" key val)))  ;"~%~A ->~%~10T ~A~2%"
  t)


(defmethod show ((fn function) &rest rest)
  (declare (ignore rest))
  (format t "~&~A~%" (function-lambda-expression fn))
  t)


(defmethod show ((lst list) &rest rest)
  (declare (ignore rest))
  (format t "~&~A~%" lst))


(defmethod show ((object t) &rest rest)
  "Prints any basic lisp object."
  (declare (ignore rest))
  (format t "~&~S~%" object)
  t)

