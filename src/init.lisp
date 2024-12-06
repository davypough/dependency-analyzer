;;;; Filename: init.lisp
;;;
;;; Initialization for the dependency analyzer.
;;; Defines the main package with the minimal public API needed by users.
;;; Includes basic utilities.


(in-package #:cl-user)


(defpackage #:dependency-analyzer
  (:use #:cl #:alexandria)
  (:nicknames #:dep)
  (:export 
    ;; Main analysis function
    #:analyze
    ;; Report generation
    #:report  
    #:generate-report
    ;; Cycle detection
    #:get-package-cycles
    #:get-project-cycles  
    #:get-file-cycles
    ;; Development
    #:prt
    #:delete-fasl
    #:dep
    #:defs
    #:refs
    #:anoms))


(in-package :dep)


(defparameter *current-tracker* nil
  "The currently active dependency tracker instance.")


;;; Development utilities


(defmacro prt (&rest forms)
  "Print the names & values of given forms (ie,variables or accessors).
   Can wrap around an expression, returning its value."
  `(progn ,@(loop for form in forms
              collect `(format t "~&  ~S => ~S~%" ',form ,form))
          ,@(last `,forms)))


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
    (flatten-directories (list in-dir1 in-dir2) out-dir '("*.lisp" "*.asd"))))


(defun delete-fasl ()
  "Delete FASL files for the dependency-analyzer system from the SBCL Windows cache directory."
  (let ((cache-dir "C:\\Users\\user\\AppData\\Local\\cache\\common-lisp\\sbcl-2.4.9-win-x64\\D\\quicklisp\\local-projects\\dependency-analyzer\\src\\"))
    (let ((fasl-files (directory (merge-pathnames "*.fasl" cache-dir))))
      (dolist (file fasl-files t)
        (ignore-errors
          (delete-file file))))))


(defun dep ()
  "Fresh reloads the dependency-analyzer system to continue testing."
  (delete-fasl)
  (ql:quickload :dependency-analyzer)
  (analyze "d:/quicklisp/local-projects/test-project/source"))


(defun defs ()
  "Pretty print all definition records from current tracker in symbol name order."
  (let ((defs nil))
    (maphash (lambda (key def)
               (declare (ignore key))
               (push def defs))
             (slot-value *current-tracker* 'definitions))
    (dolist (def (sort defs #'string< :key #'definition.name))
      (format t "~%Name: ~S~%  Type: ~S~%  File: ~A~%  Package: ~S~%  Exported: ~A~%"
              (definition.name def)
              (definition.type def)
              (project-pathname (definition.file def))
              (definition.package def)
              (definition.exported-p def)))))


(defun refs ()
  "Pretty print all reference records from current tracker in symbol name order."
  (let ((refs nil))
    (maphash (lambda (key ref-list)
               (declare (ignore key))
               (dolist (ref ref-list)
                 (push ref refs)))  
             (slot-value *current-tracker* 'references))
    (dolist (ref (sort refs #'string< :key #'reference.symbol))
      (format t "~%Symbol: ~A~%  Context: ~S~%  Type: ~A~%  File: ~A~%  Package: ~S~%  Visibility: ~A~%  Definition: ~A~%~%"
              (reference.symbol ref)
              (reference.context ref)
              (reference.type ref) 
              (project-pathname (reference.file ref))
              (reference.package ref)
              (reference.visibility ref)
              (project-pathname (definition.file (reference.definition ref)))))))


(defun anoms ()
  "Pretty print all anomaly records from current tracker in type order."
  (let ((anomaly-list nil))
    (maphash (lambda (type anomalies)
               (declare (ignore type))
               (dolist (anomaly anomalies)
                 (push anomaly anomaly-list)))
             (anomalies *current-tracker*))
    (dolist (anomaly (sort anomaly-list #'string< :key #'anomaly.type))
      (format t "~&Type: ~S~%  Severity: ~A~%  Location: ~A~%  Description: ~A~%  Context: ~S~%~%"
              (anomaly.type anomaly)
              (anomaly.severity anomaly)
              (project-pathname (anomaly.location anomaly))
              (anomaly.description anomaly)
              (anomaly.context anomaly)))))


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
      do (format t "~&~A ->~10T ~A~%" key val)))
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
