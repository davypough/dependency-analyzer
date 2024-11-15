;;;; Filename: report-utils.lisp
;;;
;;; Utility functions for report generation and formatting.
;;; Contains helper functions for path handling, tree building,
;;; and other report-related operations.


(in-package #:dep)


(defun pathname-to-string (pathname)
  "Convert a pathname to a string representation."
  (if pathname
      (namestring pathname)
      nil))


(defun simplify-path (pathname)
  "Convert a pathname to a simplified string representation."
  (if pathname
      (let ((name (enough-namestring pathname)))
        (if (char= (char name 0) #\/)
            (subseq name 1)
            name))
      nil))


(defun source-file-name (pathname)
  "Extract just the source file name from a pathname."
  (if pathname
      (file-namestring pathname)
      nil))


(defun string-to-dot-id (string)
  "Convert a string to a valid DOT graph identifier."
  (string-downcase
   (with-output-to-string (s)
     (loop for char across (or string "")
           do (case char
                ((#\Space #\/ #\\ #\. #\-)
                 (write-char #\_ s))
                (otherwise
                 (write-char char s)))))))


(defun abbreviate-path (path)
  "Create an abbreviated version of a path string."
  (let ((path-str (simplify-path path)))
    (if (> (length path-str) 50)
        (concatenate 'string "..." 
                    (subseq path-str (max 0 (- (length path-str) 47))))
        path-str)))


(defun ensure-directory-exists (pathname)
  "Ensure the directory component of pathname exists. Returns pathname if successful."
  (handler-case
      (let* ((namestring (namestring pathname))
             (dir (directory-namestring pathname))
             (normalized-dir (if (uiop:os-windows-p)
                               (uiop:native-namestring dir)
                               dir)))
        (format *error-output* "~&Attempting to access directory: ~A~%" normalized-dir)
        (ensure-directories-exist normalized-dir)
        pathname)
    (error (e)
      (error 'report-error
             :path pathname
             :reason (format nil "Could not access directory: ~A~%Error details: ~A" 
                           pathname e)))))


(defun verify-writable (pathname)
  "Verify the specified path is writable. Returns pathname if writable."
  (handler-case 
      (with-open-file (test pathname
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
        (write-char #\Space test)
        pathname)
    (error (e)
      (error 'report-error
             :path pathname
             :reason (format nil "Path is not writable: ~A" e)))))


#+ignore (defun build-file-dependency-tree (tracker)
  "Build a tree structure representing file dependencies."
  (let ((nodes (make-hash-table :test 'equal))
        (roots nil))
    ;; First pass: Create nodes for all files that are either defined in or referenced
    (maphash (lambda (file definitions)
               (declare (ignore definitions))
               ;; Create node for the defined file
               (setf (gethash file nodes)
                     (list :name (source-file-name file)
                           :full-name file
                           :children nil 
                           :parents nil))
               ;; Create nodes for all files that reference this one
               (dolist (dependent (file-dependents tracker file))
                 (unless (gethash dependent nodes)
                   (setf (gethash dependent nodes)
                         (list :name (source-file-name dependent)
                               :full-name dependent
                               :children nil
                               :parents nil)))))
             (slot-value tracker 'file-map))
    
    ;; Second pass: Connect dependencies based on dependencies and dependents
    (maphash (lambda (file node)
               ;; Connect based on file dependencies
               (dolist (dep (file-dependencies tracker file))
                 (when-let ((dep-node (gethash dep nodes)))
                   (pushnew node (getf dep-node :children)
                           :test #'equal
                           :key (lambda (n) (getf n :full-name)))
                   (pushnew dep-node (getf node :parents)
                           :test #'equal
                           :key (lambda (n) (getf n :full-name)))))
               ;; Connect based on file dependents
               (dolist (dep (file-dependents tracker file))
                 (when-let ((dep-node (gethash dep nodes)))
                   (pushnew dep-node (getf node :children)
                           :test #'equal
                           :key (lambda (n) (getf n :full-name)))
                   (pushnew node (getf dep-node :parents)
                           :test #'equal
                           :key (lambda (n) (getf n :full-name))))))
             nodes)
    
    ;; Find root nodes (files that are not depended on by any other files)
    (maphash (lambda (file node)
               (when (null (getf node :parents))
                 (push node roots)))
             nodes)
    (sort roots #'string< :key (lambda (node) (getf node :name)))))


(defun build-file-dependency-tree (tracker)
  "Build a tree structure representing file dependencies."
  (let ((nodes (make-hash-table :test 'equal))
        (roots nil))
    (maphash (lambda (file definitions)
               (declare (ignore definitions))
               (setf (gethash file nodes)
                     (list :name (source-file-name file)
                           :full-name file
                           :children nil 
                           :parents nil))
               (dolist (dependent (file-dependents tracker file))
                 (unless (gethash dependent nodes)
                   (setf (gethash dependent nodes)
                         (list :name (source-file-name dependent)
                               :full-name dependent
                               :children nil
                               :parents nil)))))
             (slot-value tracker 'file-map))
    
    (maphash (lambda (file node)
               (dolist (dep (file-dependencies tracker file))
                 (when-let ((dep-node (gethash dep nodes)))
                   (pushnew node (getf dep-node :children)
                           :test #'equal
                           :key (lambda (n) (getf n :full-name)))
                   (pushnew dep-node (getf node :parents)
                           :test #'equal
                           :key (lambda (n) (getf n :full-name)))))
               (dolist (dep (file-dependents tracker file))
                 (when-let ((dep-node (gethash dep nodes)))
                   (pushnew dep-node (getf node :children)
                           :test #'equal
                           :key (lambda (n) (getf n :full-name)))
                   (pushnew node (getf dep-node :parents)
                           :test #'equal
                           :key (lambda (n) (getf n :full-name))))))
             nodes)
    
    (maphash (lambda (file node)
               (when (null (getf node :parents))
                 (push node roots)))
             nodes)
    (sort roots #'string< :key (lambda (node) (getf node :name)))))


(defun build-package-dependency-tree (tracker)
  "Build a tree structure representing package dependencies.
   Returns two values:
   1. List of root package nodes
   2. List of circular dependencies"
  (let ((nodes (make-hash-table :test 'equal))
        (roots nil)
        (cycles nil))
    (maphash (lambda (pkg used-pkgs)
               (declare (ignore used-pkgs))
               (setf (gethash pkg nodes)
                     (list :name pkg
                           :children nil
                           :parents nil))
               (dolist (used (get-package-uses tracker pkg))
                 (unless (gethash used nodes)
                   (setf (gethash used nodes)
                         (list :name used
                               :children nil
                               :parents nil)))))
             (slot-value tracker 'package-uses))
    
    (maphash (lambda (pkg used-pkgs)
               (when (member "COMMON-LISP" used-pkgs :test #'string=)
                 (unless (gethash "COMMON-LISP" nodes)
                   (setf (gethash "COMMON-LISP" nodes)
                         (list :name "COMMON-LISP"
                               :children nil
                               :parents nil)))))
             (slot-value tracker 'package-uses))
    
    (maphash (lambda (pkg used-pkgs)
               (let ((pkg-node (gethash pkg nodes)))
                 (dolist (used used-pkgs)
                   (when-let ((used-node (gethash used nodes)))
                     (when (member pkg (get-package-uses tracker used))
                       (pushnew (format nil "~A ↔ ~A" pkg used)
                               cycles :test #'string=))
                     (unless (member pkg (get-package-uses tracker used))
                       (pushnew pkg-node (getf used-node :children)
                               :test #'equal
                               :key (lambda (n) (getf n :name)))
                       (pushnew used-node (getf pkg-node :parents)
                               :test #'equal
                               :key (lambda (n) (getf n :name))))))))
             (slot-value tracker 'package-uses))
    
    (maphash (lambda (pkg node)
               (when (null (getf node :parents))
                 (push node roots)))
             nodes)
    (values (sort roots #'string< :key (lambda (node) (getf node :name)))
            (sort cycles #'string<))))


(defun print-ascii-tree (stream roots &optional (prefix "") (last-child-p nil))
  "Print an ASCII representation of a dependency tree."
  (dolist (node (butlast roots))
    (format stream "~&~A├── ~A~%" prefix (getf node :name))
    (print-ascii-tree stream 
                      (sort (getf node :children) #'string< 
                            :key (lambda (n) (getf n :name)))
                      (concatenate 'string prefix "│   ")
                      nil))
  (when-let ((last-node (car (last roots))))
    (format stream "~&~A└── ~A~%" prefix (getf last-node :name))
    (print-ascii-tree stream 
                      (sort (getf last-node :children) #'string< 
                            :key (lambda (n) (getf n :name)))
                      (concatenate 'string prefix "    ")
                      t)))


(defun build-file-dependency-json (tracker)
  "Build JSON structure for file dependencies."
  (let ((result (make-hash-table :test 'equal)))
    (maphash (lambda (file definitions)
               (declare (ignore definitions))
               (let* ((file-str (pathname-to-string file))
                      (deps (remove nil (mapcar #'pathname-to-string 
                                              (file-dependencies tracker file))))
                      (required-by (remove nil (mapcar #'pathname-to-string 
                                                     (file-dependents tracker file)))))
                 (when (or deps required-by)
                   (setf (gethash file-str result)
                         (alexandria:alist-hash-table
                          (remove nil
                                 `(,@(when deps `(("depends_on" . ,deps)))
                                   ,@(when required-by `(("required_by" . ,required-by)))))
                          :test 'equal)))))
             (slot-value tracker 'file-map))
    result))


(defun build-package-dependency-json (tracker)
  "Build JSON structure for package dependencies."
  (let ((result (make-hash-table :test 'equal)))
    (maphash (lambda (pkg used-pkgs)
               (let ((exports (mapcar #'symbol-name (get-package-exports tracker pkg))))
                 (when (or used-pkgs exports)
                   (setf (gethash pkg result)
                         (alexandria:alist-hash-table
                          (remove nil
                                 `(,@(when used-pkgs `(("uses" . ,used-pkgs)))
                                   ,@(when exports `(("exports" . ,exports)))))
                          :test 'equal)))))
             (slot-value tracker 'package-uses))
    result))