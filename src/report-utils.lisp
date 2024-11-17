;;;; Filename: report-utils.lisp
;;;
;;; Utility functions for report generation and formatting.
;;; Contains helper functions for path handling, tree building,
;;; and other report-related operations.


(in-package #:dep)


;;; Error Conditions

(define-condition report-error (analyzer-error)
  ((path 
    :initarg :path 
    :reader report-path
    :documentation "Path that caused the error")
   (reason 
    :initarg :reason 
    :reader reason
    :documentation "Underlying cause of the error"))
  (:documentation 
   "Signaled when a report cannot be generated or saved.")
  (:report (lambda (condition stream)
             (format stream "Error generating report for path ~A: ~A"
                     (report-path condition)
                     (reason condition)))))


(defun format-anomalies (stream tracker)
  "Format all anomalies in a consistent way, grouped by type and severity."
  (let ((found-anomalies nil))
    ;; First get all anomaly types
    (maphash (lambda (type anomaly-list)
               (when anomaly-list
                 (setf found-anomalies t)
                 (format stream "~&~%~A Anomalies:~%" 
                         (string-capitalize (symbol-name type)))
                 ;; Group by severity within each type
                 (loop for severity in '(:error :warning :info)
                       for severity-anomalies = (remove severity anomaly-list 
                                                      :key #'anomaly.severity 
                                                      :test-not #'eq)
                       when severity-anomalies do
                         (format stream "  ~A:~%" (string-upcase (symbol-name severity)))
                         (dolist (a (reverse severity-anomalies)) ; Reverse to show in detection order
                           (format stream "    ~A~%" (anomaly.description a))
                           (format stream "      Location: ~A~%" (anomaly.location a))))))
             (anomalies tracker))
    ;; Return whether we found any anomalies
    found-anomalies))


(defun format-specializers (specializers)
  "Format method specializers in a readable way.
   Examples:
   - (t t) -> T T
   - ((eql :square)) -> (EQL :SQUARE)
   - (string number) -> STRING NUMBER"
  (let ((result (format nil "~{~A~^ ~}"
                       (mapcar (lambda (spec)
                               (etypecase spec
                                 ((eql t) 
                                  "T")
                                 (cons 
                                  (if (eq (car spec) 'eql)
                                      (format nil "(EQL ~A)" (cadr spec))
                                      (format nil "~A" spec)))
                                 (symbol
                                  (format nil "~A" spec))))
                             specializers))))
    (if (string= result "") "T" result)))


(defun format-qualifiers (qualifiers)
  "Format method qualifiers in a readable way.
   Examples:
   - (:before) -> :BEFORE
   - (:before :around) -> :BEFORE :AROUND
   - nil -> \"\""
  (typecase qualifiers
    (null "")
    (list (format nil "~{~A~^ ~}" 
                  (mapcar (lambda (q)
                          (typecase q
                            (keyword (format nil "~A" q))
                            (symbol (format nil "~A" q))
                            (t (princ-to-string q))))
                         qualifiers)))))


(defun format-method-signature (name qualifiers specializers)
  "Format a complete method signature.
   Examples:
   - (name () (t t)) -> name (T T)
   - (name (:before) (string t)) -> name :BEFORE (STRING T)
   - (name () ((eql :square))) -> name ((EQL :SQUARE))
   - (name (:around :writer) (my-class t)) -> name :AROUND :WRITER (MY-CLASS T)"
  (let ((qual-str (format-qualifiers qualifiers))
        (spec-str (format-specializers specializers)))
    (cond ((and (string= qual-str "") (string= spec-str ""))
           (format nil "~A" name))
          ((string= qual-str "")
           (format nil "~A (~A)" name spec-str))
          (t
           (format nil "~A ~A (~A)" name qual-str spec-str)))))


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


(defun build-file-dependency-tree (tracker)
  "Build a tree structure representing file dependencies."
  (let ((nodes (make-hash-table :test 'equal))
        (roots nil)
        (is-dependency (make-hash-table :test 'equal)))  ; Track files that are dependencies
    
    ;; First create nodes for all files
    (maphash (lambda (file definitions)
               (declare (ignore definitions))
               (setf (gethash file nodes)
                     (list :name (source-file-name file)
                           :full-name file
                           :children nil)))
             (slot-value tracker 'file-map))
    
    ;; Then establish dependencies and track which files are dependencies
    (maphash (lambda (file node)
               ;; Get files this one depends on
               (dolist (dep (file-dependencies tracker file))
                 (when-let ((dep-node (gethash dep nodes)))
                   (pushnew dep-node (getf node :children)
                           :test #'equal
                           :key (lambda (n) (getf n :full-name)))
                   ;; Mark this dep as being a dependency
                   (setf (gethash dep is-dependency) t))))
             nodes)
    
    ;; Find root nodes (files that aren't dependencies of any other file)
    (maphash (lambda (file node)
               (unless (gethash file is-dependency)
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
  "Build JSON structure for file dependencies, showing forward dependencies and their references."
  (let ((result (make-hash-table :test 'equal))
        (is-dependency (make-hash-table :test 'equal)))
    
    ;; First pass: identify which files are dependencies
    (maphash (lambda (file definitions)
               (declare (ignore definitions))
               (dolist (dep (file-dependencies tracker file))
                 (setf (gethash dep is-dependency) t)))
             (slot-value tracker 'file-map))
    
    ;; Second pass: only include non-dependency files as top-level entries
    (maphash (lambda (file definitions)
               (declare (ignore definitions))
               (unless (gethash file is-dependency)
                 (let* ((file-str (pathname-to-string file))
                        (deps (file-dependencies tracker file)))
                   (when deps  ; Only include files that have dependencies
                     (let ((deps-with-refs 
                            (mapcar (lambda (dep)
                                    (alexandria:alist-hash-table
                                     `(("file" . ,(pathname-to-string dep))
                                       ;; Add "References: " prefix to maintain consistency
                                       ("dependencies" . ,(format nil "References: ~{~A~^, ~}"
                                                               (collect-file-references 
                                                                tracker file dep))))
                                     :test 'equal))
                                  deps)))
                       (setf (gethash file-str result)
                             (alexandria:alist-hash-table
                              `(("depends_on" . ,deps-with-refs))
                              :test 'equal)))))))
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


(defmethod get-package-uses (&optional (tracker nil tracker-provided-p) package-name)
  "Get all packages that a given package uses."
  (let ((actual-tracker (if tracker-provided-p tracker (ensure-tracker))))
    (gethash package-name (slot-value actual-tracker 'package-uses))))


(defmethod get-package-exports (&optional (tracker nil tracker-provided-p) package-name)
  "Get all symbols exported by a package."
  (let ((actual-tracker (if tracker-provided-p tracker (ensure-tracker))))
    (mapcar #'(lambda (sym)
                (let ((pkg (find-package (string package-name))))
                  (if pkg
                      (intern (symbol-name sym) pkg)
                      sym)))
            (gethash (string package-name) (slot-value actual-tracker 'package-exports)))))


(defmethod file-dependencies (&optional (tracker nil tracker-provided-p) file)
  "Get all files that this file depends on.
   Returns a list of files that contain definitions used by this file.
   
   TRACKER - Optional dependency tracker instance (defaults to *current-tracker*)
   FILE - The source file to analyze dependencies for
   
   A file A depends on file B if A contains references to symbols defined in B."
  (let* ((actual-tracker (if tracker-provided-p tracker (ensure-tracker)))
         (deps ())
         (refs-seen (make-hash-table :test 'equal)))
    ;; First collect all references in this file
    (maphash (lambda (key refs)
               (declare (ignore key))
               (dolist (ref refs)
                 (when (equal (reference.file ref) file)
                   ;; Track each referenced symbol with its package
                   (let* ((sym (reference.symbol ref))
                          (pkg (reference.package ref))
                          (key (make-tracking-key sym pkg)))
                     (setf (gethash key refs-seen) t)))))
             (slot-value actual-tracker 'references))
    
    ;; Then look up the definitions for each referenced symbol
    (maphash (lambda (key _)
               (declare (ignore _))
               (let ((def (gethash key (slot-value actual-tracker 'definitions))))
                 (when def
                   (let ((def-file (definition.file def)))
                     (unless (equal def-file file)
                       (pushnew def-file deps :test #'equal))))))
             refs-seen)
    
    ;; Return sorted list of dependencies
    (sort deps #'string< 
          :key (lambda (path)
                 (if path 
                     (namestring path)
                     "")))))
