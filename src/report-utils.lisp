;;;; Filename: report-utils.lisp

;;; Utility functions for report generation and formatting.
;;; Contains helper functions for path handling, tree building,
;;; and other report-related operations.


(in-package #:dep)


(defparameter *print-width* 80
  "Default maximum width for formatted report output.")


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
   Includes project packages, their runtime dependencies, and system dependencies.
   Returns (values roots cycles)."
  (let ((nodes (make-hash-table :test 'equal))
        (roots nil)
        (cycles nil))
    
    ;; First add CL package as it's always a root
    (setf (gethash "COMMON-LISP" nodes)
          (list :name "COMMON-LISP"
                :children nil
                :parents nil))
    
    ;; Add nodes for packages defined in our source
    (maphash (lambda (pkg-name def-form)
               (declare (ignore def-form))
               (when-let* ((pkg (find-package pkg-name))
                          (primary-name (package-name pkg)))
                 (setf (gethash primary-name nodes)
                       (list :name primary-name
                            :children nil
                            :parents nil))))
             (slot-value tracker 'defined-packages))
    
    ;; Add nodes for all runtime dependencies
    (maphash (lambda (pkg-name _)
               (declare (ignore _))
               (when-let ((pkg (find-package pkg-name)))
                 ;; Get full package dependency closure
                 (dolist (dep-pkg (package-use-list pkg))
                   (let ((dep-name (package-name dep-pkg)))
                     (unless (gethash dep-name nodes)
                       (setf (gethash dep-name nodes)
                             (list :name dep-name
                                  :children nil
                                  :parents nil)))))))
             (slot-value tracker 'defined-packages))
    
    ;; Establish parent-child relationships
    (maphash (lambda (_ node)
               (declare (ignore _))
               (let* ((pkg-name (getf node :name))
                      (pkg (find-package pkg-name)))
                 (when pkg  
                   (dolist (used-pkg (package-use-list pkg))
                     (let* ((used-name (package-name used-pkg))
                            (used-node (gethash used-name nodes)))
                       (when used-node
                         ;; Check for cycles
                         (when (member pkg (package-use-list used-pkg))
                           (pushnew (format nil "~A ↔ ~A" pkg-name used-name)
                                   cycles :test #'string=))
                         ;; Record dependency
                         (unless (member pkg (package-use-list used-pkg))
                           (pushnew used-node (getf node :parents)
                                   :test #'equal
                                   :key (lambda (n) (getf n :name))))))))))
             nodes)
    
    ;; Find root packages - only COMMON-LISP and packages with no other parents
    (maphash (lambda (_ node)
               (declare (ignore _))
               (let ((parents (getf node :parents))
                     (name (getf node :name)))
                 ;; A package is a root if it's COMMON-LISP or has no parents
                 (when (or (string= name "COMMON-LISP")
                          (null parents))
                   (push node roots))))
             nodes)
    
    ;; Calculate children relationships
    (maphash (lambda (_ node)
               (declare (ignore _))
               (dolist (parent (getf node :parents))
                 (pushnew node (getf parent :children)
                         :test #'equal
                         :key (lambda (n) (getf n :name)))))
             nodes)
    
    (values (sort roots #'string< :key (lambda (node) (getf node :name)))
            (sort cycles #'string<))))


(defun print-ascii-tree (stream roots &optional (prefix "") (last-child-p nil))
  "Print an ASCII representation of a dependency tree."
  (declare (ignore last-child-p))
  (dolist (node (butlast roots))
    (format stream "~&~A-- ~A~%" prefix (getf node :name))
    (print-ascii-tree stream 
                      (sort (getf node :children) #'string< 
                            :key (lambda (n) (getf n :name)))
                      (concatenate 'string prefix "   ")
                      nil))
  (when-let ((last-node (car (last roots))))
    (format stream "~&~A-- ~A~%" prefix (getf last-node :name))
    (print-ascii-tree stream 
                      (sort (getf last-node :children) #'string< 
                            :key (lambda (n) (getf n :name)))
                      (concatenate 'string prefix "   ")
                      t)))


(defun build-file-dependency-json (tracker)
 "Build JSON structure for file dependencies."
 (let ((result (make-hash-table :test 'equal))
       (is-dependency (make-hash-table :test 'equal)))
   
   ;; First pass unchanged
   (maphash (lambda (file def-list)
              (declare (ignore def-list))
              (dolist (dep (file-dependencies tracker file))
                (setf (gethash dep is-dependency) t)))
            (slot-value tracker 'file-map))
   
   ;; Second pass enhanced with method reference details
   (maphash (lambda (file def-list)
              (declare (ignore def-list)) 
              (unless (gethash file is-dependency)
                (when-let ((file-str (project-pathname file))
                           (deps (file-dependencies tracker file)))
                  (let ((deps-with-refs 
                          (mapcar (lambda (dep)
                                   (let ((refs (collect-file-references tracker file dep)))
                                     (alexandria:alist-hash-table
                                      `(("file" . ,(project-pathname dep))
                                        ("references" . 
                                          ,(mapcar (lambda (ref)
                                                    (alexandria:alist-hash-table
                                                      `(("name" . ,(symbol-name (reference.name ref)))
                                                        ("qualifiers" . ,(reference.qualifiers ref))
                                                        ("arguments" . ,(reference.arguments ref)))
                                                      :test 'equal))
                                                  refs)))
                                      :test 'equal)))
                                 deps)))
                    (setf (gethash file-str result)
                      (alist-hash-table `(("depends_on" . ,deps-with-refs)) :test #'equal))))))
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


(defun build-package-metrics-json (tracker)
  ;; Current version only includes package metrics
  ;; Need to add:
  (let ((result (make-hash-table :test 'equal)))
    (maphash (lambda (pkg metrics)
               (setf (gethash pkg result)
                     (alexandria:alist-hash-table
                      `(("local_symbols" . ,(getf metrics :local-symbols))
                        ("inherited_symbols" . ,(getf metrics :inherited-symbols))
                        ("used_packages" . ,(getf metrics :used-packages))
                        ("exported_symbols" . ,(getf metrics :exported-symbols))
                        ("export_users" . ,(getf metrics :export-users))
                        ("export_references" . ,(getf metrics :export-references))
                        ("type_metrics" . ,(gethash pkg (slot-value tracker 'type-metrics))))  ; Add type metrics
                      :test 'equal)))
             (slot-value tracker 'package-metrics))
    result))


(defun validate-package-dependencies (pkg-graph)
  "Validate package dependency graph data structure.
   Returns t if valid, signals error with description if invalid."
  (handler-case
      (maphash (lambda (pkg deps)
                 (unless (stringp pkg)
                   (error "Package name must be a string: ~A" pkg))
                 (unless (listp deps)
                   (error "Dependencies must be a list: ~A" deps))
                 (dolist (dep deps)
                   (unless (stringp dep)
                     (error "Dependency name must be a string: ~A" dep))))
               pkg-graph)
    (error (c)
      (error "Invalid package dependency graph: ~A" c)))
  t)


(defmethod get-package-uses (&optional (tracker nil tracker-provided-p) package-name)
  "Get all packages that a given package uses."
  (let ((actual-tracker (if tracker-provided-p tracker (ensure-tracker))))
    (gethash package-name (slot-value actual-tracker 'package-uses))))


(defmethod get-package-exports (&optional (tracker nil tracker-provided-p) pkg)
  "Get all symbols exported by a package.
   PKG: package object or designator"
  (let* ((actual-tracker (if tracker-provided-p tracker (ensure-tracker)))
         (pkg-obj (if (packagep pkg) pkg (find-package pkg))))
    (when pkg-obj  ; Verify valid package
      (mapcar #'(lambda (sym)
                  (intern (symbol-name sym) pkg-obj))
              (gethash (package-name pkg-obj)  ; Keep string key for exports table
                      (slot-value actual-tracker 'package-exports))))))


(defmethod file-dependencies (&optional (tracker nil tracker-provided-p) file)
  "Get all files that this file depends on.
   Returns a list of files that contain definitions used by this file.
   
   TRACKER - Optional dependency tracker instance (defaults to *current-tracker*)
   FILE - The source file to analyze dependencies for
   
   A file A depends on file B if A contains references to symbols defined in B."
  (let ((actual-tracker (if tracker-provided-p tracker (ensure-tracker)))
        (deps nil)
        (refs-seen (make-hash-table :test 'equal)))
    ;; First collect all references in this file
    (maphash (lambda (key refs)
               (declare (ignore key))
               (dolist (ref refs)
                 (when (equal (reference.file ref) file)
                   ;; Track each referenced name with its package
                   (let* ((sym (reference.name ref))
                          (pkg (reference.package ref))
                          (key (make-tracking-key sym pkg)))
                     (setf (gethash key refs-seen) t)))))
             (slot-value actual-tracker 'references))
    
    ;; Then look up the definitions for each referenced name  
    (maphash (lambda (key _)
               (declare (ignore _))
               (let ((defs (gethash key (slot-value actual-tracker 'definitions))))
                 (dolist (def defs)
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


(defmethod get-system-cycles (&optional (tracker nil tracker-provided-p))
  "Get all recorded system dependency cycles from the tracker."
  (let ((actual-tracker (if tracker-provided-p tracker (ensure-tracker)))
        (cycles nil))
    (maphash (lambda (type anomaly-list)
               (when (eq type :system-cycle)
                 (dolist (a anomaly-list)
                   (push (anomaly.context a) cycles))))
             (slot-value actual-tracker 'anomalies))
    (sort cycles #'string< :key (lambda (cycle)
                                 (format nil "~{~A~^->~}" cycle)))))


(defun make-report-path (project-name format &key (overwrite t))
  "Create a suitable report filepath for the given format.
   Format should be one of :text, :json, or :dot.
   Returns pathname. Signals error if reports/ directory missing."
  (let* ((reports-dir (merge-pathnames #P"reports/" 
                                      (asdf:system-source-directory :dependency-analyzer)))
         (extension (ecase format
                     (:text "text")
                     (:json "json") 
                     (:dot "dot")))
         (base-name (format nil "~A-report" project-name))
         (base-path (make-pathname :name base-name
                                 :type extension
                                 :defaults reports-dir)))
    (unless (probe-file reports-dir)
      (error "Reports directory not found: ~A" reports-dir))
    (if overwrite
        base-path
        (loop for i from 1
              for path = (make-pathname :name (format nil "~A(~D)" base-name i)
                                      :type extension
                                      :defaults reports-dir)
              unless (probe-file path)
              return path))))


(defun report-to-file (pathname format tracker)
  "Write report of given format to specified file.
   Format should be one of :text, :json, or :dot."
  (with-open-file (out pathname
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
    (generate-report format tracker :stream out)))