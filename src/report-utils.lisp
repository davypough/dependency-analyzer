;;;; Filename: report-utils.lisp
;;;
;;; Utility functions for report generation and formatting.
;;; Contains helper functions for path handling, tree building,
;;; and other report-related operations.


(in-package #:dep)


(defparameter *print-width* 80
  "Default maximum width for formatted report output.")


(defun format-file-dependency-line (source target)
  "Format a file dependency line 'X depends on Y'.
   Returns both the formatted string and its display length."
  (let ((line (format nil "~A depends on ~A" 
                      (project-pathname source)
                      (project-pathname target))))
    (values line (length line))))


(defun format-references-list (refs &key depends-pos (max-width *print-width*))
  "Format a list of reference symbols with 'references' aligned under 'depends on'.
   REFS - List of reference symbols
   DEPENDS-POS - Column position of 'depends on' label
   MAX-WIDTH - Maximum line width (defaults to *print-width*)"
  (with-output-to-string (s)
    (let* ((prefix (format nil "~vA" depends-pos ""))
           (ref-strings (mapcar #'symbol-name (sort refs #'string< :key #'symbol-name)))
           ;(available-width (- max-width depends-pos))
           (first-line t))
      ;; Handle empty reference list
      (when (null refs)
        (return-from format-references-list ""))
      
      ;; Start with "references" aligned under "depends on"
      (format s "~Areferences " prefix)
      
      (loop with col = (+ depends-pos 11) ; length of "references " after prefix
            for ref in ref-strings
            do (let ((need-space (not first-line))
                    (ref-len (length ref)))
                 ;; Check if we need to wrap
                 (when (> (+ col (if need-space 2 0) ref-len) max-width)
                   ;; Start new line aligned with first "references"
                   (format s "~%~A           " prefix)
                   (setf col (+ depends-pos 11)
                         need-space nil))
                 ;; Add separator if needed
                 (when need-space
                   (format s ", ")
                   (incf col 2))
                 ;; Add reference
                 (format s "~A" ref)
                 (incf col ref-len)
                 (setf first-line nil))))))


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
                 (loop for severity in '(:ERROR :WARNING :INFO)
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


(defun project-pathname (pathname)
  "Convert a pathname to a string representation relative to project root.
   Returns a path starting with / that is relative to the project root.
   E.g., /source/file.lisp instead of /path/to/project/source/file.lisp"
  (when pathname
    (let* ((project-root (project-root *current-tracker*))
           (namestring (namestring pathname)))
      (if project-root
          (let ((relative (enough-namestring pathname project-root)))
            (if (char= (char relative 0) #\/)
                relative
                (concatenate 'string "/" relative)))
          namestring))))


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
               (declare (ignore pkg))
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
               (declare (ignore pkg))
               (when (null (getf node :parents))
                 (push node roots)))
             nodes)
    (values (sort roots #'string< :key (lambda (node) (getf node :name)))
            (sort cycles #'string<))))


(defun print-ascii-tree (stream roots &optional (prefix "") (last-child-p nil))
  "Print an ASCII representation of a dependency tree."
  (declare (ignore last-child-p))
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
                (let* ((file-str (project-pathname file))
                       (deps (file-dependencies tracker file)))
                  (when deps
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
             (anomalies actual-tracker))
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