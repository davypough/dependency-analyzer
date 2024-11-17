;;;; Filename: report.lisp
;;;
;;; Report generation functionality for dependency analysis results.
;;; Provides multiple report formats for visualizing dependencies between
;;; files, packages, and symbols. Includes robust error handling for
;;; file operations.


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


;;; Report Generation Methods

(defgeneric generate-report (format tracker &key stream)
  (:documentation "Generate a dependency report in the specified format."))


(defmethod generate-report ((format (eql :text)) tracker &key (stream *standard-output*))
  ;; Header
  (format stream "~&Dependency Analysis Report for Project: ~A~%" (project.name tracker))
  (format stream "~&================================================~%")
  
  ;; Anomalies section first
  (when (format-anomalies stream tracker)
    (format stream "~&------------------------------------------------~%"))
  
  ;; Cycle warnings - only show sections if cycles exist
  (alexandria:when-let ((cycles (get-project-cycles tracker)))
    (format stream "~&Project Dependency Cycles:~%")
    (dolist (cycle cycles)
      (format stream "  ~A~%" cycle)))
  
  (alexandria:when-let ((cycles (get-file-cycles tracker)))
    (format stream "~&File Dependency Cycles:~%")
    (dolist (cycle cycles)
      (format stream "  ~A~%" cycle)))
  
  (alexandria:when-let ((cycles (get-package-cycles tracker)))
    (format stream "~&Package Dependency Cycles:~%")
    (dolist (cycle cycles)
      (format stream "  ~A~%" cycle)))
  
  ;; Generic Functions and Methods section
  (format stream "~&~%Generic Functions and Methods:~%")
  (format stream "------------------------------------------------~%")
  (let ((generic-fns (make-hash-table :test 'equal)))
    ;; First collect all generic functions and their methods
    (maphash (lambda (key def)
               (when (typep def 'definition)
                 (case (definition.type def)
                   (:generic-function
                    (push def (gethash (definition.symbol def) generic-fns)))
                   (:method
                    (push def (gethash (definition.symbol def) generic-fns))))))
             (slot-value tracker 'definitions))
    
    ;; Then display them organized by generic function
    (maphash (lambda (gf-name definitions)
               (let ((gf-def (find :generic-function definitions 
                                 :key #'definition.type))
                     (methods (remove :generic-function definitions 
                                    :key #'definition.type)))
                 ;; Print generic function header
                 (format stream "~&~%Generic Function: ~A~%" gf-name)
                 (when gf-def
                   (format stream "  Defined in: ~A~%" 
                           (pathname-to-string (definition.file gf-def)))
                   (when-let ((lambda-list (getf (definition.context gf-def) 
                                               :lambda-list)))
                     (format stream "  Lambda List: ~A~%" lambda-list)))
                 
                 ;; Print methods sorted by qualifiers and specializers
                 (when methods
                   (format stream "  Methods:~%")
                   (dolist (method (sort methods #'string< 
                                       :key (lambda (m)
                                             (let ((context (definition.context m)))
                                               (format-method-signature 
                                                (definition.symbol m)
                                                (getf context :qualifiers)
                                                (getf context :specializers))))))
                     (let ((context (definition.context method)))
                       (format stream "    ~A~%"
                               (format-method-signature 
                                (definition.symbol method)
                                (getf context :qualifiers)
                                (getf context :specializers)))
                       (format stream "      Defined in: ~A~%" 
                               (pathname-to-string (definition.file method))))))))
             generic-fns))
  
  ;; File dependencies
  (format stream "~&~%File Dependencies:~%")
  (format stream "------------------------------------------------~%")
  (maphash (lambda (file definitions)
             (declare (ignore definitions))
             (format stream "~&File: ~A~%" (pathname-to-string file))
             (let ((deps (file-dependencies tracker file)))
               (when deps
                 (format stream "  Depends on:~%")
                 (dolist (dep deps)
                   (format stream "    ~A~%" (pathname-to-string dep)))))
             (let ((deps (file-dependents tracker file)))
               (when deps
                 (format stream "  Required by:~%")
                 (dolist (dep deps)
                   (format stream "    ~A~%" (pathname-to-string dep))))))
           (slot-value tracker 'file-map))
  
  ;; Package relationships
  (format stream "~&~%Package Dependencies:~%")
  (format stream "------------------------------------------------~%")
  (maphash (lambda (pkg used-pkgs)
             (format stream "~&Package: ~A~%" pkg)
             (when used-pkgs
               (format stream "  Uses packages:~%")
               (dolist (used used-pkgs)
                 (format stream "    ~A~%" used)))
             (let ((exports (get-package-exports tracker pkg)))
               (when exports
                 (format stream "  Exports:~%")
                 (dolist (sym (sort exports #'string< :key #'symbol-name))
                   (format stream "    ~A~%" sym)))))
           (slot-value tracker 'package-uses))
  
  ;; Symbol cross-reference section
  (format stream "~&~%Symbol Cross-Reference:~%")
  (format stream "------------------------------------------------~%")
  (let ((symbols (make-hash-table :test 'equal)))
    ;; Collect all symbols with their definitions and references
    (maphash (lambda (key def)
               (when (typep def 'definition)
                 (push def (gethash (definition.symbol def) symbols))))
             (slot-value tracker 'definitions))
    
    ;; Display organized by symbol
    (maphash (lambda (sym definitions)
               (format stream "~&~%Symbol: ~A~%" sym)
               ;; Show definitions
               (dolist (def (sort definitions #'string< 
                                 :key (lambda (d) 
                                       (format nil "~A" (definition.type d)))))
                 (format stream "  ~A definition in ~A~%" 
                         (string-capitalize (definition.type def))
                         (pathname-to-string (definition.file def)))
                 (when (definition.exported-p def)
                   (format stream "    Exported from package ~A~%" 
                           (definition.package def)))
                 (when (and (member (definition.type def) '(:method :generic-function))
                          (definition.context def))
                   (let ((context (definition.context def)))
                     (case (definition.type def)
                       (:method
                        (format stream "    Method signature: ~A~%"
                                (format-method-signature 
                                 sym
                                 (getf context :qualifiers)
                                 (getf context :specializers))))
                       (:generic-function
                        (when-let ((lambda-list (getf context :lambda-list)))
                          (format stream "    Lambda list: ~A~%" lambda-list)))))))
               
               ;; Show references
               (let ((refs (get-references tracker sym)))
                 (when refs
                   (format stream "  Referenced in:~%")
                   (dolist (ref (sort refs #'string< 
                                    :key (lambda (r) 
                                          (pathname-to-string (reference.file r)))))
                     (format stream "    ~A (~A)~%" 
                             (pathname-to-string (reference.file ref))
                             (reference.type ref))))))
             symbols)))


(defmethod generate-report ((format (eql :json)) tracker &key (stream *standard-output*))
  (let ((*print-pretty* t)
        (yason:*symbol-key-encoder* #'string-downcase)
        (yason:*symbol-encoder* #'string-downcase))
    (yason:with-output (stream :indent t)
      (yason:with-object ()
        ;; Project name
        (yason:encode-object-element "project" (project.name tracker))
        
        ;; Generic Functions and Methods
        (yason:with-object-element ("generic_functions")
          (yason:with-object ()
            (let ((generic-fns (make-hash-table :test 'equal)))
              ;; First collect all generic functions and their methods
              (maphash (lambda (key def)
                        (when (typep def 'definition)
                          (case (definition.type def)
                            (:generic-function
                             (push def (gethash (definition.symbol def) generic-fns)))
                            (:method
                             (push def (gethash (definition.symbol def) generic-fns))))))
                      (slot-value tracker 'definitions))
              
              ;; Then encode each generic function and its methods
              (maphash #'(lambda (gf-name definitions)
                          (let ((gf-def (find :generic-function definitions 
                                            :key #'definition.type))
                                (methods (remove :generic-function definitions 
                                               :key #'definition.type)))
                            (yason:encode-object-element 
                             (symbol-name gf-name)
                             (alexandria:alist-hash-table
                              `(("file" . ,(when gf-def 
                                           (pathname-to-string 
                                            (definition.file gf-def))))
                                ("lambda_list" . ,(when gf-def
                                                  (getf (definition.context gf-def)
                                                        :lambda-list)))
                                ("methods" . ,(mapcar 
                                             (lambda (method)
                                               (let ((context (definition.context method)))
                                                 (alexandria:alist-hash-table
                                                  `(("signature" 
                                                     . ,(format-method-signature
                                                         (definition.symbol method)
                                                         (getf context :qualifiers)
                                                         (getf context :specializers)))
                                                    ("qualifiers" 
                                                     . ,(getf context :qualifiers))
                                                    ("specializers" 
                                                     . ,(getf context :specializers))
                                                    ("file" 
                                                     . ,(pathname-to-string 
                                                         (definition.file method))))
                                                  :test 'equal)))
                                             methods)))
                              :test 'equal))))
                      generic-fns))))
        
        ;; File dependencies
        (let ((files (build-file-dependency-json tracker)))
          (when files
            (yason:with-object-element ("files")
              (yason:encode files))))
        
        ;; Package dependencies
        (let ((packages (build-package-dependency-json tracker)))
          (when packages
            (yason:with-object-element ("packages")
              (yason:encode packages))))
        
        ;; Symbol cross-reference
        (yason:with-object-element ("symbols")
          (yason:with-object ()
            (let ((symbol-map (make-hash-table :test 'equal)))
              ;; Collect all symbols with their definitions and references
              (maphash (lambda (key def)
                        (when (typep def 'definition)
                          (push def (gethash (definition.symbol def) symbol-map))))
                      (slot-value tracker 'definitions))
              
              ;; Encode each symbol's information
              (maphash (lambda (sym definitions)
                        (yason:with-object-element ((symbol-name sym))
                          (yason:with-object ()
                            ;; Encode definitions
                            (yason:with-object-element ("definitions")
                              (yason:with-array ()
                                (dolist (def definitions)
                                  (yason:with-object ()
                                    (yason:encode-object-element 
                                     "type" 
                                     (definition.type def))
                                    (yason:encode-object-element 
                                     "file" 
                                     (pathname-to-string (definition.file def)))
                                    (yason:encode-object-element 
                                     "package" 
                                     (definition.package def))
                                    (yason:encode-object-element 
                                     "exported" 
                                     (definition.exported-p def))
                                    (when (definition.context def)
                                      (yason:encode-object-element 
                                       "context" 
                                       (definition.context def)))))))
                            
                            ;; Encode references
                            (let ((refs (get-references tracker sym)))
                              (when refs
                                (yason:with-object-element ("references")
                                  (yason:with-array ()
                                    (dolist (ref refs)
                                      (yason:with-object ()
                                        (yason:encode-object-element 
                                         "type" 
                                         (reference.type ref))
                                        (yason:encode-object-element 
                                         "file" 
                                         (pathname-to-string (reference.file ref)))
                                        (when (reference.package ref)
                                          (yason:encode-object-element 
                                           "package" 
                                           (reference.package ref)))
                                        (when (reference.context ref)
                                          (yason:encode-object-element 
                                           "context" 
                                           (reference.context ref))))))))))))
                      symbol-map))))))))


(defmethod generate-report ((format (eql :dot)) tracker &key (stream *standard-output*))
  (format stream "digraph Dependencies {~%")
  (format stream "  rankdir=LR;~%")
  (format stream "  compound=true;~%")
  (format stream "  node [shape=box, fontname=\"Arial\"];~%")
  (format stream "  edge [fontname=\"Arial\"];~%")
  
  ;; Create package subgraph
  (format stream "  subgraph cluster_packages {~%")
  (format stream "    label=\"Package Dependencies\";~%")
  (format stream "    style=dashed;~%")
  (format stream "    node [style=filled,fillcolor=lightgrey];~%")
  
  ;; Add package nodes and edges
  (maphash (lambda (pkg used-pkgs)
             (let ((pkg-id (string-to-dot-id pkg)))
               (format stream "    \"pkg_~A\" [label=\"~A\"];~%" pkg-id pkg)
               (dolist (used used-pkgs)
                 (let ((used-id (string-to-dot-id used)))
                   (format stream "    \"pkg_~A\" [label=\"~A\"];~%" used-id used)
                   (format stream "    \"pkg_~A\" -> \"pkg_~A\";~%"
                           pkg-id used-id)))))
           (slot-value tracker 'package-uses))
  (format stream "  }~%~%")
  
  ;; Create file subgraph
  (format stream "  subgraph cluster_files {~%")
  (format stream "    label=\"File Dependencies\";~%")
  (format stream "    style=dashed;~%")
  (format stream "    node [style=filled,fillcolor=lightblue];~%")
  
  ;; Track all files we need nodes for
  (let ((all-files (make-hash-table :test 'equal)))
    ;; Collect files from file-map and dependents
    (maphash (lambda (file definitions)
               (declare (ignore definitions))
               (setf (gethash file all-files) t)
               (dolist (dependent (file-dependents tracker file))
                 (setf (gethash dependent all-files) t)))
             (slot-value tracker 'file-map))
    
    ;; Create file nodes
    (maphash (lambda (file _)
               (declare (ignore _))
               (let* ((file-name (source-file-name file))
                      (file-path (abbreviate-path file))
                      (file-id (string-to-dot-id file-name)))
                 (format stream "    \"~A\" [label=\"~A\"];~%" 
                         file-id file-path)))
             all-files)
    
    ;; Create file dependency edges
    (maphash (lambda (file definitions)
               (declare (ignore definitions))
               (let* ((file-name (source-file-name file))
                      (file-id (string-to-dot-id file-name)))
                 (dolist (dependent (file-dependents tracker file))
                   (let* ((dep-name (source-file-name dependent))
                          (dep-id (string-to-dot-id dep-name)))
                     (format stream "    \"~A\" -> \"~A\";~%"
                             dep-id file-id)))))
             (slot-value tracker 'file-map)))
  (format stream "  }~%~%")
  
  ;; Create generic function/method subgraph
  (format stream "  subgraph cluster_methods {~%")
  (format stream "    label=\"Generic Functions and Methods\";~%")
  (format stream "    style=dashed;~%")
  (format stream "    node [style=filled];~%")
  
  (let ((gf-map (make-hash-table :test 'equal))
        (nodes-seen (make-hash-table :test 'equal)))
    ;; Collect generic functions and methods
    (maphash (lambda (key def)
               (when (typep def 'definition)
                 (case (definition.type def)
                   ((:generic-function :method)
                    (push def (gethash (definition.symbol def) gf-map))))))
             (slot-value tracker 'definitions))
    
    ;; Create nodes and edges for generic functions and methods
    (maphash (lambda (gf-name definitions)
               (let ((gf-def (find :generic-function definitions 
                                 :key #'definition.type))
                     (methods (remove :generic-function definitions 
                                    :key #'definition.type)))
                 ;; Create generic function node
                 (let ((gf-id (string-to-dot-id (format nil "gf_~A" gf-name))))
                   (unless (gethash gf-id nodes-seen)
                     (setf (gethash gf-id nodes-seen) t)
                     (format stream "    \"~A\" [label=\"~A\",fillcolor=lightyellow];~%" 
                             gf-id gf-name))
                   
                   ;; Create method nodes and connect to generic function
                   (dolist (method methods)
                     (let* ((context (definition.context method))
                            (method-id (string-to-dot-id 
                                      (format nil "method_~A_~A_~A"
                                              gf-name
                                              (getf context :qualifiers)
                                              (getf context :specializers))))
                            (method-label (format-method-signature 
                                         gf-name
                                         (getf context :qualifiers)
                                         (getf context :specializers))))
                       (unless (gethash method-id nodes-seen)
                         (setf (gethash method-id nodes-seen) t)
                         (format stream "    \"~A\" [label=\"~A\",fillcolor=lightgreen];~%" 
                                 method-id method-label))
                       ;; Connect method to its generic function
                       (format stream "    \"~A\" -> \"~A\";~%"
                               method-id gf-id)
                       
                       ;; Add edges to specializer classes
                       (dolist (spec (getf context :specializers))
                         (unless (eq spec t)
                           (let* ((spec-name (if (listp spec)
                                               (if (eq (car spec) 'eql)
                                                   'eql
                                                   (cadr spec))
                                               spec))
                                  (spec-id (string-to-dot-id 
                                          (format nil "class_~A" spec-name))))
                             ;; Create class node if needed
                             (unless (gethash spec-id nodes-seen)
                               (setf (gethash spec-id nodes-seen) t)
                               (format stream "    \"~A\" [label=\"~A\",fillcolor=lightpink];~%" 
                                       spec-id spec-name))
                             ;; Connect method to specializer
                             (format stream "    \"~A\" -> \"~A\" [style=dashed];~%"
                                     method-id spec-id)))))))))
             gf-map))
  
  (format stream "  }~%~%")
  
  ;; Add any dependency cycles found
  (alexandria:when-let ((cycles (get-package-cycles tracker)))
    (format stream "  subgraph cluster_cycles {~%")
    (format stream "    label=\"Dependency Cycles\";~%")
    (format stream "    style=dashed;~%")
    (format stream "    node [style=filled,fillcolor=pink];~%")
    (loop for cycle in cycles
          for i from 0
          do (format stream "    \"cycle~D\" [label=\"~A\",shape=note];~%"
                    i cycle))
    (format stream "  }~%"))
  
  (format stream "}~%"))


;;; Main Report Function

(defun report (&optional filename)
  "Generate a comprehensive dependency report for the current project analysis.
   If FILENAME is provided, saves the report to that file. Handles file project
   errors gracefully."
  (unless *current-tracker*
    (error "No analysis results available. Please run (dep:analyze-project \"project-name\") first."))
  
  (flet ((generate-all-reports (stream)
           ;; Header
           (format stream "~&~V,,,'-<~>" 70 "")
           (format stream "~&Dependency Analysis Report~%")
           (format stream "Project: ~A~%" (project.name *current-tracker*))
           (format stream "Generated: ~A~%" (local-time:now))
           (format stream "~V,,,'-<~>~%" 70 "")
           
           ;; File Dependency Tree
           (format stream "~%File Dependency Hierarchy:~%")
           (let ((roots (build-file-dependency-tree *current-tracker*)))
             (if roots
                 (print-ascii-tree stream roots)
                 (format stream "No file dependencies found.~%")))
           (format stream "~%")

           ;; Package Dependency Tree
           (format stream "~%Package Dependency Hierarchy:~%")
           (multiple-value-bind (roots cycles) 
               (build-package-dependency-tree *current-tracker*)
             (if roots
                 (progn
                   (print-ascii-tree stream roots)
                   (when cycles
                     (format stream "~%Circular Package Dependencies:~%")
                     (dolist (cycle cycles)
                       (format stream "  ~A~%" cycle))))
                 (format stream "No package dependencies found.~%")))
           (format stream "~%")
           
           ;; Core report in text format
           (generate-report :text *current-tracker* :stream stream)
           
           ;; Add DOT graph as appendix
           (format stream "~%~%APPENDIX A: Graphviz DOT Format~%")
           (format stream "~V,,,'-<~>~%" 70 "")
           (format stream "Save the following content to a .dot file and process with Graphviz:~%~%")
           (generate-report :dot *current-tracker* :stream stream)
           
           ;; Add JSON as appendix
           (format stream "~%~%APPENDIX B: JSON Format~%")
           (format stream "~V,,,'-<~>~%" 70 "")
           (format stream "~%")
           (generate-report :json *current-tracker* :stream stream)))
    
    (if filename
        ;; Save to file with error handling
        (handler-case
            (let ((pathname (pathname filename)))
              ;; Verify directory exists and is writable
              (ensure-directory-exists pathname)
              (verify-writable pathname)
              ;; Generate report
              (with-open-file (out pathname
                               :direction :output 
                               :if-exists :supersede
                               :if-does-not-exist :create)
                (generate-all-reports out)
                (format t "~&Report saved to: ~A~%" pathname)))
          (report-error (e)
            (format *error-output* "~&Failed to save report: ~A~%" e))
          (error (e)
            (format *error-output* "~&Unexpected error saving report: ~A~%" e)))
        ;; Display to standard output
        (generate-all-reports *standard-output*)))
  
  ;; Return the tracker to allow for chaining
  *current-tracker*)