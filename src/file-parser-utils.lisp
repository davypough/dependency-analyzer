;;;; Filename: file-parser-utils.lisp
;;;
;;; Utility functions for parsing, package handling, and cycle detection.
;;; Contains pure functions for name normalization, symbol lookup,
;;; package option processing, and dependency cycle detection.

(in-package #:dep)


;;; Name and Symbol Utils


(defun normalize-designator (designator)
  "Convert a package or symbol designator to a normalized string form.
   Handles package objects, symbols (interned or uninterned), and strings uniformly.
   Returns uppercase string representation.
   Examples:
     'my-package  -> \"MY-PACKAGE\"  
     #:my-package -> \"MY-PACKAGE\"
     \"MY-PACKAGE\" -> \"MY-PACKAGE\"
     #<PACKAGE MY-PACKAGE> -> \"MY-PACKAGE\"
     'my-symbol -> \"MY-SYMBOL\"
     #:my-symbol -> \"MY-SYMBOL\""
  (typecase designator
    (package  (package-name designator))
    (string (string-upcase designator))
    (symbol (string-upcase (symbol-name designator))) ; Works for both interned and uninterned
    (t (error "Invalid designator for normalization: ~S" designator))))


(defun get-file-definitions (&optional (tracker nil tracker-provided-p) file)
  "Get all definitions in a file."
  (let ((actual-tracker (if tracker-provided-p tracker (ensure-tracker))))
    (gethash file (slot-value actual-tracker 'file-map))))


(defun collect-file-references (tracker source-file target-file)
  "Collect all references in SOURCE-FILE that reference definitions in TARGET-FILE.
   Returns a list of reference objects."
  (let ((refs ())
        (target-defs (get-file-definitions tracker target-file)))
    ;; Build hash table of symbols defined in target file for quick lookup
    (let ((target-symbols (make-hash-table :test 'equal)))
      (dolist (def target-defs)
        (push def (gethash (definition.name def) target-symbols)))
      ;; Check all references in source file to see if they reference target symbols
      (maphash (lambda (key refs-list)
                 (declare (ignore key))
                 (dolist (ref refs-list)
                   (when (and (equal (reference.file ref) source-file)
                            (gethash (reference.name ref) target-symbols))
                     (pushnew ref refs :test #'equal))))
               (slot-value tracker 'references)))
    ;; Return sorted list of references
    (sort refs #'string< :key (lambda (r) (symbol-name (reference.name r))))))


(defun record-definition (tracker name type file &key package exported-p context qualifiers specializers)
  "Record a name definition in the tracker. Creates a single definition object and 
   stores it under both a base key (name+package+type) and, if qualifiers/specializers 
   are present, a specialized key that includes method details."
  (let* ((def (make-instance 'definition 
                            :name name 
                            :type type 
                            :file file
                            :package package 
                            :exported-p exported-p 
                            :context context
                            :qualifiers qualifiers
                            :specializers specializers))
         (base-key (make-tracking-key name package type))
         (specialized-key (when (or qualifiers specializers)
                          (make-tracking-key name package type qualifiers specializers))))
    ;; Store under base key for reference lookup
    (push def (gethash base-key (slot-value tracker 'definitions)))
    ;; Store under specialized key if we have one
    (when specialized-key
      (push def (gethash specialized-key (slot-value tracker 'definitions))))
    ;; Add to file map and exports
    (push def (gethash file (slot-value tracker 'file-map)))
    (when exported-p
      (record-export tracker package name))
    def))


(defun record-reference (tracker name file &key package context visibility definition)
 "Record a name reference in the tracker.
  VISIBILITY is inherited, imported, or local (defaults to local)"
 (let* ((key (make-tracking-key name (when (symbol-package name)
                                       (package-name (symbol-package name)))))
        (ref (make-instance 'reference :name name :file file :package package :context context 
                                     :visibility (or visibility :LOCAL) :definition definition)))
   (pushnew ref (gethash key (slot-value tracker 'references))
        :test (lambda (a b)
                (and (equal (reference.name a) (reference.name b))
                     (equal (reference.file a) (reference.file b))
                     (equal (reference.package a) (reference.package b)))))
   ref))


(defun record-anomaly (tracker type severity file description &optional context)
 "Record a new anomaly in the dependency tracker"
 (let ((anomaly (make-instance 'anomaly :type type :severity severity :file file 
                                       :description description :context context)))
   (push anomaly (gethash type (anomalies tracker)))
   anomaly))


(defun record-package-use (tracker using-package used-package)
  "Record that one package uses another package."
  (pushnew used-package 
           (gethash using-package (slot-value tracker 'package-uses))
           :test #'string=))


(defun record-export (tracker package-name name)
  "Record a name as being exported from a package.
   Both package-name and name can be either strings or symbols."
  (let ((pkg (find-package (string package-name))))
    (when pkg
      (let ((exported-sym (etypecase name
                           (string (intern name pkg))
                           (symbol (intern (symbol-name name) pkg)))))
        (pushnew exported-sym 
                 (gethash (string package-name) (slot-value tracker 'package-exports))
                 :test #'eq)))))


(defun record-package-cycle (tracker cycle-chain)
  "Record a package dependency cycle."
  (pushnew cycle-chain (package-cycles tracker) :test #'string=))

(defmethod get-file-cycles (&optional tracker)
  "Get all recorded file dependency cycles."
  (file-cycles (ensure-tracker tracker)))

(defmethod get-package-cycles (&optional tracker)
  "Get all recorded package dependency cycles."
  (package-cycles (ensure-tracker tracker)))


(defun process-package-import-option (package from-pkg-name pkg-name parser name)
  "Process an :import-from package option for a single name.
   Records dependencies between packages and the imported name reference.
   
   PACKAGE - The package object being defined
   FROM-PKG-NAME - Package name to import from (string, already normalized)
   PKG-NAME - Name of package being defined (string, already normalized)
   PARSER - Current file parser for context
   name - name to import (can be string, symbol, or uninterned symbol)"
  (let* ((from-pkg (find-package from-pkg-name))
         (sym-name (normalize-designator name))
         (from-sym (and from-pkg (find-symbol sym-name from-pkg))))
    (when (and from-pkg from-sym)
      (import from-sym package)
      (record-package-use *current-tracker* pkg-name from-pkg-name)
      (record-reference *current-tracker* from-sym
                       (file parser)
                       :package from-pkg-name
                       :visibility :IMPORTED))))


(defun process-package-export-option (package pkg-name name)
  "Process an :export package option for a single name.
   Records the name as being exported from the package.
   
   PACKAGE - The package object being defined  
   PKG-NAME - Name of package being defined (string, already normalized)
   name - name to export (can be string, symbol, or uninterned symbol)
   
   Returns the exported name if successful."
  (let* ((sym-name (normalize-designator name))
         (exported-sym (and sym-name 
                           (intern sym-name package))))
    (when exported-sym
      (export exported-sym package)
      (record-export *current-tracker* pkg-name exported-sym)
      exported-sym)))


(defun detect-package-cycle (pkg-name current-packages)
  "Check for and record any package dependency cycles.
   A cycle exists if the package being processed appears in the current package stack.
   
   PKG-NAME - Name of package being checked (string, already normalized)
   CURRENT-PACKAGES - Stack of packages currently being processed (list of strings)
   
   Example cycle: A -> B -> C -> A would be recorded as \"A -> B -> C -> A\"
   
   Records both:
   1. The cycle chain in the tracker's package-cycles list
   2. An anomaly record with severity ERROR"
  (let ((position (member pkg-name current-packages :test #'string=)))
    (when position
      (let* ((cycle (cons pkg-name (ldiff current-packages position)))
             (chain (format nil "~{~A~^ -> ~}" (reverse cycle))))
        ;; Record as both a cycle and an anomaly
        (record-package-cycle *current-tracker* chain)
        (record-anomaly *current-tracker*
                       :package-cycle
                       :ERROR
                       pkg-name
                       (format nil "Package dependency cycle detected: ~A" chain)
                       cycle)))))


(defmethod analyze-subform ((parser file-parser) form)
  "Analyze a single form for symbol references, recording only references to
   symbols that have definition records. Recursively examines all subforms including
   array elements and hash tables."
  (typecase form
    (symbol 
     (let* ((pkg (symbol-package form))
            (current-pkg (current-package parser))
            (pkg-name (if pkg 
                       (package-name pkg) 
                       (current-package-name parser)))
            (current-file (file parser))
            (visibility (cond 
                       ((null pkg) :LOCAL)
                       ((eq pkg current-pkg) :LOCAL)
                       (t (multiple-value-bind (sym status)
                            (find-symbol (symbol-name form) current-pkg)
                          (declare (ignore sym))
                          (case status
                            (:inherited :INHERITED)
                            (:external :IMPORTED) 
                            (otherwise :LOCAL)))))))
       (when (gethash (make-tracking-key form pkg-name) 
                     (slot-value *current-tracker* 'definitions))
         (record-reference *current-tracker* form
                         current-file
                         :package pkg-name
                         :visibility visibility))))
    ;(cons
    ; (analyze-form parser form))
    (array
     (dotimes (i (array-total-size form))
       (analyze-subform parser (row-major-aref form i))))
    (hash-table
     (maphash (lambda (k v)
                (analyze-subform parser k)
                (analyze-subform parser v))
              form))
    (t nil)))


(defun analyze-lambda-list (parser lambda-list)
 "Analyze a lambda list for type declarations and default values that 
  could create dependencies."
 (let ((state :required))
   (dolist (item lambda-list)
     (case item
       ((&optional &rest &key &aux)
        (setf state item))
       ((&whole &environment &allow-other-keys)
        nil)
       (t (case state
            (:required 
             (typecase item
               (symbol nil) ; Simple parameter
               (cons ; Destructuring or type declaration
                (when (> (length item) 1)
                  ;; Check for type declarations
                  (analyze-definition-form parser (second item))))))
            (&optional
             (when (and (listp item) (cddr item))
               (analyze-definition-form parser (third item)))) ; Default value
            (&key
             (when (listp item)
               (let ((key-item (if (listp (car item))
                                  (cadar item)
                                  (car item))))
                 (declare (ignore key-item))
                 (when (and (listp item) (cddr item))
                   (analyze-definition-form parser (third item)))))) ; Default value
            (&aux
             (when (and (listp item) (cdr item))
               (analyze-definition-form parser (second item))))))))))


(defun cl-symbol-p (symbol)
 "Return true if symbol is from the common-lisp package."
 (and (symbolp symbol)
      (eq (symbol-package symbol) 
          (find-package :common-lisp))))


(defun collect-non-cl-symbols (form symbols)
  "Recursively collect all non-CL symbols from form into symbols list.
   Handles nested lists and ignores quoted forms."
  (typecase form
    (symbol 
     (unless (or (null form) (cl-symbol-p form))
       (push form symbols)))
    (cons
     (unless (quoted-form-p form)
       (setf symbols (collect-non-cl-symbols (car form) symbols))
       (setf symbols (collect-non-cl-symbols (cdr form) symbols)))))
  symbols)


(defun quoted-form-p (form)
 "Return true if form is quoted with quote or function quote."
 (and (consp form)
      (or (eq (car form) 'quote)
          (eq (car form) 'function))))


(defun symbol-qualified-p (symbol parser)
  "Return t if symbol appears to have been package-qualified in source.
   This is inferred when symbol's package differs from parser's current package."
  (and (symbol-package symbol)  ; Has a package
       (not (eq (symbol-package symbol) 
                (current-package parser)))))


(defun determine-symbol-visibility (symbol parser)
  "Determine how symbol is visible in current package context.
   Returns :LOCAL, :INHERITED, or :IMPORTED."
  (let ((sym-pkg (symbol-package symbol))
        (cur-pkg (current-package parser)))
    (cond 
      ((null sym-pkg) :LOCAL)  ; Uninterned symbol
      ((eq sym-pkg cur-pkg) :LOCAL)  ; In current package
      (t (multiple-value-bind (sym status)
             (find-symbol (symbol-name symbol) cur-pkg)
           (declare (ignore sym))
           (case status
             (:inherited :INHERITED)
             (:external :IMPORTED)
             (otherwise :LOCAL)))))))


(defun skip-item-p (item)
  "Return T if item should be skipped during reference analysis.
   Skips:
   - NIL and keywords
   - Common Lisp package symbols"
  (or (null item)
      (numberp item)
      (stringp item)
      (keywordp item) 
      (and (symbolp item)
           (symbol-package item)
           (eq (symbol-package item) 
               (find-package :common-lisp)))))


(defun check-package-reference (symbol parser tracker context parent-context)
  "Check package visibility for a symbol reference.
   Returns the visibility: :LOCAL, :INHERITED, or :IMPORTED.
   Records any anomalies found."
  (let* ((sym-pkg (symbol-package symbol))
         (cur-pkg (current-package parser))
         (pkg-name (if sym-pkg 
                      (package-name sym-pkg)
                      (current-package-name parser)))
         (visibility (determine-symbol-visibility symbol parser))
         (package-ops '(defpackage in-package use-package make-package 
                       rename-package delete-package)))
    
    ;; Check for unqualified cross-package reference while in CL-USER
    (when (eq cur-pkg (find-package :common-lisp-user))
      (when (and (not (symbol-qualified-p symbol parser))
                 (not (eq sym-pkg (find-package :common-lisp-user)))
                 (not (member symbol '(:use cl common-lisp)))
                 ;; Check if containing form is a package op
                 (not (or (and (consp context) (member (car context) package-ops))
                         (and (consp parent-context) (member (car parent-context) package-ops))))
                 ;; Check if this is a package name being defined
                 (not (or (and (consp context)
                              (eq (car context) 'defpackage)
                              (eq symbol (cadr context)))
                         (and (consp parent-context)
                              (eq (car parent-context) 'defpackage)
                              (eq symbol (cadr parent-context))))))
        
        (record-anomaly tracker
                        :missing-in-package
                        :WARNING
                        (file parser)
                        (format nil "Unqualified reference to ~A from package ~A without in-package declaration"
                                symbol pkg-name))))
    
    visibility))


(defun walk-form (form handler &key (log-stream *trace-output*) (log-depth t))
  "Walk a form calling HANDLER on each subform with context and depth info.
   FORM - The form to analyze
   HANDLER - Function taking (form context parent-context depth)
   LOG-STREAM - Where to send debug output (nil for no logging)
   LOG-DEPTH - Whether to track and log nesting depth"
  (labels ((walk (x context parent-context depth)
             ;; Log form being processed
             (format log-stream "~&WALK-FORM handling:~%  Form: ~S~%  Context: ~S~%  Parent: ~S~%  Depth: ~D~%" 
                     x context parent-context depth)

             (unless (skip-item-p x) 
               (when log-stream
                 (let ((indent (if log-depth 
                                (make-string (* depth 2) :initial-element #\Space)
                                "")))
                   (format log-stream "~&~AForm: ~S~%" indent x)
                   (when context
                     (format log-stream "~A Context: ~S~%" indent context))
                   (when parent-context  
                     (format log-stream "~A Parent: ~S~%" indent parent-context))
                   (format log-stream "~A~%" indent))))
             
             ;; Process form with handler - now passing parent context
             (funcall handler x context parent-context depth)
             
             ;; Recursively process subforms
             (typecase x
               (cons
                (when log-stream
                  (format log-stream "~A Processing list form~%" 
                          (if log-depth
                              (make-string (* depth 2) :initial-element #\Space)
                              "")))
                (walk (car x) x context (1+ depth)) 
                (walk (cdr x) x context (1+ depth)))
               
               (array
                (when log-stream
                  (format log-stream "~A Processing array~%"
                          (if log-depth
                              (make-string (* depth 2) :initial-element #\Space)
                              "")))
                (dotimes (i (array-total-size x))
                  (walk (row-major-aref x i) x context (1+ depth))))
               
               (hash-table
                (when log-stream
                  (format log-stream "~A Processing hash table~%"
                          (if log-depth
                              (make-string (* depth 2) :initial-element #\Space)
                              "")))
                (maphash (lambda (k v)
                          (walk k x context (1+ depth))
                          (walk v x context (1+ depth)))
                        x)))))
    
    ;; Start walking at top level
    (let ((*print-circle* nil)     ; Prevent circular printing issues
          (*print-length* 10)      ; Limit list output
          (*print-level* 5))       ; Limit nesting output
      (walk form nil nil 0))))


(defun limit-form-size (form pkg-name &key (max-depth 8) (max-elements 20))
  "Limit the size of a form for use as reference context.
   Returns form if within limits, otherwise returns truncated version.
   Strips package prefixes only when they match pkg-name."
  (let ((element-count 0))
    (labels ((clean-symbol (sym)
               (let* ((actual-sym (if (and (listp sym) 
                                          (eq (car sym) 'quote))
                                     (cadr sym)
                                     sym))
                      (pkg (and (symbolp actual-sym)
                               (symbol-package actual-sym)))
                      (sym-pkg-name (and pkg (package-name pkg))))
                 (cond ((not (symbolp actual-sym)) actual-sym)
                       ((null pkg) actual-sym)  ; Uninterned stays uninterned
                       ((string= sym-pkg-name pkg-name) ; Package matches, strip qualifier 
                        (intern (symbol-name actual-sym) 
                               (find-package pkg-name)))
                       (t actual-sym))))  ; Keep qualifier if different package
             (truncate-form (f depth)
               (when (> (incf element-count) max-elements)
                 (return-from truncate-form
                   (typecase f 
                     (cons (if (member (car f) '(quote function))
                             (list (car f) (clean-symbol (cadr f)))
                             (list (clean-symbol (car f)) (make-instance 'dots-object))))
                     (symbol (clean-symbol f))
                     (t f))))
               (typecase f
                 (cons
                  (cond ((>= depth max-depth)
                         (if (member (car f) '(quote function))
                             (list (car f) (clean-symbol (cadr f)))
                             (list (clean-symbol (car f)) (make-instance 'dots-object))))
                        ((member (car f) '(quote function))
                         (list (car f) (clean-symbol (cadr f))))
                        (t (let ((car (truncate-form (car f) (1+ depth)))
                               (cdr (truncate-form (cdr f) (1+ depth))))
                             (cons car cdr)))))
                 (symbol (clean-symbol f))
                 (t f))))
      (truncate-form form 0))))


(defun parse-defmethod-args (args)
  "Parse method specification arguments from a :method option in defgeneric.
   Returns (values qualifiers lambda-list . body).
   ARGS is the list of arguments after the :method keyword.
   Examples:
   - (:before (x) ...) -> (:before), (x), ...
   - ((x string) y) -> (), ((x string) y), ...
   - (:after (x number) y) -> (:after), ((x number) y), ..."
  (let ((qualifiers nil)
        lambda-list)
    ;; Collect qualifiers until we hit the lambda-list
    (loop for arg = (car args)
          while (and args (or (keywordp arg) (symbolp arg))
                     (not (listp arg)))
          do (push arg qualifiers)
             (setf args (cdr args))
          finally (setf lambda-list (car args)
                       args (cdr args)))
    (values (nreverse qualifiers) lambda-list args)))


(defun package-context-p (context parent-context)
  "Return T if the context indicates a form that accepts package designator references.
   Such contexts include:
   - Direct package forms (defpackage, in-package, use-package etc)
   - Package options in defpackage (:use, :import-from, :nicknames)"
  (and (consp context)
       (or ;; Direct package forms
           (member (car context) '(defpackage in-package use-package 
                                  make-package rename-package delete-package))
           ;; Package options in defpackage
           (and (consp parent-context)
                (eq (car parent-context) 'defpackage)
                (keywordp (car context))
                (member (car context) '(:use :import-from :nicknames))))))


(defun not-interned-p (symbol)
  (not (symbol-package symbol)))