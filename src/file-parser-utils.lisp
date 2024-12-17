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


(defun record-definition (tracker &key name type file package exported-p context qualifiers specializers)
  "Record a name definition in the tracker. Creates a single definition object and 
   stores it under both a base key (name+package+type) and, if qualifiers/specializers 
   are present, a specialized key that includes method details. Detects and records
   duplicate definitions of the same symbol."
  (let* ((def-name (if (eq type :PACKAGE)
                       (normalize-designator name)
                       name))
         (def (make-instance 'definition 
                            :name def-name
                            :type type 
                            :file file
                            :package package 
                            :exported-p exported-p 
                            :context context
                            :qualifiers qualifiers
                            :specializers specializers))
         (base-key (make-tracking-key name package type))
         (specialized-key (when (or qualifiers specializers)
                          (make-tracking-key name package type qualifiers specializers)))
         (existing-defs (gethash base-key (slot-value tracker 'definitions)))
         (matching-defs 
          (if specialized-key
              ;; For methods, match on exact qualifiers/specializers
              (remove-if-not (lambda (d)
                              (and (equal (definition.qualifiers d) qualifiers)
                                   (equal (definition.specializers d) specializers)))
                            existing-defs)
              ;; For non-methods, all existing defs match
              existing-defs)))
    
    ;; Check for duplicates and record anomaly if found
    (when matching-defs
      (let ((def-files (mapcar #'definition.file matching-defs)))
        (record-anomaly tracker
                :name name
                :type :duplicate-definition
                :severity :WARNING
                :file file
                :description (format nil "~A ~A defined multiple times" type name)
                :context (definition.context def)
                :files (cons file def-files))))
    
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


(defun record-reference (tracker &key name file package context visibility definitions
                                     qualifiers arguments specializers)
  "Record a name reference in the tracker.
   VISIBILITY is inherited, imported, or local (defaults to local)
   DEFINITIONS is a list of definitions this reference depends on
   QUALIFIERS, ARGUMENTS, SPECIALIZERS track method call details when applicable"
 (declare (special log-stream))
 (let* ((key (make-tracking-key name (when (symbol-package name)
                                      (package-name (symbol-package name)))))
        (ref (make-instance 'reference 
                          :name name 
                          :file file 
                          :package package 
                          :context context 
                          :visibility (or visibility :LOCAL)
                          :definitions definitions
                          :qualifiers qualifiers
                          :arguments arguments
                          :specializers specializers)))
   (pushnew ref (gethash key (slot-value tracker 'references))
        :test #'equalp)
   (format log-stream "~%~A~%" ref)
   ref))


(defun record-anomaly (tracker &key name type severity file description context files)
  "Record a new anomaly in the dependency tracker.
   For :duplicate-definition type, files must be provided as list of all definition locations."
  (when (and (eq type :duplicate-definition)
             (not files))
    (error "Files parameter required for duplicate definition anomalies"))
  (let ((anomaly (make-instance 'anomaly 
                               :name name
                               :type type 
                               :severity severity 
                               :file file
                               :description description 
                               :context context
                               :files (or files (list file)))))
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
      (record-reference *current-tracker*
                  :name from-sym
                  :file (file parser)
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
                :name (first cycle)
                :type :package-cycle
                :severity :ERROR
                :file pkg-name
                :description (format nil "Package dependency cycle detected: ~A" chain)
                :context cycle)))))


(defmethod analyze-subform ((parser file-parser) form)
  "Analyze a single form for symbol references, recording only references to
   symbols that have definition records. Recursively examines all subforms including
   array elements and hash tables."
  (declare (special log-stream))
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
         (record-reference *current-tracker*
                  :name form
                  :file current-file
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
        
        ;; MODIFIED: Updated to use keyword args
        (record-anomaly tracker
                        :name symbol
                        :type :missing-in-package 
                        :severity :WARNING
                        :file (file parser)
                        :description (format nil "Unqualified reference to ~A from package ~A without in-package declaration"
                                           symbol pkg-name)
                        :context (limit-form-size parent-context symbol))))
    
    visibility))


(defun walk-form (form handler)
  "Walk a form calling HANDLER on each subform with context and depth info.
   FORM - The form to analyze
   HANDLER - Function taking (form context parent-context depth)"
  (declare (special log-stream))
  (labels ((walk (x context parent-context depth)
             (format log-stream "~&WALK(ING):~%  Expression: ~S~%  Context: ~S~%  Parent: ~S~%  Depth: ~D~%" 
                                x context parent-context depth)
             (unless (skip-item-p x)  ;skip non-referring items
               (funcall handler x context parent-context depth)  
               ;; Recursively process subforms
               (typecase x
                 (cons 
                   (dolist (element x)
                     (walk element x context (1+ depth))))
                 (array 
                   (dotimes (i (array-total-size x))
                     (walk (row-major-aref x i) x context (1+ depth))))
                 (hash-table 
                   (maphash (lambda (k v)
                             (walk k x context (1+ depth))
                             (walk v x context (1+ depth)))
                           x))))))
    
    ;; Start walking at top level
    (let ((*print-circle* nil)     ; Prevent circular printing issues
          (*print-length* 10)      ; Limit list output
          (*print-level* 5))       ; Limit nesting output
      (format log-stream "~&-------------------------------~%")
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


(defun find-method-call-definitions (parser form)
  "Enhanced method definition matching."
  (let* ((name-and-quals (analyze-call-qualifiers (car form)))
         (name (car name-and-quals))
         (qualifiers (cdr name-and-quals))
         (args (cdr form))
         (pkg-name (current-package-name parser)))
    
    (let* ((gf-key (make-tracking-key name pkg-name :GENERIC-FUNCTION))
           (gf-defs (remove-if (lambda (def)
                                (equal (definition.file def) (file parser)))
                              (gethash gf-key (slot-value *current-tracker* 'definitions)))))
      
      (let* ((method-key (make-tracking-key name pkg-name :METHOD))
             (all-methods (remove-if (lambda (def)
                                     (equal (definition.file def) (file parser)))
                                   (gethash method-key (slot-value *current-tracker* 'definitions))))
             (applicable-methods
              (remove-if-not 
               (lambda (method-def)
                 (and (= (length (definition.specializers method-def))
                        (length args))
                      ;; Enhanced qualifier compatibility
                      (qualifiers-compatible-p 
                       (definition.qualifiers method-def)
                       qualifiers)
                      ;; Specializer checking as before
                      (every #'specializer-compatible-p
                             (definition.specializers method-def)
                             args)))
               all-methods)))
        
        (values (car gf-defs) applicable-methods)))))


(defun qualifiers-compatible-p (method-qualifiers call-qualifiers)
  "Test if method qualifiers are compatible with call qualifiers.
   Handles method combination rules and qualifier ordering."
  (cond ((null call-qualifiers) t)  ; Unqualified call matches primary
        ((null method-qualifiers) nil)  ; Qualified call needs matching method
        ;; Check ordered qualifier matches
        ((equal method-qualifiers call-qualifiers) t)
        ;; Special case for :around/:before/:after combinations
        ((and (member :around call-qualifiers)
              (member :around method-qualifiers)) t)
        (t nil)))


(defun analyze-call-qualifiers (name-form)
  "Analyze a method call name form to extract name and qualifiers.
   Handles multiple qualifier lists correctly."
  (etypecase name-form
    (symbol (list name-form))
    (cons (if (every #'keywordp (butlast name-form))
              (list* (car (last name-form)) (butlast name-form))
              (list (car name-form))))))


(defun specializer-compatible-p (specializer arg)
  "Test if an argument form could be compatible with a method specializer.
   SPECIALIZER can be symbol, string, or specializer form
   ARG is the actual form from the method call"
  (cond
    ;; T specializer matches anything 
    ((or (eq specializer t)
         (eq specializer 'cl:t)
         (string= (princ-to-string specializer) "T"))
     t)
    
    ;; EQL specializer cases
    ((and (consp specializer) 
          (eq (car specializer) 'eql))
     (equal (cadr specializer) arg))
    ((and (stringp specializer)
          (> (length specializer) 5)
          (string= (subseq specializer 0 5) "(EQL "))
     (equal (read-from-string specializer) arg))
    
    ;; Constructor call for class
    ((and (consp arg)
          (symbolp (car arg))
          (string= (symbol-name (car arg))
                  (concatenate 'string "MAKE-" 
                             (princ-to-string specializer))))
     t)
    
    ;; Class/type specializer
    ((ignore-errors (find-class (if (symbolp specializer)
                                   specializer
                                   (read-from-string specializer))
                               nil))
     (if (constantp arg)
         (typep (eval arg) specializer)
         t))
    
    ;; Conservative default
    (t t)))


(defun analyze-function-call-context (symbol context parent-context)
  "Analyze if a symbol appears in a function call context.
   Returns (values call-p name args) where:
   - call-p: T if symbol is being called as a function
   - name: The function name being called
   - args: List of argument forms
   
   Cases handled:
   - Direct calls: (foo arg1 arg2)
   - Funcall form: (funcall #'foo arg1 arg2)
   - Apply form: (apply #'foo args)
   - Special method forms: (call-next-method)"
  (declare (ignore parent-context))
  (when (and context (listp context))
    (cond
      ;; Direct function call
      ((and (eq symbol (car context))
            (not (quoted-form-p context)))
       (values t symbol (cdr context)))
      
      ;; Funcall/apply with function quote
      ((and (member (car context) '(funcall apply))
            (listp (second context))
            (eq (car (second context)) 'function)
            (eq (cadr (second context)) symbol))
       (values t symbol (cddr context)))
      
      ;; Funcall/apply with symbol
      ((and (member (car context) '(funcall apply))
            (eq (second context) symbol))
       (values t symbol (cddr context)))
      
      ;; Special method forms
      ((and (member symbol '(call-next-method next-method-p make-method))
            (eq symbol (car context)))
       (values t symbol (cdr context)))
      
      ;; Not a function call
      (t (values nil nil nil)))))


(defun try-definition-types (subform pkg-name parser context visibility)
  "Attempts to find definitions for a given symbol in various definition types.
  Records references or anomalies based on the results."
  (declare (special log-stream))
  (let ((found-defs nil))
    ;; Try all valid definition types
    (dolist (try-type +valid-definition-types+)
      (let* ((key (make-tracking-key subform pkg-name try-type))
             (defs (gethash key (slot-value *current-tracker* 'definitions))))
        ;; Collect definitions from other files
        (dolist (def defs)
          (unless (equal (definition.file def) (file parser))
            (push def found-defs)))))
    
    ;; Record the appropriate result
    (if found-defs
        ;; Record reference with all found definitions
        (record-reference *current-tracker*
                  :name subform
                  :file (file parser)
                  :package pkg-name
                  :context (limit-form-size context pkg-name)
                  :visibility visibility
                  :definitions found-defs)

        ;; Record anomaly if no matching definition found
        (record-anomaly *current-tracker*
                :name subform
                :type :undefined-reference 
                :severity :WARNING
                :file (file parser)
                :description (format nil "Reference to undefined symbol ~A" subform)
                :context (limit-form-size context pkg-name)))))


(defun handle-method-call (subform parser pkg-name context visibility name args)
  "Handles analysis and recording of method calls."
  (declare (special log-stream))
  (multiple-value-bind (gf-def method-defs)
      (find-method-call-definitions parser (cons name args))
    (if gf-def
        ;; Method call - record with all applicable definitions
        (when (or gf-def method-defs)
          (let* ((name-and-quals (analyze-call-qualifiers name))
                 (qualifiers (cdr name-and-quals))
                 (specializers (remove-duplicates 
                              (alexandria:flatten 
                               (mapcar #'definition.specializers method-defs))
                              :test #'equal)))
            (record-reference *current-tracker*
                  :name subform
                  :file (file parser)
                  :package pkg-name 
                  :context (limit-form-size context pkg-name)
                  :visibility visibility
                  :definitions (cons gf-def method-defs)
                  :qualifiers qualifiers
                  :arguments args
                  :specializers specializers)))
        ;; Not a method call - check other definition types
        (try-definition-types subform pkg-name parser context visibility))))
