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


(defun symbol-status (sym pkg)
  "Determines the status of a symbol in a package, :internal, :external, :inherited, :nil."
  (let ((status (nth-value 1 (find-symbol (symbol-name sym) pkg))))
    (if status
      status
      :nil)))


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
    (sort refs #'string< 
          :key (lambda (r) 
                (with-output-to-string (s)
                  (format s "~A~{~A~}~{~A~}" 
                         (reference.name r)
                         (reference.qualifiers r)
                         (reference.arguments r)))))))


(defun record-definition (tracker &key name type file package status context qualifiers specializers)
  "Record a name definition in the tracker. For methods, qualifiers and specializers
   are used to create a specialized lookup key. Records anomalies for duplicate definitions."
  (let* ((def (make-instance 'definition 
                            :name name
                            :type type 
                            :file file
                            :package package
                            :status status
                            :context context
                            :qualifiers qualifiers
                            :specializers specializers))
         ;; Generate appropriate key based on definition type
         (key (if (eq type :METHOD)
                 (make-tracking-key name package type qualifiers specializers)
                 (make-tracking-key name package type)))
         ;; Look for existing definitions with this key
         (existing-defs (gethash key (slot-value tracker 'definitions)))
         ;; Filter out definitions from same file
         (other-file-defs (remove-if (lambda (d)
                                       (equal (definition.file d) file))
                                     existing-defs)))
    
    ;; Check for duplicates and record anomaly if found in other files
    (when other-file-defs
      (let ((def-files (mapcar #'definition.file other-file-defs)))
        (record-anomaly tracker
                       :name name
                       :type :duplicate-definition
                       :severity :WARNING
                       :file file
                       :description (format nil "~A also defined in ~A" name def-files)
                       :package package
                       :context (definition.context def))))
    
    ;; Store definition under the computed key
    (setf (gethash key (slot-value tracker 'definitions))
      (cons def existing-defs))
    ;; Add to file map
    (let ((file-defs (gethash file (slot-value tracker 'file-map))))
      (setf (gethash file (slot-value tracker 'file-map))
          (cons def file-defs)))
    ;; Record exports for non-package definitions
    (unless (eq type :package)
      (when (eq status :external)
        (record-export tracker package name)))
    def))


(defun record-reference (tracker &key name file type package context visibility definitions
                                      qualifiers arguments)
  "Record a name reference in the tracker.
   VISIBILITY is inherited, imported, or local (defaults to local)
   DEFINITIONS is a non-empty list of definitions this reference depends on
   QUALIFIERS tracks method qualifiers in the call 
   ARGUMENTS is alternating list of argument values and their types"
  (declare (special log-stream))
  (let* ((key (make-tracking-key name (etypecase name
                                        (string name) 
                                        (symbol (when (symbol-package name)
                                                  (package-name (symbol-package name)))))))
         (ref (make-instance 'reference 
                          :name name 
                          :file file
                          :type type
                          :package package 
                          :context context 
                          :visibility visibility
                          :definitions definitions
                          :qualifiers qualifiers
                          :arguments arguments)))
   (pushnew ref (gethash key (slot-value tracker 'references))
        :test #'equalp)
   (format log-stream "~%~A~%" ref)
   ref))


(defun record-anomaly (tracker &key name type severity file description package context)
  "Record a new anomaly in the dependency tracker.
   For :duplicate-definition type, files must be provided as list of all definition locations."
  ;(when (and (eq type :duplicate-definition)
  ;           (not files))
  ;  (error "Files parameter required for duplicate definition anomalies"))
  (let ((anomaly (make-instance 'anomaly 
                               :name name
                               :type type 
                               :severity severity 
                               :file file
                               :description description
                               :package package
                               :context context)))
    (pushnew anomaly (gethash type (anomalies tracker))
                     :test #'equal :key #'anomaly.file)
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

(defun get-file-cycles (&optional tracker)
  "Get all recorded file dependency cycles."
  (file-cycles (ensure-tracker tracker)))

(defun get-package-cycles (&optional tracker)
  "Get all recorded package dependency cycles."
  (package-cycles (ensure-tracker tracker)))


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
                :package chain
                :description (format nil "Package dependency cycle detected: ~A" chain)
                :context cycle)))))


(defun analyze-lambda-list (parser lambda-list)
  "Analyze lambda list using runtime validation where possible.
   Records type dependencies and validates parameter patterns."
  (labels ((validate-type-spec (type-spec)
             "Validate and record type specifier dependency"
             (when type-spec
               (cond
                 ;; Class name
                 ((and (symbolp type-spec)
                       (find-class type-spec nil))
                  (analyze-definition-form parser type-spec))
                 ;; Type specifier
                 ((ignore-errors (subtypep type-spec t))
                  (analyze-definition-form parser type-spec)))))
           
           (analyze-default-form (form)
             "Analyze potential dependencies in default value form"
             (when form
               (analyze-definition-form parser form)))
           
           (analyze-destructuring-pattern (pattern)
             "Analyze nested destructuring pattern"
             (when (consp pattern)
               (dolist (elem pattern)
                 (typecase elem
                   (cons 
                    (if (and (= (length elem) 2)
                            (symbolp (car elem)))
                        ;; Type declaration
                        (validate-type-spec (cadr elem))
                        ;; Nested pattern
                        (analyze-destructuring-pattern elem)))
                   (symbol nil))))) ; Simple var
           
           (analyze-specialized-parameter (param)
             "Analyze method parameter specializer"
             (when (and (consp param)
                       (= (length param) 2)
                       (symbolp (car param)))
               (let ((specializer (cadr param)))
                 (typecase specializer
                   ;; EQL specializer
                   ((cons (eql eql))
                    (analyze-definition-form parser (cadr specializer)))
                   ;; Class/type specializer
                   (symbol
                    (validate-type-spec specializer)))))))
    
    (let ((state :required))
      (dolist (item lambda-list)
        (cond
          ;; Lambda list keywords change state
          ((member item lambda-list-keywords)
           (setf state item))
          
          ;; Process parameter based on current state
          (t
           (case state
             (:required
              (typecase item
                (symbol nil) ; Simple parameter
                (cons
                 (if (and (= (length item) 2)
                         (symbolp (car item)))
                     ;; Method specializer
                     (analyze-specialized-parameter item)
                     ;; Destructuring pattern
                     (analyze-destructuring-pattern item)))))
             
             (&optional
              (typecase item
                (symbol nil) ; Simple optional
                (cons
                 ;; (param default supplied-p)
                 (when (cddr item)
                   (analyze-default-form (third item))))))
             
             (&key
              (typecase item
                (symbol nil) ; Simple keyword
                (cons
                 (let* ((key-part (car item))
                        (has-key-name (consp key-part)))
                   ;; ((key var) default supplied-p)
                   (when (cddr item)
                     (analyze-default-form (third item)))
                   ;; Check for type declaration
                   (when (and has-key-name
                            (cddr key-part))
                     (validate-type-spec (third key-part)))))))
             
             (&aux
              (when (and (consp item)
                         (cdr item))
                (analyze-default-form (second item)))))))))))


(defun cl-symbol-p (item)
 "Return true if item is from the common-lisp package."
 (and (symbolp item)
      (eq (symbol-package item) 
          (find-package :common-lisp))))


(defun quoted-symbol-p (item)
 "Return true if item is quoted with quote or function quote;
  eg, 'var, (quote var), #'foo, (function foo)."
 (and (consp item)
      (or (eq (car item) 'quote)
          (eq (car item) 'function))))


(defun symbol-qualified-p (symbol parser)
  "Return t if symbol appears to have been package-qualified in source.
   This is inferred when symbol's package differs from parser's current package."
  (and (symbol-package symbol)  ; Has a package
       (not (eq (symbol-package symbol) 
                (current-package parser)))))


(defun skip-item-p (form)
  "Return T if form should be skipped during definition and reference analysis."
  (or (null form)
      (numberp form) 
      (stringp form)
      (keywordp form)
      (cl-symbol-p form)
      (quoted-symbol-p form)))


(defun skip-definition-form (form)
  "Return T if form should be skipped during reference analysis."
  (and (consp form)
       (member (car form) '(defun defvar defparameter defconstant 
                            defclass defstruct defmethod defgeneric 
                            define-condition defmacro define-method-combination
                            define-modify-macro define-compiler-macro
                            define-symbol-macro defsetf define-setf-expander
                            deftype))))


(defun check-package-reference (symbol parser tracker context parent-context)
  "Check symbol reference visibility using runtime package state."
  (let* ((sym-pkg (symbol-package (if (and (consp symbol)
                                         (eq (car symbol) 'quote))
                                    (cadr symbol)
                                    symbol)))
         (cur-pkg (current-package parser))
         (pkg-name (if sym-pkg 
                      (package-name sym-pkg)
                      (current-package-name parser)))
         (package-ops '(defpackage in-package use-package make-package 
                       rename-package delete-package)))

    ;; Determine visibility using runtime package state
    (multiple-value-bind (found-sym status)
        (when cur-pkg
          (find-symbol (string symbol) cur-pkg))
      (let ((visibility 
             (cond
               ;; Uninterned symbol
               ((null sym-pkg) :LOCAL)
               ;; Symbol in current package
               ((eq sym-pkg cur-pkg) :LOCAL)
               ;; Found in current package
               (found-sym
                (case status
                  (:inherited :INHERITED)
                  (:external :IMPORTED)
                  (t :LOCAL)))
               ;; Not found
               (t :LOCAL))))

        ;; Check for unqualified references from CL-USER
        (when (eq cur-pkg (find-package :common-lisp-user))
          (when (and (not (eq (project-package *current-tracker*) cur-pkg))
                 ;; Not qualified with package
                 (not (eq sym-pkg (find-package :common-lisp-user)))
                 ;; Not a CL symbol
                 (not (eq sym-pkg (find-package :common-lisp)))
                 ;; Not a package operation keyword
                 (not (member symbol '(:use cl common-lisp)))
                 ;; Not in a package operation form
                 (not (or (and (consp context)
                              (member (car context) package-ops))
                         (and (consp parent-context)
                              (member (car parent-context) package-ops))))
                 ;; Not a package being defined
                 (not (or (and (consp context)
                              (eq (car context) 'defpackage)
                              (eq symbol (cadr context)))
                         (and (consp parent-context)
                              (eq (car parent-context) 'defpackage)
                              (eq symbol (cadr parent-context))))))
            
            (record-anomaly tracker
                           :name symbol
                           :type :possible-missing-in-package-for-reference 
                           :severity :WARNING
                           :file (file parser)
                           :package cur-pkg
                           :description (format nil "Unqualified reference to ~A from package ~A without in-package declaration"
                                              symbol pkg-name)
                           :context parent-context)))
        visibility))))


(defun walk-form (form handler)
  "Walk a form calling HANDLER on each subform with context and parent context info.
   FORM - The form to analyze
   HANDLER - Function taking (form context parent-context)"
  (declare (special log-stream))
  (labels ((walk (x context parent-context)
             (format log-stream "~&WALK(ING):~%  Expression: ~S~%  Context: ~S~%  Parent: ~S~%" 
                              x context parent-context)
             (unless (skip-item-p x)  ;skip non-referring items
               (funcall handler x context parent-context)  
               ;; Recursively process subforms
               (typecase x
                 (cons 
                   (dolist (element x)
                     (walk element x context)))
                 (array 
                   (dotimes (i (array-total-size x))
                     (walk (row-major-aref x i) x context)))
                 (hash-table 
                   (maphash (lambda (k v)
                             (walk k x context)
                             (walk v x context))
                           x))))))
    
    ;; Start walking at top level
    (let ((*print-circle* nil)     ; Prevent circular printing issues
          (*print-length* 10)      ; Limit list length 
          (*print-level* 5))       ; Limit nesting output
      (format log-stream "~&-------------------------------~%")
      (walk form form form))))


(defun package-context-p (context parent-context)
  "Return T if the context indicates a form that accepts package designator references.
   Such contexts include:
   - Direct package forms (defpackage, in-package, use-package etc)  
   - Package options in defpackage (:use, :import-from, :nicknames)"
  (and (consp context)
       (or ;; Direct package forms
           (member (car context) '(defpackage in-package use-package 
                                   make-package rename-package delete-package))
           ;; Package options
           (and (consp parent-context)
                (eq (car parent-context) 'defpackage)
                (keywordp (car context))
                (member (car context) '(:use :import-from :nicknames))))))


(defun not-interned-p (symbol)
  (not (symbol-package symbol)))


(defun find-method-call-definitions (parser form)
  "Find method and generic function definitions for a method call form."
  (let* ((name (car form))
         (args (mapcar (lambda (arg)
                        (ignore-errors (eval arg))) 
                      (cdr form)))
         (gf (ignore-errors (fdefinition name))))
    (when (typep gf 'standard-generic-function)
      (let* ((methods (ignore-errors (compute-applicable-methods gf args)))
             (pkg-name (current-package-name parser))
             (gf-key (make-tracking-key name pkg-name :GENERIC-FUNCTION))
             (gf-defs (gethash gf-key (slot-value *current-tracker* 'definitions)))
             (method-defs (loop for method in methods
                               for specs = (mapcar #'class-name 
                                                 (closer-mop:method-specializers method))
                               for method-key = (make-tracking-key name pkg-name :METHOD 
                                                                 nil  ; qualifiers 
                                                                 specs)
                               append (gethash method-key (slot-value *current-tracker* 'definitions)))))
        (values (car gf-defs) method-defs)))))


(defun type-compatible-p (specializer arg parser)
  "Test if argument could match specializer using runtime type info.
   Uses only standard CL type operations for compatibility checks."
  (flet ((get-make-name (type)
           (intern (concatenate 'string "MAKE-" (string type))
                  (if (symbolp type)
                      (symbol-package type)
                      *package*))))
    
    (cond
      ;; T specializer matches anything
      ((or (eq specializer t)
           (eq specializer 'cl:t)
           (string= (princ-to-string specializer) "T"))
       t)
      
      ;; EQL specializer
      ((and (listp specializer)
           (eq (car specializer) 'eql))
       (let ((eql-val (cadr specializer)))
         (cond
           ;; Constant value - direct comparison
           ((or (numberp arg) (characterp arg) 
                (stringp arg) (pathnamep arg))
            (equal eql-val arg))
           ;; Symbol comparison
           ((symbolp arg)
            (eq eql-val arg))
           ;; Non-constant - assume potentially compatible
           (t t))))
      
      ;; Constructor form 
      ((and (or (and (consp arg)
                     (symbolp (car arg))
                     (eq (car arg) (get-make-name specializer)))
                (and (symbolp arg)
                     (eq arg (get-make-name specializer))))
            (find-package (current-package-name parser)))
       (let ((*package* (find-package (current-package-name parser))))
         (handler-case
             (if (consp arg)
                 ;; Verify constructor exists and returns compatible type
                 (when (fboundp (car arg))
                   (or 
                    ;; Check if specializer names a class
                    (find-class specializer nil)
                    ;; Or if it's a type specifier
                    (ignore-errors
                      (subtypep specializer t))))
                 ;; Just verify function existence for symbol
                 (fboundp arg))
           (error () t))))
      
      ;; Class/type specializer
      ((symbolp specializer)
       (multiple-value-bind (subtype-p valid-p)
           (ignore-errors 
             (cond
               ;; Known literal type
               ((or (numberp arg) (characterp arg)
                    (stringp arg) (pathnamep arg))
                (subtypep (type-of arg) specializer))
               ;; make-instance form
               ((and (consp arg)
                     (eq (car arg) 'make-instance))
                (ignore-errors
                  (and (find-class (second arg) nil)
                       (subtypep (second arg) specializer))))
               ;; Symbol - check if names a type
               ((symbolp arg)
                (or 
                 ;; Check as class name
                 (and (find-class arg nil)
                      (subtypep arg specializer))
                 ;; Check as type specifier
                 (ignore-errors
                   (subtypep arg specializer))))
               ;; Other forms - assume compatible
               (t t)))
         (if valid-p
             subtype-p
             t)))
      
      ;; Unknown/complex specializer - conservative
      (t t))))


(defun analyze-call-qualifiers (name-form)
  "Analyze a method call name form to extract name and qualifiers.
   Handles multiple qualifier lists correctly."
  (etypecase name-form
    (symbol (list name-form))
    (cons (if (every #'keywordp (butlast name-form))
              (list* (car (last name-form)) (butlast name-form))
              (list (car name-form))))))


(defun specializer-compatible-p (specializer arg)
  "Test if an argument form could match a method specializer.
   Uses runtime type information when available.
   Returns T if potentially compatible, NIL if definitely incompatible."
  (labels ((parse-eql-spec (spec)
             "Parse EQL specializer form, handling string/cons formats"
             (typecase spec
               (cons 
                (when (eq (car spec) 'eql)
                  (cadr spec)))
               (string
                (ignore-errors
                  (let ((form (read-from-string spec)))
                    (when (and (consp form)
                              (eq (car form) 'eql))
                      (cadr form)))))))
           
           (get-constructor-name (type-name)
             "Get constructor function name for a type"
             (intern (concatenate 'string "MAKE-" 
                                (string type-name))
                    (if (symbolp type-name)
                        (symbol-package type-name)
                        *package*)))
           
           (constant-value-p (form)
             "Test if form represents a constant value"
             (or (numberp form)
                 (characterp form)
                 (stringp form)
                 (pathnamep form)
                 (and (symbolp form)
                      (not (boundp form)))))
           
           (valid-type-p (type)
             "Test if type specifier names a valid type"
             (or (find-class type nil)
                 (ignore-errors
                   (subtypep type t)))))
    
    (cond
      ;; T specializer - matches anything
      ((or (eq specializer t)
           (eq specializer 'cl:t)
           (string= (princ-to-string specializer) "T"))
       t)
      
      ;; EQL specializer
      ((or (and (consp specializer)
                (eq (car specializer) 'eql))
           (and (stringp specializer)
                (> (length specializer) 5)
                (string= (subseq specializer 0 5) "(EQL ")))
       (let ((eql-val (parse-eql-spec specializer)))
         (cond
           ;; Constant value comparison
           ((constant-value-p arg)
            (equal eql-val arg))
           ;; Variable or complex form - assume potentially compatible
           (t t))))
      
      ;; Constructor form for class
      ((and (consp arg)
            (symbolp (car arg))
            (eq (car arg)
                (get-constructor-name specializer)))
       (if (valid-type-p specializer)
           ;; Verify constructor exists
           (fboundp (get-constructor-name specializer))
           t))
      
      ;; Non-constant compound form
      ((consp arg)
       (or 
        ;; make-instance form check
        (and (eq (car arg) 'make-instance)
             (valid-type-p (second arg))
             (subtypep (second arg) specializer))
        ;; Other forms - conservative
        t))
      
      ;; Simple type specializer with constant
      ((and (not (consp arg))
            (valid-type-p specializer))
       (if (constant-value-p arg)
           (typep arg specializer)
           t))
      
      ;; Unknown/complex case - conservative
      (t t))))


(defun analyze-function-call-context (symbol context)
  "Analyze if a symbol appears in a function call context.
   Returns (values call-p name args caller-type), where:
   - call-p: T if symbol is being called as a function
   - name: The function name being called
   - args: List of argument forms
   - caller-type: One of :direct :funcall :apply :method :macro"
  (labels ((quoted-p (form)
             "Test if form is quoted"
             (and (consp form)
                  (member (car form) '(quote function))))
           
           (function-name-p (sym)
             "Test if symbol can be a function name"
             (and sym
                  (symbolp sym)
                  (not (keywordp sym))
                  (not (special-operator-p sym))
                  (not (macro-function sym))))
           
           (valid-function-p (sym)
             "Test if symbol names a valid function"
             (or (cl-symbol-p sym)  ; CL package functions always valid
                 (and (function-name-p sym)
                      (fboundp sym)))))
    
    (when (and context (listp context))
      (cond
        ;; Direct function call 
        ((and (eq symbol (car context))
              (not (quoted-p context))
              (valid-function-p symbol))
         (values t 
                 symbol 
                 (cdr context)
                 :direct))
        
        ;; Funcall with #'function form
        ((and (eq (car context) 'funcall)
              (listp (second context))
              (eq (car (second context)) 'function)
              (eq (cadr (second context)) symbol)
              (valid-function-p symbol))
         (values t 
                 symbol 
                 (cddr context)
                 :funcall))
        
        ;; Apply with #'function form
        ((and (eq (car context) 'apply)
              (listp (second context))
              (eq (car (second context)) 'function)
              (eq (cadr (second context)) symbol)
              (valid-function-p symbol))
         (values t 
                 symbol 
                 (cddr context)
                 :apply))
        
        ;; Funcall/apply with symbol
        ((and (member (car context) '(funcall apply))
              (eq (second context) symbol)
              (valid-function-p symbol))
         (values t 
                 symbol 
                 (cddr context)
                 (if (eq (car context) 'funcall)
                     :funcall
                     :apply)))
        
        ;; Method combination forms
        ((and (member symbol '(call-next-method next-method-p make-method))
              (eq symbol (car context)))
         (values t 
                 symbol 
                 (cdr context)
                 :method))
        
        ;; Not a function call
        (t (values nil nil nil nil))))))


(defun slot-definition-p (symbol context)
  "Returns true if symbol appears as a struct/class slot definition."
  (and (consp context)
       (or (and (eq (car context) 'defstruct)
                (consp (cdr context))
                (or (symbolp (cadr context))
                    (and (consp (cadr context))
                         (symbolp (caadr context)))))
       (member symbol (cddr context) :key (lambda (x)
                                          (if (consp x) (car x) x))))))


(defun extract-specializers (lambda-list)
  "Extract specializer types from a method lambda list.
   Returns list of type specializers as symbols or lists for (eql ...) forms.
   Returns (T) for non-method lambda lists.
   Examples: 
    ((data string) x &key k) -> (STRING T)
    (a (b number) (c (eql 42))) -> (T NUMBER (EQL 42))"
  (loop for param in (loop for param in lambda-list
                          until (and (symbolp param) 
                                   (member param lambda-list-keywords))
                          collect param)
        collect (if (listp param)
                   (second param) 
                   t)))


(defun lambda-list-p (form)
  "Test if form appears to be a method lambda list."
  (and (listp form)
       (every (lambda (x)
                (or (symbolp x)
                    (and (listp x)
                         (= (length x) 2)
                         (symbolp (car x)))))
              form)))


(defun analyze-method-specializers (parser lambda-list)
  "Analyze specializer types in method lambda list."
  (dolist (param lambda-list)
    (when (and (listp param)
               (= (length param) 2))
      (let ((specializer (second param)))
        (typecase specializer
          (cons  ; (eql value) specializer
           (when (eq (car specializer) 'eql)
             (analyze-definition-form parser (cadr specializer))))
          (symbol  ; Class/type specializer
           (analyze-definition-form parser specializer)))))))

;;;;;;;;;;;;


(defun get-defstruct-conc-name (name current-form)
  "Extract the :conc-name prefix for a defstruct.
   Returns string to prepend to slot names."
  (if (and (consp (second current-form))           ; Has options list
           (consp (cadr current-form)))            ; Not just a name
      (let ((options (cdadr current-form)))        ; Skip struct name
        (let ((conc-option (find :conc-name options :key #'car)))
          (if conc-option
              (let ((prefix (cadr conc-option)))
                (cond ((null prefix) "")       ; (:conc-name nil)
                      ((stringp prefix) prefix)
                      (t (string prefix))))    ; Symbol case
              (format nil "~A-" (string name)))))
      (format nil "~A-" (string name))))      ; Default case


(defun record-slot-accessors (parser name class current-form)
  "Record all slot accessor definitions for a class/struct/condition"
  (declare (special log-stream))
  (let ((pkg (current-package parser)))
    (if (typep class 'structure-class)
        ;; Structure slots use naming pattern with conc-name
        (let ((prefix (get-defstruct-conc-name name current-form)))
          (dolist (slot (c2mop:class-slots class))
            (let ((accessor-name 
                   (intern (format nil "~A~A" 
                                 prefix
                                 (symbol-name (c2mop:slot-definition-name slot)))
                           pkg)))
              (when (fboundp accessor-name)
                (record-definition *current-tracker*
                                 :name accessor-name
                                 :type :function
                                 :file (file parser)
                                 :package pkg
                                 :status (symbol-status accessor-name pkg)
                                 :context current-form)))))
        ;; CLOS class slots use MOP info
        (progn 
          (c2mop:finalize-inheritance class)
          (dolist (slot (c2mop:class-slots class))
            ;; Record reader methods
            (dolist (reader (c2mop:slot-definition-readers slot))
              (when (fboundp reader)
                (record-definition *current-tracker*
                                 :name reader
                                 :type :function
                                 :file (file parser)
                                 :package pkg
                                 :status (symbol-status reader pkg)
                                 :context current-form)))
            ;; Record writer methods
            (dolist (writer (c2mop:slot-definition-writers slot))
              (when (fboundp `(setf ,writer))
                (record-definition *current-tracker*
                                 :name writer
                                 :type :function
                                 :file (file parser)
                                 :package pkg
                                 :status (symbol-status writer pkg)
                                 :context current-form))))))))


(defun record-defstruct-functions (parser name class current-form)
  "Record constructor, copier, and predicate functions for structures"
  (declare (special log-stream) (ignorable class))
  (let ((pkg (current-package parser)))
    ;; Constructor
    (let ((make-name (intern (format nil "MAKE-~A" (symbol-name name)) pkg)))
      (when (fboundp make-name)
        (record-definition *current-tracker*
                          :name make-name
                          :type :function
                          :file (file parser)
                          :package pkg
                          :status (symbol-status make-name pkg)
                          :context current-form)))
    ;; Copier
    (let ((copy-name (intern (format nil "COPY-~A" (symbol-name name)) pkg)))
      (when (fboundp copy-name)
        (record-definition *current-tracker*
                          :name copy-name
                          :type :function
                          :file (file parser)
                          :package pkg
                          :status (symbol-status copy-name pkg)
                          :context current-form)))
    ;; Predicate
    (let ((pred-name (intern (format nil "~A-P" (symbol-name name)) pkg)))
      (when (fboundp pred-name)
        (record-definition *current-tracker*
                          :name pred-name
                          :type :function
                          :file (file parser)
                          :package pkg
                          :status (symbol-status pred-name pkg)
                          :context current-form)))))


(defun analyze-defclass/defstruct/define-condition (parser name form)
  "Analyze a defclass, defstruct, or define-condition form.
   Records the primary type definition and all implicitly defined functions."
  (declare (special log-stream))
  (let ((def-op (first form))
        (class (find-class name nil)))
    (when class   ;only analyze runtime objects if class object exists
      ;; Common accessor analysis
      (record-slot-accessors parser name class form)
      ;; Only defstruct has additional implicit functions
      (when (eq def-op 'defstruct)
        (record-defstruct-functions parser name class form)))))


(defun analyze-defpackage/make-package (parser name current-form)
  "Handle defpackage/make-package form recording nicknames as definitions.
   Uses runtime package info when available, falls back to form parsing
   for make-package when package doesn't yet exist."
  (declare (special log-stream))
  (let* ((file (file parser))
         (context current-form)
         (form-type (car current-form))
         (package (find-package name)))
    
    ;; Try runtime first for both forms
    (if package
        ;; Use runtime info when available
        (dolist (nickname (package-nicknames package))
          (record-definition *current-tracker*
                           :name nickname
                           :type :PACKAGE  
                           :file file
                           :context context))
        
        ;; Fall back to parsing for make-package only
        (when (eq form-type 'make-package)
          (loop for (key val) on (cddr current-form) by #'cddr
                when (eq key :nicknames)
                do (dolist (nick (ensure-list (if (and (consp val)
                                                      (eq (car val) 'quote))
                                               (cadr val)
                                               val)))
                     (record-definition *current-tracker*
                                      :name nick
                                      :type :PACKAGE
                                      :file file
                                      :context context)))))
    
    ;; Process exports only when package exists
    (when package
      (do-external-symbols (sym package)
        (cond 
          ((boundp sym)  ; Variable binding
           (record-definition *current-tracker*
                            :name sym
                            :type :VARIABLE
                            :file file
                            :package package
                            :status :EXTERNAL
                            :context context))
          ((macro-function sym)  ; Macro binding - check before fboundp
           (record-definition *current-tracker*
                            :name sym
                            :type :MACRO
                            :file file
                            :package package
                            :status :EXTERNAL
                            :context context))
          ((fboundp sym)  ; Function binding (not macro)
           (if (typep (symbol-function sym) 'standard-generic-function)
               (record-definition *current-tracker*
                                :name sym
                                :type :GENERIC-FUNCTION
                                :file file
                                :package package
                                :status :EXTERNAL
                                :context context)
               (record-definition *current-tracker*
                                :name sym
                                :type :FUNCTION
                                :file file
                                :package package
                                :status :EXTERNAL
                                :context context)))
          ((find-class sym nil)  ; Class definition
           (record-definition *current-tracker*
                            :name sym
                            :type :STRUCTURE/CLASS/CONDITION
                            :file file
                            :package package
                            :status :EXTERNAL
                            :context context))
          ((ignore-errors (subtypep nil sym))  ; Type definition
           (record-definition *current-tracker*
                            :name sym
                            :type :DEFTYPE
                            :file file
                            :package package
                            :status :EXTERNAL
                            :context context)))))))


(defun get-symbol-reference-type (sym context)
  "Derive the reference type of a symbol."
  (cond ((boundp sym) :variable)
        ((macro-function sym) :macro) 
        ((fboundp sym) 
         (let ((fn (fdefinition sym)))
           (cond ((compatible-method-p sym context) :method)
                 ((c2mop:typep fn 'standard-generic-function) :generic-function)
                 (t :function))))
        ((ignore-errors (find-class sym)) :STRUCTURE/CLASS/CONDITION)
        ((and (subtypep sym t)
              (not (ignore-errors (find-class sym)))
              (not (cl-symbol-p sym))) :DEFTYPE)
        ((nth-value 1 (macroexpand-1 sym)) :symbol-macro)))


(defun compatible-method-p (sym context)
  (when (fboundp sym)
    (let* ((fn (fdefinition sym))
           (args (rest context))
           (sym-pkg (symbol-package sym)))
      (when (typep fn 'standard-generic-function)
        (some (lambda (method)
                (every (lambda (spec arg)
                        (type-compatible-p 
                          (class-name spec)  ; Convert class object to symbol name
                          arg 
                          sym-pkg))
                      (c2mop:method-specializers method)
                      args))
              (c2mop:generic-function-methods fn))))))


(defun get-visibility (sym current-pkg)
  "Finds the visibility of a symbol--eg, (:inherited #<PACKAGE \"PKG1\">)"
  (let ((sym-pkg (symbol-package sym)))
    (if (eq sym-pkg current-pkg)
      (list :local current-pkg))
    (let ((usedp (member sym-pkg (package-use-list current-pkg)))
          (importedp (or (member sym (package-internal-symbols current-pkg))
                         (member sym (package-external-symbols current-pkg)))))
      (cond ((and usedp (not importedp)) (list :inherited current-pkg))
            ((and (not usedp) importedp) (list :imported current-pkg))
            ((and usedp importedp)       (list :inherited :imported current-pkg))
            (t                           (list :not-visible current-pkg))))))


(defun package-external-symbols (pkg)
  "Return a list of all external symbols in PKG."
  (let (acc)
    (do-external-symbols (sym pkg)
      (push sym acc))
    (nreverse acc)))


(defun package-internal-symbols (pkg)
  "Return a list of all internal symbols in PKG.
By 'internal' here we mean symbols accessible from PKG but not exported.
DO-SYMBOLS iterates over all symbols accessible from PKG, including inherited ones."
  (let (acc)
    (do-symbols (sym pkg)
      ;; Check that the symbol is actually home to PKG (so we don't pick up inherited ones),
      ;; and that it is not exported.
      (when (and (eq (symbol-package sym) pkg)
                 (not (symbol-exported-p sym pkg)))
        (push sym acc)))
    (nreverse acc)))


(defun symbol-exported-p (sym pkg)
  "Return T if SYM is exported from PKG; otherwise NIL."
  (eq (nth-value 1 (find-symbol (symbol-name sym) pkg)) :external))
