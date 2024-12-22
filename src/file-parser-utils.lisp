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
    (sort refs #'string< 
          :key (lambda (r) 
                (with-output-to-string (s)
                  (format s "~A~{~A~}~{~A~}" 
                         (reference.name r)
                         (reference.qualifiers r)
                         (reference.arguments r)))))))


(defun record-definition (tracker &key name type file package exported-p context qualifiers lambda-list)
  "Record a name definition in the tracker. Creates a single definition object and 
   stores it under both a base key (name+package+type) and, if qualifiers/lambda-list 
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
                            :qualifiers (when qualifiers (list qualifiers)) ; Ensure list
                            :lambda-list lambda-list))
         (base-key (make-tracking-key name package type))
         (specialized-key (when (and (eq type :METHOD) lambda-list)
                          (make-tracking-key name package type 
                                           (ensure-list qualifiers)
                                           (extract-specializers lambda-list))))
         ;; For methods, use specialized key to find matches
         (existing-defs (if specialized-key
                           (gethash specialized-key (slot-value tracker 'definitions))
                           (gethash base-key (slot-value tracker 'definitions))))
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
                       :description (format nil "~A ~A defined multiple times" type name)
                       :context (definition.context def)
                       :files (cons file def-files))))
    
    ;; Store under both keys
    (push def (gethash base-key (slot-value tracker 'definitions)))
    (when specialized-key
      (push def (gethash specialized-key (slot-value tracker 'definitions))))
    ;; Add to file map and exports  
    (push def (gethash file (slot-value tracker 'file-map)))
    (when exported-p
      (record-export tracker package name))
    def))


(defun record-reference (tracker &key name file package context visibility definitions
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
                          :package package 
                          :context context 
                          :visibility (or visibility :LOCAL)
                          :definitions definitions
                          :qualifiers qualifiers
                          :arguments arguments)))
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
  "Check symbol reference visibility using runtime package state.
   Returns symbol visibility (:LOCAL :INHERITED :IMPORTED).
   Records anomalies for unqualified cross-package references."
  (let* ((sym-pkg (symbol-package symbol))
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
          (when (and 
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
                           :type :missing-in-package 
                           :severity :WARNING
                           :file (file parser)
                           :description (format nil "Unqualified reference to ~A from package ~A without in-package declaration"
                                              symbol pkg-name)
                           :context (limit-form-size parent-context symbol))))
        
        visibility))))


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
   Returns (values qualifiers lambda-list body).
   ARGS is the list of arguments after the :method keyword.
   Examples:
   - (:before (x) ...) -> (:before), (x), ...
   - ((x string) y) -> (), ((x string) y), ...
   - (:after (x number) y) -> (:after), ((x number) y), ...
   - (:around (x) &key k) -> (:around), (x &key k), ...
   Method lambda lists can contain &optional, &rest, &key, &allow-other-keys
   but not &aux."
  (let ((qualifiers nil)
        lambda-list)
    ;; Collect qualifiers until we hit the lambda-list
    (loop for arg = (car args)
          while (and args 
                    (or (keywordp arg) (symbolp arg))
                    (not (member arg lambda-list-keywords))
                    (not (and (listp arg) 
                            (or (symbolp (car arg))  ; Regular param
                                (and (listp (car arg))  ; Specialized param
                                     (= (length (car arg)) 2))))))
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
  "Find method and generic function definitions for a method call form.
   Returns (values gf-def method-defs) where:
   - gf-def is the generic function definition from our tables
   - method-defs are the applicable method definitions from our tables
   FORM is of form: (name . args)"
  (let* ((name (car form))
         (args (cdr form))
         (gf (ignore-errors (fdefinition name))))
    (when (typep gf 'standard-generic-function)
      (let* ((methods (ignore-errors (compute-applicable-methods gf args)))
             (pkg-name (current-package-name parser))
             (gf-key (make-tracking-key name pkg-name :GENERIC-FUNCTION))
             (gf-defs (gethash gf-key (slot-value *current-tracker* 'definitions)))
             (method-defs (loop for method in methods
                               for method-key = (make-tracking-key name pkg-name :METHOD)
                               append (gethash method-key (slot-value *current-tracker* 'definitions)))))
        (values (car gf-defs) method-defs)))))


(defun match-optional-arguments (lambda-list args key-start-pos parser)
  "Match arguments against optional parameters in a lambda list.
   Returns T if arguments could match the optional parameters.
   Handles type declarations and nested destructuring patterns."
  (labels ((find-section-end (start lambda-list)
             "Find end of optional section"
             (or (position-if #'lambda-list-keyword-p 
                            lambda-list 
                            :start (or start 0))
                 (length lambda-list)))
           
           (lambda-list-keyword-p (item)
             "Test for lambda-list section markers"
             (member item '(&optional &rest &key &aux &allow-other-keys &whole &environment)))
           
           (extract-type-spec (param)
             "Get type declaration from parameter"
             (typecase param
               (symbol t)  ; Simple parameter
               (cons      ; Complex parameter
                (if (listp (car param))
                    ;; Destructuring or specializer
                    (or (third (car param)) t)
                    ;; Optional with default/supplied-p
                    (or (third param) t)))))
           
           (valid-optional-param-p (param)
             "Verify optional parameter syntax"
             (typecase param
               (symbol t)  ; Simple var
               (cons      ; Complex form
                (and (or (symbolp (car param))     ; (var default supplied-p)
                        (and (listp (car param))   ; ((var type) default supplied-p)
                             (<= (length (car param)) 3)
                             (symbolp (caar param))))
                     (<= (length param) 3)))))
           
           (count-required-params (lambda-list)
             "Count required parameters before &optional"
             (let ((opt-pos (position '&optional lambda-list)))
               (if opt-pos
                   opt-pos
                   0))))
    
    (let* ((required-count (count-required-params lambda-list))
           (opt-start (position '&optional lambda-list))
           (opt-section-end (and opt-start
                                (find-section-end opt-start lambda-list)))
           (opt-params (when opt-start
                        (subseq lambda-list 
                                (1+ opt-start) 
                                opt-section-end)))
           (max-opt-args (length opt-params))
           (provided-args (length args)))
      
      ;; Validate all optional parameters
      (when (and opt-start opt-params)
        (unless (every #'valid-optional-param-p opt-params)
          (return-from match-optional-arguments nil)))
      
      (cond
        ;; No optional parameters - must have zero args
        ((null opt-start)
         (zerop provided-args))
        
        ;; Key parameters present - check boundary
        ((and key-start-pos 
              (> provided-args (- key-start-pos required-count)))
         nil)
        
        ;; Too many arguments
        ((> provided-args max-opt-args)
         nil)
        
        ;; Match each argument against its parameter
        (t
         (loop for arg in args
               for param in opt-params
               always (let ((type-spec (extract-type-spec param)))
                       (or (eq type-spec t)
                           (type-compatible-p type-spec arg parser)))))))))


(defun lambda-list-accepts-keys-p (lambda-list key-args)
  "Test if a lambda list can accept the given keyword arguments.
   Handles complex parameter patterns and keyword normalization."
  (labels ((find-section-start (section lambda-list)
             "Find starting position of a lambda list section"
             (position section lambda-list))
           
           (find-section-end (start lambda-list)
             "Find end of current lambda list section"
             (or (position-if (lambda (x) 
                              (and (symbolp x)
                                   (char= (char (symbol-name x) 0) #\&)))
                            lambda-list
                            :start (1+ start))
                 (length lambda-list)))
           
           (extract-key-name (param-spec)
             "Extract normalized keyword name from parameter spec"
             (typecase param-spec
               (symbol 
                (intern (symbol-name param-spec) :keyword))
               (cons
                (if (listp (car param-spec))
                    ;; ((keyword var) default supplied-p)
                    (caar param-spec)
                    ;; (var default supplied-p)
                    (intern (symbol-name (car param-spec)) :keyword)))))
           
           (valid-key-param-p (param)
             "Validate keyword parameter syntax"
             (typecase param
               (symbol 
                (not (member param lambda-list-keywords)))
               (cons
                (and (or (symbolp (car param))     ; (var default supplied-p)
                        (and (listp (car param))   ; ((key var) default supplied-p)
                             (= (length (car param)) 2)
                             (or (keywordp (caar param))
                                 (symbolp (caar param)))
                             (symbolp (cadar param))))
                     (<= (length param) 3)))))
           
           (get-allowed-keys (key-params)
             "Extract allowed keywords from parameters"
             (loop for param in key-params
                   when (valid-key-param-p param)
                   collect (extract-key-name param))))
    
    (let* ((key-start (find-section-start '&key lambda-list))
           (rest-start (find-section-start '&rest lambda-list))
           (allow-others-p (and key-start
                              (find '&allow-other-keys lambda-list 
                                   :start key-start)))
           (key-section-end (and key-start
                                (find-section-end key-start lambda-list))))
      
      (cond
        ;; No key args provided - always valid
        ((null key-args)
         t)
        
        ;; No &key in lambda list - check &rest
        ((null key-start)
         (and rest-start t))
        
        ;; Has &allow-other-keys - all keys valid
        (allow-others-p
         t)
        
        ;; Must validate against declared parameters
        (t
         (let* ((key-params (subseq lambda-list 
                                   (1+ key-start)
                                   key-section-end))
                (allowed-keys (get-allowed-keys key-params)))
           ;; Every provided key must be allowed
           (every (lambda (key-arg)
                   (member (car key-arg) allowed-keys))
                 key-args)))))))


(defun qualifier-match-p (call-quals method-quals)
  "Test if method qualifiers match call qualifiers, following CLOS rules.
   Handles method combination scenarios like :before/:after/:around."
  (or (equal call-quals method-quals)  ; Exact match
      ;; :around can wrap any other qualifier
      (and (member :around call-quals)  
           (member :around method-quals))
      ;; :before/:after care about order
      (and (member :before call-quals)
           (equal (position :before call-quals)
                  (position :before method-quals)))
      (and (member :after call-quals)
           (equal (position :after call-quals)
                  (position :after method-quals)))))


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


(defun qualifiers-compatible-p (method-qualifiers call-qualifiers)
  "Test if method qualifiers are compatible using method combination rules.
   Handles standard, :before, :after, :around qualifier combinations."
  (let ((method-quals (if (listp method-qualifiers)
                         method-qualifiers
                         (list method-qualifiers)))
        (call-quals (if (listp call-qualifiers)
                       call-qualifiers
                       (list call-qualifiers))))
    
    (labels ((primary-method-p (quals)
               "Test if quals indicate a primary method"
               (null quals))
             
             (around-method-p (quals)
               "Test if quals contain :around"
               (member :around quals))
             
             (before-method-p (quals)
               "Test if quals contain :before"
               (member :before quals))
             
             (after-method-p (quals)
               "Test if quals contain :after"
               (member :after quals))
             
             (qualifiers-match (method-q call-q)
               "Test if qualifier lists match exactly"
               (equal method-q call-q))
             
             (qualifier-positions-match (quals1 quals2 qualifier)
               "Test if qualifier appears at same position in both lists"
               (= (or (position qualifier quals1) -1)
                  (or (position qualifier quals2) -1))))
      
      (cond
        ;; Unqualified call matches any primary method
        ((null call-quals)
         (primary-method-p method-quals))
        
        ;; Qualified call needs a non-primary method
        ((primary-method-p method-quals)
         nil)
        
        ;; Exact qualifier match
        ((qualifiers-match method-quals call-quals)
         t)
        
        ;; :around methods can wrap any method
        ((and (around-method-p call-quals)
              (around-method-p method-quals))
         t)
        
        ;; :before methods must maintain order
        ((and (before-method-p call-quals)
              (before-method-p method-quals))
         (qualifier-positions-match call-quals method-quals :before))
        
        ;; :after methods must maintain order
        ((and (after-method-p call-quals)
              (after-method-p method-quals))
         (qualifier-positions-match call-quals method-quals :after))
        
        ;; No other combinations are valid
        (t nil)))))


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
  "Handles analysis and recording of method calls. Creates either:
   - A reference record if call matches an existing method 
   - An anomaly for no matching method"
  (declare (special log-stream))
  (multiple-value-bind (gf-def method-defs)
      (find-method-call-definitions parser (cons name args))
    (if gf-def
        ;; Have generic function - check method matches
        (if method-defs
            ;; Valid method call - record reference with all definitions
            (let* ((name-and-quals (analyze-call-qualifiers name))
                   (qualifiers (cdr name-and-quals))
                   (typed-args (loop for arg in args
                                   collect arg
                                   collect (cond ((null arg) 'null)
                                               ((stringp arg) 'string)
                                               ((numberp arg) 'number)
                                               ((keywordp arg) 'keyword)
                                               ((symbolp arg) 'symbol)
                                               ((consp arg) 'unanalyzed)
                                               (t (type-of arg))))))
              (record-reference *current-tracker*
                              :name subform
                              :file (file parser)
                              :package pkg-name 
                              :context (limit-form-size context pkg-name)
                              :visibility visibility
                              :definitions (cons gf-def method-defs)
                              :qualifiers qualifiers
                              :arguments typed-args))
            ;; No matching method - create anomaly
            (record-anomaly *current-tracker*
                          :name subform
                          :type :invalid-method-call 
                          :severity :ERROR
                          :file (file parser)
                          :description (format nil "No applicable method for ~A with arguments ~S" 
                                             name args)
                          :context (limit-form-size context pkg-name)))
        ;; No generic function found
        (try-definition-types subform pkg-name parser context visibility))))


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


(defun lambda-bindings (bindings)
  "Extract bound parameter names from a lambda list or binding form.
   Handles simple parameters, destructuring patterns, and specializers.
   Returns list of symbols that are bound by this binding form."
  (cond ((null bindings) nil)
        ((atom bindings) (list bindings))  ; &rest param
        ((symbolp (car bindings)) 
         (cons (car bindings) (lambda-bindings (cdr bindings))))
        ((consp (car bindings))  ; Destructuring or specializer
         (if (and (= (length (car bindings)) 2) 
                 (symbolp (caar bindings)))
             (cons (caar bindings) (lambda-bindings (cdr bindings))) ; (var type) specializer
             (append (lambda-bindings (car bindings))  ; Destructuring
                    (lambda-bindings (cdr bindings)))))
        (t nil)))