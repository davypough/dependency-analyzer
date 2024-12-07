;;;; Filename: file-parser.lisp
;;;
;;; Core file parsing functionality for dependency analysis.
;;; Analyzes source files to track symbol definitions, references,
;;; and package relationships without expanding macros.
;;; Also provides ASDF component parsing coordination.


(in-package #:dep)


(defun parse-definitions-in-file (parser log-stream)
  "First pass parser that records definitions. Analyzes all forms recursively,
   maintaining package context during traversal. Logs analysis progress to provided log-stream."
  (with-slots (file parsing-files) parser
    ;; Initialize package context
    (setf (current-package parser) (find-package :common-lisp-user)
          (current-package-name parser) "COMMON-LISP-USER"
          *package* (find-package :common-lisp-user))
    (push file parsing-files)
    (with-open-file (stream file :direction :input)
      (loop for form = (read stream nil :eof)
            until (eq form :eof)
            do (cond
                 ;; Handle defpackage - eval and analyze
                 ((and (consp form) (eq (car form) 'defpackage))
                  (eval form)
                  (analyze-definition-form parser form log-stream))
                 
                 ;; Handle in-package - eval and update context  
                 ((and (consp form) (eq (car form) 'in-package))
                  (eval form)
                  (analyze-in-package parser form))
                 
                 ;; Handle other package forms - just eval
                 ((and (consp form)
                      (member (car form) '(use-package make-package rename-package delete-package)))
                  (eval form))
                 
                 ;; All other forms
                 (t (analyze-definition-form parser form log-stream)))
               (terpri log-stream)))
    (pop parsing-files)))


(defun parse-references-in-file (parser log-stream)
 "Second pass parser that records references to previously recorded definitions.
  Processes in-package forms to maintain proper package context, 
  then analyzes all forms for references."
 (with-slots (file package) parser
   ;; Reset to CL-USER before processing each file
   (setf (current-package parser) (find-package :common-lisp-user)
         (current-package-name parser) "COMMON-LISP-USER"
         *package* (find-package :common-lisp-user))
   (with-open-file (stream file :direction :input)
     (loop for form = (read stream nil :eof)
           until (eq form :eof)
           do (when (and (consp form) (eq (car form) 'in-package))
                (eval form)
                (analyze-in-package parser form))
              (analyze-reference-form parser form log-stream)
              (terpri log-stream)))))


(defmethod analyze-definition-form ((parser file-parser) form &optional log-stream)
  "Analyze form for definitions using walk-form with specialized handler.
   Records definitions and processes nested definitions maintaining package context."
  (labels ((handle-definition (sym context parent-context depth)
            (when (and (consp form) (symbolp (car form)))
              (case (car form)
                (defpackage (analyze-defpackage parser form))
                ;; Type definitions
                (defclass (analyze-defclass parser form))
                (defstruct (analyze-defstruct parser form))
                (deftype (analyze-deftype parser form))
                ;; Variable definitions
                ((defvar defparameter defconstant) (analyze-defvar-defparameter-defconstant parser form))
                ;; Function definitions 
                ((defun defmacro) (analyze-defun-defmacro parser form))
                (defgeneric (analyze-defgeneric parser form))
                (defmethod (analyze-defmethod parser form))
                (defsetf (analyze-defsetf parser form))
                ;; Condition and method definitions
                (define-condition (analyze-define-condition parser form))
                (define-method-combination (analyze-method-combination parser form))
                (define-setf-expander (analyze-define-setf-expander parser form))
                (define-modify-macro (analyze-define-modify-macro parser form))
                (define-compiler-macro (analyze-define-compiler-macro parser form))
                (define-symbol-macro (analyze-define-symbol-macro parser form))
                ;; Special setf forms
                (setf (when (and (cddr form) (listp (second form)))
                       (case (caadr form)
                         (symbol-value 
                          (analyze-setf-symbol-value parser form))
                         ((symbol-function fdefinition macro-function)
                          (analyze-setf-symbol-function-fdefinition-macro-function parser form)))))))))
    
    ;; Only walk non-defpackage forms
    (if (and (consp form) (eq (car form) 'defpackage))
        (handle-definition form nil nil 0)  ; Process defpackage directly
        (let ((*package* (find-package :dep)))
          (walk-form form #'handle-definition :log-stream log-stream)))))


(defmethod analyze-reference-form ((parser file-parser) form &optional log-stream)
  "Analyze form recording references to definitions in different files.
   Handles both symbol references and string references to packages."
  (let ((contexts (make-hash-table :test 'equal))) ; Cache contexts by package
    (labels ((get-limited-context (pkg-name)
               (or (gethash pkg-name contexts)
                   (setf (gethash pkg-name contexts)
                         (limit-form-size form pkg-name))))
             (handle-reference (sym context parent-context depth)
               (typecase sym
                 (symbol
                  (unless (or (null sym)            ; Skip NIL
                              (keywordp sym)          ; Skip keywords
                              (cl-symbol-p sym))      ; Skip CL package symbols
                    (let* ((sym-pkg (symbol-package sym))
                           (cur-pkg (current-package parser))
                           (pkg-name (if sym-pkg
                                          (package-name sym-pkg)
                                          (current-package-name parser))))
                      ;; Check for unqualified cross-package reference from CL-USER
                      (multiple-value-bind (visibility anomaly-p)
                          (check-package-reference sym parser *current-tracker* context parent-context depth)
                        (declare (ignore anomaly-p))
                        ;; If symbol matches a definition record in a different file, record the dependency
                        (let* ((key (make-tracking-key sym pkg-name))
                               (def (gethash key (slot-value *current-tracker* 'definitions))))
                          (when (and def (not (equal (definition.file def) (file parser))))
                            (record-reference *current-tracker* sym
                                            (file parser)
                                            :package pkg-name
                                            :context (get-limited-context pkg-name)
                                            :visibility visibility
                                            :definition def)))))))
                 (string
                  ;; Check if this string matches a package definition
                  (let* ((norm-name (normalize-package-name sym))
                         (key (make-tracking-key norm-name))
                         (def (gethash key (slot-value *current-tracker* 'definitions))))
                    (when (and def
                               (eq (definition.type def) :PACKAGE)
                               (not (equal (definition.file def) (file parser))))
                      (record-reference *current-tracker* sym
                                        (file parser)
                                        :package "KEYWORD"
                                        :context (get-limited-context "KEYWORD")
                                        :visibility :LOCAL
                                        :definition def)))))))
      
      ;; Walk the form looking for references
      (walk-form form #'handle-reference :log-stream log-stream))))



(defun analyze-in-package (parser form)
  "Handle in-package forms by updating the current package context.
   Signals an error if referenced package doesn't exist."
  (let* ((name (normalize-package-name (second form)))
         (package (find-package name)))
    (if package
        (setf (current-package parser) package
              (current-package-name parser) (package-name package))
        (error "~&Cannot accurately analyze dependencies:~%~
                File: ~A~%~
                Form: ~S~%~
                References undefined package: ~A~%~%~
                Please ensure all package definitions compile successfully before analysis."
               (project-pathname (file parser)) form name))))


(defun analyze-defpackage (parser form)
  "Handle defpackage form, recording package definition and analyzing:
   - Package name and its dependencies
   - Package inheritance (:use)
   - Symbol importing (:import-from)
   - Symbol exporting (:export)
   - All other package options for potential dependencies"
  (destructuring-bind (def-op name &rest options) form
    (declare (ignore def-op)) 
    ;; 1. Form Analysis Context
    (let* ((pkg-name (normalize-package-name name))
           (file (file parser))
           ;; Extract standard package options
           (use-list (cdr (assoc :use options)))
           (nicknames (cdr (assoc :nicknames options)))
           ;; Create package with standard options 
           (package (or (find-package pkg-name)
                       (make-package pkg-name 
                                   :use (mapcar #'normalize-package-name use-list)
                                   :nicknames (mapcar #'normalize-package-name nicknames))))
           (context (limit-form-size form pkg-name)))

      ;; Record definition before any analysis starts
      (record-definition *current-tracker* pkg-name
                        :PACKAGE
                        file
                        :package pkg-name  
                        :exported-p t     
                        :context context)

      ;; 2. Primary Definition Record
      ;; (Moved to section 1 above)
      
      ;; 3. Interface Elements
      (let ((exported-syms nil))
        ;; Collect all exported symbols for recording
        (dolist (option options)
          (when (and (listp option) (eq (car option) :export))
            (dolist (sym (cdr option))
              (let ((exported-sym (process-package-export-option 
                                  package pkg-name sym)))
                (when exported-sym 
                  (push exported-sym exported-syms))))))
        
        ;; Record all exported symbols
        (dolist (sym exported-syms)
          (record-export *current-tracker* pkg-name sym)))

      ;; 4. Specification Elements
      ;; Process package use relationships - simplified to just record dependency
      (dolist (option options)
        (when (and (listp option) (eq (car option) :use))
          (dolist (used-pkg (cdr option))
            (let ((used-name (normalize-package-name used-pkg)))
              (record-package-use *current-tracker* pkg-name used-name)))))

      ;; 5. Implementation Elements
      ;; Process imports and other package options
      (dolist (option options)
        (when (listp option)
          (case (car option)
            (:import-from
             (let ((from-pkg (normalize-package-name (second option))))
               (dolist (sym (cddr option))
                 (process-package-import-option 
                  package from-pkg pkg-name parser sym))))
            ((:documentation :size :nicknames)
             nil)  ; Skip these options
            (t (unless (eq (car option) :export)
                 ;; Process other options but skip already handled ones
                 (dolist (item (cdr option))
                   (when (and (symbolp item) (not (keywordp item)))
                     (analyze-definition-form parser item))))))))

      ;; Check for package dependency cycles
      (detect-package-cycle pkg-name (parsing-packages parser)))))


(defun analyze-defstruct (parser form)
 "Handle defstruct form. Analyzes:
  - Structure name and exportedness
  - Interface functions (constructor, copier, predicate)
  - Include option for inheritance
  - Slot definitions and their types
  - Forward references in slot types"
 (destructuring-bind (def-op name-and-options &rest slots) form
   (declare (ignore def-op))
   ;; Primary analysis context
   (let* ((pkg (current-package parser))
          (pkg-name (current-package-name parser))
          (file (file parser))
          (forward-refs nil)  ; Track forward references
          (context (limit-form-size form pkg-name)))

     ;; Process structure name and options
     (let* ((struct-name (if (listp name-and-options)
                            (car name-and-options)
                            name-and-options))
            (options (when (listp name-and-options) 
                      (cdr name-and-options)))
            (conc-name-prefix     ; Used for accessor generation
             (let ((conc-option (find :conc-name options :key #'car)))
               (cond ((null conc-option)
                      (concatenate 'string (string struct-name) "-"))  ; Default
                     ((or (null (cdr conc-option))    ; (:conc-name)
                         (null (second conc-option)))  ; (:conc-name nil) 
                      "")
                     (t (string (second conc-option)))))))
       
       ;; Record main structure definition
       (record-definition *current-tracker* struct-name
                         :STRUCTURE
                         file
                         :package pkg-name
                         :exported-p (eq (nth-value 1 
                                       (find-symbol (symbol-name struct-name) pkg))
                                      :external)
                         :context context)

       ;; Check for included structure inheritance
       (let ((included (find :include options :key #'car)))
         (when included
           (let* ((included-name (second included))
                  (included-key (make-tracking-key included-name pkg-name))
                  (included-def (gethash included-key 
                                       (slot-value *current-tracker* 'definitions))))
             (unless included-def
               (push (cons included-name 'include) forward-refs)))))

       ;; Process each slot definition
       (dolist (slot slots)
         (let ((slot-spec (if (listp slot) slot (list slot))))
           ;; Check for slot type specifications
           (when (listp slot-spec)
             (let ((type-option (cdr (member :type slot-spec))))
               (when type-option
                 (let ((type-name (first type-option)))
                   (when (and type-name 
                            (symbolp type-name)
                            (not (cl-symbol-p type-name)))
                     (let* ((type-key (make-tracking-key type-name pkg-name))
                            (type-def (gethash type-key 
                                             (slot-value *current-tracker* 'definitions))))
                       (when (and (not type-def)
                                (not (find-class type-name nil)))
                         (push (cons type-name (car slot-spec)) 
                               forward-refs))))))))
           
           ;; Generate and record accessor function 
           (let* ((accessor (intern (concatenate 'string conc-name-prefix
                                               (string (if (listp slot-spec)
                                                         (car slot-spec)
                                                         slot-spec)))
                                  pkg))
                  (accessor-context (limit-form-size form pkg-name)))
             (record-definition *current-tracker* accessor
                              :FUNCTION
                              file
                              :package pkg-name
                              :exported-p (eq (nth-value 1 
                                            (find-symbol (string accessor) pkg))
                                           :external)
                              :context accessor-context))))

       ;; Record any forward reference anomalies
       (dolist (fref forward-refs)
         (record-anomaly *current-tracker*
                        :structure-forward-reference
                        :WARNING
                        file
                        (format nil "Forward reference to structure ~A in ~A of structure ~A"
                                (car fref) 
                                (if (eq (cdr fref) 'include)
                                    "include option"
                                    (format nil "slot ~A" (cdr fref)))
                                struct-name)))))))


(defun analyze-defvar-defparameter-defconstant (parser form)
  "Handle defvar, defparameter, and defconstant forms. Analyzes:
   - Variable name and its exportedness
   - Initialization form for dependencies
   - Type declarations if present 
   Returns the variable definition record."
  (destructuring-bind (def-op name &optional initform &rest decls) form
    (declare (ignore def-op))
    ;; 1. Form Analysis Context
    (let* ((pkg (current-package parser))
           (pkg-name (current-package-name parser))
           (file (file parser))
           (context (limit-form-size form pkg-name)))
      
      ;; 2. Primary Definition Record
      (record-definition *current-tracker* name
                        :VARIABLE
                        file
                        :package pkg-name
                        :exported-p (eq (nth-value 1 
                                       (find-symbol (symbol-name name) pkg))
                                      :external)
                        :context context)
      
      ;; 3. Interface Elements 
      ;; None for variables
      
      ;; 4. Specification Elements
      ;; Process type declarations if present
      (dolist (decl decls)
        (analyze-definition-form parser decl))
      
      ;; 5. Implementation Elements
      ;; Analyze initialization form if present
      (when initform
        (analyze-definition-form parser initform)))))


(defun analyze-defun-defmacro (parser form)
  "Handle defun and defmacro forms. Analyzes:
   - Function/macro name and its exportedness
   - Lambda list parameters and their types/defaults
   - Declarations in body
   - Body forms for nested definitions
   Returns the function/macro definition record."
  (destructuring-bind (def-op name lambda-list &rest body) form
    ;; 1. Form Analysis Context
    (let* ((pkg (current-package parser))
           (pkg-name (current-package-name parser))
           (file (file parser))
           (context (limit-form-size form pkg-name)))
      
      ;; 2. Primary Definition Record
      (record-definition *current-tracker* name
                        (if (eq def-op 'defun) 
                            :FUNCTION 
                            :MACRO)
                        file
                        :package pkg-name
                        :exported-p (eq (nth-value 1 
                                       (find-symbol (symbol-name name) pkg))
                                      :external)
                        :context context)
      
      ;; 3. Interface Elements
      ;; For macros, record any helper functions defined in the body
      (when (eq def-op 'defmacro)
        (record-helper-functions parser body))
      
      ;; 4. Specification Elements
      ;; Process lambda list including type declarations and defaults
      (analyze-lambda-list parser lambda-list)
      ;; Analyze all lambda list items for potential nested definitions
      (dolist (item lambda-list)
        (analyze-definition-form parser item))
     
      ;; 5. Implementation Elements
      ;; Process declarations and body forms
      (multiple-value-bind (decls forms)
          (parse-body body :documentation t)
        ;; Handle declarations by analyzing all declaration specs
        (dolist (decl decls) 
          (analyze-definition-form parser decl))
        ;; Analyze body forms fully
        (dolist (form forms)
          (unless (quoted-form-p form)
            (analyze-definition-form parser form)))
        ;; For macros, record symbols used in body
        (when (eq def-op 'defmacro)
          (record-macro-body-symbols *current-tracker* name nil))))))


(defun analyze-defgeneric (parser form)
 "Handle defgeneric form. Analyzes:
  - Generic function name and exportedness
  - Lambda list parameters and declarations
  - Method combination type and arguments
  - Method options and qualifiers
  - Generic/method class specifications
  Returns the generic function definition record."
 (destructuring-bind (def-op name lambda-list &rest options) form
   (declare (ignore def-op))
   ;; 1. Form Analysis Context
   (let* ((pkg (current-package parser))
          (pkg-name (current-package-name parser))
          (file (file parser))
          (context (limit-form-size form pkg-name)))

     ;; 2. Primary Definition Record  
     (record-definition *current-tracker* name
                       :GENERIC-FUNCTION
                       file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name) pkg))
                                     :external)
                       :context context)

     ;; 3. Interface Elements
     ;; Collect method names from :method options 
     (dolist (option options)
       (when (and (listp option) 
                  (eq (car option) :method))
         (push name method-functions)
         ;; Record method definition with full defgeneric form context
         (record-definition *current-tracker* name
                          :METHOD
                          file
                          :package pkg-name
                          :exported-p (eq (nth-value 1 
                                         (find-symbol (symbol-name name) pkg))
                                        :external)
                          :context context)))

     ;; 4. Specification Elements
     ;; 4a. Process lambda list for parameters and types
     (when lambda-list
       (analyze-lambda-list parser lambda-list))

     ;; 4b. Process method combination and arguments
     (dolist (option options)
       (when (and (listp option) 
                  (eq (car option) :method-combination))
         (let ((combination-name (second option))
               (combination-args (cddr option)))
           ;; Process combination arguments
           (dolist (arg combination-args)
             (unless (quoted-form-p arg)
               (analyze-definition-form parser arg))))))

     ;; 5. Implementation Elements
     (dolist (option options)
       (when (listp option)
         (case (car option)
           (:argument-precedence-order
            ;; Validate args exist in lambda list
            (let ((arg-names (mapcar (lambda (arg)
                                     (if (listp arg) (car arg) arg))
                                   lambda-list)))
              (dolist (arg (cdr option))
                (unless (member arg arg-names)
                  (record-anomaly *current-tracker*
                                :generic-function-error
                                :WARNING
                                file
                                (format nil "Argument ~S in :argument-precedence-order not in lambda list" 
                                      arg))))))

           ((:generic-function-class :method-class)
            ;; Record class references
            (analyze-definition-form parser (second option)))

           (:method
            ;; Process method options similar to defmethod
            (let ((method-qualifiers nil)
                  (method-lambda-list nil)
                  (remaining-forms (cdr option)))
              ;; Collect qualifiers
              (loop while (and remaining-forms 
                             (not (listp (car remaining-forms))))
                    do (let ((qual (pop remaining-forms)))
                         (push qual method-qualifiers)
                         (when (consp qual)
                           (analyze-definition-form parser qual))))
              ;; Process lambda list and body
              (when remaining-forms
                (setf method-lambda-list (pop remaining-forms))
                ;; Handle specialized parameters
                (loop for param in method-lambda-list
                      when (listp param)
                      do (destructuring-bind (param-name &optional specializer) param
                           (declare (ignore param-name))
                           (when specializer
                             (typecase specializer
                               (symbol 
                                (analyze-definition-form parser specializer))
                               (cons
                                (case (car specializer)
                                  (eql 
                                   (unless (quoted-form-p (second specializer))
                                     (analyze-definition-form parser (second specializer))))
                                  (t 
                                   (analyze-definition-form parser specializer))))))))
                ;; Process method body
                (dolist (form remaining-forms)
                  (unless (quoted-form-p form)
                    (analyze-definition-form parser form))))))

           (t 
            ;; Handle any other options
            (dolist (element (cdr option))
              (unless (quoted-form-p element)
                (analyze-definition-form parser element))))))))))


(defun analyze-defmethod (parser form) 
 "Handle defmethod form. Analyzes:
  - Method name and exportedness
  - Method qualifiers (e.g. :before, :after)
  - Lambda list with specializers
  - EQL specializers
  - Body declarations and forms"
 (destructuring-bind (def-op name &rest body) form
   (declare (ignore def-op)) 
   ;; 1. Form Analysis Context
   (let* ((pkg (current-package parser))
          (pkg-name (current-package-name parser))
          (file (file parser))
          (qualifiers nil)
          (lambda-list nil)
          (method-body nil)
          (context (limit-form-size form pkg-name)))
     
     ;; 2. Primary Definition Record
     (record-definition *current-tracker* name
                       :METHOD
                       file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name) pkg))
                                     :external)
                       :context context)
     
     ;; 3. Interface Elements
     ;; Collect qualifiers until we hit lambda list
     (loop for rest on body
           while (and rest (not (listp (car rest))))
           do (let* ((qual (pop rest))
                     (qual-context (limit-form-size qual pkg-name)))
                (push qual qualifiers)
                ;; Analyze qualifier fully
                (analyze-definition-form parser qual))
           finally (when rest
                    (setf lambda-list (pop rest)
                          method-body rest)))
     
     ;; 4. Specification Elements
     ;; 4a. Process specialized lambda list parameters 
     (when lambda-list
       (loop for param in lambda-list
             do (typecase param
                  (symbol 
                   (analyze-definition-form parser param))
                  (cons  ; Specialized parameter
                   (destructuring-bind (param-name &optional specializer) param
                     (analyze-definition-form parser param-name)
                     (when specializer
                       (analyze-definition-form parser specializer)))))))
     
     ;; 4b. Analyze lambda list for type declarations and defaults
     (analyze-lambda-list parser lambda-list)
     
     ;; 5. Implementation Elements
     (when method-body
       ;; Process declarations at start of body
       (multiple-value-bind (decls forms)
           (parse-body method-body :documentation t)
         ;; Handle declarations
         (dolist (decl decls)
           (analyze-definition-form parser decl))
         ;; Analyze remaining body forms recursively
         (dolist (form forms)
           (analyze-definition-form parser form)))))))


(defun analyze-defsetf (parser form)
 "Handle defsetf forms in both short and long forms. Analyzes:
  - Access function name and exportedness
  - Short form: update function reference
  - Long form: lambda list, store vars, declarations, body
  - Helper functions defined in body
  Returns the setf definition record."
 (destructuring-bind (def-op name &rest args) form
   (declare (ignore def-op))
   ;; 1. Form Analysis Context
   (let* ((pkg (current-package parser))
          (pkg-name (current-package-name parser))
          (file (file parser))
          (context (limit-form-size form pkg-name)))

     ;; 2. Primary Definition Record
     (record-definition *current-tracker* name
                       :SETF
                       file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name) pkg))
                                     :external)
                       :context context)

     ;; 3. Interface Elements
     (if (and (car args) (listp (car args)))
       ;; Long form - collect helper functions from body
       (let ((helper-context (limit-form-size (cdddr args) pkg-name)))
         (record-helper-functions parser (cdddr args)))
       ;; Short form - analyze update function
       (when (car args)
         (analyze-definition-form parser (car args))))

     ;; 4. Specification Elements
     (if (and (car args) (listp (car args)))
         ;; Long form
         (destructuring-bind (lambda-list store-vars &rest body) args
           ;; Process access function lambda list
           (analyze-lambda-list parser lambda-list)
           (dolist (item lambda-list)
             (analyze-definition-form parser item))
           ;; Process store variables and their type declarations
           (dolist (var store-vars)
             (analyze-definition-form parser var))
           ;; Process body forms
           (multiple-value-bind (decls forms)
               (parse-body body :documentation t)
             ;; Handle declarations
             (dolist (decl decls)
               (analyze-definition-form parser decl))
             ;; Analyze body forms
             (dolist (form forms)
               (analyze-definition-form parser form))))
         ;; Short form - all analysis already done in step 3
         nil))))


(defun analyze-defclass (parser form)
 "Handle defclass form. Analyzes:
  - Class name and exportedness
  - Superclasses and their references
  - Slot definitions with accessor/reader/writer methods
  - Class options including :metaclass and :default-initargs
  - Documentation strings"
 (destructuring-bind (def-op name superclasses slots &rest options) form
   (declare (ignore def-op))
   ;; 1. Form Analysis Context
   (let* ((pkg (current-package parser))
          (pkg-name (current-package-name parser))
          (file (file parser))
          (context (limit-form-size form pkg-name)))

     ;; 2. Primary Definition Record
     (record-definition *current-tracker* name
                       :CLASS
                       file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name) pkg))
                                     :external)
                       :context context)

     ;; 3. Interface Elements
     (let ((interface-functions nil))
       ;; Collect all accessor/reader/writer functions from slots
       (dolist (slot slots)
         (when (listp slot)  ; Skip atomic slot names
           (destructuring-bind (slot-name &rest slot-options) slot
             (declare (ignore slot-name))
             (loop for (option value) on slot-options by #'cddr
                   when (member option '(:reader :writer :accessor))
                   do (let ((func-context (limit-form-size slot pkg-name)))
                        (push value interface-functions)
                        ;; Record interface function with its context
                        (record-definition *current-tracker* value
                                        :FUNCTION
                                        file
                                        :package pkg-name
                                        :exported-p (eq (nth-value 1 
                                                      (find-symbol (symbol-name value) pkg))
                                                     :external)
                                        :context func-context))))))

     ;; 4. Specification Elements
     ;; 4a. Process superclasses - full analysis
     (dolist (super superclasses)
       (analyze-definition-form parser super))

     ;; 4b. Process slot specifications
     (dolist (slot slots)
       (when (listp slot)
         (destructuring-bind (slot-name &rest slot-options) slot
           ;; Analyze slot name and all options fully
           (analyze-definition-form parser slot-name)
           (loop for (option value) on slot-options by #'cddr
                 do (analyze-definition-form parser option)
                    (analyze-definition-form parser value)))))

     ;; 5. Implementation Elements
     (dolist (option options)
       (when (listp option)
         (case (car option)
           (:metaclass
            ;; Full analysis of metaclass spec 
            (analyze-definition-form parser (second option)))
           
           (:default-initargs
            ;; Full analysis of initarg forms
            (loop for (key value) on (cdr option) by #'cddr
                  do (analyze-definition-form parser key)
                     (analyze-definition-form parser value)))
           
           (:documentation
            nil)  ; Skip documentation strings
           
           (t 
            ;; Full analysis of any other class options
            (dolist (element (cdr option))
              (analyze-definition-form parser element))))))))))


(defun analyze-deftype (parser form)
 "Handle deftype form. Analyzes:
  - Type name and exportedness
  - Type lambda list with defaults
  - Type specifier expansion in body
  - References to other type definitions
  - Type environment parameters
  Returns the type definition record."
 (destructuring-bind (def-op name lambda-list &rest body) form
   (declare (ignore def-op))
   
   ;; 1. Form Analysis Context
   (let* ((pkg (current-package parser))
          (pkg-name (current-package-name parser))
          (file (file parser))
          (context (limit-form-size form pkg-name)))

     ;; 2. Primary Definition Record
     (record-definition *current-tracker* name
                       :TYPE 
                       file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name) pkg))
                                     :external)
                       :context context)

     ;; 3. Interface Elements
     ;; Type definitions don't generate interface functions

     ;; 4. Specification Elements
     ;; 4a. Process type lambda list
     (analyze-lambda-list parser lambda-list)
     (dolist (param lambda-list)
       (analyze-definition-form parser param))

     ;; 5. Implementation Elements
     ;; Process body forms and declarations
     (multiple-value-bind (decls forms)
         (parse-body body :documentation t)
       ;; Handle declarations
       (dolist (decl decls)
         (analyze-definition-form parser decl))
       
       ;; Analyze all expansion forms
       (dolist (form forms)
         (analyze-definition-form parser form))))))


(defun analyze-define-condition (parser form)
  "Handle define-condition forms, recording condition definition and analyzing:
   - Condition name and its exportedness
   - Superclass relationships
   - Slot definitions including reader/writer/accessor functions
   - Special condition options like :report
   - Non-CL symbols in slot types and initforms"
  (destructuring-bind (def-op name superclasses &rest slots-and-options) form
    (declare (ignore def-op))
    
    ;; 1. Form Analysis Context
    (let* ((pkg (current-package parser))
           (pkg-name (current-package-name parser))
           (file (file parser))
           (context (limit-form-size form pkg-name)))
      
      ;; 2. Primary Definition Record
      (record-definition *current-tracker* name
                        :CONDITION
                        file
                        :package pkg-name
                        :exported-p (eq (nth-value 1 
                                       (find-symbol (symbol-name name) pkg))
                                      :external)
                        :context context)
      
      ;; 3. Interface Elements 
      (let ((interface-functions nil))
        ;; Process slots for readers/writers/accessors
        (dolist (slot slots-and-options)
          (when (and (listp slot) (car slot))  ; Valid slot definition
            (loop for (option value) on (cdr slot) by #'cddr
                  when (member option '(:reader :writer :accessor))
                  do (let ((func-context (limit-form-size slot pkg-name)))
                       (push value interface-functions)
                       (record-definition *current-tracker* value
                                        :FUNCTION
                                        file
                                        :package pkg-name
                                        :exported-p (eq (nth-value 1 
                                                      (find-symbol (symbol-name value) pkg))
                                                     :external)
                                        :context func-context)))))
      
      ;; 4. Specification Elements
      ;; 4a. Process superclasses
      (dolist (super superclasses)
        (analyze-definition-form parser super))
      
      ;; 4b. Process slots and analyze all components
      (dolist (slot slots-and-options)
        (when (and (listp slot) (car slot))  ; Valid slot definition
          ;; Analyze slot name
          (analyze-definition-form parser (car slot))
          ;; Analyze all slot options
          (loop for (option value) on (cdr slot) by #'cddr
                do (analyze-definition-form parser option)
                   (analyze-definition-form parser value))))
      
      ;; 5. Implementation Elements
      (dolist (option slots-and-options)
        (when (and (listp option) (keywordp (car option)))
          (case (car option)
            (:report
             (analyze-definition-form parser (second option)))
            (:default-initargs
             (loop for (key value) on (cdr option) by #'cddr
                   do (analyze-definition-form parser key)
                      (analyze-definition-form parser value)))
            (t 
             ;; Analyze all parts of other options
             (dolist (element (cdr option))
               (analyze-definition-form parser element))))))))))


(defun analyze-define-method-combination (parser form)
  "Handle define-method-combination forms. Records the definition and analyzes:
   - In short form: option values and auxiliary parameters 
   - In long form: lambda list, method group patterns and options,
     method-group-specifiers, and body forms including:
     - :arguments
     - :generic-function
     - Method group specs with ordering, predicates and descriptions"
  (destructuring-bind (def-op name &rest args) form
    (declare (ignore def-op))

    ;; 1. Form Analysis Context
    (let* ((pkg (current-package parser))
           (pkg-name (current-package-name parser))
           (file (file parser))
           (context (limit-form-size form pkg-name)))

      ;; 2. Primary Definition Record
      (record-definition *current-tracker* name
                        :FUNCTION
                        file
                        :package pkg-name
                        :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name) pkg))
                                     :external)
                        :context context)

      ;; 3. Interface Elements
      ;; None needed - no interface functions generated

      ;; 4. Specification Elements
      ;; 4a. Process lambda list and body based on form type
      (if (and (car args) (listp (car args)))
          ;; Long form handling
          (destructuring-bind (lambda-list &rest body) args
            ;; Analyze lambda list completely
            (analyze-lambda-list parser lambda-list)
            (dolist (param lambda-list)
              (analyze-definition-form parser param))

            ;; Process body forms including method group specs
            (multiple-value-bind (decls forms)
                (parse-body body :documentation t)
              ;; Handle declarations
              (dolist (decl decls)
                (analyze-definition-form parser decl))

              ;; Process all body forms
              (dolist (form forms)
                (let ((form-context (limit-form-size form pkg-name)))
                  (analyze-definition-form parser form)
                  (when (and (listp form) 
                            (member (car form) '(:arguments :generic-function :documentation)))
                      ;; Handle option forms 
                      (dolist (part (cdr form))
                        (analyze-definition-form parser part)))))))

          ;; Short form handling
          (progn
            ;; Process operator name if provided
            (when (car args)
              (analyze-definition-form parser (car args)))

            ;; Process options and their values
            (loop for (option value) on (cdr args) by #'cddr
                  do (analyze-definition-form parser option)
                     (analyze-definition-form parser value)))))))


(defun analyze-define-modify-macro (parser form)
 "Handle define-modify-macro forms. Analyzes:
  - Macro name and exportedness
  - Lambda list parameters and their types
  - Underlying function reference
  - Documentation string if present"
 (destructuring-bind (def-op name lambda-list function &optional documentation) form
   (declare (ignore def-op documentation))
   ;; 1. Form Analysis Context
   (let* ((pkg (current-package parser))
          (pkg-name (current-package-name parser))
          (file (file parser))
          (context (limit-form-size form pkg-name)))

     ;; 2. Primary Definition Record
     (record-definition *current-tracker* name
                       :MACRO
                       file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name) pkg))
                                     :external)
                       :context context)

     ;; 3. Interface Elements
     ;; Analyze underlying function
     (analyze-definition-form parser function)

     ;; 4. Specification Elements
     ;; Process lambda list for parameters and types
     (analyze-lambda-list parser lambda-list)
     ;; Analyze all lambda list items
     (dolist (item lambda-list)
       (analyze-definition-form parser item))

     ;; 5. Implementation Elements
     ;; None - modify macros don't have implementation bodies
     )))


(defun analyze-define-compiler-macro (parser form)
 "Handle define-compiler-macro forms. Analyzes:
  - Compiler macro name and exportedness
  - Lambda list parameters and their types
  - Body declarations and forms
  - Helper functions defined in body"
 (destructuring-bind (def-op name lambda-list &rest body) form
   (declare (ignore def-op))
   ;; 1. Form Analysis Context
   (let* ((pkg (current-package parser))
          (pkg-name (current-package-name parser))
          (file (file parser))
          (context (limit-form-size form pkg-name)))

     ;; 2. Primary Definition Record
     (record-definition *current-tracker* name
                       :MACRO
                       file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name) pkg))
                                     :external)
                       :context context)

     ;; 3. Interface Elements
     (record-helper-functions parser body)

     ;; 4. Specification Elements
     ;; Process lambda list for parameters and types
     (analyze-lambda-list parser lambda-list)
     (dolist (item lambda-list)
       (analyze-definition-form parser item))

     ;; 5. Implementation Elements
     ;; Process body forms and declarations
     (multiple-value-bind (decls forms)
         (parse-body body :documentation t)
       ;; Handle declarations
       (dolist (decl decls)
         (analyze-definition-form parser decl))
       ;; Analyze body forms
       (dolist (form forms)
         (analyze-definition-form parser form))
       ;; Record macro body symbols for expansion analysis
       (record-macro-body-symbols *current-tracker* name nil)))))


(defun analyze-define-symbol-macro (parser form)
 "Handle define-symbol-macro form. Analyzes:
  - Symbol name and exportedness
  - Expansion form dependencies
  - Symbols used in expansion
  Returns the symbol-macro definition record."
 (destructuring-bind (def-op name expansion-form) form
   (declare (ignore def-op))
   ;; 1. Form Analysis Context
   (let* ((pkg (current-package parser))
          (pkg-name (current-package-name parser))
          (file (file parser))
          (context (limit-form-size form pkg-name)))

     ;; 2. Primary Definition Record
     (record-definition *current-tracker* name
                       :SYMBOL-MACRO
                       file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name) pkg))
                                     :external)
                       :context context)

     ;; 3. Interface Elements
     ;; Symbol macros don't generate interface functions
     ;; but may be referenced like regular variables

     ;; 4. Specification Elements
     ;; Look for type declarations preceding the definition
     (let ((preceding-forms (preceding-forms-in-body form parser)))
       (dolist (prev-form preceding-forms)
         (analyze-definition-form parser prev-form)))

     ;; 5. Implementation Elements
     ;; Analyze expansion form for dependencies
     (analyze-definition-form parser expansion-form)
     ;; Record symbols used in expansion for macro processing
     (record-macro-body-symbols *current-tracker* name nil))))


(defun analyze-setf-symbol-value (parser form)
 "Handle (setf (symbol-value 'name) value-form) forms. Analyzes:
  - Variable name and exportedness
  - Value form for dependencies
  - Any related type declarations
  Returns the variable definition record."
 (destructuring-bind (setf (symbol-value quoted-name) value-form) form
   ;; 1. Form Analysis Context
   (let* ((pkg (current-package parser))
          (pkg-name (current-package-name parser))
          (file (file parser))
          (name (second quoted-name))  ; Extract symbol from 'name form
          (context (limit-form-size form pkg-name)))

     ;; 2. Primary Definition Record
     (record-definition *current-tracker* name
                       :VARIABLE
                       file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name) pkg))
                                     :external)
                       :context context)

     ;; 3. Interface Elements
     ;; None - variables don't have interface elements

     ;; 4. Specification Elements
     ;; Look for any type declarations preceding the setf
     (let ((preceding-forms (preceding-forms-in-body form parser)))
       (dolist (prev-form preceding-forms)
         (analyze-definition-form parser prev-form)))

     ;; 5. Implementation Elements
     ;; Analyze full value form
     (analyze-definition-form parser value-form))))


(defun analyze-setf-symbol-function-fdefinition-macro-function (parser form)
  "Handle (setf (symbol-function|fdefinition|macro-function 'name) function-form) definitions.
   Form structure: (setf (accessor 'name) function-form)"
  (let* ((accessor (caadr form))           ; symbol-function, fdefinition, or macro-function
         (name (cadr (cadr (cadr form))))  ; Get symbol from ('accessor 'name)
         (function-form (third form))      ; The function/lambda form
         (package (current-package parser))
         (context (limit-form-size form (current-package-name parser))))

    ;; 1. Form Analysis Context
    (let* ((pkg (current-package parser))
           (pkg-name (current-package-name parser))
           (file (file parser)))

      ;; 2. Primary Definition Record - based on accessor type
      (cond
        ;; Handle symbol-function and fdefinition
        ((member accessor '(symbol-function fdefinition))
         (record-definition *current-tracker* name
                          :FUNCTION
                          file
                          :package pkg-name
                          :exported-p (eq (nth-value 1 
                                        (find-symbol (symbol-name name) package))
                                       :external)
                          :context context))
        ;; Handle macro-function
        ((eq accessor 'macro-function)
         (record-definition *current-tracker* name
                          :MACRO
                          file
                          :package pkg-name
                          :exported-p (eq (nth-value 1 
                                        (find-symbol (symbol-name name) package))
                                       :external)
                          :context context)))

      ;; 3. Interface Elements
      ;; None needed - these forms define the functions/macros directly

      ;; 4. Specification Elements
      ;; Look for any type declarations preceding the setf
      (let ((preceding-forms (preceding-forms-in-body form parser)))
        (dolist (prev-form preceding-forms)
          (analyze-definition-form parser prev-form)))

      ;; 5. Implementation Elements
      ;; Analyze the function form completely
      (analyze-definition-form parser function-form)
      ;; For lambda forms, analyze their internals
      (when (and (listp function-form)
                (eq (car function-form) 'lambda))
        ;; Analyze lambda list
        (analyze-lambda-list parser (cadr function-form))
        (dolist (item (cadr function-form))
          (analyze-definition-form parser item))
        ;; Analyze body forms
        (multiple-value-bind (decls forms)
            (parse-body (cddr function-form) :documentation t)
          ;; Handle declarations
          (dolist (decl decls)
            (analyze-definition-form parser decl))
          ;; Analyze remaining body forms
          (dolist (form forms)
            (analyze-definition-form parser form)))))))


(defun analyze-define-setf-expander (parser form)
  "Handle define-setf-expander forms. Analyzes:
   - Access function name and exportedness
   - Lambda list parameters and their types/defaults
   - Body forms including temporary/store variable bindings
   - Helper functions defined in body
   Returns the setf-expander definition record."
  (destructuring-bind (def-op name lambda-list &rest body) form
    (declare (ignore def-op))
    ;; 1. Form Analysis Context
    (let* ((pkg (current-package parser))
           (pkg-name (current-package-name parser))
           (file (file parser))
           (context (limit-form-size form pkg-name)))

      ;; 2. Primary Definition Record
      (record-definition *current-tracker* name
                        :FUNCTION  ; Setf expanders are functions
                        file
                        :package pkg-name
                        :exported-p (eq (nth-value 1 
                                       (find-symbol (symbol-name name) pkg))
                                      :external)
                        :context context)

      ;; 3. Interface Elements
      ;; Record helper functions like in defmacro
      (record-helper-functions parser body)

      ;; 4. Specification Elements
      ;; Process lambda list for parameters and types
      (analyze-lambda-list parser lambda-list)
      (dolist (item lambda-list)
        (analyze-definition-form parser item))

      ;; 5. Implementation Elements
      ;; Process body forms and declarations
      (multiple-value-bind (decls forms)
          (parse-body body :documentation t)
        ;; Handle declarations
        (dolist (decl decls)
          (analyze-definition-form parser decl))
        ;; Analyze remaining body forms
        (dolist (form forms)
          (analyze-definition-form parser form))
        ;; Record macro body symbols for expansion analysis
        (record-macro-body-symbols *current-tracker* name nil)))))
