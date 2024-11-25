;;;; Filename: file-parser.lisp
;;;
;;; Core file parsing functionality for dependency analysis.
;;; Analyzes source files to track symbol definitions, references,
;;; and package relationships without expanding macros.
;;; Also provides ASDF component parsing coordination.


(in-package #:dep)


(defmethod parse-definitions-in-file ((parser file-parser))
  "First pass parser that records definitions.
   Analyzes all forms recursively, maintaining package context during traversal."
  (with-slots (file parsing-files) parser
    (push file parsing-files)
    (with-open-file (stream file :direction :input)
      (loop for form = (read stream nil nil)
            unless (null form)
              do (analyze-definition-form parser form)))
    (pop parsing-files)))


(defmethod parse-references-in-file ((parser file-parser))
  "Second pass parser that records references to previously recorded definitions.
   Processes in-package forms to maintain proper package context, 
   then analyzes all forms for references. Unlike the definition pass, 
   this analyzes both cons forms and atoms since references can occur as either."
  (with-slots (file package) parser
    (with-open-file (stream file :direction :input)
      (let ((*package* package))
        (loop for form = (read stream nil nil)
              while form
              do (if (consp form)
                     (if (eq (car form) 'in-package)
                         (analyze-in-package parser form)
                         (analyze-reference-form parser form))
                     (analyze-reference-form parser form)))))))


(defmethod analyze-in-package ((parser file-parser) form)
  "Handle in-package forms by updating the current package context."
  (let* ((name (normalize-package-name (second form)))
         (package (or (find-package name)
                     (make-package name))))
    (setf (current-package parser) package
          (current-package-name parser) (package-name package)
          *package* package)))


(defmethod analyze-definition-form ((parser file-parser) form)
 "Analyze a form for definitions and process all subforms for nested definitions.
  Maintains correct package context through all nested forms."
 (typecase form
   (cons
    (if (symbolp (car form))
        (let ((operator (car form)))
          (case operator
            (in-package (analyze-in-package parser form))
            (defpackage (analyze-defpackage parser form))
            (defclass (analyze-defclass parser form))
            (defstruct (analyze-defstruct parser form))
            (deftype (analyze-deftype parser form))
            ((defvar defparameter defconstant) (analyze-defvar-defparameter-defconstant parser form))
            ((defun defmacro) (analyze-defun-defmacro parser form))
            (defgeneric (analyze-defgeneric parser form))
            (defmethod (analyze-defmethod parser form))
            (define-condition (analyze-define-condition parser form))
            (define-method-combination (analyze-method-combination parser form))
            (define-setf-expander (analyze-define-setf-expander parser form))
            (define-modify-macro (analyze-define-modify-macro parser form))
            (define-compiler-macro (analyze-define-compiler-macro parser form))
            (define-symbol-macro (analyze-define-symbol-macro parser form))
            (setf (when (and (cddr form)
                             (listp (second form)))
                    (case (caadr form)
                      ((symbol-value)
                         (analyze-setf-symbol-value parser form))
                      ((symbol-function fdefinition macro-function)
                         (analyze-setf-symbol-function-fdefinition-macro-function parser form)))))
            (t (dolist (subform (cdr form))
                 (analyze-definition-form parser subform)))))
        (dolist (subform form)
          (analyze-definition-form parser subform))))
   ((and array (not string)) 
    (dotimes (i (array-total-size form))
      (analyze-definition-form parser (row-major-aref form i))))
   (hash-table
    (maphash (lambda (k v)
               (analyze-definition-form parser k)
               (analyze-definition-form parser v))
             form))
   (t nil)))


(defmethod analyze-reference-form ((parser file-parser) form)
  "Walk form recording :CALL and :REFERENCE symbol references.
   :CALL references are symbols in operator position.
   :REFERENCE references are symbols in value/key position.
   All symbols are checked against definition records."
  (labels ((walk (x op-position)
             (typecase x
               (cons 
                (when op-position  
                  (walk (car x) t))    
                (walk (cdr x) nil))    
               (array
                (dotimes (i (array-total-size x))
                  (walk (row-major-aref x i) nil)))
               (hash-table
                (maphash (lambda (k v)
                          (walk k nil)
                          (walk v nil))
                        x))
               (symbol
                (when x  ; Skip nil
                  (let* ((pkg (or (symbol-package x) 
                                (current-package parser)))
                         (pkg-name (package-name pkg))
                         (visibility (cond
                                     ((null (symbol-package x)) :LOCAL)
                                     ((eq pkg (current-package parser)) :LOCAL)
                                     (t (multiple-value-bind (sym status)
                                            (find-symbol (symbol-name x) 
                                                       (current-package parser))
                                          (declare (ignore sym))
                                          (case status
                                            (:inherited :INHERITED)
                                            (:external :IMPORTED)
                                            (otherwise :LOCAL)))))))
                    (when (gethash (make-tracking-key x pkg-name)
                                 (slot-value *current-tracker* 'definitions))
                      (record-reference *current-tracker* x
                                      (if op-position :CALL :REFERENCE)
                                      (file parser)
                                      :package pkg-name
                                      :visibility visibility)))))
               (t nil))))
    (walk form nil)))


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
           (package (or (find-package pkg-name)
                       (make-package pkg-name)))
           (spec-symbols nil))

      ;; 2. Primary Definition Record
      (record-definition *current-tracker* pkg-name
                        :PACKAGE
                        file
                        :package "KEYWORD"  ; Package names are keywords
                        :exported-p t)      ; Package names always accessible
      
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
          (push sym spec-symbols)))

      ;; 4. Specification Elements
      ;; Process package use relationships
      (dolist (option options)
        (when (and (listp option) (eq (car option) :use))
          (dolist (used-pkg (cdr option))
            (let ((used-name (normalize-package-name used-pkg)))
              (process-package-use-option package used-name options parser)
              (push used-name spec-symbols)))))

      ;; 5. Implementation Elements
      ;; Process imports and other package options
      (dolist (option options)
        (when (listp option)
          (case (car option)
            (:import-from
             (let ((from-pkg (normalize-package-name (second option))))
               (push from-pkg spec-symbols)
               (dolist (sym (cddr option))
                 (process-package-import-option 
                  package from-pkg pkg-name parser sym)
                 (push sym spec-symbols))))
            ((:documentation :size :nicknames)
             nil)  ; Skip these options
            (t (unless (eq (car option) :export)
                 ;; Collect symbols from other options
                 (dolist (item (cdr option))
                   (unless (keywordp item)
                     (push item spec-symbols))))))))

      ;; 6. Symbol Collection/Association
      ;; Store unique package names and symbols with package definition
      (let ((unique-symbols (remove-duplicates spec-symbols :test #'string=)))
        (when unique-symbols
          (setf (definition.spec-symbols 
                 (get-definitions pkg-name))
                unique-symbols)))

      ;; Check for package dependency cycles
      (detect-package-cycle pkg-name (parsing-packages parser)))))


(defmethod analyze-defstruct ((parser file-parser) form)
 (destructuring-bind (def-op name-and-options &rest slots) form
   (declare (ignore def-op))
   ;; 1. Form Analysis Context
   (let* ((pkg (current-package parser))
          (pkg-name (current-package-name parser))
          (file (file parser))
          (spec-symbols nil))  ; Collect non-CL symbols for dependency tracking

     ;; 2. Primary Definition Record
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
                                      :external))

       ;; 3. Interface Elements
       (let ((interface-functions nil))
         ;; 3a. Constructor handling
         (let ((constructor-options (remove-if-not 
                                   (lambda (x) (eq (car x) :constructor))
                                   options)))
           (cond ((null constructor-options)  ; Default case
                  (push (intern (concatenate 'string "MAKE-" (string struct-name)) pkg)
                        interface-functions))
                 (t  ; Handle explicit constructors
                  (dolist (opt constructor-options)
                    (when (and (cdr opt)  ; Has a name
                             (not (null (second opt))))  ; Name not nil
                      (push (second opt) interface-functions))))))

         ;; 3b. Copier handling
         (let ((copier-option (find :copier options :key #'car)))
           (when (or (null copier-option)  ; Default case
                    (and (cdr copier-option)  ; Explicit name given
                         (not (null (second copier-option)))))
             (push (if (and copier-option (second copier-option))
                      (second copier-option)
                      (intern (concatenate 'string "COPY-" (string struct-name)) pkg))
                   interface-functions)))

         ;; 3c. Predicate handling
         (let ((predicate-option (find :predicate options :key #'car)))
           (when (or (null predicate-option)  ; Default case
                    (and (cdr predicate-option)  ; Explicit name given
                         (not (null (second predicate-option)))))
             (push (if (and predicate-option (second predicate-option))
                      (second predicate-option)
                      (intern (concatenate 'string (string struct-name) "-P") pkg))
                   interface-functions)))

         ;; Record all interface function definitions
         (dolist (func interface-functions)
           (record-definition *current-tracker* func
                            :FUNCTION
                            file
                            :package pkg-name
                            :exported-p (eq (nth-value 1 
                                          (find-symbol (symbol-name func) pkg))
                                         :external))))

       ;; 4. Specification Elements 
       ;; 4a. Process included structure if present
       (let ((included (find :include options :key #'car)))
         (when included
           (push (second included) spec-symbols)  ; Record included structure name
           ;; Process included slot overrides for spec symbols
           (loop for override in (cddr included)
                 when (listp override)
                 do (dolist (sym (extract-spec-symbols-from-slot override))
                      (push sym spec-symbols)))))

       ;; 4b. Process slot definitions and types
       (dolist (slot slots)
         (let ((slot-spec (if (listp slot) slot (list slot))))
           ;; Collect spec symbols from slot definition
           (dolist (sym (extract-spec-symbols-from-slot slot-spec))
             (push sym spec-symbols))
           ;; Generate and record accessor function
           (let ((accessor (intern (concatenate 'string conc-name-prefix
                                              (string (if (listp slot)
                                                        (car slot)
                                                        slot)))
                                 pkg)))
             (record-definition *current-tracker* accessor
                              :FUNCTION
                              file
                              :package pkg-name
                              :exported-p (eq (nth-value 1 
                                            (find-symbol (string accessor) pkg))
                                           :external)))))

       ;; 5. Implementation Elements - no additional processing needed
       ;; beyond what's handled in slot processing above

       ;; 6. Symbol Collection/Association 
       ;; Store unique non-CL spec symbols with structure definition
       (let ((unique-symbols (remove-duplicates (remove-if #'cl-symbol-p spec-symbols))))
         (when unique-symbols
           (setf (definition.spec-symbols 
                  (get-definitions struct-name))
                 unique-symbols)))))))


;;; Helper function to extract specification symbols from a slot definition
(defun extract-spec-symbols-from-slot (slot-spec)
 "Extract potential definition-reference symbols from a slot specification.
  Only collects symbols that could be user-defined, ignoring CL symbols."
 (let ((symbols nil))
   (when (listp slot-spec)
     (destructuring-bind (name &optional initform &rest slot-options) slot-spec
       (declare (ignore name))
       ;; Check initform for non-quoted symbols
       (when (and initform (not (quoted-form-p initform)))
         (collect-non-cl-symbols initform symbols))
       ;; Process slot options
       (loop for (option value) on slot-options by #'cddr
             do (case option
                  (:type (collect-non-cl-symbols value symbols))))))
   symbols))


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
           (spec-symbols nil))  ; Collect non-CL symbols for dependency tracking
      
      ;; 2. Primary Definition Record
      (record-definition *current-tracker* name
                        :VARIABLE
                        file
                        :package pkg-name
                        :exported-p (eq (nth-value 1 
                                       (find-symbol (symbol-name name) pkg))
                                      :external))
      
      ;; 3. Interface Elements - None for variables
      
      ;; 4. Specification Elements
      ;; Process type declarations if present
      (dolist (decl decls)
        (when (and (listp decl) 
                   (eq (car decl) 'declare))
          (dolist (spec (cdr decl))
            (when (and (listp spec)
                      (eq (car spec) 'type))
              (unless (cl-symbol-p (second spec))
                (push (second spec) spec-symbols))))))
      
      ;; 5. Implementation Elements
      ;; Analyze initialization form if present
      (when initform
        (unless (quoted-form-p initform)
          (collect-non-cl-symbols initform spec-symbols)))
      
      ;; 6. Symbol Collection/Association
      ;; Store unique non-CL spec symbols with variable definition
      (let ((unique-symbols (remove-duplicates (remove-if #'cl-symbol-p spec-symbols))))
        (when unique-symbols
          (setf (definition.spec-symbols 
                 (get-definitions name))
                unique-symbols))))))


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
           (spec-symbols nil))  ; Collect non-CL symbols for dependency tracking
      
      ;; 2. Primary Definition Record
      (record-definition *current-tracker* name
                        (if (eq def-op 'defun) 
                            :FUNCTION 
                            :MACRO)
                        file
                        :package pkg-name
                        :exported-p (eq (nth-value 1 
                                       (find-symbol (symbol-name name) pkg))
                                      :external))
      
      ;; 3. Interface Elements 
      ;; For macros, record any helper functions defined in the body
      (when (eq def-op 'defmacro)
        (let ((helper-functions nil))
          (dolist (form body)
            (when (and (listp form) 
                      (member (car form) '(defun flet labels)))
              (push (if (eq (car form) 'defun)
                       (second form)
                       (caar (cdr form)))
                    helper-functions)))
          ;; Record helper functions as internal
          (dolist (func helper-functions)
            (record-definition *current-tracker* func
                             :FUNCTION
                             file
                             :package pkg-name
                             :exported-p nil))))
      
      ;; 4. Specification Elements
      ;; Process lambda list including type declarations and defaults
      (analyze-lambda-list parser lambda-list)
      ;; Collect non-CL symbols from lambda list type declarations
      (dolist (item lambda-list)
        (when (and (listp item) (> (length item) 1))
          (let ((type-spec (second item)))
            (unless (or (quoted-form-p type-spec)
                       (cl-symbol-p type-spec))
              (push type-spec spec-symbols)))))
      
      ;; 5. Implementation Elements
      ;; Process declarations and body forms
      (multiple-value-bind (decls forms)
          (parse-body body :documentation t)
        ;; Handle declarations
        (dolist (decl decls)
          (when (and (listp decl) (eq (car decl) 'declare))
            (dolist (spec (cdr decl))
              (when (and (listp spec) (eq (car spec) 'type))
                (unless (cl-symbol-p (second spec))
                  (push (second spec) spec-symbols))))))
        ;; Analyze body forms for nested definitions
        (dolist (form forms)
          (unless (quoted-form-p form)
            (collect-non-cl-symbols form spec-symbols)))
        ;; For macros, record symbols used in body
        (when (eq def-op 'defmacro)
          (record-macro-body-symbols *current-tracker* name spec-symbols)))
      
      ;; 6. Symbol Collection/Association
      ;; Store unique non-CL spec symbols with function/macro definition
      (let ((unique-symbols (remove-duplicates (remove-if #'cl-symbol-p spec-symbols))))
        (when unique-symbols
          (setf (definition.spec-symbols 
                 (get-definitions name))
                unique-symbols))))))


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
                 (when (and (listp item) (cddr item))
                   (analyze-definition-form parser (third item)))))) ; Default value
            (&aux
             (when (and (listp item) (cdr item))
               (analyze-definition-form parser (second item))))))))))


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
          (spec-symbols nil))

     ;; 2. Primary Definition Record  
     (record-definition *current-tracker* name
                       :GENERIC-FUNCTION
                       file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name) pkg))
                                     :external))

     ;; 3. Interface Elements
     ;; Collect method names from :method options
     (let ((method-functions nil))
       (dolist (option options)
         (when (and (listp option) 
                    (eq (car option) :method))
           (push name method-functions)))
       ;; Record all method definitions
       (dolist (func method-functions)
         (record-definition *current-tracker* func
                          :METHOD
                          file
                          :package pkg-name
                          :exported-p (eq (nth-value 1 
                                         (find-symbol (symbol-name func) pkg))
                                        :external))))

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
           ;; Record method combination type
           (unless (cl-symbol-p combination-name)
             (push combination-name spec-symbols))
           ;; Process combination arguments
           (dolist (arg combination-args)
             (unless (quoted-form-p arg)
               (collect-non-cl-symbols arg spec-symbols))))))

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
            (let ((class-name (second option)))
              (unless (cl-symbol-p class-name)
                (push class-name spec-symbols))))

           (:method
            ;; Process method options similar to defmethod
            (let ((method-qualifiers nil)
                  (method-lambda-list nil)
                  (remaining-forms (cdr option)))
              ;; Collect qualifiers
              (loop while (and remaining-forms 
                             (not (listp (car remaining-forms))))
                    do (let ((qual (pop remaining-forms)))
                         (push qual qualifiers)
                         (when (consp qual)
                           (collect-non-cl-symbols qual spec-symbols))))
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
                                (unless (cl-symbol-p specializer)
                                  (push specializer spec-symbols)))
                               (cons
                                (case (car specializer)
                                  (eql 
                                   (unless (quoted-form-p (second specializer))
                                     (collect-non-cl-symbols 
                                      (second specializer) spec-symbols)))
                                  (t 
                                   (collect-non-cl-symbols 
                                    specializer spec-symbols))))))))
                ;; Process method body
                (dolist (form remaining-forms)
                  (unless (quoted-form-p form)
                    (collect-non-cl-symbols form spec-symbols)))))

           (:declare
            ;; Process declarations
            (dolist (decl (cdr option))
              (when (and (listp decl)
                        (eq (car decl) 'type))
                (unless (cl-symbol-p (second decl))
                  (push (second decl) spec-symbols)))))

           (t 
            ;; Handle any other options
            (dolist (element (cdr option))
              (unless (quoted-form-p element)
                (collect-non-cl-symbols element spec-symbols)))))))

     ;; 6. Symbol Collection/Association
     ;; Store unique non-CL spec symbols with generic function definition
     (let ((unique-symbols (remove-duplicates (remove-if #'cl-symbol-p spec-symbols))))
       (when unique-symbols
         (setf (definition.spec-symbols 
                (get-definitions name))
               unique-symbols)))))))


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
          (spec-symbols nil)
          (qualifiers nil)
          (lambda-list nil)
          (method-body nil))
     
     ;; 2. Primary Definition Record
     (record-definition *current-tracker* name
                       :METHOD
                       file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name) pkg))
                                     :external))
     
     ;; 3. Interface Elements
     ;; Collect qualifiers until we hit lambda list
     (loop for rest on body
           while (and rest (not (listp (car rest))))
           do (let ((qual (pop rest)))
                (push qual qualifiers)
                ;; Analyze qualifier if it's a compound form
                (when (consp qual)
                  (collect-non-cl-symbols qual spec-symbols)))
           finally (when rest
                    (setf lambda-list (pop rest)
                          method-body rest)))
     
     ;; 4. Specification Elements
     ;; 4a. Process specialized lambda list parameters 
     (when lambda-list
       (loop for param in lambda-list
             do (typecase param
                  (symbol nil) ; Simple parameter
                  (cons  ; Specialized parameter
                   (destructuring-bind (param-name &optional specializer) param
                     (declare (ignore param-name))
                     (when specializer
                       ;; Handle different specializer types
                       (typecase specializer
                         (symbol  ; Regular type specializer
                          (unless (cl-symbol-p specializer)
                            (push specializer spec-symbols)))
                         (cons    ; EQL or other specializer
                          (case (car specializer)
                            (eql  ; (eql form)
                             (when (cddr specializer)
                               (collect-non-cl-symbols (third specializer) 
                                                     spec-symbols))
                             (unless (quoted-form-p (second specializer))
                               (collect-non-cl-symbols (second specializer) 
                                                     spec-symbols)))
                            (t    ; Other specializer form
                             (collect-non-cl-symbols specializer 
                                                   spec-symbols)))))))))))
     
     ;; 4b. Analyze lambda list for type declarations and defaults
     (analyze-lambda-list parser lambda-list)
     
     ;; 5. Implementation Elements
     (when method-body
       ;; Process declarations at start of body
       (multiple-value-bind (decls forms)
           (parse-body method-body :documentation t)
         ;; Handle declarations
         (dolist (decl decls)
           (when (and (listp decl) (eq (car decl) 'declare))
             (dolist (spec (cdr decl))
               (when (and (listp spec) (eq (car spec) 'type))
                 (unless (cl-symbol-p (second spec))
                   (push (second spec) spec-symbols))))))
         ;; Analyze remaining body forms
         (dolist (form forms)
           (unless (quoted-form-p form)
             (collect-non-cl-symbols form spec-symbols)))))
     
     ;; 6. Symbol Collection/Association
     ;; Store unique non-CL spec symbols with method definition
     (let ((unique-symbols (remove-duplicates (remove-if #'cl-symbol-p spec-symbols))))
       (when unique-symbols
         (setf (definition.spec-symbols 
                (get-definitions name))
               unique-symbols))))))


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
           (spec-symbols nil))   ; Collect non-CL symbols for dependency tracking
      
      ;; 2. Primary Definition Record
      (record-definition *current-tracker* name
                        :CONDITION
                        file
                        :package pkg-name
                        :exported-p (eq (nth-value 1 
                                       (find-symbol (symbol-name name) pkg))
                                      :external))
      
      ;; 3. Interface Elements - collect for later recording
      (let ((interface-functions nil))
        ;; Process slots for readers/writers/accessors
        (dolist (slot slots-and-options)
          (when (and (listp slot) (car slot))  ; Valid slot definition
            (loop for (option value) on (cdr slot) by #'cddr
                  when (member option '(:reader :writer :accessor))
                  do (push value interface-functions))))
        
        ;; Record all interface function definitions
        (dolist (func interface-functions)
          (record-definition *current-tracker* func
                           :FUNCTION
                           file
                           :package pkg-name
                           :exported-p (eq (nth-value 1 
                                         (find-symbol (symbol-name func) pkg))
                                        :external))))
      
      ;; 4. Specification Elements
      ;; 4a. Process superclasses
      (dolist (super superclasses)
        (unless (cl-symbol-p super)
          (push super spec-symbols)))
      
      ;; 4b. Process slots and collect specification symbols
      (dolist (slot slots-and-options)
        (when (and (listp slot) (car slot))  ; Valid slot definition
          (loop for (option value) on (cdr slot) by #'cddr
                do (case option
                     (:type 
                      (collect-non-cl-symbols value spec-symbols))
                     (:initform 
                      (unless (quoted-form-p value)
                        (collect-non-cl-symbols value spec-symbols)))))))
      
      ;; 5. Implementation Elements
      (dolist (option slots-and-options)
        (when (and (listp option) (keywordp (car option)))
          (case (car option)
            (:report
             (let ((reporter (second option)))
               (typecase reporter
                 (symbol  ; Function name
                  (push reporter spec-symbols))
                 (cons    ; Lambda form
                  (when (eq (car reporter) 'lambda)
                    (analyze-lambda-list parser (cadr reporter))
                    (dolist (form (cddr reporter))
                      (analyze-definition-form parser form)))))))
            (:default-initargs
             (loop for (key value) on (cdr option) by #'cddr
                   unless (quoted-form-p value)
                   do (collect-non-cl-symbols value spec-symbols))))))
      
      ;; 6. Symbol Collection/Association
      ;; Store unique non-CL spec symbols with condition definition
      (let ((unique-symbols (remove-duplicates (remove-if #'cl-symbol-p spec-symbols))))
        (when unique-symbols
          (setf (definition.spec-symbols 
                 (get-definitions name))
                unique-symbols))))))


(defun analyze-define-method-combination (parser form)
  "Handle define-method-combination forms. Records the definition and analyzes all parts
   for nested definitions including group names, patterns, and option values. Supports 
   both short and long forms."
  (destructuring-bind (def-op name &rest args) form
    (declare (ignore def-op))
    (let ((package (current-package parser)))
      ;; Record the method combination definition
      (record-definition *current-tracker* name
                        :FUNCTION
                        (file parser)
                        :package (current-package-name parser)
                        :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name)
                                                 package))
                                     :external))
      (if (and (car args) (listp (car args)))
          ;; Long form
          (destructuring-bind (lambda-list &rest body) args
            ;; Analyze lambda list for type declarations and defaults
            (analyze-lambda-list parser lambda-list)
            ;; Process all body forms including method group specs
            (dolist (form body)
              (if (and (listp form) 
                       (member (car form) '(:arguments :generic-function :documentation)))
                  ;; Handle option forms - analyze all parts
                  (dolist (part (cdr form))
                    (analyze-definition-form parser part))
                  ;; Handle method group spec
                  (when (listp form)
                    (destructuring-bind (group-name pattern &rest group-options) form
                      ;; Analyze group name - could be user-defined
                      (analyze-definition-form parser group-name)
                      ;; Analyze pattern - could contain user predicates
                      (analyze-definition-form parser pattern)
                      ;; Analyze all option keywords and values
                      (loop for (key value) on group-options by #'cddr
                            do (analyze-definition-form parser key)
                            do (analyze-definition-form parser value)))))))
          ;; Short form - analyze all options and their values
          (loop for (option value) on args by #'cddr
                do (analyze-definition-form parser option)
                do (analyze-definition-form parser value))))))


(defun analyze-setf-expander (parser form)
 "Handle define-setf-expander forms. Analyzes:
  - Expander name and exportedness
  - Lambda list parameters and their types
  - Body declarations and forms 
  - Sub-definitions in body"
 (destructuring-bind (def-op name lambda-list &rest body) form
   (declare (ignore def-op))
   ;; 1. Form Analysis Context
   (let* ((pkg (current-package parser))
          (pkg-name (current-package-name parser))
          (file (file parser))
          (spec-symbols nil))

     ;; 2. Primary Definition Record
     (record-definition *current-tracker* name
                       :FUNCTION  ; Setf expanders are functions
                       file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name) pkg))
                                     :external))
     
     ;; 3. Interface Elements
     ;; Collect any helper functions defined in the body
     (let ((helper-functions nil))
       (dolist (form body)
         (when (and (listp form)
                    (member (car form) '(defun flet labels)))
           (push (if (eq (car form) 'defun)
                    (second form)
                    (caar (cdr form)))
                 helper-functions)))
       ;; Record helper functions as internal
       (dolist (func helper-functions)
         (record-definition *current-tracker* func
                          :FUNCTION
                          file
                          :package pkg-name
                          :exported-p nil)))

     ;; 4. Specification Elements
     ;; Process lambda list for parameters and types
     (analyze-lambda-list parser lambda-list)
     ;; Collect non-CL symbols from lambda list type declarations
     (dolist (item lambda-list)
       (when (and (listp item) (> (length item) 1))
         (let ((type-spec (second item)))
           (unless (or (quoted-form-p type-spec)
                      (cl-symbol-p type-spec))
             (push type-spec spec-symbols)))))

     ;; 5. Implementation Elements
     ;; Process body forms and declarations
     (multiple-value-bind (decls forms)
         (parse-body body :documentation t)
       ;; Handle declarations
       (dolist (decl decls)
         (when (and (listp decl) (eq (car decl) 'declare))
           (dolist (spec (cdr decl))
             (when (and (listp spec) (eq (car spec) 'type))
               (unless (cl-symbol-p (second spec))
                 (push (second spec) spec-symbols))))))
       ;; Analyze remaining body forms
       (dolist (form forms)
         (unless (quoted-form-p form)
           (collect-non-cl-symbols form spec-symbols))))

     ;; 6. Symbol Collection/Association
     ;; Store unique non-CL spec symbols with expander definition
     (let ((unique-symbols (remove-duplicates (remove-if #'cl-symbol-p spec-symbols))))
       (when unique-symbols
         (setf (definition.spec-symbols 
                (get-definitions name))
               unique-symbols))))))


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
          (spec-symbols nil))

     ;; 2. Primary Definition Record
     (record-definition *current-tracker* name
                       :MACRO
                       file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name) pkg))
                                     :external))

     ;; 3. Interface Elements
     ;; Record underlying function as a reference
     (unless (cl-symbol-p function)
       (push function spec-symbols))

     ;; 4. Specification Elements
     ;; Process lambda list for parameters and types
     (analyze-lambda-list parser lambda-list)
     ;; Collect non-CL symbols from lambda list type declarations
     (dolist (item lambda-list)
       (when (and (listp item) (> (length item) 1))
         (let ((type-spec (second item)))
           (unless (or (quoted-form-p type-spec)
                      (cl-symbol-p type-spec))
             (push type-spec spec-symbols)))))

     ;; 5. Implementation Elements
     ;; None - modify macros don't have implementation bodies
     
     ;; 6. Symbol Collection/Association
     ;; Store unique non-CL spec symbols with macro definition
     (let ((unique-symbols (remove-duplicates (remove-if #'cl-symbol-p spec-symbols))))
       (when unique-symbols
         (setf (definition.spec-symbols 
                (get-definitions name))
               unique-symbols))))))


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
          (spec-symbols nil))

     ;; 2. Primary Definition Record
     (record-definition *current-tracker* name
                       :MACRO
                       file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name) pkg))
                                     :external))

     ;; 3. Interface Elements
     ;; Collect helper functions defined in the body
     (let ((helper-functions nil))
       (dolist (form body)
         (when (and (listp form)
                    (member (car form) '(defun flet labels)))
           (push (if (eq (car form) 'defun)
                    (second form)
                    (caar (cdr form)))
                 helper-functions)))
       ;; Record helper functions as internal
       (dolist (func helper-functions)
         (record-definition *current-tracker* func
                          :FUNCTION
                          file
                          :package pkg-name
                          :exported-p nil)))

     ;; 4. Specification Elements
     ;; Process lambda list for parameters and types
     (analyze-lambda-list parser lambda-list)
     ;; Collect non-CL symbols from lambda list type declarations
     (dolist (item lambda-list)
       (when (and (listp item) (> (length item) 1))
         (let ((type-spec (second item)))
           (unless (or (quoted-form-p type-spec)
                      (cl-symbol-p type-spec))
             (push type-spec spec-symbols)))))

     ;; 5. Implementation Elements
     ;; Process body forms and declarations
     (multiple-value-bind (decls forms)
         (parse-body body :documentation t)
       ;; Handle declarations
       (dolist (decl decls)
         (when (and (listp decl) (eq (car decl) 'declare))
           (dolist (spec (cdr decl))
             (when (and (listp spec) (eq (car spec) 'type))
               (unless (cl-symbol-p (second spec))
                 (push (second spec) spec-symbols))))))
       ;; Analyze body forms and record their symbols
       (dolist (form forms)
         (unless (quoted-form-p form)
           (collect-non-cl-symbols form spec-symbols)))
       ;; Record macro body symbols for expansion analysis
       (record-macro-body-symbols *current-tracker* name spec-symbols))

     ;; 6. Symbol Collection/Association
     ;; Store unique non-CL spec symbols with compiler macro definition
     (let ((unique-symbols (remove-duplicates (remove-if #'cl-symbol-p spec-symbols))))
       (when unique-symbols
         (setf (definition.spec-symbols 
                (get-definitions name))
               unique-symbols))))))


(defun analyze-setf-symbol-value (parser form)
 "Handle (setf (symbol-value 'name) value) forms. Analyzes:
  - Variable name and exportedness
  - Value form for dependencies
  - Any related type declarations
  Returns the variable definition record."
 (destructuring-bind (setf (symbol-value quoted-name) value-form) form
   ;; 1. Form Analysis Context
   (let* ((pkg (current-package parser))
          (pkg-name (current-package-name parser))
          (file (file parser))
          (spec-symbols nil)
          (name (second quoted-name)))  ; Extract symbol from 'name form

     ;; 2. Primary Definition Record
     (record-definition *current-tracker* name
                       :VARIABLE
                       file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name) pkg))
                                     :external))

     ;; 3. Interface Elements
     ;; None - variables don't have interface elements

     ;; 4. Specification Elements
     ;; Look for any type declarations preceding the setf
     (let ((preceding-forms (preceding-forms-in-body form parser)))
       (dolist (prev-form preceding-forms)
         (when (and (listp prev-form)
                    (eq (car prev-form) 'declare))
           (dolist (decl (cdr prev-form))
             (when (and (listp decl)
                       (eq (car decl) 'type)
                       (eq (third decl) name))
               (unless (cl-symbol-p (second decl))
                 (push (second decl) spec-symbols)))))))

     ;; 5. Implementation Elements
     ;; Analyze value form for dependencies
     (unless (quoted-form-p value-form)
       (collect-non-cl-symbols value-form spec-symbols))

     ;; 6. Symbol Collection/Association
     ;; Store unique non-CL spec symbols with variable definition
     (let ((unique-symbols (remove-duplicates (remove-if #'cl-symbol-p spec-symbols))))
       (when unique-symbols
         (setf (definition.spec-symbols 
                (get-definitions name))
               unique-symbols))))))


(defun preceding-forms-in-body (form parser)
 "Find forms that precede the given form in the same body context.
  Used to find relevant declarations for symbol-value and symbol-function forms.
  Returns a list of preceding forms in reverse order (most recent first)."
 (let* ((file (file parser))
        (preceding nil))
   (with-open-file (stream file :direction :input)
     (loop with found = nil
           for current-form = (read stream nil nil)
           while (and current-form (not found))
           do (if (equal current-form form)
                 (setf found t)
                 (push current-form preceding))
           finally (return (remove-if-not 
                          ;; Only keep declarations and type specs
                          (lambda (f)
                            (and (listp f)
                                 (or (eq (car f) 'declare)
                                     (and (eq (car f) 'type)
                                          (= (length f) 3)))))
                          preceding))))))


(defun analyze-setf-symbol-function-fdefinition-macro-function (parser form)
  "Handle (setf (symbol-function|fdefinition|macro-function 'name) function-form) definitions.
   Form structure: (setf (accessor 'name) function-form)"
  (let ((accessor (caadr form))           ; symbol-function, fdefinition, or macro-function
        (name (cadr (cadr (cadr form))))         ; Get symbol from ('accessor 'name)
        (function-form (third form))      ; The function/lambda form
        (package (current-package parser)))
    (cond
      ;; Handle symbol-function and fdefinition
      ((member accessor '(symbol-function fdefinition))
       (when (and (listp function-form)
                  (eq (car function-form) 'lambda))
         ;; Record function definition
         (record-definition *current-tracker* name
                          :FUNCTION
                          (file parser)
                          :package (current-package-name parser)
                          :exported-p (eq (nth-value 1 
                                        (find-symbol (symbol-name name) package))
                                       :external))
         ;; Analyze lambda list and body
         (analyze-lambda-list parser (cadr function-form))
         (dolist (form (cddr function-form))
           (analyze-definition-form parser form))))
      ;; Handle macro-function
      ((eq accessor 'macro-function)
       (when (listp function-form)  ; Macro function might not use lambda directly
         ;; Record macro definition
         (record-definition *current-tracker* name
                          :MACRO
                          (file parser)
                          :package (current-package-name parser)
                          :exported-p (eq (nth-value 1 
                                        (find-symbol (symbol-name name) package))
                                       :external))
         ;; Analyze the function body
         (dolist (form (cdr function-form))
           (analyze-definition-form parser form)))))))


(defun analyze-defclass (parser form)
 "Handle defclass form. Analyzes:
  - Class name and exportedness
  - Superclasses and their references
  - Slot definitions and all their options
  - Generated accessor/reader/writer methods
  - Class options including :metaclass and :default-initargs
  - Documentation strings
  Returns the class definition record."
 (destructuring-bind (def-op name superclasses slots &rest options) form
   (declare (ignore def-op))
   ;; 1. Form Analysis Context
   (let* ((pkg (current-package parser))
          (pkg-name (current-package-name parser))
          (file (file parser))
          (spec-symbols nil))

     ;; 2. Primary Definition Record
     (record-definition *current-tracker* name
                       :CLASS
                       file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name) pkg))
                                     :external))

     ;; 3. Interface Elements
     (let ((interface-functions nil))
       ;; Collect all accessor/reader/writer functions from slots
       (dolist (slot slots)
         (when (listp slot)  ; Skip atomic slot names
           (destructuring-bind (slot-name &rest slot-options) slot
             (declare (ignore slot-name))
             (loop for (option value) on slot-options by #'cddr
                   when (member option '(:reader :writer :accessor))
                   do (push value interface-functions)))))
       
       ;; Record all interface function definitions
       (dolist (func interface-functions)
         (record-definition *current-tracker* func
                          :FUNCTION
                          file
                          :package pkg-name
                          :exported-p (eq (nth-value 1 
                                        (find-symbol (symbol-name func) pkg))
                                       :external))))

     ;; 4. Specification Elements
     ;; 4a. Process superclasses
     (dolist (super superclasses)
       (unless (cl-symbol-p super)
         (push super spec-symbols)))

     ;; 4b. Process slot specifications
     (dolist (slot slots)
       (when (listp slot)
         (destructuring-bind (slot-name &rest slot-options) slot
           (declare (ignore slot-name))
           (loop for (option value) on slot-options by #'cddr
                 do (case option
                      (:type 
                       ;; Collect type specifier symbols
                       (unless (cl-symbol-p value)
                         (collect-non-cl-symbols value spec-symbols)))
                      (:initform
                       ;; Analyze initialization form
                       (unless (quoted-form-p value)
                         (collect-non-cl-symbols value spec-symbols)))
                      (:allocation
                       ;; Check for custom allocation types
                       (unless (member value '(:instance :class))
                         (push value spec-symbols)))
                      (:initarg
                       ;; Record any non-keyword initargs
                       (unless (keywordp value)
                         (push value spec-symbols))))))))

     ;; 5. Implementation Elements
     (dolist (option options)
       (when (listp option)
         (case (car option)
           (:metaclass
            ;; Record metaclass reference
            (let ((metaclass (second option)))
              (unless (cl-symbol-p metaclass)
                (push metaclass spec-symbols))))
           
           (:default-initargs
            ;; Analyze default initarg forms
            (loop for (key value) on (cdr option) by #'cddr
                  do (unless (quoted-form-p value)
                       (collect-non-cl-symbols value spec-symbols))))
           
           (:documentation
            nil)  ; Skip documentation strings
           
           (t 
            ;; Handle any other class options
            (dolist (element (cdr option))
              (unless (quoted-form-p element)
                (collect-non-cl-symbols element spec-symbols)))))))

     ;; 6. Symbol Collection/Association
     ;; Store unique non-CL spec symbols with class definition
     (let ((unique-symbols (remove-duplicates (remove-if #'cl-symbol-p spec-symbols))))
       (when unique-symbols
         (setf (definition.spec-symbols 
                (get-definitions name))
               unique-symbols))))))


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
          (spec-symbols nil))

     ;; 2. Primary Definition Record
     (record-definition *current-tracker* name
                       :TYPE
                       file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name) pkg))
                                     :external))

     ;; 3. Interface Elements
     ;; Type definitions don't generate interface functions
     ;; but may be used in other type specifiers

     ;; 4. Specification Elements
     ;; 4a. Process type lambda list
     (when lambda-list
       (loop for param in lambda-list
             do (typecase param
                  (symbol 
                   nil) ; Skip basic parameters
                  (cons  ; Handle parameters with defaults
                   (when (cddr param)  ; Has default value
                     (let ((default (third param)))
                       (unless (quoted-form-p default)
                         (collect-non-cl-symbols default spec-symbols))))))))

     ;; 5. Implementation Elements
     ;; Process body forms and declarations
     (multiple-value-bind (decls forms)
         (parse-body body :documentation t)
       ;; Handle declarations
       (dolist (decl decls)
         (when (and (listp decl) (eq (car decl) 'declare))
           (dolist (spec (cdr decl))
             (case (car spec)
               ;; Handle type declarations specially
               (type 
                (unless (cl-symbol-p (second spec))
                  (push (second spec) spec-symbols)))
               ;; collect other declaration dependencies
               (t (collect-non-cl-symbols (cdr spec) spec-symbols))))))
       
       ;; Analyze expansion form(s)
       (dolist (form forms)
         (typecase form
           (cons
            (case (car form)
              ;; Look specifically for type specifier forms
              ((and or not satisfies)
               (dolist (type-spec (cdr form))
                 (unless (cl-symbol-p type-spec)
                   (push type-spec spec-symbols))))
              ;; Handle (integer 0 *) style forms
              ((integer rational real float number)
               nil) ; Skip standard type specifiers
              ;; Regular form analysis
              (t 
               (unless (quoted-form-p form)
                 (collect-non-cl-symbols form spec-symbols)))))
           ;; Handle atomic type specifiers
           (symbol
            (unless (cl-symbol-p form)
              (push form spec-symbols))))))
     
     ;; 6. Symbol Collection/Association
     ;; Store unique non-CL spec symbols with type definition
     (let ((unique-symbols (remove-duplicates (remove-if #'cl-symbol-p spec-symbols))))
       (when unique-symbols
         (setf (definition.spec-symbols 
                (get-definitions name))
               unique-symbols))))))


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
          (spec-symbols nil))

     ;; 2. Primary Definition Record
     (record-definition *current-tracker* name
                       :SYMBOL-MACRO
                       file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name) pkg))
                                     :external))

     ;; 3. Interface Elements
     ;; Symbol macros don't generate interface functions
     ;; but may be referenced like regular variables

     ;; 4. Specification Elements
     ;; Look for type declarations preceding the definition
     (let ((preceding-forms (preceding-forms-in-body form parser)))
       (dolist (prev-form preceding-forms)
         (when (and (listp prev-form)
                    (eq (car prev-form) 'declare))
           (dolist (decl (cdr prev-form))
             (when (and (listp decl)
                       (eq (car decl) 'type)
                       (eq (third decl) name))
               (unless (cl-symbol-p (second decl))
                 (push (second decl) spec-symbols)))))))

     ;; 5. Implementation Elements
     ;; Analyze expansion form for dependencies
     (unless (quoted-form-p expansion-form)
       (collect-non-cl-symbols expansion-form spec-symbols))
     ;; Record symbols used in expansion for macro processing
     (record-macro-body-symbols *current-tracker* name spec-symbols)

     ;; 6. Symbol Collection/Association
     ;; Store unique non-CL spec symbols with symbol-macro definition
     (let ((unique-symbols (remove-duplicates (remove-if #'cl-symbol-p spec-symbols))))
       (when unique-symbols
         (setf (definition.spec-symbols 
                (get-definitions name))
               unique-symbols))))))


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
          (spec-symbols nil))

     ;; 2. Primary Definition Record
     (record-definition *current-tracker* name
                       :SETF
                       file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name) pkg))
                                     :external))

     ;; 3. Interface Elements
     (if (and (car args) (listp (car args)))
         ;; Long form - collect helper functions from body
         (let ((helper-functions nil))
           (dolist (form (cdddr args))  ; Skip lambda-list and store-vars
             (when (and (listp form)
                       (member (car form) '(defun flet labels)))
               (push (if (eq (car form) 'defun)
                        (second form)
                        (caar (cdr form)))
                     helper-functions)))
           ;; Record helper functions as internal
           (dolist (func helper-functions)
             (record-definition *current-tracker* func
                              :FUNCTION
                              file
                              :package pkg-name
                              :exported-p nil)))
         ;; Short form - reference update function
         (unless (cl-symbol-p (car args))
           (push (car args) spec-symbols)))

     ;; 4. Specification Elements
     (if (and (car args) (listp (car args)))
         ;; Long form
         (destructuring-bind (lambda-list store-vars &rest body) args
           ;; Process access function lambda list
           (analyze-lambda-list parser lambda-list)
           ;; Process store variables
           (dolist (var store-vars)
             (when (and (listp var) (> (length var) 1))
               ;; Check for type declarations in store vars
               (let ((type-spec (second var)))
                 (unless (cl-symbol-p type-spec)
                   (push type-spec spec-symbols)))))
           ;; Collect type specs from lambda list
           (dolist (item lambda-list)
             (when (and (listp item) (> (length item) 1))
               (let ((type-spec (second item)))
                 (unless (or (quoted-form-p type-spec)
                            (cl-symbol-p type-spec))
                   (push type-spec spec-symbols))))))
         ;; Short form - no additional specs needed
         nil)

     ;; 5. Implementation Elements
     (when (and (car args) (listp (car args)))
       ;; Process long form body
       (let ((body (cdddr args)))
         (multiple-value-bind (decls forms)
             (parse-body body :documentation t)
           ;; Handle declarations
           (dolist (decl decls)
             (when (and (listp decl) (eq (car decl) 'declare))
               (dolist (spec (cdr decl))
                 (when (and (listp spec) (eq (car spec) 'type))
                   (unless (cl-symbol-p (second spec))
                     (push (second spec) spec-symbols))))))
           ;; Analyze body forms
           (dolist (form forms)
             (unless (quoted-form-p form)
               (collect-non-cl-symbols form spec-symbols)))
           ;; Record setf body symbols for expansion
           (record-macro-body-symbols *current-tracker* name spec-symbols))))

     ;; 6. Symbol Collection/Association
     ;; Store unique non-CL spec symbols with setf definition
     (let ((unique-symbols (remove-duplicates (remove-if #'cl-symbol-p spec-symbols))))
       (when unique-symbols
         (setf (definition.spec-symbols 
                (get-definitions name))
               unique-symbols))))))


(defun record-helper-functions (parser body)
 "Record helper functions defined within a body using defun/flet/labels.
  PARSER - The file parser for context
  BODY - List of body forms to scan for function definitions
  Returns list of helper function names recorded."
 (let ((helper-functions nil)
       (file (file parser))
       (pkg-name (current-package-name parser))
       (pkg (current-package parser)))
   ;; Scan body for function definitions
   (dolist (form body)
     (when (and (listp form)
                (member (car form) '(defun flet labels)))
       (push (if (eq (car form) 'defun)
                (second form)
                (caar (cdr form)))
             helper-functions)))
   ;; Record each helper function
   (dolist (func helper-functions)
     (record-definition *current-tracker* func
                      :FUNCTION
                      file
                      :package pkg-name
                      :exported-p nil))
   helper-functions))

We can then simplify the Interface Elements section in our analyzers to:
  ;; 3. Interface Elements
(record-helper-functions parser body)
Looking back at analyze-defmacro and analyze-defun from earlier, we were recording helper functions there as well.
The pattern of collecting helper functions appears in:
analyze-defsetf (just added)
analyze-define-symbol-macro
analyze-define-compiler-macro
analyze-setf-symbol-function-fdefinition-macro-function
Let me check analyze-defun-defmacro... yes, we were doing helper function collection there too.
(let ((helper-functions nil))
  (dolist (form body)
    (when (and (listp form)
               (member (car form) '(defun flet labels)))
      (push (if (eq (car form) 'defun)
               (second form)
               (caar (cdr form)))
            helper-functions)))
  ;; Record helper functions as internal
  (dolist (func helper-functions)
    (record-definition *current-tracker* func
                     :FUNCTION
                     file
                     :package pkg-name
                     :exported-p nil)))