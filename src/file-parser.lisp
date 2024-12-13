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
  "Analyze form for definitions using walk-form with systematic handler.
   Records definitions and processes nested definitions maintaining package context."
  (labels ((definition-handler (subform context parent-context depth)
             (declare (ignorable subform context parent-context depth))
             (when (and (consp subform)
                       (symbolp (car subform)) 
                       (not (quoted-form-p subform)))
               (case (car subform)
                 ;; Package definition forms
                 (defpackage (analyze-defpackage parser subform))
                 ;; Type/class family
                 (defclass (analyze-defclass parser subform))
                 (defstruct (analyze-defstruct parser subform))
                 (deftype (analyze-deftype parser subform))
                 (define-condition (analyze-define-condition parser subform))
                 ;; Variable definitions
                 ((defvar defparameter defconstant) 
                  (analyze-defvar-defparameter-defconstant parser subform))
                 ;; Function family
                 ((defun defmacro) (analyze-defun-defmacro parser subform))
                 (defgeneric (analyze-defgeneric parser subform))
                 (defmethod (analyze-defmethod parser subform))
                 ;; Setf forms
                 (defsetf (analyze-defsetf parser subform))
                 (define-setf-expander (analyze-define-setf-expander parser subform))
                 ;; Other definition forms
                 (define-symbol-macro (analyze-define-symbol-macro parser subform))
                 (define-modify-macro (analyze-define-modify-macro parser subform))
                 (define-compiler-macro (analyze-define-compiler-macro parser subform))
                 (define-method-combination (analyze-define-method-combination parser subform))
                 ;; Handle setf of symbol-function etc
                 (setf (when (and (cddr subform) (listp (second subform)))
                        (case (caadr subform)
                          (symbol-value 
                           (analyze-setf-symbol-value parser subform))
                          ((symbol-function fdefinition macro-function)
                           (analyze-setf-symbol-function-fdefinition-macro-function 
                            parser subform)))))))))
    (walk-form form #'definition-handler :log-stream log-stream)))


(defmethod analyze-reference-form ((parser file-parser) form &optional log-stream)
  "Analyze form recording references to definitions in different files.
   Handles references to packages (via string/keyword/uninterned-symbol) and 
   references to other defined symbols. Special handling for method calls to track
   both generic function and applicable method definitions."
  (labels ((handle-reference (subform context parent-context depth)
             (declare (ignorable subform context parent-context depth))
             (typecase subform
               ((or string keyword (and symbol (satisfies not-interned-p)))
                ;; Only process as package ref in package contexts
                (when (package-context-p context parent-context)
                  (let* ((norm-name (normalize-designator subform))
                         (key (make-tracking-key norm-name nil :PACKAGE))
                         (defs (gethash key (slot-value *current-tracker* 'definitions))))
                    ;; Collect all definitions from other files
                    (let ((other-file-defs 
                           (remove-if (lambda (def)  
                                        (equal (definition.file def) (file parser)))
                                      defs)))
                      (when other-file-defs
                        (record-reference *current-tracker*
                                          subform
                                          (file parser)
                                          :package norm-name 
                                          :context (limit-form-size parent-context norm-name)
                                          :visibility :LOCAL
                                          :definitions other-file-defs))))))
               (symbol
                (unless (or (null subform)           ; Skip NIL
                            (cl-symbol-p subform)) ; Skip CL package symbols
                  (let* ((sym-pkg (symbol-package subform))
                         (pkg-name (if sym-pkg
                                       (package-name sym-pkg)
                                       (current-package-name parser)))
                         (visibility (check-package-reference subform parser *current-tracker*
                                                               context parent-context)))
                    
                    ;; Check if symbol is in function call context
                    (multiple-value-bind (call-p name args)
                        (analyze-function-call-context subform context parent-context)
                      (if call-p
                          ;; Function call - handle method call
                          (handle-method-call subform parser pkg-name context visibility name args)
                          ;; Not a function call - check all definition types
                          (try-definition-types subform pkg-name parser context visibility)))))))))
    ;; Walk the form applying handler
    (walk-form form #'handle-reference :log-stream log-stream)))


(defun analyze-in-package (parser form)
  "Handle in-package forms by updating the current package context.
   Signals an error if referenced package doesn't exist."
  (let* ((name (normalize-designator (second form)))
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
    (let* ((pkg-name (normalize-designator name))
           (file (file parser))
           ;; Extract standard package options
           (use-list (cdr (assoc :use options)))
           (nicknames (cdr (assoc :nicknames options)))
           ;; Create package with standard options 
           (package (or (find-package pkg-name)
                       (make-package pkg-name 
                                   :use (mapcar #'normalize-designator use-list)
                                   :nicknames (mapcar #'normalize-designator nicknames))))
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
            (let ((used-name (normalize-designator used-pkg)))
              (record-package-use *current-tracker* pkg-name used-name)))))

      ;; 5. Implementation Elements
      ;; Process imports and other package options
      (dolist (option options)
        (when (listp option)
          (case (car option)
            (:import-from
             (let ((from-pkg (normalize-designator (second option))))
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
  "Handle defstruct form. Records structure definition and its accessor functions,
   letting walk-form handle nested definitions and references.
   PARSER - The file parser providing context
   FORM - The full definition form: (defstruct name-and-options &rest slots)"
  (let* ((pkg (current-package parser))
         (pkg-name (current-package-name parser))
         (file (file parser))
         (name-and-options (second form))
         (struct-name (if (listp name-and-options)
                         (car name-and-options)
                         name-and-options))
         (options (when (listp name-and-options)
                   (cdr name-and-options)))
         (context (limit-form-size form pkg-name))
         ;; Determine accessor prefix based on :conc-name option
         (conc-name-prefix 
          (let ((conc-option (find :conc-name options :key #'car)))
            (cond ((null conc-option)
                   (concatenate 'string (string struct-name) "-"))  ; Default
                  ((or (null (cdr conc-option))    ; (:conc-name)
                      (null (second conc-option)))  ; (:conc-name nil) 
                   "")
                  (t (string (second conc-option)))))))
    
    ;; Record structure definition
    (record-definition *current-tracker* struct-name
                      :STRUCTURE/CLASS/CONDITION
                      file
                      :package pkg-name
                      :exported-p (eq (nth-value 1 
                                     (find-symbol (symbol-name struct-name) pkg))
                                    :external)
                      :context context)
    
    ;; Record accessor function definitions 
    (dolist (slot (cddr form))  ; Skip defstruct and name-and-options
      (let* ((slot-name (if (listp slot) (car slot) slot))
             (accessor (intern (concatenate 'string conc-name-prefix
                                          (string slot-name))
                             pkg))
             (accessor-context (limit-form-size form pkg-name)))
        (record-definition *current-tracker* accessor
                          :FUNCTION
                          file
                          :package pkg-name
                          :exported-p (eq (nth-value 1 
                                         (find-symbol (string accessor) pkg))
                                        :external)
                          :context accessor-context)))))


(defun analyze-defvar-defparameter-defconstant (parser form)
  "Handle defvar, defparameter, and defconstant forms. Records the primary variable 
   definition, letting walk-form handle any nested definitions in initforms or declarations.
   PARSER - The file parser providing context
   FORM - The full definition form: (defvar|defparameter|defconstant name [initform [decls...]])"
  (let* ((pkg (current-package parser))
         (pkg-name (current-package-name parser))
         (file (file parser))
         (name (second form))
         (context (limit-form-size form pkg-name)))
    (record-definition *current-tracker* name
                      :VARIABLE
                      file
                      :package pkg-name
                      :exported-p (eq (nth-value 1 (find-symbol (symbol-name name) pkg))
                                      :external)
                      :context context)))


(defun analyze-defun-defmacro (parser form)
  "Handle defun and defmacro forms. Analyzes:
   - Function/macro name and exportedness"
  (let* ((def-op (first form))
         (name (second form))
         (pkg (current-package parser))
         (pkg-name (current-package-name parser))
         (file (file parser))
         (context (limit-form-size form pkg-name)))
    (record-definition *current-tracker* name
                        (if (eq def-op 'defun) 
                            :FUNCTION 
                            :MACRO)
                        file
                        :package pkg-name
                        :exported-p (eq (nth-value 1 
                                       (find-symbol (symbol-name name) pkg))
                                      :external)
                        :context context)))


(defun analyze-defgeneric (parser form)
  "Handle defgeneric form. Records both the generic function and any methods 
   specified in the options list. Analyzes:
   - Generic function name and exportedness
   - Method specifiers from :method options
   FORM is of form: (defgeneric name lambda-list &rest options)"
  (destructuring-bind (def-op name lambda-list &rest options) form
    (declare (ignore def-op lambda-list))
    (let* ((pkg (current-package parser))
           (pkg-name (current-package-name parser))
           (file (file parser))
           (context (limit-form-size form pkg-name)))
      ;; Record the generic function definition
      (record-definition *current-tracker* name
                        :GENERIC-FUNCTION
                        file
                        :package pkg-name
                        :exported-p (eq (nth-value 1 
                                       (find-symbol (symbol-name name) pkg))
                                      :external)
                        :context context)
      ;; Process any :method options to record method definitions
      (dolist (option options)
        (when (and (listp option) (eq (car option) :method))
          (destructuring-bind (method-key &rest method-spec) option
            (declare (ignore method-key))
            (multiple-value-bind (qualifiers lambda-list body)
                (parse-defmethod-args method-spec)
              (declare (ignore body))
              ;; Extract specializers from lambda list required params
              (let ((specializers (mapcar (lambda (param)
                                          (if (listp param)
                                              (string (cadr param))  ; (var type) form
                                              "T"))                  ; Just var
                                        (remove-if (lambda (x)
                                                   (member x lambda-list-keywords))
                                                 lambda-list))))
                ;; Record the method definition
                (record-definition *current-tracker* name
                                 :METHOD
                                 file
                                 :package pkg-name
                                 :exported-p (eq (nth-value 1
                                                (find-symbol (symbol-name name) pkg))
                                               :external)
                                 :context context
                                 :qualifiers qualifiers
                                 :specializers specializers)))))))))


(defun analyze-defmethod (parser form)
  "Handle defmethod form. Analyzes:
   - Method name, qualifiers, specializers and exportedness.
   Handles both ordinary methods and setf methods.
   Records actual specializer forms for exact method applicability testing."
  (let* ((after-defmethod (cdr form))
         ;; Handle (setf name) form properly 
         (name-and-qualifiers (if (and (consp (car after-defmethod))
                                     (eq (caar after-defmethod) 'setf))
                                (cons (car after-defmethod) (cdr after-defmethod))
                                after-defmethod))
         ;; Split into name, qualifiers, lambda-list, body
         (name (if (consp (car name-and-qualifiers))
                  (car name-and-qualifiers)  ; (setf name) form
                  (car name-and-qualifiers)))
         (rest (if (consp (car name-and-qualifiers))
                  (cdr name-and-qualifiers)  ; After (setf name)
                  (cdr name-and-qualifiers)))
         ;; Collect qualifiers until we hit the lambda-list
         (qualifiers nil)
         lambda-list
         (remaining rest))
    ;; Collect qualifiers until we hit a list that looks like a lambda list
    (loop while remaining do
          (let ((item (car remaining)))
            (if (and (listp item) 
                    (every (lambda (x) 
                            (or (symbolp x) 
                                (and (listp x) (= (length x) 2)))) ; var or (var type)
                          item))
                (progn 
                  (setf lambda-list item)
                  (return))
                (push item qualifiers))
            (setf remaining (cdr remaining))))
    (setf qualifiers (nreverse qualifiers))
    
    (when lambda-list  ; Only process if we found a valid lambda list
      (let* ((pkg (current-package parser))
             (pkg-name (current-package-name parser))
             (file (file parser))
             (context (limit-form-size form pkg-name))
             ;; Extract required parameters, excluding lambda list keywords
             (required-params (remove-if (lambda (x) 
                                        (member x lambda-list-keywords))
                                       lambda-list))
             ;; Extract specializer forms directly
             (specializers 
              (mapcar (lambda (param)
                       (if (listp param)
                           (second param)  ; Extract specializer form
                           t))            ; Unspecialized = T
                     required-params)))
        ;; Record method definition with actual specializer forms
        (record-definition *current-tracker* name
                          :METHOD
                          file
                          :package pkg-name
                          :exported-p (eq (nth-value 1 
                                         (find-symbol (string name) pkg))
                                        :external)
                          :context context
                          :qualifiers qualifiers
                          :specializers specializers)))))


(defun analyze-defsetf (parser form)
 "Handle defsetf forms in both short and long forms. Analyzes:
  - Access function name and exportedness
  - Short form: update function reference
  - Long form: lambda list, store vars, declarations, body
  Returns the setf definition record."
   (let* ((name (second form))
          (pkg (current-package parser))
          (pkg-name (current-package-name parser))
          (file (file parser))
          (context (limit-form-size form pkg-name)))
     (record-definition *current-tracker* name
                       :SETF
                       file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name) pkg))
                                     :external)
                       :context context)))


(defun analyze-defclass (parser form)
 "Handle defclass form. Analyzes:
  - Class name and exportedness
  - Superclasses and their references
  - Slot definitions with accessor/reader/writer methods
  - Class options including :metaclass and :default-initargs
  - Documentation strings"
   (let* ((name (second form))
          (slots (fourth form))
          (pkg (current-package parser))
          (pkg-name (current-package-name parser))
          (file (file parser))
          (context (limit-form-size form pkg-name)))
     ;; Record class definition
     (record-definition *current-tracker* name
                       :STRUCTURE/CLASS/CONDITION
                       file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name) pkg))
                                     :external)
                       :context context)
     ;; Record accessor/reader/writer methods
     (dolist (slot slots)
       (when (listp slot)  ; Skip atomic slot names
         (let ((slot-options (cdr slot)))  ; Simplified - don't need slot-name anymore
           (loop for (option value) on slot-options by #'cddr
                 do (case option
                      (:reader
                       (record-definition *current-tracker* value
                                        :METHOD 
                                        file
                                        :package pkg-name
                                        :exported-p (eq (nth-value 1 (find-symbol (string value) pkg)) :external)
                                        :context context
                                        :specializers (list (string name))))
                      (:writer
                       (record-definition *current-tracker* value
                                        :METHOD
                                        file
                                        :package pkg-name
                                        :exported-p (eq (nth-value 1 (find-symbol (string value) pkg)) :external)
                                        :context context
                                        :specializers (list "T" (string name))))
                      (:accessor
                       ;; Record reader method
                       (record-definition *current-tracker* value
                                        :METHOD
                                        file
                                        :package pkg-name
                                        :exported-p (eq (nth-value 1 (find-symbol (string value) pkg)) :external)
                                        :context context
                                        :specializers (list (string name)))
                       ;; Record (setf accessor) writer method
                       (let ((setf-name (list 'setf value)))
                         (record-definition *current-tracker* setf-name
                                          :METHOD
                                          file
                                          :package pkg-name
                                          :exported-p (eq (nth-value 1 (find-symbol (string value) pkg)) :external)
                                          :context context
                                          :specializers (list "T" (string name))))))))))))


(defun analyze-deftype (parser form)
 "Handle deftype form. Analyzes:
  - Type name and exportedness"
 (let* ((name (second form))
        (pkg (current-package parser))
        (pkg-name (current-package-name parser))
        (file (file parser))
        (context (limit-form-size form pkg-name)))
   (record-definition *current-tracker* name
                     :TYPE 
                     file
                     :package pkg-name
                     :exported-p (eq (nth-value 1 
                                    (find-symbol (symbol-name name) pkg))
                                   :external)
                     :context context)))


(defun analyze-define-condition (parser form)
  "Handle define-condition forms, recording condition definition and analyzing:
   - Condition name and its exportedness
   - Superclass relationships
   - Slot definitions including reader/writer/accessor functions
   - Special condition options like :report
   - Non-CL symbols in slot types and initforms"
  (destructuring-bind (def-op name superclasses &rest slots-and-options) form
    (declare (ignore def-op superclasses))
    
    (let* ((pkg (current-package parser))
           (pkg-name (current-package-name parser))
           (file (file parser))
           (context (limit-form-size form pkg-name)))
      (record-definition *current-tracker* name
                        :STRUCTURE/CLASS/CONDITION
                        file
                        :package pkg-name
                        :exported-p (eq (nth-value 1 
                                       (find-symbol (symbol-name name) pkg))
                                      :external)
                        :context context)
      ;; Process slot accessors
      (dolist (slot slots-and-options)
        (when (and (listp slot) (car slot))  ; Valid slot definition
          (loop for (option value) on (cdr slot) by #'cddr
                when (member option '(:reader :writer :accessor))
                do (let ((func-context (limit-form-size slot pkg-name)))
                     (record-definition *current-tracker* value
                                      :FUNCTION
                                      file
                                      :package pkg-name
                                      :exported-p (eq (nth-value 1 
                                                    (find-symbol (symbol-name value) pkg))
                                                   :external)
                                      :context func-context))))))))


(defun analyze-define-method-combination (parser form)
  "Handle define-method-combination forms. Analyzes:
   - Method combination name and exportedness"
  (let* ((name (second form))
         (pkg (current-package parser))
         (pkg-name (current-package-name parser))
         (file (file parser))
         (context (limit-form-size form pkg-name)))
    (record-definition *current-tracker* name
                      :FUNCTION
                      file
                      :package pkg-name
                      :exported-p (eq (nth-value 1 
                                    (find-symbol (symbol-name name) pkg))
                                   :external)
                      :context context)))


(defun analyze-define-modify-macro (parser form)
 "Handle define-modify-macro forms. Analyzes:
  - Macro name and exportedness
  - Lambda list parameters and their types
  - Underlying function reference
  - Documentation string if present"
   (let* ((name (second form))
          (pkg (current-package parser))
          (pkg-name (current-package-name parser))
          (file (file parser))
          (context (limit-form-size form pkg-name)))
     (record-definition *current-tracker* name
                       :MACRO
                       file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name) pkg))
                                     :external)
                       :context context)))


(defun analyze-define-compiler-macro (parser form)
 "Handle define-compiler-macro forms. Analyzes:
  - Compiler macro name and exportedness"
   (let* ((name (second form))
          (pkg (current-package parser))
          (pkg-name (current-package-name parser))
          (file (file parser))
          (context (limit-form-size form pkg-name)))
     (record-definition *current-tracker* name
                       :MACRO
                       file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name) pkg))
                                     :external)
                       :context context)))


(defun analyze-define-symbol-macro (parser form)
 "Handle define-symbol-macro form. Analyzes:
  - Symbol name and exportedness"
 (let* ((name (second form))
        (pkg (current-package parser))
        (pkg-name (current-package-name parser))
        (file (file parser))
        (context (limit-form-size form pkg-name)))
   (record-definition *current-tracker* name
                     :SYMBOL-MACRO
                     file
                     :package pkg-name
                     :exported-p (eq (nth-value 1 
                                    (find-symbol (symbol-name name) pkg))
                                   :external)
                     :context context)))


(defun analyze-setf-symbol-value (parser form)
 "Handle (setf (symbol-value 'name) value-form) forms. Analyzes:
  - Variable name and exportedness"
 (let* ((name (second (second (second form)))) ; Extract quoted symbol
        (pkg (current-package parser))
        (pkg-name (current-package-name parser))
        (file (file parser))
        (context (limit-form-size form pkg-name)))
   (record-definition *current-tracker* name
                     :VARIABLE
                     file
                     :package pkg-name
                     :exported-p (eq (nth-value 1 
                                    (find-symbol (symbol-name name) pkg))
                                   :external)
                     :context context)))


(defun analyze-setf-symbol-function-fdefinition-macro-function (parser form)
  "Handle (setf (symbol-function|fdefinition|macro-function 'name) function-form) definitions."
  (let* ((accessor (caadr form))           ; Get symbol-function, fdefinition, or macro-function
         (name (second (second (cadr form)))) ; Get the quoted symbol
         (pkg (current-package parser))
         (pkg-name (current-package-name parser))
         (file (file parser))
         (context (limit-form-size form pkg-name)))
    (record-definition *current-tracker* name
                      (if (eq accessor 'macro-function)
                          :MACRO
                          :FUNCTION)
                      file
                      :package pkg-name
                      :exported-p (eq (nth-value 1 
                                     (find-symbol (symbol-name name) pkg))
                                    :external)
                      :context context)))


(defun analyze-define-setf-expander (parser form)
  "Handle define-setf-expander forms. Analyzes:
   - Access function name and exportedness"
    (let* ((name (second form))
           (pkg (current-package parser))
           (pkg-name (current-package-name parser))
           (file (file parser))
           (context (limit-form-size form pkg-name)))
      (record-definition *current-tracker* name
                        :FUNCTION  ; Setf expanders are functions
                        file
                        :package pkg-name
                        :exported-p (eq (nth-value 1 
                                       (find-symbol (symbol-name name) pkg))
                                      :external)
                        :context context)))
