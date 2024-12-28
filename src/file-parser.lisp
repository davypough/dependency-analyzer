;;;; Filename: file-parser.lisp
;;;
;;; Core file parsing functionality for dependency analysis.
;;; Analyzes source files to track symbol definitions, references,
;;; and package relationships without expanding macros.
;;; Also provides ASDF component parsing coordination.


(in-package #:dep)


(defun parse-definitions-in-file (parser)
  "First pass parser that records definitions. Analyzes all forms recursively,
   maintaining package context during traversal. Logs analysis progress to provided log-stream."
  (declare (special log-stream))
  (with-slots (file parsing-files) parser
    ;; Reset to CL-USER before processing each file
    (setf (current-package parser) (find-package :common-lisp-user)
          (current-package-name parser) "COMMON-LISP-USER"
          *package* (find-package :common-lisp-user))
    (push file parsing-files)
    (with-open-file (stream file :direction :input)
      (loop for form = (read stream nil :eof)
            until (eq form :eof)
            do (when (and (consp form) (eq (first form) 'in-package))
                 (eval form)
                 (analyze-in-package parser form))
               (analyze-definition-form parser form)
               (terpri log-stream)))
    (pop parsing-files)))


(defun parse-references-in-file (parser)
  "Second pass parser that records references to previously recorded definitions.
   Processes in-package forms to maintain proper package context, 
   then analyzes all forms for references."
  (declare (special log-stream))
  (with-slots (file package) parser
    ;; Reset to CL-USER before processing each file
    (setf (current-package parser) (find-package :common-lisp-user)
          (current-package-name parser) "COMMON-LISP-USER"
          *package* (find-package :common-lisp-user))
    (with-open-file (stream file :direction :input)
      (loop for form = (read stream nil :eof)
            until (eq form :eof)
            do (when (and (consp form) (eq (first form) 'in-package))
                 (eval form)
                 (analyze-in-package parser form))
               (analyze-reference-form parser form)
               (terpri log-stream)))))


(defun analyze-definition-form (parser form)
  "Analyze form for definitions, recording location and basic info.
   Handles all standard definition forms, setf forms, and extended definitions."
  (declare (special log-stream))
  (when (and (consp form)
             (symbolp (first form))
             (not (quoted-form-p form)))
    (walk-form form
      (lambda (current-form context parent-context depth)
        (declare (ignore context parent-context depth))
        (when (and (consp current-form)
                   (symbolp (first current-form))
                   (not (quoted-form-p current-form)))
          (let ((head (first current-form)))
            (cond

              ;; Variable definitions 
              ((member head '(defvar defparameter defconstant))
               (let ((name (second current-form)))
                 (record-definition *current-tracker*
                                  :name name
                                  :type :variable 
                                  :file (file parser)
                                  :package (current-package parser)
                                  :status (symbol-status name (symbol-package name))
                                  :context (limit-form-size current-form name))))

              ;; Function/macro definitions
              ((member head '(defun defmacro))
               (record-definition *current-tracker*
                                :name (second current-form)
                                :type (if (eq head 'defun) :function :macro)
                                :file (file parser)
                                :package (current-package parser)
                                :status (symbol-status (second current-form) (symbol-package (second current-form)))
                                :context current-form))

              ;; Generic function
              ((eq head 'defgeneric)
               (record-definition *current-tracker*
                   :name (second current-form)
                   :type :generic-function
                   :file (file parser)
                   :package (current-package parser)
                   :status (symbol-status (second current-form) 
                                        (symbol-package (second current-form)))
                   :context current-form))

              ;; Method definitions
              ((eq head 'defmethod)
               (multiple-value-bind (method-name qualifiers lambda-list body)
                   (destructure-method-form current-form)
                 (declare (ignore method-name body))
                 (record-definition *current-tracker*
                     :name (second current-form)
                     :type :method
                     :file (file parser)
                     :package (current-package parser)
                     :status (symbol-status (second current-form) 
                                          (symbol-package (second current-form)))
                     :context current-form
                     :qualifiers qualifiers
                     :lambda-list lambda-list)))

              ;; Structure/class system
              ((member head '(defclass defstruct define-condition))
               (let ((name (if (and (eq head 'defstruct)    ; Handle defstruct options
                                  (consp (second current-form)))
                             (caadr current-form)               
                             (second current-form))))
                 (record-definition *current-tracker*
                                  :name name
                                  :type :structure/class/condition
                                  :file (file parser)
                                  :package (current-package parser)
                                  :status (symbol-status name (symbol-package name))
                                  :context current-form)
                 (analyze-defclass/defstruct/define-condition parser name current-form)))

              ;; Type system
              ((eq head 'deftype)
               (record-definition *current-tracker*
                                :name (second current-form)
                                :type :type
                                :file (file parser)
                                :package (current-package parser)
                                :status (symbol-status (second current-form) (symbol-package (second current-form)))
                                :context current-form))

              ;; Package system
              ((eq head 'defpackage)
               (record-definition *current-tracker*
                                :name (second current-form)  ;string designator
                                :type :package
                                :file (file parser)
                                :context current-form))

              ;; Setf forms
              ((and (eq head 'setf) 
                    (consp (second current-form))
                    (symbolp (caadr current-form))
                    (consp (cdadr current-form))
                    (consp (cadadr current-form))
                    (eq (first (cadadr current-form)) 'quote))
               (let ((accessor (caadr current-form))
                     (name (second (cadadr current-form))))
                 (case accessor
                   (symbol-value 
                    (record-definition *current-tracker*
                                   :name name
                                   :type :variable
                                   :file (file parser)
                                   :package (current-package parser)
                                   :status (symbol-status name (symbol-package name))
                                   :context current-form))
                   ((symbol-function fdefinition)
                    (record-definition *current-tracker*
                                   :name name
                                   :type :function
                                   :file (file parser)
                                   :package (current-package parser)
                                   :status (symbol-status name (symbol-package name))
                                   :context current-form))
                   (macro-function
                    (record-definition *current-tracker*
                                   :name name
                                   :type :macro
                                   :file (file parser)
                                   :package (current-package parser)
                                   :status (symbol-status name (symbol-package name))
                                   :context current-form)))))

              ;; Extended definition forms
              ((member head '(defsetf define-setf-expander))
               (record-definition *current-tracker*
                                :name (second current-form)
                                :type :setf
                                :file (file parser) 
                                :package (current-package parser)
                                :status (symbol-status (second current-form) (symbol-package (second current-form)))
                                :context current-form))

              ((eq head 'define-symbol-macro)
               (record-definition *current-tracker*
                                :name (second current-form)
                                :type :symbol-macro
                                :file (file parser)
                                :package (current-package parser)
                                :status (symbol-status (second current-form) (symbol-package (second current-form)))
                                :context current-form))

              ((member head '(define-modify-macro define-compiler-macro))
               (record-definition *current-tracker*
                                :name (second current-form)
                                :type :macro
                                :file (file parser)
                                :package (current-package parser)
                                :status (symbol-status (second current-form) (symbol-package (second current-form)))
                                :context current-form))

              ((eq head 'define-method-combination)
               (record-definition *current-tracker*
                                :name (second current-form)
                                :type :function
                                :file (file parser)
                                :package (current-package parser)
                                :status (symbol-status (second current-form) (symbol-package (second current-form)))
                                :context current-form))))))))
  form)


(defmethod analyze-reference-form ((parser file-parser) form)
  "Analyze form recording references to definitions in different files.
   Handles references to packages (via string/keyword/uninterned-symbol) and 
   references to other defined symbols. Special handling for method calls to track
   both generic function and applicable method definitions."
  (declare (special log-stream))
  (labels ((handle-reference (subform context parent-context depth)
             (declare (ignorable subform context parent-context depth))
             (typecase subform
               ((or string keyword (and symbol (satisfies not-interned-p)))
                 ;; Only process as package ref in package contexts
                 (when (package-context-p context parent-context)
                   (let* ((norm-name (normalize-designator subform))
                          (key (make-tracking-key norm-name norm-name :PACKAGE))  ;; Changed from nil to norm-name
                          (defs (gethash key (slot-value *current-tracker* 'definitions))))
                     ;; Collect all definitions from other files  
                     (let ((other-file-defs (remove-if (lambda (def)  
                                                         (equal (definition.file def) (file parser)))
                                            defs)))
                       (when other-file-defs
                         (record-reference *current-tracker*  ;record a package reference
                          :name norm-name
                          :file (file parser)
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
                        (analyze-function-call-context subform context) ;operator?
                      (if call-p
                          ;; Check if method first & process
                          (handle-method-call subform parser pkg-name context visibility name args)
                          ;; Not a method call - check all standard definition types
                          (try-definition-types subform pkg-name parser context visibility)))))))))
    ;; Walk the form applying handler
    (walk-form form #'handle-reference)))


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


#+ignore (defun analyze-defpackage (parser form)
  "Handle defpackage form using runtime package information.
   Records package definition, use relationships, and exports."
  (declare (special log-stream))
  (destructuring-bind (def-op name &rest options) form
    (declare (ignore def-op))
    (let* ((pkg-name (normalize-designator name))
           (file (file parser))
           (package (find-package pkg-name))
           (context (limit-form-size form pkg-name)))
      
      ;; Record primary package definition
      (record-definition *current-tracker*
                        :name pkg-name
                        :type :PACKAGE
                        :file file
                        :package pkg-name
                        :context context)
      
      (when package
        ;; Record actual package use relationships
        (dolist (used-pkg (package-use-list package))
          (record-package-use *current-tracker* 
                            pkg-name
                            (package-name used-pkg)))
        
        ;; Record exported symbols
        (do-external-symbols (sym package)
          (record-export *current-tracker* pkg-name sym))
        
        ;; Handle package options from form to track original source location
        (dolist (option options)
          (when (listp option)
            (case (first option)
              (:import-from
               (let ((from-pkg (normalize-designator (second option))))
                 (dolist (sym (cddr option))
                   (when (find-symbol (string sym) from-pkg)
                     (process-package-import-option package 
                                                  from-pkg
                                                  pkg-name
                                                  parser
                                                  sym)))))
              
              ;; Skip options we handle through runtime info
              ((:use :export :nicknames :documentation :size)
               nil)
              
              ;; Process other options for potential dependencies
              (t (unless (eq (first option) :export)
                   (dolist (item (cdr option))
                     (when (and (symbolp item) 
                              (not (keywordp item)))
                       (analyze-definition-form parser item))))))))
        
        ;; Check for package dependency cycles
        (detect-package-cycle pkg-name (parsing-packages parser))))))


(defun analyze-defstruct (parser form)
  "Handle defstruct form using runtime class information.
   PARSER - The file parser providing context
   FORM - The full definition form: (defstruct name-and-options &rest slots)"
  (declare (special log-stream))
  (let* ((pkg-name (current-package-name parser))
         (file (file parser))
         (name-and-options (second form))
         (struct-name (if (listp name-and-options)
                         (first name-and-options)
                         name-and-options))
         (context (limit-form-size form pkg-name))
         (struct-class (find-class struct-name nil)))
    
    ;; Record structure definition
    (record-definition *current-tracker*
                      :name struct-name
                      :type :STRUCTURE/CLASS/CONDITION
                      :file file
                      :package pkg-name
                      :context context)
    
    (when struct-class
      ;; Record constructor 
      (let ((make-name (intern (format nil "MAKE-~A" struct-name)
                              (symbol-package struct-name))))
        (when (fboundp make-name)
          (record-definition *current-tracker*
                            :name make-name
                            :type :FUNCTION
                            :file file
                            :package pkg-name
                            :context context)))
      
      ;; Record copier if it exists
      (let ((copy-name (intern (format nil "COPY-~A" struct-name)
                              (symbol-package struct-name))))
        (when (fboundp copy-name)
          (record-definition *current-tracker*
                            :name copy-name
                            :type :FUNCTION 
                            :file file
                            :package pkg-name
                            :context context)))
      
      ;; Record predicate if it exists
      (let ((pred-name (intern (format nil "~A-P" struct-name)
                              (symbol-package struct-name))))
        (when (fboundp pred-name)
          (record-definition *current-tracker*
                            :name pred-name
                            :type :FUNCTION
                            :file file
                            :package pkg-name
                            :context context)))
      
      ;; Extract slot names from the defstruct form
      (let ((slot-forms (cddr form)))
        (dolist (slot-form slot-forms)
          (let* ((slot-name (if (listp slot-form) 
                               (first slot-form)
                               slot-form))
                 (accessor-name (intern (format nil "~A-~A" struct-name slot-name)
                                      (symbol-package struct-name))))
            
            ;; Record reader method if it exists
            (when (fboundp accessor-name)
              (record-definition *current-tracker*
                               :name accessor-name
                               :type :METHOD
                               :file file
                               :package pkg-name
                               :context context
                               :lambda-list `((struct ,struct-name))))
            
            ;; Record writer method if it exists
            (let ((writer-name accessor-name))
              (when (fboundp `(setf ,writer-name))
                (record-definition *current-tracker*
                                 :name writer-name
                                 :type :METHOD
                                 :file file
                                 :package pkg-name
                                 :context context
                                 :lambda-list `(new-value (struct ,struct-name)))))))))))


(defun analyze-defvar-defparameter-defconstant (parser form)
  "Handle special variable and constant definitions using runtime verification.
   Distinguishes between defvar, defparameter, and defconstant forms.
   Verifies appropriate binding status and value characteristics."
  (declare (special log-stream))
  (when (and (listp form)
             (>= (length form) 2)     ; At least operator and name needed
             (member (first form) '(defvar defparameter defconstant)))
    (let* ((op (first form))
           (name (second form))
           (pkg-name (current-package-name parser))
           (file (file parser))
           (context (limit-form-size form pkg-name))
           (sym (find-symbol (string name)
                           (find-package pkg-name)))
           (has-initform (>= (length form) 3)))
      
      ;; Runtime verification by type
      (when (and sym
                 (case op
                   ;; For constants: must be bound, must be constant
                   (defconstant
                    (and has-initform          ; Must have initial value
                         (constantp sym)       ; Must be marked constant
                         (boundp sym)))        ; Must have a value
                   
                   ;; For special vars: must be bound, not constant
                   ((defvar defparameter)
                    (and (boundp sym)         ; Must have a value
                         (not (constantp sym)) ; Must not be constant
                         ;; Test special binding capability
                         (ignore-errors
                           (let ((test-value (gensym)))
                             (progv (list sym) (list test-value)
                               (eq (symbol-value sym) test-value))))))))
        
        (record-definition *current-tracker*
                          :name name
                          :type (case op
                                 (defconstant :CONSTANT)
                                 (otherwise :SPECIAL-VARIABLE))
                          :file file
                          :package pkg-name
                          :context context)))))


(defun analyze-defun-defmacro (parser form)
  "Handle defun and defmacro forms using runtime function information.
   Verifies function exists and examines its runtime properties."
  (declare (special log-stream))
  (let* ((def-op (first form))
         (name (second form))
         (pkg-name (current-package-name parser))
         (file (file parser))
         (context (limit-form-size form pkg-name))
         (sym (when (symbolp name)
               (find-symbol (symbol-name name)
                          (find-package pkg-name)))))

    (when sym
      ;; Check if function/macro actually exists
      (when (if (eq def-op 'defun)
                (fboundp sym)
                (macro-function sym))
        (record-definition *current-tracker*
                          :name name
                          :type (if (eq def-op 'defun)
                                  :FUNCTION
                                  :MACRO)
                          :file file
                          :package pkg-name
                          :context context
                          ;; Record lambda list if function exists
                          :lambda-list (when (fboundp sym)
                                       (let ((fn (symbol-function sym)))
                                         (when fn
                                           (second (function-lambda-expression fn))))))))))


(defun analyze-defgeneric (parser form)
  "Handle defgeneric form using runtime function information.
   Analyzes:
   - Generic function name and exportedness
   - Method specifiers from :method options
   FORM is of form: (defgeneric name lambda-list &rest options)"
  (destructuring-bind (def-op name lambda-list &rest options) form
    (declare (ignore def-op))
    (let* ((pkg-name (current-package-name parser))
           (file (file parser))
           (context (limit-form-size form pkg-name)))

      ;; Record generic function definition
      (record-definition *current-tracker*
                        :name name
                        :type :GENERIC-FUNCTION
                        :file file
                        :package pkg-name
                        :context context
                        :lambda-list lambda-list)

      ;; Analyze option declarations
      (dolist (option options)
        (when (listp option)
          (case (first option)
            (:method 
             ;; Handle method definition
             (destructuring-bind (method-key &rest method-spec) option
               (declare (ignore method-key))
               (multiple-value-bind (qualifiers lambda-list body)
                   (parse-defmethod-args method-spec)
                 (declare (ignore body))
                 ;; Record method definition
                 (record-definition *current-tracker*
                                  :name name
                                  :type :METHOD
                                  :file file
                                  :package pkg-name
                                  :context context
                                  :qualifiers qualifiers
                                  :lambda-list lambda-list))))
            (:argument-precedence-order
             ;; Track argument precedence for method combination
             (dolist (arg (cdr option))
               (when (symbolp arg)
                 (analyze-definition-form parser arg))))
            (:documentation
             ;; Skip documentation strings
             nil)
            (otherwise
             ;; Process any other option arguments 
             (dolist (item (cdr option))
               (when (and (symbolp item)
                         (not (keywordp item)))
                 (analyze-definition-form parser item))))))))))


(defun analyze-defmethod (parser form)
  "Handle defmethod form. Analyzes:
   - Method name, qualifiers, and lambda list
   Handles both ordinary methods and setf methods."
  (multiple-value-bind (name qualifiers lambda-list body)
      (destructure-method-form form)
    
    ;; Verify we have valid components
    (when (and name lambda-list)
      (let* ((pkg-name (current-package-name parser))
             (file (file parser))
             (context (limit-form-size form pkg-name))
             (gf-name (if (consp name)  ; Handle (setf x) names
                         (second name)
                         name)))

        ;; Record method definition
        (record-definition *current-tracker*
                          :name gf-name
                          :type :METHOD
                          :file file
                          :package pkg-name
                          :context context
                          :qualifiers qualifiers
                          :lambda-list lambda-list)

        ;; Analyze parameter specializers
        (analyze-method-specializers parser lambda-list)

        ;; Analyze method body forms
        (dolist (form body)
          (analyze-definition-form parser form))))))


(defun analyze-defsetf (parser form)
  "Handle defsetf forms using runtime environment verification.
   Distinguishes between short and long forms, verifying appropriate 
   runtime constructs exist for each. Confirms setf expansion capability."
  (declare (special log-stream))
  (when (and (listp form)
             (>= (length form) 3)              ; Minimum: defsetf, name, update/lambda
             (eq (first form) 'defsetf))
    (let* ((name (second form))
           (update-or-lambda (third form))
           (short-form-p (symbolp update-or-lambda))
           (pkg-name (current-package-name parser))
           (file (file parser))
           (context (limit-form-size form pkg-name))
           (name-sym (find-symbol (string name)
                                (find-package pkg-name))))
      
      ;; Verify runtime existence based on form type
      (when (and name-sym
                 (fboundp name-sym)     ; Access function must exist
                 (if short-form-p
                     ;; Short form verification
                     (let* ((update-name update-or-lambda)
                            (update-sym (find-symbol (string update-name)
                                                   (find-package pkg-name))))
                       (and update-sym
                            (fboundp update-sym)
                            ;; Test basic setf expansion works
                            (ignore-errors
                              (get-setf-expansion `(,name-sym 'dummy))
                              t)))
                     ;; Long form verification
                     (ignore-errors
                       ;; Test expansion with dummy values
                       (get-setf-expansion `(,name-sym 'dummy))
                       t)))
        
        ;; Record definition with appropriate details
        (record-definition *current-tracker*
                          :name name
                          :type :SETF
                          :file file
                          :package pkg-name
                          :context context
                          :lambda-list (unless short-form-p
                                       update-or-lambda))))))


(defun analyze-defclass (parser form)
  "Handle defclass form using runtime information where possible.
   Analyzes class definition and its accessor methods."
  (declare (special log-stream))
  (let* ((name (second form))
         (superclasses (third form))
         (slots (fourth form))
         (pkg-name (current-package-name parser))
         (file (file parser))
         (context (limit-form-size form pkg-name))
         (class (find-class name nil)))
    
    ;; Record class definition
    (record-definition *current-tracker*
                      :name name
                      :type :STRUCTURE/CLASS/CONDITION 
                      :file file
                      :package pkg-name
                      :context context)
    
    (when class
      ;; Record superclass relationships
      (dolist (super superclasses)
        (when (find-class super nil)
          (record-reference *current-tracker*
                          :name super
                          :file file
                          :package pkg-name
                          :context context
                          :visibility :LOCAL)))
      
      ;; Process slot definitions and their accessors
      (dolist (slot-form slots)
        (let ((slot-options (when (listp slot-form)
                             (cdr slot-form))))
          
          ;; Handle reader methods
          (let ((readers (loop for (key val) on slot-options by #'cddr
                              when (eq key :reader)
                              collect val)))
            (dolist (reader readers)
              (when (fboundp reader)
                (record-definition *current-tracker*
                                 :name reader
                                 :type :METHOD
                                 :file file
                                 :package pkg-name
                                 :context context
                                 :lambda-list `((object ,name))))))
          
          ;; Handle writer methods
          (let ((writers (loop for (key val) on slot-options by #'cddr
                              when (eq key :writer) 
                              collect val)))
            (dolist (writer writers)
              (when (fboundp writer)
                (record-definition *current-tracker*
                                 :name writer
                                 :type :METHOD
                                 :file file
                                 :package pkg-name
                                 :context context
                                 :lambda-list `(new-value (object ,name))))))
          
          ;; Handle accessor methods
          (let ((accessors (loop for (key val) on slot-options by #'cddr
                                when (eq key :accessor)
                                collect val)))
            (dolist (accessor accessors)
              (when (fboundp accessor)
                ;; Reader method
                (record-definition *current-tracker*
                                 :name accessor
                                 :type :METHOD
                                 :file file
                                 :package pkg-name
                                 :context context
                                 :lambda-list `((object ,name)))
                ;; Writer method
                (let ((writer accessor))
                  (when (fboundp `(setf ,writer))
                    (record-definition *current-tracker*
                                     :name writer
                                     :type :METHOD
                                     :file file
                                     :package pkg-name
                                     :context context
                                     :lambda-list `(new-value (object ,name)))))))))))))


(defun analyze-deftype (parser form)
  "Handle deftype form using runtime environment.
   Verifies type exists and is usable by testing expansion and subtype
   relationships. Validates type parameters and expansion capability."
  (declare (special log-stream))
  (when (and (listp form)
             (>= (length form) 3)              ; name and lambda-list required
             (eq (first form) 'deftype))
    (let* ((name (second form))
           (lambda-list (third form))          ; Extract lambda list
           (pkg-name (current-package-name parser))
           (file (file parser))
           (context (limit-form-size form pkg-name))
           (sym (find-symbol (string name) 
                           (find-package pkg-name))))
      
      ;; Multi-step runtime verification:
      (when (and sym
                 ;; 1. Test type expansion works
                 (ignore-errors
                   (let ((expanded (eval `(type-expand-1 '(,sym)))))
                     (and expanded t)))
                 ;; 2. Verify it creates valid type
                 (ignore-errors
                   (eval `(typep nil '(,sym)))
                   t)
                 ;; 3. Verify subtype relationship exists
                 (ignore-errors
                   (multiple-value-bind (result valid)
                       (subtypep `(,sym) t)
                     (and result valid))))
        
        (record-definition *current-tracker*
                          :name name
                          :type :TYPE
                          :file file
                          :package pkg-name
                          :context context
                          :lambda-list (when (listp lambda-list)
                                       lambda-list))))))


(defun analyze-define-condition (parser form)
  "Handle define-condition forms using runtime condition class information.
   Analyzes both the condition class itself and associated methods."
  (declare (special log-stream))
  (destructuring-bind (def-op name superclasses &rest slots-and-options) form
    (declare (ignore def-op))
    
    (let* ((pkg-name (current-package-name parser))
           (file (file parser))
           (context (limit-form-size form pkg-name))
           (condition (find-class name nil)))  ; Get runtime condition class
      
      ;; Record condition definition if it exists at runtime
      (when condition
        (record-definition *current-tracker*
                          :name name
                          :type :STRUCTURE/CLASS/CONDITION
                          :file file
                          :package pkg-name
                          :context context)

        ;; Process superclasses via runtime hierarchy
        (dolist (super superclasses)
          (let ((super-class (find-class super nil)))
            (when super-class  ; Only record existing superclasses
              (record-reference *current-tracker*
                              :name super
                              :file file
                              :package pkg-name
                              :context context
                              :visibility :LOCAL))))

        ;; Process slot accessors verifying runtime existence
        (dolist (slot slots-and-options)
          (when (and (listp slot) (first slot))  ; Valid slot definition
            (loop for (option value) on (cdr slot) by #'cddr
                  when (and (member option '(:reader :writer :accessor))
                           (fboundp value))     ; Only record existing functions
                  do (let ((func-context (limit-form-size slot pkg-name)))
                       (case option
                         (:reader 
                          (record-definition *current-tracker*
                                           :name value
                                           :type :METHOD
                                           :file file
                                           :package pkg-name
                                           :context func-context
                                           :lambda-list `((condition ,name))))
                         (:writer
                          (record-definition *current-tracker*
                                           :name value
                                           :type :METHOD
                                           :file file
                                           :package pkg-name
                                           :context func-context
                                           :lambda-list `(new-value (condition ,name))))
                         (:accessor
                          (record-definition *current-tracker*
                                           :name value
                                           :type :METHOD
                                           :file file
                                           :package pkg-name
                                           :context func-context
                                           :lambda-list `((condition ,name)))
                          ;; Handle setf accessor - verify it exists
                          (when (fboundp `(setf ,value))
                            (let ((setf-name value))
                              (record-definition *current-tracker*
                                               :name setf-name
                                               :type :METHOD
                                               :file file
                                               :package pkg-name
                                               :context func-context
                                               :lambda-list `(new-value (condition ,name)))))))))))))))


(defun analyze-define-method-combination (parser form)
  "Handle define-method-combination forms using runtime environment.
   Verifies the method combination exists and is usable.
   Supports both long and short form combinations."
  (declare (special log-stream))
  (when (and (listp form)
             (>= (length form) 2)
             (eq (first form) 'define-method-combination))
    (let* ((name (second form))
           (short-form-p (and (> (length form) 3)  ; Check for short form keys
                             (member :operator (cddr form))))
           (pkg-name (current-package-name parser))
           (file (file parser))
           (context (limit-form-size form pkg-name))
           (sym (find-symbol (string name) 
                           (find-package pkg-name))))
      
      ;; Runtime verification strategy:
      ;; 1. Verify symbol exists
      ;; 2. Try to find a generic function using this combination
      ;; 3. For short form, verify operator exists
      (when (and sym
                 ;; Test if combination works in a generic function
                 (ignore-errors
                   (eval `(defgeneric test-combination (x)
                           (:method-combination ,sym)
                           (:method ((x t)) x)))
                   t)
                 ;; For short form, verify operator
                 (or (not short-form-p)
                     (let ((op (second (member :operator (cddr form)))))
                       (and op (fboundp op)))))
          
        (record-definition *current-tracker*
                          :name name
                          :type :METHOD-COMBINATION  ; More specific type
                          :file file
                          :package pkg-name
                          :context context
                          :lambda-list (when (and (> (length form) 2)
                                               (listp (third form)))
                                       (third form)))))))


(defun analyze-define-modify-macro (parser form)
  "Handle define-modify-macro forms using runtime environment.
   Verifies macro exists and underlying update function is available.
   Validates basic form structure and expansion capability."
  (declare (special log-stream))
  (when (and (listp form)
             (>= (length form) 4)              ; name, lambda-list, fn required
             (<= (length form) 5)              ; optional doc string
             (eq (first form) 'define-modify-macro))
    (destructuring-bind (op name lambda-list function-name &optional doc)
        form
      (declare (ignore op doc))
      (when (and (symbolp name)               ; Verify name is symbol
                 (listp lambda-list)          ; Verify lambda list exists
                 (symbolp function-name))     ; Verify function name is symbol
        (let* ((pkg-name (current-package-name parser))
               (file (file parser))
               (context (limit-form-size form pkg-name))
               (name-sym (find-symbol (string name) 
                                    (find-package pkg-name)))
               (fn-sym (find-symbol (string function-name)
                                  (find-package pkg-name))))
          
          ;; Verify using runtime environment:
          ;; 1. Name exists and has macro expansion
          ;; 2. Update function exists
          ;; 3. Basic expansion works
          (when (and name-sym
                    fn-sym
                    (macro-function name-sym)  ; Has macro definition
                    (fboundp fn-sym)          ; Update function exists
                    ;; Try basic expansion
                    (ignore-errors
                      (macroexpand-1 `(,name-sym some-place 1))
                      t))
            
            ;; Record both the macro and its setf relationship
            (record-definition *current-tracker*
                             :name name
                             :type :MODIFY-MACRO     ; More specific type
                             :file file
                             :package pkg-name
                             :context context)))))))


(defun analyze-define-compiler-macro (parser form)
  "Handle define-compiler-macro forms using runtime environment.
   Verifies compiler macro exists and has associated function.
   Supports both direct function names and setf forms."
  (declare (special log-stream))
  (when (and (listp form)
             (>= (length form) 4)              ; name, lambda-list, body needed
             (eq (first form) 'define-compiler-macro))
    (let* ((name-form (second form))           ; Get raw name form
           (setf-form-p (and (listp name-form) ; Check if setf form
                            (eq (first name-form) 'setf)
                            (= (length name-form) 2)))
           (base-sym (if setf-form-p           ; Extract base symbol
                        (second name-form)
                        name-form))
           (pkg-name (current-package-name parser))
           (file (file parser))
           (context (limit-form-size form pkg-name))
           (sym (find-symbol (string base-sym) 
                           (find-package pkg-name))))
      
      ;; Runtime verification differs for setf vs regular forms
      (when (and sym
                 (if setf-form-p
                     ;; For setf forms, check both function and compiler macro
                     (and (fboundp `(setf ,sym))
                          (compiler-macro-function `(setf ,sym)))
                     ;; For regular forms, check both base function and compiler macro
                     (and (fboundp sym)
                          (compiler-macro-function sym))))
        
        (record-definition *current-tracker*
                          :name (if setf-form-p  ; Record full name form
                                  name-form
                                  base-sym)
                          :type :COMPILER-MACRO
                          :file file
                          :package pkg-name
                          :context context)))))


(defun analyze-define-symbol-macro (parser form)
  "Handle define-symbol-macro form using runtime environment.
   Verifies macro existence through macroexpand-1 and validates
   symbol is appropriate for symbol-macro definition."
  (declare (special log-stream))
  (when (and (listp form)                    ; Basic form validation
             (= (length form) 3)             ; define-symbol-macro name expansion
             (eq (first form) 'define-symbol-macro)
             (symbolp (second form)))        ; Ensure name is a symbol
    (let* ((name (second form))
           (pkg-name (current-package-name parser))
           (file (file parser))
           (context (limit-form-size form pkg-name))
           (sym (find-symbol (string name) (find-package pkg-name))))
      
      ;; Verify using runtime environment:
      ;; 1. Symbol exists
      ;; 2. Has symbol-macro expansion
      ;; 3. Not a constant/keyword
      ;; 4. Expansion is available
      (when (and sym
                 (not (constantp sym))
                 (not (keywordp sym))
                 (multiple-value-bind (expansion exists-p)
                     (ignore-errors 
                       (macroexpand-1 sym))
                   (declare (ignore expansion))
                   exists-p))
        
        (record-definition *current-tracker*
                          :name name
                          :type :SYMBOL-MACRO
                          :file file
                          :package pkg-name
                          :context context)))))


(defun analyze-setf-symbol-value (parser form)
  "Handle (setf (symbol-value 'name) value-form) forms by examining runtime env.
   Verifies symbol exists and has appropriate binding. Records only if:
   1. Symbol exists and is bound
   2. Not a constant
   3. Name suggests special/dynamic var (earmuffs convention)"
  (declare (special log-stream))
  (when (and (listp form)                    ; Verify basic form structure
             (= (length form) 3)             ; setf form should have 3 parts
             (listp (second form))           ; (symbol-value ...) part
             (eq (caadr form) 'symbol-value) ; Verify it's symbol-value
             (= (length (second form)) 2))   ; symbol-value should have 1 arg
    (let* ((quoted-name (second (second form)))
           (name (if (and (listp quoted-name)     ; Handle quoted symbol
                         (eq (first quoted-name) 'quote))
                    (second quoted-name)
                    quoted-name))
           (pkg-name (current-package-name parser))
           (file (file parser))
           (context (limit-form-size form pkg-name))
           (sym (find-symbol (string name) (find-package pkg-name)))
           (name-string (and sym (symbol-name sym))))
      
      ;; Only record if:
      ;; 1. Symbol exists 
      ;; 2. Is bound
      ;; 3. Not constant
      ;; 4. Follows special variable naming convention (earmuffs)
      ;; 5. Not a keyword
      (when (and sym
                 (boundp sym)
                 (not (constantp sym))
                 (not (keywordp sym))
                 name-string
                 (> (length name-string) 2)
                 (char= (char name-string 0) #\*)
                 (char= (char name-string (1- (length name-string))) #\*))
        
        (record-definition *current-tracker*
                          :name name
                          :type :VARIABLE
                          :file file
                          :package pkg-name
                          :context context)))))


(defun analyze-setf-symbol-function-fdefinition-macro-function (parser form)
  "Handle (setf (symbol-function|fdefinition|macro-function 'name) function-form) definitions.
   Uses runtime environment to verify function/macro existence."
  (declare (special log-stream))
  (when (and (listp form)               ; Validate form structure
             (= (length form) 3)        ; setf form should have 3 parts
             (listp (second form))
             (= (length (second form)) 2))
    (let* ((accessor (caadr form))
           (quoted-name (second (second form)))
           (name (if (and (listp quoted-name)    ; Handle quoted symbol
                         (eq (first quoted-name) 'quote))
                    (second quoted-name)
                    quoted-name))
           (pkg-name (current-package-name parser))
           (file (file parser))
           (context (limit-form-size form pkg-name))
           (sym (find-symbol (string name) (find-package pkg-name))))
      
      ;; Verify symbol exists and has appropriate binding
      (when (and sym 
                 (case accessor
                   (macro-function (macro-function sym))
                   ((symbol-function fdefinition) 
                    (and (fboundp sym)
                         (not (macro-function sym))))))
        
        (record-definition *current-tracker*
                          :name name
                          :type (if (macro-function sym) 
                                   :MACRO
                                   :FUNCTION)
                          :file file
                          :package pkg-name
                          :context context)))))


(defun analyze-define-setf-expander (parser form)
  "Handle define-setf-expander forms using runtime environment.
   Only records expander if it exists in the runtime environment."
  (declare (special log-stream))
  (let* ((name (second form))
         (pkg-name (current-package-name parser))
         (file (file parser))
         (context (limit-form-size form pkg-name))
         (sym (find-symbol (string name) (find-package pkg-name))))
    ;; Only record if symbol exists and has a setf expander
    (when (and sym
               (ignore-errors (get-setf-expansion `(,sym) nil)))
      (record-definition *current-tracker*
                        :name name
                        :type :FUNCTION  ; Setf expanders are functions
                        :file file
                        :package pkg-name
                        :context context))))
