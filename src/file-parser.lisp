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
                  (analyze-definition-form parser form))
                 
                 ;; Handle in-package - eval and update context  
                 ((and (consp form) (eq (car form) 'in-package))
                  (eval form)
                  (analyze-in-package parser form))
                 
                 ;; Handle other package forms - just eval
                 ((and (consp form)
                      (member (car form) '(use-package make-package rename-package delete-package)))
                  (eval form))
                 
                 ;; All other forms
                 (t (analyze-definition-form parser form)))
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
            do (when (and (consp form) (eq (car form) 'in-package))
                 (eval form)
                 (analyze-in-package parser form))
               (analyze-reference-form parser form)
               (terpri log-stream)))))


(defmethod analyze-definition-form ((parser file-parser) form)
  "Analyze form for definitions, recording location and basic info.
   Handles all standard definition forms, setf forms, and extended definitions."
  (declare (special log-stream))
  (when (and (consp form)
             (symbolp (car form))
             (not (quoted-form-p form)))
    (let ((op (car form)))
      ;; Record variable definition before walking initialization
      (when (member op '(defvar defparameter defconstant))
        (let ((name (cadr form)))
          (record-definition *current-tracker*
                           :name name
                           :type :variable 
                           :file (file parser)
                           :package (current-package-name parser)
                           :context (limit-form-size form name))))
      
      ;; Handle all possible definition types
      (walk-form form
        (lambda (subform context parent-context depth)
          (declare (ignore parent-context depth))
          (when (and (consp subform)
                    (symbolp (car subform))
                    (not (quoted-form-p subform)))
            (let ((sub-op (car subform)))
              (cond
                ;; Function/macro definitions
                ((member sub-op '(defun defmacro))
                 (record-definition *current-tracker*
                                  :name (cadr subform)
                                  :type (if (eq sub-op 'defun) :function :macro)
                                  :file (file parser)
                                  :package (current-package-name parser)
                                  :context context))

                ;; Generic function system
                ((member sub-op '(defgeneric defmethod))
                 (record-definition *current-tracker*
                                  :name (cadr subform)
                                  :type (if (eq sub-op 'defgeneric) :generic-function :method)
                                  :file (file parser)
                                  :package (current-package-name parser)
                                  :context context))

                ;; Structure/class system
                ((member sub-op '(defclass defstruct define-condition))
                 (let ((name (if (and (eq sub-op 'defstruct)    ; Handle defstruct options
                                    (consp (cadr subform)))
                               (caadr subform)               
                               (cadr subform))))             
                   (record-definition *current-tracker*
                                    :name name
                                    :type :structure/class/condition
                                    :file (file parser)
                                    :package (current-package-name parser)
                                    :context context)))

                ;; Type system
                ((eq sub-op 'deftype)
                 (record-definition *current-tracker*
                                  :name (cadr subform)
                                  :type :type
                                  :file (file parser)
                                  :package (current-package-name parser)
                                  :context context))

                ;; Package system
                ((eq sub-op 'defpackage)
                 (record-definition *current-tracker*
                                  :name (cadr subform)
                                  :type :package
                                  :file (file parser)
                                  :package (current-package-name parser)
                                  :context context))

                ;; Setf forms
                ((and (eq sub-op 'setf) 
                     (consp (cadr subform))
                     (symbolp (caadr subform))
                     (consp (cdadr subform))
                     (consp (cadadr subform))
                     (eq (car (cadadr subform)) 'quote))
                 (let ((accessor (caadr subform))
                       (name (cadr (cadadr subform))))
                   (case accessor
                     (symbol-value 
                      (record-definition *current-tracker*
                                       :name name
                                       :type :variable
                                       :file (file parser)
                                       :package (current-package-name parser)
                                       :context context))
                     ((symbol-function fdefinition)
                      (record-definition *current-tracker*
                                       :name name
                                       :type :function
                                       :file (file parser)
                                       :package (current-package-name parser)
                                       :context context))
                     (macro-function
                      (record-definition *current-tracker*
                                       :name name
                                       :type :macro
                                       :file (file parser)
                                       :package (current-package-name parser)
                                       :context context)))))

                ;; Extended definition forms
                ((member sub-op '(defsetf define-setf-expander))
                 (record-definition *current-tracker*
                                  :name (cadr subform)
                                  :type :setf
                                  :file (file parser) 
                                  :package (current-package-name parser)
                                  :context context))

                ((eq sub-op 'define-symbol-macro)
                 (record-definition *current-tracker*
                                  :name (cadr subform)
                                  :type :symbol-macro
                                  :file (file parser)
                                  :package (current-package-name parser)
                                  :context context))

                ((member sub-op '(define-modify-macro define-compiler-macro))
                 (record-definition *current-tracker*
                                  :name (cadr subform)
                                  :type :macro
                                  :file (file parser)
                                  :package (current-package-name parser)
                                  :context context))

                ((eq sub-op 'define-method-combination)
                 (record-definition *current-tracker*
                                  :name (cadr subform)
                                  :type :function
                                  :file (file parser)
                                  :package (current-package-name parser)
                                  :context context))))))))
  form))


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


(defun analyze-defpackage (parser form)
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
                        :exported-p t
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
            (case (car option)
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
              (t (unless (eq (car option) :export)
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
                         (car name-and-options)
                         name-and-options))
         (context (limit-form-size form pkg-name))
         (struct-class (find-class struct-name nil)))
    
    ;; Record structure definition
    (record-definition *current-tracker*
                      :name struct-name
                      :type :STRUCTURE/CLASS/CONDITION
                      :file file
                      :package pkg-name
                      :exported-p (eq (nth-value 1 
                                     (find-symbol (symbol-name struct-name) 
                                                (current-package parser)))
                                    :external)
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
                            :exported-p (eq (nth-value 1
                                           (find-symbol (symbol-name make-name)
                                                      (current-package parser)))
                                          :external)
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
                            :exported-p (eq (nth-value 1
                                           (find-symbol (symbol-name copy-name)
                                                      (current-package parser)))
                                          :external)
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
                            :exported-p (eq (nth-value 1
                                           (find-symbol (symbol-name pred-name)
                                                      (current-package parser)))
                                          :external)
                            :context context)))
      
      ;; Extract slot names from the defstruct form
      (let ((slot-forms (cddr form)))
        (dolist (slot-form slot-forms)
          (let* ((slot-name (if (listp slot-form) 
                               (car slot-form)
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
                               :exported-p (eq (nth-value 1
                                              (find-symbol (symbol-name accessor-name)
                                                         (current-package parser)))
                                             :external)
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
                                 :exported-p (eq (nth-value 1
                                                (find-symbol (symbol-name writer-name)
                                                           (current-package parser)))
                                               :external)
                                 :context context
                                 :lambda-list `(new-value (struct ,struct-name)))))))))))


(defun analyze-defvar-defparameter-defconstant (parser form)
  "Handle defvar, defparameter, and defconstant forms. Records the primary variable 
   definition, letting walk-form handle any nested definitions in initforms or declarations.
   PARSER - The file parser providing context
   FORM - The full definition form: (defvar|defparameter|defconstant name [initform [decls...]])"
  (declare (special log-stream))
  (let* ((pkg-name (current-package-name parser))
         (file (file parser))
         (name (second form))
         (context (limit-form-size form pkg-name)))
    ;; MODIFIED: Updated to use keyword args
    (record-definition *current-tracker*
                      :name name
                      :type :VARIABLE
                      :file file
                      :package pkg-name
                      :exported-p (eq (nth-value 1 (find-symbol (symbol-name name) (current-package parser)))
                                    :external)
                      :context context)))


(defun analyze-defun-defmacro (parser form)
  "Handle defun and defmacro forms. Analyzes:
   - Function/macro name and exportedness"
  (declare (special log-stream))
  (let* ((def-op (first form))
         (name (second form))
         (pkg-name (current-package-name parser))
         (file (file parser))
         (context (limit-form-size form pkg-name)))
    ;; MODIFIED: Updated to use keyword args
    (record-definition *current-tracker*
                      :name name
                      :type (if (eq def-op 'defun) 
                              :FUNCTION 
                              :MACRO)
                      :file file
                      :package pkg-name
                      :exported-p (eq (nth-value 1 
                                     (find-symbol (symbol-name name) (current-package parser)))
                                    :external)
                      :context context)))


(defun analyze-defgeneric (parser form)
  "Handle defgeneric form. Records both the generic function and any methods 
   specified in the options list. Analyzes:
   - Generic function name and exportedness
   - Method specifiers from :method options
   FORM is of form: (defgeneric name lambda-list &rest options)"
  (declare (special log-stream))
  (destructuring-bind (def-op name lambda-list &rest options) form
    (declare (ignore def-op lambda-list))
    (let* ((pkg-name (current-package-name parser))
           (file (file parser))
           (context (limit-form-size form pkg-name)))
      ;; Record generic function
      (record-definition *current-tracker*
                        :name name
                        :type :GENERIC-FUNCTION
                        :file file
                        :package pkg-name
                        :exported-p (eq (nth-value 1 
                                       (find-symbol (string name) (current-package parser)))
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
              (record-definition *current-tracker*
                               :name name
                               :type :METHOD
                               :file file
                               :package pkg-name
                               :exported-p (eq (nth-value 1
                                              (find-symbol (string name) (current-package parser)))
                                             :external)
                               :context context
                               :qualifiers qualifiers
                               :lambda-list lambda-list))))))))


(defun analyze-defmethod (parser form)
  "Handle defmethod form. Analyzes:
   - Method name, qualifiers, and lambda list.
   Handles both ordinary methods and setf methods."
  (declare (special log-stream))
  (let* ((after-defmethod (cdr form))
         ;; Handle (setf name) form properly 
         (name-and-qualifiers (if (and (consp (car after-defmethod))
                                     (eq (caar after-defmethod) 'setf))
                                (cons (car after-defmethod) (cdr after-defmethod))
                                after-defmethod))
         ;; Split into name, qualifiers, lambda-list, body
         (name (if (consp (car name-and-qualifiers))
                  (cadr (car name-and-qualifiers))  ; (setf name) form
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
      (let* ((pkg-name (current-package-name parser))
             (file (file parser))
             (context (limit-form-size form pkg-name)))
        (record-definition *current-tracker*
                          :name name
                          :type :METHOD
                          :file file
                          :package pkg-name
                          :exported-p (eq (nth-value 1 
                                         (find-symbol (string name) (current-package parser)))
                                        :external)
                          :context context
                          :qualifiers qualifiers
                          :lambda-list lambda-list)))))


(defun analyze-defsetf (parser form)
 "Handle defsetf forms in both short and long forms. Analyzes:
  - Access function name and exportedness
  - Short form: update function reference
  - Long form: lambda list, store vars, declarations, body
  Returns the setf definition record."
   (declare (special log-stream))
   (let* ((name (second form))
          (pkg-name (current-package-name parser))
          (file (file parser))
          (context (limit-form-size form pkg-name)))
     ;; MODIFIED: Updated to use keyword args
     (record-definition *current-tracker*
                       :name name
                       :type :SETF
                       :file file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name) (current-package parser)))
                                     :external)
                       :context context)))


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
                      :exported-p (eq (nth-value 1
                                     (find-symbol (symbol-name name)
                                                (current-package parser)))
                                    :external)
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
        (let  ((slot-options (when (listp slot-form)
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
                                 :exported-p (eq (nth-value 1 
                                                (find-symbol (symbol-name reader)
                                                           (current-package parser)))
                                               :external)
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
                                 :exported-p (eq (nth-value 1
                                                (find-symbol (symbol-name writer)
                                                           (current-package parser)))
                                               :external)
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
                                 :exported-p (eq (nth-value 1
                                                (find-symbol (symbol-name accessor)
                                                           (current-package parser)))
                                               :external)
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
                                     :exported-p (eq (nth-value 1
                                                    (find-symbol (symbol-name writer)
                                                               (current-package parser)))
                                                   :external)
                                     :context context
                                     :lambda-list `(new-value (object ,name)))))))))))))


(defun analyze-deftype (parser form)
 "Handle deftype form. Analyzes:
  - Type name and exportedness"
  (declare (special log-stream))
 (let* ((name (second form))
        (pkg-name (current-package-name parser))
        (file (file parser))
        (context (limit-form-size form pkg-name)))
   ;; MODIFIED: Updated to use keyword args
   (record-definition *current-tracker*
                     :name name
                     :type :TYPE 
                     :file file
                     :package pkg-name
                     :exported-p (eq (nth-value 1 
                                    (find-symbol (symbol-name name) (current-package parser)))
                                   :external)
                     :context context)))


(defun analyze-define-condition (parser form)
  "Handle define-condition forms, recording condition definition and analyzing:
   - Condition name and its exportedness
   - Superclass relationships
   - Slot definitions including reader/writer/accessor functions
   - Special condition options like :report
   - Non-CL symbols in slot types and initforms"
  (declare (special log-stream))
  (destructuring-bind (def-op name superclasses &rest slots-and-options) form
    (declare (ignore def-op superclasses))
    
    (let* ((pkg-name (current-package-name parser))
           (file (file parser))
           (context (limit-form-size form pkg-name)))
      ;; Record condition definition
      (record-definition *current-tracker*
                        :name name
                        :type :STRUCTURE/CLASS/CONDITION
                        :file file
                        :package pkg-name
                        :exported-p (eq (nth-value 1 
                                       (find-symbol (symbol-name name) (current-package parser)))
                                      :external)
                        :context context)
      ;; Process slot accessors
      (dolist (slot slots-and-options)
        (when (and (listp slot) (car slot))  ; Valid slot definition
          (loop for (option value) on (cdr slot) by #'cddr
                when (member option '(:reader :writer :accessor))
                do (let ((func-context (limit-form-size slot pkg-name)))
                     (case option
                       (:reader 
                        (record-definition *current-tracker*
                                         :name value
                                         :type :METHOD
                                         :file file
                                         :package pkg-name
                                         :exported-p (eq (nth-value 1 (find-symbol (string value) (current-package parser))) :external)
                                         :context func-context
                                         :qualifiers nil
                                         :lambda-list `((condition ,name))))
                       (:writer
                        (record-definition *current-tracker*
                                         :name value
                                         :type :METHOD
                                         :file file
                                         :package pkg-name
                                         :exported-p (eq (nth-value 1 (find-symbol (string value) (current-package parser))) :external)
                                         :context func-context
                                         :qualifiers nil
                                         :lambda-list `(new-value (condition ,name))))
                       (:accessor
                        (record-definition *current-tracker*
                                         :name value
                                         :type :METHOD
                                         :file file
                                         :package pkg-name
                                         :exported-p (eq (nth-value 1 (find-symbol (string value) (current-package parser))) :external)
                                         :context func-context
                                         :qualifiers nil
                                         :lambda-list `((condition ,name)))
                        (let ((setf-name value))
                          (record-definition *current-tracker*
                                           :name setf-name
                                           :type :METHOD
                                           :file file
                                           :package pkg-name
                                           :exported-p (eq (nth-value 1 (find-symbol (string value) (current-package parser))) :external)
                                           :context func-context
                                           :qualifiers nil
                                           :lambda-list `(new-value (condition ,name)))))))))))))


(defun analyze-define-method-combination (parser form)
  "Handle define-method-combination forms. Analyzes:
   - Method combination name and exportedness"
  (declare (special log-stream))
  (let* ((name (second form))
         (pkg-name (current-package-name parser))
         (file (file parser))
         (context (limit-form-size form pkg-name)))
    ;; MODIFIED: Updated to use keyword args
    (record-definition *current-tracker*
                      :name name
                      :type :FUNCTION
                      :file file
                      :package pkg-name
                      :exported-p (eq (nth-value 1 
                                    (find-symbol (string name) (current-package parser)))
                                   :external)
                      :context context)))


(defun analyze-define-modify-macro (parser form)
 "Handle define-modify-macro forms. Analyzes:
  - Macro name and exportedness
  - Lambda list parameters and their types
  - Underlying function reference
  - Documentation string if present"
  (declare (special log-stream))
  (let* ((name (second form))
          (pkg-name (current-package-name parser))
          (file (file parser))
          (context (limit-form-size form pkg-name)))
     ;; MODIFIED: Updated to use keyword args
     (record-definition *current-tracker*
                       :name name
                       :type :MACRO
                       :file file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (string name) (current-package parser)))
                                     :external)
                       :context context)))


(defun analyze-define-compiler-macro (parser form)
 "Handle define-compiler-macro forms. Analyzes:
  - Compiler macro name and exportedness"
  (declare (special log-stream))
   (let* ((name (second form))
          (pkg-name (current-package-name parser))
          (file (file parser))
          (context (limit-form-size form pkg-name)))
     ;; MODIFIED: Updated to use keyword args
     (record-definition *current-tracker*
                       :name name
                       :type :MACRO
                       :file file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (string name) (current-package parser)))
                                     :external)
                       :context context)))


(defun analyze-define-symbol-macro (parser form)
 "Handle define-symbol-macro form. Analyzes:
  - Symbol name and exportedness"
  (declare (special log-stream))
 (let* ((name (second form))
        (pkg-name (current-package-name parser))
        (file (file parser))
        (context (limit-form-size form pkg-name)))
   ;; MODIFIED: Fixed to use keyword args consistently
   (record-definition *current-tracker*
                     :name name
                     :type :SYMBOL-MACRO
                     :file file
                     :package pkg-name
                     :exported-p (eq (nth-value 1 
                                    (find-symbol (symbol-name name) (current-package parser)))
                                   :external)
                     :context context)))


(defun analyze-setf-symbol-value (parser form)
 "Handle (setf (symbol-value 'name) value-form) forms. Analyzes:
  - Variable name and exportedness"
  (declare (special log-stream))
 (let* ((name (second (second (second form)))) ; Extract quoted symbol
        (pkg-name (current-package-name parser))
        (file (file parser))
        (context (limit-form-size form pkg-name)))
   ;; MODIFIED: Fixed to use keyword args consistently
   (record-definition *current-tracker*
                     :name name
                     :type :VARIABLE
                     :file file
                     :package pkg-name
                     :exported-p (eq (nth-value 1 
                                    (find-symbol (symbol-name name) (current-package parser)))
                                   :external)
                     :context context)))


(defun analyze-setf-symbol-function-fdefinition-macro-function (parser form)
  "Handle (setf (symbol-function|fdefinition|macro-function 'name) function-form) definitions."
  (declare (special log-stream))
  (let* ((accessor (caadr form))           ; Get symbol-function, fdefinition, or macro-function
         (name (second (second (cadr form)))) ; Get the quoted symbol
         (pkg-name (current-package-name parser))
         (file (file parser))
         (context (limit-form-size form pkg-name)))
    ;; MODIFIED: Fixed to use keyword args consistently
    (record-definition *current-tracker*
                      :name name
                      :type (if (eq accessor 'macro-function)
                               :MACRO
                               :FUNCTION)
                      :file file
                      :package pkg-name
                      :exported-p (eq (nth-value 1 
                                     (find-symbol (symbol-name name) (current-package parser)))
                                    :external)
                      :context context)))


(defun analyze-define-setf-expander (parser form)
  "Handle define-setf-expander forms. Analyzes:
   - Access function name and exportedness"
  (declare (special log-stream))
    (let* ((name (second form))
           (pkg-name (current-package-name parser))
           (file (file parser))
           (context (limit-form-size form pkg-name)))
      ;; MODIFIED: Fixed to use keyword args consistently
      (record-definition *current-tracker*
                        :name name
                        :type :FUNCTION  ; Setf expanders are functions
                        :file file
                        :package pkg-name
                        :exported-p (eq (nth-value 1 
                                       (find-symbol (string name) (current-package parser)))
                                      :external)
                        :context context)))
