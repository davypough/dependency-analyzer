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
  (labels ((definition-handler (subform context parent-context depth)
             (declare (ignore parent-context depth))
             (when (and (consp subform)
                       (symbolp (car subform))
                       (not (quoted-form-p subform)))
               (let ((op (car subform)))
                 (cond
                   ;; Standard variable definitions
                   ((member op '(defvar defparameter defconstant))
                    (record-definition *current-tracker*
                                     :name (cadr subform)
                                     :type :variable 
                                     :file (file parser)
                                     :package (current-package-name parser)
                                     :context context))
                   
                   ;; Function/macro definitions
                   ((member op '(defun defmacro))
                    (record-definition *current-tracker*
                                     :name (cadr subform)
                                     :type (if (eq op 'defun) :function :macro)
                                     :file (file parser)
                                     :package (current-package-name parser)
                                     :context context))

                   ;; Generic function system
                   ((member op '(defgeneric defmethod))
                    (record-definition *current-tracker*
                                     :name (cadr subform)
                                     :type (if (eq op 'defgeneric) :generic-function :method)
                                     :file (file parser)
                                     :package (current-package-name parser)
                                     :context context))

                   ;; Structure/class system
                   ((member op '(defclass defstruct define-condition))
                    (let ((name (if (and (eq op 'defstruct)    ; Only handle defstruct specially
                                       (consp (cadr subform)))  ; Is it (name options...)?
                                  (caadr subform)               ; Yes: extract name from list
                                  (cadr subform))))             ; No: use directly
                      (record-definition *current-tracker*
                                       :name name
                                       :type :structure/class/condition
                                       :file (file parser)
                                       :package (current-package-name parser)
                                       :context context)))

                   ;; Type system
                   ((eq op 'deftype)
                    (record-definition *current-tracker*
                                     :name (cadr subform)
                                     :type :type
                                     :file (file parser)
                                     :package (current-package-name parser)
                                     :context context))

                   ;; Package system
                   ((eq op 'defpackage)
                    (record-definition *current-tracker*
                                     :name (cadr subform)
                                     :type :package
                                     :file (file parser)
                                     :package (current-package-name parser)
                                     :context context))

                   ;; Setf forms
                   ((and (eq op 'setf) 
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
                   ((member op '(defsetf define-setf-expander))
                    (record-definition *current-tracker*
                                     :name (cadr subform)
                                     :type :setf
                                     :file (file parser) 
                                     :package (current-package-name parser)
                                     :context context))

                   ((eq op 'define-symbol-macro)
                    (record-definition *current-tracker*
                                     :name (cadr subform)
                                     :type :symbol-macro
                                     :file (file parser)
                                     :package (current-package-name parser)
                                     :context context))

                   ((member op '(define-modify-macro define-compiler-macro))
                    (record-definition *current-tracker*
                                     :name (cadr subform)
                                     :type :macro
                                     :file (file parser)
                                     :package (current-package-name parser)
                                     :context context))

                   ((eq op 'define-method-combination)
                    (record-definition *current-tracker*
                                     :name (cadr subform)
                                     :type :function
                                     :file (file parser)
                                     :package (current-package-name parser)
                                     :context context)))))))
    (walk-form form #'definition-handler)))


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
                        (analyze-function-call-context subform context parent-context)
                      (if call-p
                          ;; Function call - handle method call
                          (handle-method-call subform parser pkg-name context visibility name args)
                          ;; Not a function call - check all definition types
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
  "Handle defpackage form, recording package definition and analyzing:
   - Package name and its dependencies
   - Package inheritance (:use)
   - Symbol importing (:import-from)
   - Symbol exporting (:export)
   - All other package options for potential dependencies"
  (declare (special log-stream))
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
      ;; MODIFIED: Updated to use keyword args
      (record-definition *current-tracker*
                        :name pkg-name
                        :type :PACKAGE
                        :file file
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
  "Handle defstruct form. Records structure definition and its accessor methods,
   letting walk-form handle nested definitions and references.
   PARSER - The file parser providing context
   FORM - The full definition form: (defstruct name-and-options &rest slots)"
  (declare (special log-stream))
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
    (record-definition *current-tracker*
                      :name struct-name
                      :type :STRUCTURE/CLASS/CONDITION
                      :file file
                      :package pkg-name
                      :exported-p (eq (nth-value 1 
                                     (find-symbol (symbol-name struct-name) pkg))
                                    :external)
                      :context context)
    
    ;; Record accessor method definitions 
    (dolist (slot (cddr form))  ; Skip defstruct and name-and-options
      (let* ((slot-name (if (listp slot) (car slot) slot))
             (accessor (intern (concatenate 'string conc-name-prefix
                                         (string slot-name))
                             pkg))
             (accessor-context (limit-form-size form pkg-name)))
        ;; Record reader method
        (record-definition *current-tracker*
                          :name accessor
                          :type :METHOD
                          :file file
                          :package pkg-name
                          :exported-p (eq (nth-value 1 
                                         (find-symbol (string accessor) pkg))
                                        :external)
                          :context accessor-context
                          :qualifiers nil
                          :lambda-list `((struct ,struct-name)))
        ;; Record (setf accessor) writer method
        (let ((setf-name accessor))
          (record-definition *current-tracker*
                            :name setf-name
                            :type :METHOD
                            :file file
                            :package pkg-name
                            :exported-p (eq (nth-value 1 
                                           (find-symbol (string accessor) pkg))
                                          :external)
                            :context accessor-context
                            :qualifiers nil
                            :lambda-list `(new-value (struct ,struct-name))))))))


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
                      :exported-p (eq (nth-value 1 (find-symbol (symbol-name name) pkg))
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
                                     (find-symbol (symbol-name name) pkg))
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
                                       (find-symbol (string name) pkg))
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
                                              (find-symbol (string name) pkg))
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
                                         (find-symbol (string name) pkg))
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
  (declare (special log-stream))
   (let* ((name (second form))
          (slots (fourth form))
          (pkg-name (current-package-name parser))
          (file (file parser))
          (context (limit-form-size form pkg-name)))
     ;; Record class definition
     (record-definition *current-tracker*
                       :name name
                       :type :STRUCTURE/CLASS/CONDITION
                       :file file
                       :package pkg-name
                       :exported-p (eq (nth-value 1 
                                      (find-symbol (string name) pkg))
                                     :external)
                       :context context)
     ;; Record accessor/reader/writer methods
     (dolist (slot slots)
       (when (listp slot)  ; Skip atomic slot names
         (let ((slot-options (cdr slot)))  
           (loop for (option value) on slot-options by #'cddr
                 do (case option
                      (:reader
                       (record-definition *current-tracker*
                                        :name value
                                        :type :METHOD 
                                        :file file
                                        :package pkg-name
                                        :exported-p (eq (nth-value 1 (find-symbol (string value) pkg)) :external)
                                        :context context
                                        :qualifiers nil
                                        :lambda-list `((object ,name))))
                      (:writer
                       (record-definition *current-tracker*
                                        :name value
                                        :type :METHOD
                                        :file file
                                        :package pkg-name
                                        :exported-p (eq (nth-value 1 (find-symbol (string value) pkg)) :external)
                                        :context context
                                        :qualifiers nil
                                        :lambda-list `(new-value (object ,name))))
                      (:accessor
                       ;; Record reader method
                       (record-definition *current-tracker*
                                        :name value
                                        :type :METHOD
                                        :file file
                                        :package pkg-name
                                        :exported-p (eq (nth-value 1 (find-symbol (string value) pkg)) :external)
                                        :context context
                                        :qualifiers nil
                                        :lambda-list `((object ,name)))
                       ;; Record (setf accessor) writer method
                       (let ((setf-name value))
                         (record-definition *current-tracker*
                                          :name setf-name
                                          :type :METHOD
                                          :file file
                                          :package pkg-name
                                          :exported-p (eq (nth-value 1 (find-symbol (string value) pkg)) :external)
                                          :context context
                                          :qualifiers nil
                                          :lambda-list `(new-value (object ,name))))))))))))


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
                                       (find-symbol (symbol-name name) pkg))
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
                                         :exported-p (eq (nth-value 1 (find-symbol (string value) pkg)) :external)
                                         :context func-context
                                         :qualifiers nil
                                         :lambda-list `((condition ,name))))
                       (:writer
                        (record-definition *current-tracker*
                                         :name value
                                         :type :METHOD
                                         :file file
                                         :package pkg-name
                                         :exported-p (eq (nth-value 1 (find-symbol (string value) pkg)) :external)
                                         :context func-context
                                         :qualifiers nil
                                         :lambda-list `(new-value (condition ,name))))
                       (:accessor
                        (record-definition *current-tracker*
                                         :name value
                                         :type :METHOD
                                         :file file
                                         :package pkg-name
                                         :exported-p (eq (nth-value 1 (find-symbol (string value) pkg)) :external)
                                         :context func-context
                                         :qualifiers nil
                                         :lambda-list `((condition ,name)))
                        (let ((setf-name value))
                          (record-definition *current-tracker*
                                           :name setf-name
                                           :type :METHOD
                                           :file file
                                           :package pkg-name
                                           :exported-p (eq (nth-value 1 (find-symbol (string value) pkg)) :external)
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
                                    (find-symbol (string name) pkg))
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
                                      (find-symbol (string name) pkg))
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
                                      (find-symbol (string name) pkg))
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
                                    (find-symbol (symbol-name name) pkg))
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
                                    (find-symbol (symbol-name name) pkg))
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
                                     (find-symbol (symbol-name name) pkg))
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
                                       (find-symbol (string name) pkg))
                                      :external)
                        :context context)))
