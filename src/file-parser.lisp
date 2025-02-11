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


(defun parse-package-symbols-in-file (parser)
  "Third pass parser that validates package/symbol consistency.
   Updates package context during iteration and hands forms to analyzer."
  (declare (special log-stream))
  (with-slots (file package) parser
    ;; Reset to CL-USER before processing each file
    (setf (current-package parser) (find-package :common-lisp-user)
          (current-package-name parser) "COMMON-LISP-USER"
          *package* (find-package :common-lisp-user))

    (with-open-file (stream file :direction :input)
      (let ((previous nil))
        (loop for form = (read stream nil :eof)
              for form-count from 1
              until (eq form :eof)
              do (detect-package-symbol-inconsistency parser form previous)
                 (when (and (consp form) (eq (car form) 'in-package))
                   (eval form)
                   (analyze-in-package parser form))
                 (setf previous form))))

    ;; Print analysis trace info
    (format log-stream "~&Package state for ~A:~%" (project-pathname file))
    (format log-stream "  Current package: ~A~%" (current-package parser))))


(defun analyze-definition-form (parser form)
  "Analyze raw form from source file for definitions, recording location and basic info."
  (declare (special log-stream))
  (walk-form form
    (lambda (current-form context parent-context &optional form)
      (declare (ignorable context parent-context form))
      (when (and (consp current-form)  
                 (symbolp (first current-form))
                 (not (quoted-symbol-p (first current-form))))
          (let ((head (first current-form)))
            (cond
              ;; Variable definition 
              ((member head '(defvar defparameter defconstant))
               (let ((name (second current-form)))
                 (record-definition *current-tracker*
                                    :name name
                                    :type :variable 
                                    :file (file parser)
                                    :package (current-package parser)  ;from last in-package, not runtime lookup--could be in several
                                    :status (symbol-status name (current-package parser))  ;(symbol-package name))
                                    :context current-form)))

              ;; Function definition
              ((eq head 'defun)
               (let* ((full-name (second current-form))
                      (base-name (if (and (listp full-name) (eq (car full-name) 'setf))
                                   (cadr full-name)
                                   full-name)))
                 (record-definition *current-tracker*
                                    :name full-name
                                    :type :function
                                    :file (file parser)
                                    :package (current-package parser)
                                    :status (symbol-status base-name (current-package parser))
                                    :context current-form)))

              ;; Macro definition
              ((eq head 'defmacro)
               (record-definition *current-tracker*
                                  :name (second current-form)
                                  :type :macro
                                  :file (file parser)
                                  :package (current-package parser)
                                  :status (symbol-status (second current-form) (current-package parser))
                                  :context current-form))

              ;; Generic function definition
              ((eq head 'defgeneric)
               (record-definition *current-tracker*
                                  :name (second current-form)
                                  :type :generic-function
                                  :file (file parser)
                                  :package (current-package parser)
                                  :status (symbol-status (second current-form) (current-package parser))
                                  :context current-form))

              ;; Method definition
              ((eq head 'defmethod)
               (let* ((rest (cddr current-form))  ; Skip defmethod and name
                      (qualifiers (loop while (and rest (keywordp (car rest)))
                                        collect (pop rest)))
                      (lambda-list (car rest)))
                 (record-definition *current-tracker*
                                    :name (second current-form)
                                    :type :method
                                    :file (file parser)
                                    :package (current-package parser)
                                    :status (symbol-status (second current-form) (current-package parser))
                                    :context current-form
                                    :qualifiers qualifiers 
                                    :specializers (extract-specializers lambda-list))))

              ;; Structure/class definition
              ((member head '(defclass defstruct define-condition))
               (let ((name (if (and (eq head 'defstruct)  ;handle defstruct options
                                    (consp (second current-form)))
                             (caadr current-form)               
                             (second current-form))))
                 (record-definition *current-tracker*
                                    :name name
                                    :type :structure/class/condition
                                    :file (file parser)
                                    :package (current-package parser)
                                    :status (symbol-status name (current-package parser))
                                    :context current-form)
                 (analyze-defclass/defstruct/define-condition parser name current-form)))

              ;; Type definition
              ((eq head 'deftype)
               (record-definition *current-tracker*
                                  :name (second current-form)
                                  :type :deftype
                                  :file (file parser)
                                  :package (current-package parser)
                                  :status (symbol-status (second current-form) (current-package parser))
                                  :context current-form))

              ;; Package system
              ((member head '(defpackage make-package))
               (record-definition *current-tracker*
                                  :name (second current-form)  ;eg, "PKG", :pkg, #:pkg
                                  :type :package
                                  :file (file parser)
                                  :context current-form)
               (analyze-defpackage/make-package parser (second current-form) current-form))

              ;; Setf form definitions  eg, (setf (symbol-value 'sym-var) 0)
              ((and (eq head 'setf) 
                    (consp (second current-form))
                    (symbolp (caadr current-form))
                    (member (caadr current-form) '(symbol-value symbol-function macro-function))
                    (consp (cdadr current-form))
                    (consp (cadadr current-form))
                    (eq (first (cadadr current-form)) 'quote))
               (let ((accessor (caadr current-form))
                     (name (second (cadadr current-form))))
                 (record-definition *current-tracker*
                                    :name name
                                    :type (case accessor
                                            (symbol-value :variable)
                                            (symbol-function :function)
                                            (macro-function :macro))
                                    :file (file parser)
                                    :package (current-package parser)
                                    :status (symbol-status name (current-package parser))
                                    :context current-form)))

              ;; Extended definition forms
              ((member head '(defsetf define-setf-expander))
               (record-definition *current-tracker*
                                :name (second current-form)
                                :type :setf
                                :file (file parser) 
                                :package (current-package parser)
                                :status (symbol-status (second current-form) (current-package parser))
                                :context current-form))

              ((eq head 'define-symbol-macro)
               (record-definition *current-tracker*
                                :name (second current-form)
                                :type :symbol-macro
                                :file (file parser)
                                :package (current-package parser)
                                :status (symbol-status (second current-form) (current-package parser))
                                :context current-form))

              ((member head '(define-modify-macro define-compiler-macro))
               (record-definition *current-tracker*
                                :name (second current-form)
                                :type :macro
                                :file (file parser)
                                :package (current-package parser)
                                :status (symbol-status (second current-form) (current-package parser))
                                :context current-form))

              ((eq head 'define-method-combination)
               (record-definition *current-tracker*
                                :name (second current-form)
                                :type :function
                                :file (file parser)
                                :package (current-package parser)
                                :status (symbol-status (second current-form) (current-package parser))
                                :context current-form)))))))
  form)


(defun analyze-reference-form (parser top-level-form)
  "Analyze source top-level-forms, recording references to definitions in different files."
  (declare (special log-stream))
  (let (;(gf-top-level-form (copy-list top-level-form))  
        (gf-p nil))  
    (labels ((handle-reference (current-form context parent-context)
               (if (and (typep current-form '(or character package string symbol))
                        (find-package current-form))
                 ;; Process a package designator
                 (let* ((key (make-tracking-key current-form nil :package))  
                        (defs (gethash key (slot-value *current-tracker* 'definitions)))
                        (other-file-defs (remove-if (lambda (def)
                                                      (equal (definition.file def) (file parser)))
                                                    defs)))
                   (when other-file-defs
                     (record-reference *current-tracker*
                                       :name current-form
                                       :type :package
                                       :file (file parser)
                                       :context parent-context
                                       :definitions other-file-defs)))
                 (when (symbolp current-form)
                   ;; Process a symbol reference
                   (unless (or (cl-symbol-p current-form)
                               (def*-name-p current-form context))
                     (if (potential-gf-reference-p current-form context)
                       ;; For potential generic function references in form, just set flag and continue walking
                       (setf gf-p t)
                       ;; For non-generic-functions, record reference if it has other-file-defs
                       (let* ((sym-type (get-symbol-reference-type current-form))
                              (sym-pkg (symbol-package current-form))
                              (key (make-tracking-key current-form sym-pkg sym-type))
                              (defs (gethash key (slot-value *current-tracker* 'definitions)))
                              (other-file-defs (remove-if (lambda (def)
                                                            (equal (definition.file def) (file parser)))
                                                          defs)))
                          (when other-file-defs
                            (record-reference *current-tracker*
                                              :name current-form
                                              :file (file parser)
                                              :type sym-type
                                              :package sym-pkg
                                              :context parent-context
                                              :visibility (get-visibility current-form (current-package parser))
                                              :definitions other-file-defs)))))))))
      ;; Walk the top-level-form first
      (walk-form top-level-form #'handle-reference)
      ;; Then analyze any method refs found
      (when gf-p
        (analyze-method-references parser top-level-form)))))


(defun pushit-transform (form predicate result-sym)
  "Recursively walk input CONTEXT-FORM.  
  If (atom FORM) is recognized by PREDICATE, replace it with (PUSHIT 'atom RESULT).
  If any element of FORM is a method ref (recognized by PREDICATE),
  replace the entire list with (PUSHIT form RESULT).
  Otherwise preserve structure and recurse."
  (cond
    ((atom form)
     (when (funcall predicate form)
       (push form (slot-value *current-tracker* 'pushit-symbols)))
     form)
    ((and (consp form)
          (not (member (first form) '(defgeneric defmethod)))
          (or (funcall predicate (car form))  ; Function position
              (some (lambda (arg)             ; Any argument position
                     (or (funcall predicate arg)
                         (and (consp arg)     ; Handle #'fn forms
                              (member (car arg) '(quote function))
                              (funcall predicate (cadr arg)))))
                   (cdr form))))
     (if (assoc (car form) +comparison-functions+)
         `(pushit (,(car form) ,@(mapcar (lambda (x) `(eval ',x)) (cdr form))) ,result-sym)
         `(pushit ,form ,result-sym)))
    (t
     (cons (pushit-transform (car form) predicate result-sym)
           (mapcar (lambda (x)
                    (pushit-transform x predicate result-sym))
                  (cdr form))))))


(defun detect-package-symbol-inconsistency (parser form previous)
  "Analyze a form for package/symbol consistency.
   Records anomalies for package declaration and symbol binding issues."
  (declare (special log-stream))
  (when (and (consp form) (symbolp (car form)))
    (let ((head (car form))
          (current-file (file parser))
          (current-pkg (current-package parser))
          (cl-user-pkg (find-package "COMMON-LISP-USER")))
      
      ;; Late in-package in file
      (when (member head '(in-package))
        (when (and previous  ; Not first form
                   (not (and (consp previous)  ; Previous form must be package definition
                            (member (car previous) '(defpackage make-package)))))
          (when (consp previous)
                    (member (car previous) '(defpackage make-package))))
          (record-anomaly *current-tracker*
                       :type :late-in-package
                       :severity :INFO
                       :file current-file
                       :package current-pkg
                       :context form
                       :description (format nil "In-package occurs after non-package form in ~A"
                                            (project-pathname current-file))))

      ;; Analyze definitions for package consistency 
      (when (member head '(defun defvar defparameter defmacro define-condition deftype define-method-combination
                           defclass defstruct defmethod defgeneric defsetf define-setf-expander
                           define-symbol-macro define-modify-macro define-compiler-macro))
        (let* ((def-name (second form))
               (runtime-def-pkg (and (symbolp def-name) (symbol-package def-name))))
          ;; Check definition package consistency
          (cond 
            ;; No package context but defining symbols
            ((and (eq current-pkg cl-user-pkg)
                  (member current-pkg (slot-value *current-tracker* 'project-owned-packages)))
             (record-anomaly *current-tracker*
                         :type :possible-missing-in-package-for-definition
                         :severity :WARNING
                         :file current-file
                         :package current-pkg
                         :context form
                         :description (format nil "~A is being defined in the default CL-USER package" def-name)))
            
            ;; Definition possibly in wrong package
            ((and runtime-def-pkg 
                  (not (eq runtime-def-pkg current-pkg)))
             (record-anomaly *current-tracker*
                         :type :possible-definition-in-wrong-package
                         :severity :WARNING
                         :file current-file
                         :package runtime-def-pkg
                         :context form
                         :description (format nil "Symbol ~A defined in ~A but current package is ~A"
                                              def-name runtime-def-pkg current-pkg)))))))))


(defun analyze-in-package (parser form)
  "Handle in-package forms by updating the current package context.
   Signals an error if referenced package doesn't exist."
  (let* ((pkg-designator (second form))
         (pkg (find-package pkg-designator)))
    (if pkg
        (setf (current-package parser) pkg
              (current-package-name parser) (package-name pkg))
        (format t "~2%Cannot accurately analyze dependencies:~%~
                File: ~A~%~
                Form: ~S~%~
                References undefined package: ~A~2%~
                Please ensure all package definitions compile and load successfully before analysis.~2%"
               (project-pathname (file parser)) form pkg-designator))))


(defun function-designator-p (item)
  (or (functionp item)
      (and (symbolp item)
           (fboundp item))
      (and (listp item)
           (case (first item)
             (lambda t)
             (function (function-designator-p (second item)))
             (setf (and (= (length item) 2)
                       (symbolp (second item))
                       (fboundp `(setf ,(second item)))))))))


(defun user-gf-designator-p (item)
  (and (function-designator-p item)
       (not (cl-symbol-p item))
       (if (functionp item)
           (typep item 'generic-function)
           (typep (fdefinition (if (listp item)
                                  (if (eq (first item) 'function)
                                      (second item)
                                      item)
                                  item))
                 'generic-function))))


(defun slot-definition-gf-references (slot-def)
  (and (listp slot-def)
       (loop for (key value) on (rest slot-def) by #'cddr
             when (member key '(:accessor :reader :writer))
             collect value)))


(defun potential-gf-reference-p (sym context)
  "May include some symbols that masquerade as gf references--eg, bound var in let statement."
  (and (ignore-errors (typep (fdefinition sym) 'generic-function))  ;sym must name a gf
       (not (and (consp context)  ;exclude defgeneric and defmethod name position (but already excluded in def* exclusion earlier)
                 (member (first context) '(defgeneric defmethod))
                 (eq sym (second context))))
       (not (loop for (x y) on context  ;exclude slot method implicit definitions for defclass & define-condition
                  thereis (and (member x '(:reader :writer :accessor))
                               (eq y sym))))))


(defun record-method-reference (parser form gf args)
  "Record reference from method ref to its matching definition.
   PARSER - File parser instance for current file context
   FORM - Original source form containing the ref 
   GF - Generic function being called
   ARGS - List of argument values/mock instances to match"
  (let ((gf-name (c2mop:generic-function-name gf))
        (pkg (current-package parser)))

    ;; Record reference for each matching method
    (dolist (method (compute-applicable-methods gf args))
      (let* ((specs (mapcar #'class-name (c2mop:method-specializers method)))
             (quals (method-qualifiers method))
             ;; Build lookup key to find method definition
             (key (make-tracking-key gf-name
                                   (symbol-package gf-name)
                                   :method 
                                   quals 
                                   specs))
             (defs (gethash key (slot-value *current-tracker* 'definitions)))
             ;; Filter out definitions from same file before recording reference
             (other-file-defs (remove-if (lambda (def)
                                           (equal (definition.file def) 
                                                  (file parser)))
                                          defs)))
        (when other-file-defs  ; Only record if we have defs from other files
          (record-reference *current-tracker*
                            :name gf-name
                            :type :method
                            :file (file parser)
                            :context form
                            :package pkg
                            :qualifiers quals
                            :arguments args 
                            :definitions other-file-defs))))))


(defun create-mock-instance (specializer)
  "Create a minimal instance sufficient for method dispatch.
   SPECIALIZER - A type specifier from method parameter list.
   Returns instance that satisfies typep for the specializer."
  
  (cond
    ;; Handle EQL specializer: (eql value)
    ((and (consp specializer)
          (eq (car specializer) 'eql))
     (cadr specializer))  ; Return the actual value
    
    ;; Handle built-in types with specific constructors
    ((eq specializer 'string) "")
    ((eq specializer 'number) 0)
    ((eq specializer 'vector) (vector))
    ((eq specializer 'array) (make-array 0))
    ((eq specializer 'hash-table) (make-hash-table))
    ((eq specializer 'package) (find-package :common-lisp))
    ((eq specializer 'symbol) 'mock-symbol)
    
    ;; Handle CLOS classes through instantiation
    (t
     (let ((class (find-class specializer nil)))
       (when class
         ;; For known types, create minimal instance
         (handler-case
             ;; Try direct instantiation first
             (make-instance specializer)
           (error ()
             ;; On failure, try allocate-instance
             (allocate-instance class))))))))


(defun process-lambda-list (lambda-list)
  "Build mock instance bindings for specialized parameters.
   Returns: list of bindings for let form"
  (let ((bindings nil))
    (loop for param in lambda-list
          until (and (symbolp param)
                    (member param lambda-list-keywords))
          when (consp param)
          do (let ((param-name (car param))
                   (param-type (second param)))
               (when param-type
                 (let ((binding `(,param-name (create-mock-instance ',param-type))))
                   (push binding bindings)))))
    bindings))


(defun extract-method-components (form)
  "Extract components from a defmethod form.
   Returns: (values name qualifiers lambda-list body)"
  (let* ((rest (cddr form))
         (qualifiers (loop while (and rest (keywordp (car rest)))
                           collect (pop rest)))
         (lambda-list (car rest))
         (body (cdr rest)))
    (values (second form) qualifiers lambda-list body)))


(defun build-eval-context (form)
  "Build let form to establish bindings for typed parameters in definition forms."
    (if (consp form)
        (case (car form)
          ;; Method definition
          ((defmethod)
           (multiple-value-bind (name qualifiers lambda-list body)
               (extract-method-components form)
             (declare (ignore name qualifiers))
             (let ((bindings (process-lambda-list lambda-list)))
               ;(print `(:debug-method-bindings :bindings ,bindings))
               (let ((result-form 
                      (if bindings
                          `(let ,(reverse bindings)
                             ,@body)
                          `(progn ,@body))))
                 result-form))))
          
          ;; Function definitions with type declarations
          ((flet labels) 
           (let* ((name (second form))
                  (lambda-list (third form))
                  (body (cdddr form))
                  (declarations (when (and (consp (car body))
                                         (eq (caar body) 'declare))
                                (pop body)))
                  (bindings nil))
             ;; Process type declarations
             (when declarations
               (dolist (decl (cdr declarations))
                 (when (eq (car decl) 'type)
                   (push `(,(third decl) (create-mock-instance ',(second decl)))
                         bindings))))
             (list* (car form) name lambda-list
                     (append (when declarations (list declarations))
                             (if bindings
                                 `((let ,(reverse bindings)
                                     ,@body))
                                 body)))))
          ;; No case matched - return original form
          (otherwise form))
        ;; Not a cons - error
        (error "Form ~A is not consp" form)))


#+ignore (defun build-eval-context (form)
  "Build let form to establish bindings for typed parameters in definition forms."
  (let ((definition-p nil))
    (if (consp form)
        (case (car form)
          ;; Method definition
          ((defmethod)
           (setf definition-p t)
           (multiple-value-bind (name qualifiers lambda-list body)
               (extract-method-components form)
             (declare (ignore name qualifiers))
             (let ((bindings (process-lambda-list lambda-list)))
               ;(print `(:debug-method-bindings :bindings ,bindings))
               (let ((result-form 
                      (if bindings
                          `(let ,(reverse bindings)
                             ,@body)
                          `(progn ,@body))))
                 (values result-form definition-p)))))
          
          ;; Function definitions with type declarations
          ((flet labels) 
           (setf definition-p t)
           (let* ((name (second form))
                  (lambda-list (third form))
                  (body (cdddr form))
                  (declarations (when (and (consp (car body))
                                         (eq (caar body) 'declare))
                                (pop body)))
                  (bindings nil))
             ;; Process type declarations
             (when declarations
               (dolist (decl (cdr declarations))
                 (when (eq (car decl) 'type)
                   (push `(,(third decl) (create-mock-instance ',(second decl)))
                         bindings))))
             (values
              (list* (car form) name lambda-list
                     (append (when declarations (list declarations))
                             (if bindings
                                 `((let ,(reverse bindings)
                                     ,@body))
                                 body)))
              definition-p)))
          ;; No case matched - return original form
          (otherwise (values form definition-p)))
        ;; Not a cons - error
        (error "Form ~A is not consp" form))))


(defun extract-method-refs (transformed-form refs-sym)
  "Extract actual method refs from a transformed form."
  (let ((wrapped-form
          `(let ((,refs-sym nil))
             ,transformed-form
             ,refs-sym)))
    (handler-case (eval wrapped-form)
      (error (c)
        (warn "Error evaluating method refs: ~A" c)
        '()))))


(defun analyze-method-references (parser gf-top-level-form) 
  "Analyze generic functions to track method references."
  (let* ((context-form (build-eval-context gf-top-level-form))
         (refs-sym (gensym "REFS"))
         (pushit-form (pushit-transform context-form #'user-gf-designator-p refs-sym))
         (method-refs (extract-method-refs pushit-form refs-sym)))
    (dolist (ref method-refs)
      (let ((gf (first ref))
            (args (rest ref)))
        (record-method-reference parser gf-top-level-form gf args)))))
