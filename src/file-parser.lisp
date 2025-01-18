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
  (let ((gf-top-level-form (copy-list top-level-form))  ;revised top-level-form if there are generic function calls
        (gf-p nil))  ;is there a generic function call in the top-level-form
    (labels ((handle-reference (current-form context parent-context)
               ;(declare (ignorable context))
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
                   ;; Process a symbol reference
                   (unless (skip-item-p current-form)  ; (skip-definition-form current-form))
                     (when (symbolp current-form)
                       (let* ((sym-pkg (symbol-package current-form))
                              (sym-type (get-symbol-reference-type current-form context))
                              (key (make-tracking-key current-form sym-pkg sym-type)))
                         ;; If this is a GF call, save it for later analysis
                         (when (and (member sym-type '(:generic-function :method))
                                    (consp context)
                                    (eq (first context) current-form))
                           (setf gf-p t))  ;the context is a generic function call
                         ;; Record the basic reference
                         (let* ((defs (gethash key (slot-value *current-tracker* 'definitions)))
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
      ;; Then analyze any method calls found
      (when gf-p
        (analyze-method-references parser gf-top-level-form)))))


(defun analyze-method-references (parser gf-top-level-form)
  "Analyze generic function calls to track method references."
  (let* ((pushit-top-level-form (pushit-transform gf-top-level-form #'generic-function-p))
         (pre-eval-form `(let (result) ,pushit-top-level-form result))
         (method-calls (eval pre-eval-form)))
    ;; Debug print
    ;(let ((*package* (find-package :common-lisp-user)))
    ;  (prt gf-top-level-form pushit-top-level-form pre-eval-form method-calls))
    ;; Process each captured call
    (dolist (call method-calls)
      (when (and (consp call)          ; Must be a cons
                 (symbolp (car call))   ; With symbol in car position
                 (fboundp (car call))   ; That's bound
                 (typep (fdefinition (car call)) 'standard-generic-function)) ; To a generic function
        (let* ((name (car call))
               (args (cdr call))
               ;; Get generic function and applicable methods  
               (methods (compute-applicable-methods (fdefinition name) args)))
          ;; Record references to matching method definitions
          (dolist (method methods)
            (let ((specs (mapcar #'class-name 
                                (closer-mop:method-specializers method)))
                  (quals (method-qualifiers method)))
              ;; Look up and record reference to this specific method
              (let* ((key (make-tracking-key name 
                                           (symbol-package name)
                                           :method 
                                           quals 
                                           specs))
                     (defs (gethash key (slot-value *current-tracker* 'definitions))))
                (when defs
                  (record-reference *current-tracker*
                                  :name name
                                  :type :method
                                  :file (file parser)
                                  :context gf-top-level-form
                                  :package (symbol-package name)
                                  :qualifiers quals
                                  :arguments args
                                  :definitions defs))))))))))      


(defun generic-function-p (sym)
  (and (symbolp sym)
       (fboundp sym)
       (typep (fdefinition sym) 'standard-generic-function)))


(defun pushit-transform (form predicate)
  "Walk FORM. If (car FORM) satisfies PREDICATE, wrap it in (PUSHIT form).
   For compound forms like EXPT, preserve structure and recur into arguments."
  (cond ((atom form)
         form)
        ;; If head is a method, wrap in pushit
        ((funcall predicate (car form))
         (list 'pushit 
               (cons (car form)
                     (mapcar (lambda (x) (pushit-transform x predicate))
                            (cdr form)))))
        ;; Otherwise preserve the form structure and recur
        (t
         (cons (car form)
               (mapcar (lambda (x) (pushit-transform x predicate))
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
                  (member current-pkg (project-owned-packages *current-tracker*)))
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