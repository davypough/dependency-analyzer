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
      (loop for form = (read stream nil :eof)
            for form-count from 1
            until (eq form :eof)
            do (when (and (consp form) (eq (car form) 'in-package))
                 (eval form)
                 (analyze-in-package parser form))
               (analyze-package-symbols-form parser form form-count)))

    ;; Print analysis trace info
    (format log-stream "~&Package state for ~A:~%" (project-pathname file))
    (format log-stream "  Current package: ~A~%" (current-package parser))))


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
                                  :context current-form)))

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
                                 :specializers (extract-specializers lambda-list))))

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
              ((member head '(defpackage make-package))
               (record-definition *current-tracker*
                     :name (second current-form)  ;string/symbol designator
                     :type :package
                     :file (file parser)
                     :context current-form)
               (analyze-defpackage/make-package parser (second current-form) current-form))

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


(defun analyze-reference-form (parser form)
  "Analyze form recording references to definitions in different files.
   Handles references to packages (via string/keyword/uninterned-symbol) and 
   references to other defined symbols."
  (declare (special log-stream))
  (labels ((handle-reference (subform context parent-context depth)
             (declare (ignorable subform context parent-context depth))
             (unless (skip-item-p subform)
               (typecase subform
                 ((or string keyword (and symbol (satisfies not-interned-p)))
                   ;; Only process as package ref in package contexts
                   (when (package-context-p context parent-context)
                     (let* ((norm-name (normalize-designator subform))
                            (key (make-tracking-key norm-name norm-name :PACKAGE))
                            (defs (gethash key (slot-value *current-tracker* 'definitions))))
                       (let ((other-file-defs (remove-if (lambda (def)  
                                                         (equal (definition.file def) (file parser)))
                                              defs)))
                         (when other-file-defs
                           (record-reference *current-tracker*
                            :name norm-name
                            :file (file parser)
                            :package subform
                            :context parent-context
                            :visibility :LOCAL
                            :definitions other-file-defs))))))
                 (symbol
                   (let* ((sym-pkg (symbol-package subform))
                          (pkg-name (if sym-pkg
                                      (package-name sym-pkg)
                                      (current-package-name parser)))
                          (visibility (check-package-reference subform parser *current-tracker*
                                                            context parent-context)))
                     ;; Check if symbol is in function call context
                     (multiple-value-bind (call-p name args)
                         (analyze-function-call-context subform context)
                       (if call-p
                           ;; Check if method first & process
                           (handle-method-call subform parser pkg-name parent-context visibility name args)
                           ;; Not a method call - check all standard definition types
                           (try-definition-types subform pkg-name parser parent-context visibility)))))))))
    ;; Walk the form applying handler
    (walk-form form #'handle-reference)))


(defun analyze-package-symbols-form (parser form form-count)
  "Analyze a form for package/symbol consistency.
   Records anomalies for package declaration and symbol binding issues."
  (declare (special log-stream))
  (when (and (consp form) (symbolp (car form)))
    (let* ((head (car form))
           (current-file (file parser))
           (current-pkg (current-package parser))
           (project-pkg (project-package *current-tracker*)))

      (format log-stream "~&Form ~D: ~S~%" form-count form)
      
      ;; Late in-package in file
      (when (member head '(in-package))
        (when (> form-count 2)
          (record-anomaly *current-tracker*
                       :name "IN-PACKAGE-LOCATION-ISSUE"
                       :type :late-in-package
                       :severity :WARNING
                       :file current-file
                       :package current-pkg
                       :context form
                       :description (format nil "In-package occurs after initial forms in ~S" current-pkg))))

      ;; Analyze definitions for package consistency 
      (when (member head '(defun defvar defparameter defmacro define-condition deftype define-method-combination
                           defclass defstruct defmethod defgeneric defsetf define-setf-expander
                           define-symbol-macro define-modify-macro define-compiler-macro))
        (let* ((def-name (second form))
               (cl-user-pkg (find-package "COMMON-LISP-USER"))
               (runtime-def-pkg (and (symbolp def-name) (symbol-package def-name))))
          ;; Check definition package consistency
          (cond 
            ;; No package context but defining symbols
            ((and (eq current-pkg cl-user-pkg)
                  (not (eq project-pkg cl-user-pkg)))
             (record-anomaly *current-tracker*
                         :name def-name 
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
                         :name def-name
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
  (let* ((pkg-designator (second form))   ;(normalize-designator (second form)))
         (pkg (find-package pkg-designator)))
    (if pkg
        (setf (current-package parser) pkg
              (current-package-name parser) (package-name pkg))
        (error "~&Cannot accurately analyze dependencies:~%~
                File: ~A~%~
                Form: ~S~%~
                References undefined package: ~A~2%~
                Please ensure all package definitions compile and load successfully before analysis."
               (project-pathname (file parser)) form pkg-designator))))
