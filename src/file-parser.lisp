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
  "Analyze raw form from source file for definitions, recording location and basic info.
   Handles all standard definition forms, setf forms, and extended definitions."
    (declare (special log-stream))
    (walk-form form
      (lambda (current-form context parent-context)
        (declare (ignorable context parent-context))
        (when (and (consp current-form)  ;only analyze potential definition forms
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
               (record-definition *current-tracker*
                                  :name (second current-form)
                                  :type :function
                                  :file (file parser)
                                  :package (current-package parser)
                                  :status (symbol-status (second current-form) (current-package parser))
                                  :context current-form))

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


(defun analyze-reference-form (parser form)
  "Analyze source forms, recording references to definitions in different files."
  (declare (special log-stream))
  (labels ((handle-reference (current-form context parent-context)
             (declare (ignorable context parent-context))
             (if (and (typep current-form '(or character package string symbol))
                      (find-package current-form))  ;process a package designator
               (let* ((key (make-tracking-key current-form nil :package))  
                      (defs (gethash key (slot-value *current-tracker* 'definitions)))
                      (other-file-defs (remove-if (lambda (def)
                                                       (equal (definition.file def) (file parser)))
                                                     defs)))
                 (if other-file-defs
                   (record-reference *current-tracker*
                                     :name current-form
                                     :type :package
                                     :file (file parser)
                                     :context parent-context
                                     :definitions other-file-defs)
                   (record-anomaly *current-tracker*
                                         :type :undefined-package
                                         :severity :error
                                         :file (file parser)
                                         :package (current-package parser)
                                         :context parent-context
                                         :description (format nil "Reference to undefined package ~A" current-form))))
               (unless (or (skip-item-p current-form) (skip-definition-form current-form))
                 (when (symbolp current-form)
                   (let* ((sym-pkg (symbol-package current-form))
                          (sym-type (get-symbol-reference-type current-form parent-context))
                          (key (if (eq sym-type :method)
                                 (let* ((args (and (consp parent-context) (cdr parent-context)))
                                        (qualifiers (loop for arg in args
                                                          while (keywordp arg)
                                                          collect arg))
                                        (values (nthcdr (length qualifiers) args)))
                                   (make-tracking-key current-form sym-pkg sym-type qualifiers (mapcar #'type-of values)))
                                 (make-tracking-key current-form sym-pkg sym-type)))
                          (defs (gethash key (slot-value *current-tracker* 'definitions)))
                          (other-file-defs (remove-if (lambda (def)
                                                        (equal (definition.file def) (file parser)))
                                                      defs)))
                     (if other-file-defs
                       (record-reference *current-tracker*
                                                          :name current-form
                                                          :file (file parser)
                                                          :type sym-type
                                                          :package sym-pkg  
                                                          :context parent-context
                                                          :visibility (get-visibility current-form (current-package parser))
                                                          :definitions other-file-defs)
                       (record-anomaly *current-tracker*
                                          :type :undefined-symbol
                                          :severity :error
                                          :file (file parser)
                                          :context parent-context
                                          :description (format nil "Reference to undefined symbol ~A" current-form)))))))))
    ;; Walk the form breadth-first applying handler
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
