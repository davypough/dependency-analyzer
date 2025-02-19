;;;; Filename:  package-symbol-analysis.lisp

;;;; Analyze the use of package symbols in the user's project files.


(in-package :dep)


(defun parse-package-symbols-in-file (parser)
  "Third pass analysis of package/symbol relationships in source code.
   Processes forms sequentially while maintaining package context.
   Records anomalies related to package declarations, naming conventions,
   and symbol accessibility."
  (with-slots (file) parser
    ;; Reset to CL-USER before processing each file for consistent analysis
    (setf (current-package parser) (find-package :common-lisp-user)
          (current-package-name parser) "COMMON-LISP-USER"
          *package* (find-package :common-lisp-user))
    (with-open-file (stream file :direction :input)
        (loop for form = (read stream nil :eof)
              for form-count from 1
              until (eq form :eof)
              do 
                 ;; Process definition-related checks
                 (when (and (consp form) (symbolp (car form)))
                   (analyze-definition-context parser form)
                   (check-naming-conventions parser form))
                 
                 ;; Analyze symbol references within current context
                 (validate-symbol-references parser form)
                 
                 ;; Check for direct package references
                 (analyze-package-references parser form)
                 
                 ;; Update package context if needed
                 (when (and (consp form) (eq (car form) 'in-package))
                   (eval form)
                   (analyze-in-package parser form))))))


(defun analyze-definition-context (parser form)
  "Analyzes definition forms in their source context for package/symbol relationships.
   Focuses on shadowing and visibility patterns beyond compiler checks.
   Assumes all code has been successfully compiled and loaded."
  (when (and (consp form) 
             (symbolp (car form))
             (member (car form) +def*-forms+))
    (let* ((head (car form))
           (current-pkg (current-package parser))
           (current-file (file parser)))
      
      ;; Extract name correctly handling complex definition forms
      (multiple-value-bind (name-string base-symbol setf-form-p)
          (extract-definition-name-string (second form))
        (declare (ignore name-string))  ; Used only in compiler checks
        
        ;; Skip package definition analysis - handled elsewhere
        (unless (eq head 'defpackage)
          ;; Check for potential shadowing of inherited symbols
          (when-let ((inherited-sym (find-symbol (symbol-name base-symbol) 
                                               current-pkg)))
            (multiple-value-bind (sym status) 
                (find-symbol (symbol-name base-symbol) current-pkg)
              (when (and sym
                        (eq status :inherited)
                        ;; Exclude valid CLOS method extensions
                        (not (and (eq head 'defmethod)
                                (let ((gf (ignore-errors 
                                          (fdefinition (if setf-form-p
                                                         (second form)
                                                         base-symbol)))))
                                  (and gf (typep gf 'generic-function))))))
                (record-anomaly *current-tracker*
                  :type :shadowed-definition
                  :severity :warning
                  :file current-file
                  :package current-pkg
                  :context form
                  :description 
                  (format nil "Definition of ~A shadows inherited symbol from ~A"
                         base-symbol
                         (package-name (symbol-package inherited-sym))))))))))))


(defun extract-definition-name-string (name-form)
  "Extract string representation of a definition name.
   Handles:
   - Regular symbols (point)
   - Package-qualified symbols (pkg:point) 
   - Internal symbols (#:point)
   - Setf forms (setf point)
   - Structure name+options ((point :conc-name pt-))
   Returns: (values name-string base-symbol setf-form-p)"
  (etypecase name-form
    (symbol (values (symbol-name name-form)
                    name-form
                    nil))
    (cons (cond 
            ;; Handle (setf symbol) forms
            ((and (eq (car name-form) 'setf)
                  (symbolp (cadr name-form)))
             (values (format nil "(~A ~A)"
                           (car name-form)
                           (cadr name-form))
                     (cadr name-form)
                     t))
            ;; Handle (name options...) structure forms, allowing package qualifiers
            ((symbolp (car name-form))
             (values (symbol-name (car name-form))
                     (car name-form)
                     nil))
            ;; Handle (pkg:name options...) structure forms
            ((and (consp (car name-form))
                  (eq (caar name-form) 'package)
                  (symbolp (cadar name-form)))
             (values (symbol-name (cadar name-form))
                     (cadar name-form)
                     nil))
            (t (error "Invalid definition name form: ~S" name-form))))))


(defun check-naming-conventions (parser form)
  "Validates naming patterns that affect symbol visibility and package relationships.
   Focuses on special variables and constants that have package-wide effects.
   Assumes code is compiled and loaded - checks only conventions affecting visibility."
  (when (and (consp form) (symbolp (car form)))
    (let ((head (car form))
          (current-pkg (current-package parser))
          (current-file (file parser)))
      
      ;; Only analyze special vars and constants
      (when (member head '(defvar defparameter defconstant))
        (let* ((name (second form))
               (str-name (symbol-name name)))
          
          ;; Detect special variable without earmuffs
          (when (and (member head '(defvar defparameter))
                    (or (< (length str-name) 2)  ; Too short for earmuffs
                        (not (and (char= (char str-name 0) #\*)
                                 (char= (char str-name (1- (length str-name))) #\*)))))
            (record-anomaly *current-tracker*
              :type :special-var-naming
              :severity :info
              :file current-file
              :package current-pkg
              :context form
              :description 
              (format nil "Special variable ~A defined without earmuffs convention (*~A*)"
                      name (string-trim "*" str-name))))
          
          ;; Detect constant without plus signs
          (when (and (eq head 'defconstant)
                    (or (< (length str-name) 2)  ; Too short for plus signs
                        (not (and (char= (char str-name 0) #\+)
                                 (char= (char str-name (1- (length str-name))) #\+)))))
            (record-anomaly *current-tracker*
              :type :constant-naming
              :severity :info
              :file current-file
              :package current-pkg
              :context form
              :description 
              (format nil "Constant ~A defined without plus-sign convention (+~A+)"
                      name (string-trim "+" str-name)))))))))


(defun validate-symbol-references (parser form)
  "Validates symbol references within their package context.
   Analyzes access patterns to internal, external, and inherited symbols.
   Works with runtime package state to verify actual symbol relationships."
  (walk-form form
    (lambda (current-form context parent-context)
      (when (symbolp current-form)
        (let ((current-pkg (current-package parser))
              (current-file (file parser)))
          
          ;; Skip compiler-verified cases
          (unless (or (cl-symbol-p current-form)     ; CL package symbols
                     (keywordp current-form)         ; Keywords 
                     (not (symbol-package current-form)) ; Uninterned
                     (def*-name-p current-form context)) ; Definition names
            
            ;; Examine package qualification usage
            (let* ((sym-pkg (symbol-package current-form))
                   (sym-name (symbol-name current-form))
                   (qualified-p (not (eq sym-pkg current-pkg))))
              
              ;; Check package-qualified reference to internal symbol
              (when (and qualified-p
                        (not (eq sym-pkg (find-package :keyword)))
                        (eq (nth-value 1 (find-symbol sym-name sym-pkg))
                            :internal))
                (record-anomaly *current-tracker*
                  :type :qualified-internal-reference
                  :severity :warning
                  :file current-file
                  :package current-pkg
                  :description
                  (format nil "Package-qualified reference to internal symbol ~A:~A"
                          (package-name sym-pkg) current-form)
                  :context parent-context))
              
              ;; Check consistent package qualification
              (when (and (not qualified-p)
                        (eq (nth-value 1 (find-symbol sym-name current-pkg))
                            :inherited))
                (when (find-external-symbol sym-name sym-pkg)
                  (record-anomaly *current-tracker*
                    :type :unqualified-external-reference
                    :severity :info
                    :file current-file
                    :package current-pkg
                    :description
                    (format nil "Unqualified reference to external symbol ~A from ~A"
                            current-form (package-name sym-pkg))
                    :context parent-context))))))))))

(defun find-external-symbol (name pkg)
  "Returns true if NAME is an external symbol in PKG."
  (multiple-value-bind (sym status)
      (find-symbol name pkg)
    (and sym (eq status :external))))


(defun analyze-package-references (parser form)
  "Analyzes direct references to package names in source code.
   Identifies cases where package variables might be preferable.
   Works with runtime package state to validate references."
  (walk-form form
    (lambda (current-form context parent-context)
      (when (and (or (stringp current-form)     ; "PACKAGE"
                     (keywordp current-form)     ; :package
                     (and (symbolp current-form) ; #:package
                          (not (symbol-package current-form))))
                 (find-package (string current-form)))
        
        (let ((current-pkg (current-package parser))
              (current-file (file parser)))
          
          ;; Skip references in package definition contexts
          (unless (and (consp context)
                      (member (first context) 
                              '(defpackage in-package make-package delete-package))
                      ;; For package definitions, check if defining same package
                      (or (not (eq (first context) 'defpackage))
                          (equal (string (second context))
                                 (string current-form))))
            
            ;; Skip references in import/export forms
            (unless (and (consp parent-context)
                        (member (first parent-context)
                                '(:use :import-from :export)))
              
              ;; Record direct package reference
              (record-anomaly *current-tracker*
                :type :inline-package-reference
                :severity :info
                :file current-file
                :package current-pkg
                :description 
                (format nil "Package name ~S referenced directly. Consider using a package variable instead"
                        current-form)
                :context context))))))))

#|
(defun parse-package-symbols-in-file (parser)
  "Third pass parser that validates package/symbol consistency.
   Updates package context during iteration and hands forms to analyzer."
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
                 (setf previous form))))))


(defun detect-package-symbol-inconsistency (parser form previous)
  "Analyze a form for package/symbol consistency and naming conventions.
   Records anomalies for:
   - Late in-package declarations
   - Package/symbol context mismatches
   - Special variable/constant naming conventions"
  (when (and (consp form) (symbolp (car form)))
    (let ((head (car form))
          (current-file (file parser))
          (current-pkg (current-package parser))
          (cl-user-pkg (find-package "COMMON-LISP-USER")))
      
      ;; Late in-package check
      (when (member head '(in-package))
        (when (and previous  ; Not first form
                   (not (and (consp previous)  ; Previous form must be package definition
                            (member (car previous) '(defpackage make-package)))))
          (record-anomaly *current-tracker*
                       :type :late-in-package
                       :severity :info
                       :file current-file
                       :package current-pkg
                       :context form
                       :description (format nil "In-package occurs after non-package form in ~A"
                                            (project-pathname current-file)))))

      ;; Analyze definitions for package consistency 
      (when (member head +def*-forms+)
        (let* ((def-name (second form)))
          (multiple-value-bind (name-string base-symbol setf-form-p)
                   (extract-definition-name-string def-name)
                 (declare (ignore name-string))
                 ;; Use base-symbol for package lookup
                 (let ((runtime-def-pkg (and base-symbol (symbol-package base-symbol))))
                   ;; Package consistency checks
                   (cond 
                     ;; No package context but defining symbols
                     ((and (eq current-pkg cl-user-pkg)
                           (member current-pkg (slot-value *current-tracker* 'project-owned-packages)))
                      (record-anomaly *current-tracker*
                                    :type :possible-missing-in-package-for-definition
                                    :severity :info
                                    :file current-file
                                    :package current-pkg
                                    :context form
                                    :description (format nil "~A is being defined in the default CL-USER package" 
                                                       def-name)))
                     
                     ;; Skip package check for methods on standard generic functions
                     ((and (eq head 'defmethod)
                           (let ((gf (and (fboundp base-symbol)
                                        (fdefinition (if setf-form-p 
                                                       def-name 
                                                       base-symbol)))))
                             (and gf 
                                  (typep gf 'standard-generic-function)
                                  (eq runtime-def-pkg 
                                      (find-package :common-lisp)))))
                      nil)
                     
                     ;; Definition possibly in wrong package
                     ((and runtime-def-pkg 
                           (not (eq runtime-def-pkg current-pkg)))
                      (record-anomaly *current-tracker*
                                    :type :possible-definition-in-wrong-package
                                    :severity :info
                                    :file current-file
                                    :package runtime-def-pkg
                                    :context form
                                    :description (format nil "Symbol ~A defined in ~A but current package is ~A"
                                                       def-name runtime-def-pkg current-pkg))))))))

      ;; Check special variable and constant naming conventions
      (when (member head '(defvar defparameter defconstant))
        (let* ((name (second form))
               (str-name (symbol-name name)))
          (cond 
            ;; Check special vars (defvar/defparameter)
            ((and (member head '(defvar defparameter))
                 (not (and (> (length str-name) 2)
                          (char= (char str-name 0) #\*)
                          (char= (char str-name (1- (length str-name))) #\*))))
             (record-anomaly *current-tracker*
               :type :special-var-naming
               :severity :info
               :file current-file
               :package current-pkg
               :context form
               :description (format nil "Special variables like ~A normally have earmuff names (*~A*)" 
                                  name (string-trim "*" str-name))))
            
            ;; Check constants
            ((and (eq head 'defconstant)
                 (not (and (> (length str-name) 2)
                          (char= (char str-name 0) #\+)
                          (char= (char str-name (1- (length str-name))) #\+))))
             (record-anomaly *current-tracker*
               :type :constant-naming
               :severity :info
               :file current-file
               :package current-pkg
               :context form
               :description (format nil "Constants like ~A normally have plus-sign names (+~A+)"
                                  name (string-trim "+" str-name)))))))

      ;; Check for potential shadowing of inherited symbols
      (when (member head +def*-forms+)
        (multiple-value-bind (name-string base-symbol setf-form-p)
            (extract-definition-name-string (second form))
          (when (and (nth-value 1 (find-symbol (symbol-name base-symbol) current-pkg))
                     (not (and (eq head 'defmethod)
                              (let ((existing (ignore-errors 
                                              (fdefinition (if setf-form-p
                                                             (second form)
                                                             base-symbol)))))
                                (and existing (typep existing 'generic-function))))))
            (record-anomaly *current-tracker*
              :type :shadowed-definition
              :severity :warning
              :file current-file
              :package current-pkg
              :description (format nil "Definition of ~A shadows inherited symbol from ~A"
                                 name-string
                                 (package-name (symbol-package base-symbol))))))))))
|#