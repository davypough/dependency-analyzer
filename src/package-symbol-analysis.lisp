;;;; Filename:  package-symbol-analysis.lisp

;;;; Analyze the use of package symbols in the user's project files.


(in-package :dep)


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
                         :severity :info
                         :file current-file
                         :package current-pkg
                         :context form
                         :description (format nil "~A is being defined in the default CL-USER package" def-name)))
            
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
                                              def-name runtime-def-pkg current-pkg))))))

      ;; Check special variable and constant naming conventions, earmuffs & +-signs
      (when (member head '(defvar defparameter defconstant))
        (let* ((name (second form))
               (str-name (string name)))
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
                                  name (string-trim "+" str-name))))))))))


#+ignore (defun detect-package-symbol-inconsistency (parser form previous)
  "Analyze a form for package/symbol consistency.
   Records anomalies for package declaration and symbol binding issues."
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
                         :severity :INFO
                         :file current-file
                         :package current-pkg
                         :context form
                         :description (format nil "~A is being defined in the default CL-USER package" def-name)))
            
            ;; Definition possibly in wrong package
            ((and runtime-def-pkg 
                  (not (eq runtime-def-pkg current-pkg)))
             (record-anomaly *current-tracker*
                         :type :possible-definition-in-wrong-package
                         :severity :INFO
                         :file current-file
                         :package runtime-def-pkg
                         :context form
                         :description (format nil "Symbol ~A defined in ~A but current package is ~A"
                                              def-name runtime-def-pkg current-pkg)))))))))


