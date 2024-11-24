;;;; Filename: file-parser.lisp
;;;
;;; Core file parsing functionality for dependency analysis.
;;; Analyzes source files to track symbol definitions, references,
;;; and package relationships without expanding macros.
;;; Also provides ASDF component parsing coordination.


(in-package #:dep)


(defmethod parse-definitions-in-file ((parser file-parser))
  "First pass parser that records definitions.
   Analyzes all forms recursively, maintaining package context during traversal."
  (with-slots (file parsing-files) parser
    (push file parsing-files)
    (with-open-file (stream file :direction :input)
      (loop for form = (read stream nil nil)
            unless (null form)
              do (analyze-definition-form parser form)))
    (pop parsing-files)))


(defmethod parse-references-in-file ((parser file-parser))
  "Second pass parser that records references to previously recorded definitions.
   Processes in-package forms to maintain proper package context, 
   then analyzes all forms for references. Unlike the definition pass, 
   this analyzes both cons forms and atoms since references can occur as either."
  (with-slots (file package) parser
    (with-open-file (stream file :direction :input)
      (let ((*package* package))
        (loop for form = (read stream nil nil)
              while form
              do (if (consp form)
                     (if (eq (car form) 'in-package)
                         (analyze-in-package parser form)
                         (analyze-reference-form parser form))
                     (analyze-reference-form parser form)))))))


(defmethod analyze-in-package ((parser file-parser) form)
  "Handle in-package forms by updating the current package context."
  (let* ((name (normalize-package-name (second form)))
         (package (or (find-package name)
                     (make-package name))))
    (setf (current-package parser) package
          (current-package-name parser) (package-name package)
          *package* package)))


(defmethod analyze-definition-form ((parser file-parser) form)
 "Analyze a form for definitions and process all subforms for nested definitions.
  Maintains correct package context through all nested forms."   (print form) (break)
 (typecase form
   (cons
    (if (symbolp (car form))
        (let ((operator (car form)))
          (case operator
            (in-package (analyze-in-package parser form))
            (defpackage (analyze-defpackage parser form))
            (defstruct (analyze-defstruct parser form))
            ((defvar defparameter defconstant) (analyze-defvar-defparameter-defconstant parser form))
            ((defun defmacro) (analyze-defun-defmacro parser form))
            (defgeneric (analyze-defgeneric parser form))
            (defmethod (analyze-defmethod parser form))
            (define-condition (analyze-define-condition parser form))
            (define-method-combination (analyze-method-combination parser form))
            (define-setf-expander (analyze-define-setf-expander parser form))
            (define-modify-macro (analyze-define-modify-macro parser form))
            (define-compiler-macro (analyze-define-compiler-macro parser form))
            (setf (when (and (cddr form)
                             (listp (second form)))
                    (case (caadr form)
                      ((symbol-value)
                         (analyze-setf-symbol-value parser form))
                      ((symbol-function fdefinition macro-function)
                         (analyze-setf-symbol-function-fdefinition-macro-function parser form)))))
            (t (dolist (subform (cdr form))
                 (analyze-definition-form parser subform)))))
        (dolist (subform form)
          (analyze-definition-form parser subform))))
   ((and array (not string)) 
    (dotimes (i (array-total-size form))
      (analyze-definition-form parser (row-major-aref form i))))
   (hash-table
    (maphash (lambda (k v)
               (analyze-definition-form parser k)
               (analyze-definition-form parser v))
             form))
   (t nil)))


(defmethod analyze-reference-form ((parser file-parser) form)
  "Walk form recording :CALL and :REFERENCE symbol references.
   :CALL references are symbols in operator position.
   :REFERENCE references are symbols in value/key position.
   All symbols are checked against definition records."
  (labels ((walk (x op-position)
             (typecase x
               (cons 
                (when op-position  
                  (walk (car x) t))    
                (walk (cdr x) nil))    
               (array
                (dotimes (i (array-total-size x))
                  (walk (row-major-aref x i) nil)))
               (hash-table
                (maphash (lambda (k v)
                          (walk k nil)
                          (walk v nil))
                        x))
               (symbol
                (when x  ; Skip nil
                  (let* ((pkg (or (symbol-package x) 
                                (current-package parser)))
                         (pkg-name (package-name pkg))
                         (visibility (cond
                                     ((null (symbol-package x)) :LOCAL)
                                     ((eq pkg (current-package parser)) :LOCAL)
                                     (t (multiple-value-bind (sym status)
                                            (find-symbol (symbol-name x) 
                                                       (current-package parser))
                                          (declare (ignore sym))
                                          (case status
                                            (:inherited :INHERITED)
                                            (:external :IMPORTED)
                                            (otherwise :LOCAL)))))))
                    (when (gethash (make-tracking-key x pkg-name)
                                 (slot-value *current-tracker* 'definitions))
                      (record-reference *current-tracker* x
                                      (if op-position :CALL :REFERENCE)
                                      (file parser)
                                      :package pkg-name
                                      :visibility visibility)))))
               (t nil))))
    (walk form nil)))


(defun analyze-defpackage (parser form)
  "Record package definition and analyze package options for dependencies.
   Form is (defpackage name options...)"
  (let* ((name (normalize-package-name (second form)))
         (options (cddr form))
         (package (or (find-package name)
                     (make-package name))))
    ;; Record the package definition itself
    (record-definition *current-tracker* name
                      :PACKAGE
                      (file parser)
                      :package "KEYWORD"  ; Package names are keywords
                      :exported-p t)      ; Package names are always accessible
    ;; Process each package option
    (dolist (option options)
      (when (listp option)
        (case (car option)
          (:use
           ;; Record dependencies on used packages
           (dolist (used-pkg (cdr option))
             (let ((used-name (normalize-package-name used-pkg)))
               (process-package-use-option package used-name options parser))))
          ((:import-from :use-package)
           ;; Record dependencies on packages we import from
           (let ((from-pkg (normalize-package-name (second option))))
             (dolist (sym (cddr option))
               (process-package-import-option package from-pkg name parser sym))))
          (:export
           ;; Record exported symbols in tracker's package-exports
           (dolist (sym (cdr option))
             (process-package-export-option package name sym))))))))


(defun analyze-defstruct (parser form)
  "Handle structure definitions, recording structure name and analyzing all forms recursively.
   Collects user-introduced keywords by flattening the entire form and removing standard keywords."
  (let ((standard-keywords '(:conc-name :constructor :copier :predicate :include 
                           :print-function :print-object :type :named :initial-offset
                           :read-only :accessor :reader :writer)))
    (destructuring-bind (def-op name-and-options &rest slots) form
      (declare (ignore def-op))
      (let* ((name (if (listp name-and-options) 
                      (car name-and-options)
                      name-and-options))
             (package (current-package parser))
             (conc-name-prefix (let ((conc-option (when (listp name-and-options)
                                                  (find :conc-name (cdr name-and-options) 
                                                        :key #'car))))
                                (cond ((null conc-option)
                                     (concatenate 'string (symbol-name name) "-"))
                                    ((null (second conc-option))
                                     "")
                                    (t (let ((prefix (second conc-option)))
                                         (if (symbolp prefix)
                                             (symbol-name prefix)
                                             prefix))))))
             ;; Get user-introduced keywords from entire form
             (user-keywords  (set-difference (remove-if-not #'keywordp 
                                                        (alexandria:flatten form))
                                         standard-keywords)))

        ;; Record the structure definition with collected user keywords
        (record-definition *current-tracker* name
                          :STRUCTURE
                          (file parser)
                          :package (current-package-name parser)
                          :exported-p (eq (nth-value 1 
                                        (find-symbol (symbol-name name) package))
                                       :external)
                          :user-data user-keywords)

        ;; Record accessor definitions and analyze all forms recursively
        (dolist (slot slots)
          (let* ((slot-name (if (listp slot) (car slot) slot))
                 (accessor-name (intern (concatenate 'string conc-name-prefix 
                                                   (symbol-name slot-name))
                                      package)))
            ;; Record accessor function definition
            (record-definition *current-tracker* accessor-name
                             :FUNCTION
                             (file parser)
                             :package (current-package-name parser)
                             :exported-p (eq (nth-value 1 
                                           (find-symbol (symbol-name accessor-name) 
                                                      package))
                                          :external))
            ;; Analyze the full slot form recursively
            (when (listp slot)
              (analyze-definition-form parser slot))))

        ;; Analyze all structure options recursively
        (when (listp name-and-options)
          (dolist (option (cdr name-and-options))
            (analyze-definition-form parser option)))))))


(defun analyze-defvar-defparameter-defconstant (parser form)
  "Handle defvar, defparameter, and defconstant forms.
   Records variable definition and analyzes initialization forms for nested definitions."
  (let* ((name (second form))
         (package (current-package parser)))
    ;; Record the variable definition
    (record-definition *current-tracker* name
                      :VARIABLE
                      (file parser)
                      :package (current-package-name parser)
                      :exported-p (eq (nth-value 1 
                                    (find-symbol (symbol-name name) package))
                                   :external))
    ;; Check for nested definitions in initialization form if present
    (when (cddr form)
      (analyze-definition-form parser (third form)))))


(defun analyze-defun-defmacro (parser form)
 "Handle defun/defmacro forms. Analyzes lambda list for type declarations and looks
  for nested definitions in function body."
 (let* ((operator (car form))
        (name (second form))
        (lambda-list (third form))
        (body (cdddr form))
        (package (current-package parser)))
   ;; Record the main function/macro definition
   (record-definition *current-tracker* name
                     (if (eq operator 'defun)
                         :FUNCTION 
                         :MACRO)
                     (file parser)
                     :package (current-package-name parser)
                     :exported-p (eq (nth-value 1 
                                   (find-symbol (symbol-name name) package))
                                  :external))
   ;; Analyze lambda list for type declarations and default value definitions
   (analyze-lambda-list parser lambda-list)
   ;; Look for definitions in body forms
   (dolist (form body)
     (analyze-definition-form parser form))))


(defun analyze-lambda-list (parser lambda-list)
 "Analyze a lambda list for type declarations and default values that 
  could create dependencies."
 (let ((state :required))
   (dolist (item lambda-list)
     (case item
       ((&optional &rest &key &aux)
        (setf state item))
       ((&whole &environment &allow-other-keys)
        nil)
       (t (case state
            (:required 
             (typecase item
               (symbol nil) ; Simple parameter
               (cons ; Destructuring or type declaration
                (when (> (length item) 1)
                  ;; Check for type declarations
                  (analyze-definition-form parser (second item))))))
            (&optional
             (when (and (listp item) (cddr item))
               (analyze-definition-form parser (third item)))) ; Default value
            (&key
             (when (listp item)
               (let ((key-item (if (listp (car item))
                                  (cadar item)
                                  (car item))))
                 (when (and (listp item) (cddr item))
                   (analyze-definition-form parser (third item)))))) ; Default value
            (&aux
             (when (and (listp item) (cdr item))
               (analyze-definition-form parser (second item))))))))))


(defun analyze-defgeneric (parser form)
 "Handle defgeneric form. Records generic function definition and analyzes
  lambda list and options for nested definitions."
 (let* ((name (second form))
        (lambda-list (third form))
        (options (nthcdr 3 form))
        (package (current-package parser)))
   ;; Record the generic function definition
   (record-definition *current-tracker* name
                     :GENERIC-FUNCTION
                     (file parser)
                     :package (current-package-name parser)
                     :exported-p (eq (nth-value 1 
                                   (find-symbol (symbol-name name) package))
                                  :external))
   ;; Analyze lambda list for type declarations 
   (analyze-lambda-list parser lambda-list)
   ;; Process generic function options
   (loop for option in options
         when (listp option) do
         (case (car option)
           (:argument-precedence-order
            ;; Arguments are just symbols, no need to analyze
            nil)
           (:method-combination
            ;; Analyze method combination name and arguments
            (analyze-definition-form parser (second option))
            (dolist (arg (cddr option))
              (analyze-definition-form parser arg)))
           (:method
            ;; Analyze method qualifiers, specializers and body
            (let ((method-qualifiers nil)
                  (method-lambda-list nil)
                  (rest-forms (cdr option)))
              ;; Collect qualifiers
              (loop while (and rest-forms (not (listp (car rest-forms))))
                    do (push (pop rest-forms) method-qualifiers))
              ;; Get lambda list and analyze it
              (when rest-forms
                (setf method-lambda-list (pop rest-forms))
                (analyze-lambda-list parser method-lambda-list)
                ;; Analyze method body
                (dolist (form rest-forms)
                  (analyze-definition-form parser form)))))))))


(defun analyze-defmethod (parser form)
"Handle defmethod form. Records method definition and analyzes specializers,
 lambda list and body forms for nested definitions."
(let* ((spec (cdr form))
       (name (car spec))
       (qualifiers (loop while (and (cdr spec) 
                                  (not (listp (cadr spec))))
                       collect (cadr spec)
                       do (setf spec (cdr spec))))
       (lambda-list (car (last spec 2)))
       (body (car (last spec)))
       (package (current-package parser)))
  ;; Record the method definition
  (record-definition *current-tracker* name
                   :METHOD
                   (file parser)
                   :package (current-package-name parser)
                   :exported-p (eq (nth-value 1 
                                 (find-symbol (symbol-name name) package))
                                :external))
  ;; Analyze each parameter's specializer for definitions
  (dolist (param lambda-list)
    (when (listp param)
      (cond
        ;; Standard specializer - (param type)
        ((= (length param) 2)
         (analyze-definition-form parser (second param)))
        ;; EQL specializer - (param (eql form))
        ((and (= (length param) 2)
              (listp (second param))
              (eq (first (second param)) 'eql))
         (analyze-definition-form parser (second (second param)))))))
  ;; Analyze lambda list for type declarations
  (analyze-lambda-list parser lambda-list)
  ;; Analyze method body
  (dolist (form body)
    (analyze-definition-form parser form))))


(defun analyze-define-condition (parser form)
  "Handle define-condition forms, recording condition name, superclasses,
   and analyzing for nested definitions in all forms. Collects user-introduced
   keywords by flattening the entire form and removing standard keywords."
  (let ((standard-keywords '(:documentation :default-initargs :report
                           :reader :writer :accessor :allocation :initarg 
                           :initform :type)))
    (destructuring-bind (def-op name-and-options superclasses &rest options-and-slots) form
      (declare (ignore def-op))
      ;; Handle both simple name and (name options...) forms
      (let* ((name (if (and (listp name-and-options)
                           (symbolp (car name-and-options))) 
                      (car name-and-options)
                      name-and-options))
             (name-options (when (and (listp name-and-options)
                                    (symbolp (car name-and-options)))
                            (cdr name-and-options)))
             (package (current-package parser))
             ;; Get conc-name for accessor generation 
             (conc-name-option (find :conc-name name-options :key #'car))
             (conc-name (cond ((null conc-name-option)
                              (concatenate 'string (symbol-name name) "-"))
                             ((null (second conc-name-option))
                              "")
                             (t (let ((prefix (second conc-name-option)))
                                  (if (symbolp prefix)
                                      (symbol-name prefix)
                                      prefix)))))
             (export-p (and (find :export name-options :key #'car)
                           (cadr (find :export name-options :key #'car))))
             ;; Get user-introduced keywords from entire form
             (user-keywords (set-difference (remove-if-not #'keywordp 
                                                         (alexandria:flatten form))
                                          standard-keywords)))

        ;; Record the condition definition with collected user keywords
        (record-definition *current-tracker* name
                          :CONDITION
                          (file parser)
                          :package (current-package-name parser)
                          :exported-p (or export-p 
                                        (eq (nth-value 1 
                                            (find-symbol (symbol-name name) package))
                                           :external))
                          :user-data user-keywords)

        ;; Analyze superclasses for nested definitions
        (dolist (super superclasses)
          (analyze-definition-form parser super))

        ;; Process options and slots
        (loop for option in options-and-slots
              when (and (listp option) (keywordp (car option)))
              do (case (car option)
                   (:report  
                    (analyze-definition-form parser (second option)))
                   (:default-initargs
                    (loop for (key val) on (cdr option) by #'cddr
                          do (analyze-definition-form parser val)))
                   (:documentation nil))
              when (and (listp option) (symbolp (car option)))
              do (let* ((slot-name (car option))
                       (slot-options (cdr option))
                       (accessor-name (intern (concatenate 'string conc-name 
                                                         (symbol-name slot-name))
                                           package)))
                   ;; Record accessor function definition if needed
                   (let ((reader-opt (find :reader slot-options :key #'car))
                         (accessor-opt (find :accessor slot-options :key #'car)))
                     (cond 
                       (reader-opt
                        ;; Record the reader function and check its form for definitions
                        (record-definition *current-tracker* (second reader-opt)
                                         :FUNCTION
                                         (file parser)
                                         :package (current-package-name parser)
                                         :exported-p export-p)
                        (when (cddr reader-opt)
                          (analyze-definition-form parser (third reader-opt))))
                       (accessor-opt
                        ;; Record the accessor function and check its form for definitions
                        (record-definition *current-tracker* (second accessor-opt)
                                         :FUNCTION
                                         (file parser)
                                         :package (current-package-name parser)
                                         :exported-p export-p)
                        (when (cddr accessor-opt)
                          (analyze-definition-form parser (third accessor-opt))))
                       (t
                        ;; Record default accessor
                        (record-definition *current-tracker* accessor-name
                                         :FUNCTION
                                         (file parser)
                                         :package (current-package-name parser)
                                         :exported-p export-p))))
                   ;; Analyze slot options for nested definitions
                   (loop for (key value) on slot-options by #'cddr
                         do (case key
                              (:type (analyze-definition-form parser value))
                              (:initform (analyze-definition-form parser value))
                              (otherwise (analyze-definition-form parser value))))))))))


(defun analyze-define-method-combination (parser form)
  "Handle define-method-combination forms. Records the definition and analyzes all parts
   for nested definitions including group names, patterns, and option values. Supports 
   both short and long forms."
  (destructuring-bind (def-op name &rest args) form
    (declare (ignore def-op))
    (let ((package (current-package parser)))
      ;; Record the method combination definition
      (record-definition *current-tracker* name
                        :FUNCTION
                        (file parser)
                        :package (current-package-name parser)
                        :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name)
                                                 package))
                                     :external))
      (if (and (car args) (listp (car args)))
          ;; Long form
          (destructuring-bind (lambda-list &rest body) args
            ;; Analyze lambda list for type declarations and defaults
            (analyze-lambda-list parser lambda-list)
            ;; Process all body forms including method group specs
            (dolist (form body)
              (if (and (listp form) 
                       (member (car form) '(:arguments :generic-function :documentation)))
                  ;; Handle option forms - analyze all parts
                  (dolist (part (cdr form))
                    (analyze-definition-form parser part))
                  ;; Handle method group spec
                  (when (listp form)
                    (destructuring-bind (group-name pattern &rest group-options) form
                      ;; Analyze group name - could be user-defined
                      (analyze-definition-form parser group-name)
                      ;; Analyze pattern - could contain user predicates
                      (analyze-definition-form parser pattern)
                      ;; Analyze all option keywords and values
                      (loop for (key value) on group-options by #'cddr
                            do (analyze-definition-form parser key)
                            do (analyze-definition-form parser value)))))))
          ;; Short form - analyze all options and their values
          (loop for (option value) on args by #'cddr
                do (analyze-definition-form parser option)
                do (analyze-definition-form parser value))))))


(defun analyze-setf-expander (parser form)
  "Handle define-setf-expander forms, recording function definition and analyzing body."
  (destructuring-bind (def-op name lambda-list &rest body) form
    (declare (ignore def-op))
    (let ((package (current-package parser)))
      ;; Record the setf expander definition as a function
      (record-definition *current-tracker* name
                        :FUNCTION  
                        (file parser)
                        :package (current-package-name parser)
                        :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name)
                                                 package))
                                     :external))
      ;; Analyze lambda list for references
      (analyze-lambda-list parser lambda-list)
      ;; Analyze body forms 
      (analyze-body-with-declarations parser body))))


(defun analyze-define-modify-macro (parser form)
 "Handle define-modify-macro forms, recording function and analyzing lambda list."
 (destructuring-bind (def-op name lambda-list function &optional documentation) form
   (declare (ignore def-op documentation))
   (let ((package (current-package parser)))
     ;; Record the macro definition
     (record-definition *current-tracker* name
                       :MACRO 
                       (file parser)
                       :package (current-package-name parser)
                       :exported-p (eq (nth-value 1 
                                     (find-symbol (symbol-name name)
                                                package))
                                    :external))
     ;; Analyze lambda list for references
     (analyze-lambda-list parser lambda-list)
     ;; Analyze function reference
     (analyze-definition-form parser function))))


(defun analyze-define-compiler-macro (parser form)
 "Handle define-compiler-macro forms, recording macro and analyzing body."
 (destructuring-bind (def-op name lambda-list &rest body) form
   (declare (ignore def-op))
   (let ((package (current-package parser)))
     ;; Record the compiler macro definition
     (record-definition *current-tracker* name
                       :MACRO
                       (file parser)
                       :package (current-package-name parser)
                       :exported-p (eq (nth-value 1 (find-symbol (symbol-name name) package))
                                       :external))
     ;; Analyze lambda list for references
     (analyze-lambda-list parser lambda-list)
     ;; Analyze body forms
     (analyze-body-with-declarations parser body))))


(defun analyze-setf-symbol-value (parser form)
  "Handle (setf (symbol-value 'name) value) definitions.
   Form structure: (setf (symbol-value 'name) value-form)"
  (let ((name (cadr (cadr (cadr form))))        ; Extract symbol from 'symbol form
        (value-form (third form))
        (package (current-package parser)))
    ;; Record the variable definition  
    (record-definition *current-tracker* name
                      :VARIABLE 
                      (file parser)
                      :package (current-package-name parser)
                      :exported-p (eq (nth-value 1 
                                    (find-symbol (symbol-name name) package))
                                   :external))
    ;; Analyze the value form for nested definitions
    (analyze-definition-form parser value-form)))


(defun analyze-setf-symbol-function-fdefinition-macro-function (parser form)
  "Handle (setf (symbol-function|fdefinition|macro-function 'name) function-form) definitions.
   Form structure: (setf (accessor 'name) function-form)"
  (let ((accessor (caadr form))           ; symbol-function, fdefinition, or macro-function
        (name (cadr (cadr (cadr form))))         ; Get symbol from ('accessor 'name)
        (function-form (third form))      ; The function/lambda form
        (package (current-package parser)))
    (cond
      ;; Handle symbol-function and fdefinition
      ((member accessor '(symbol-function fdefinition))
       (when (and (listp function-form)
                  (eq (car function-form) 'lambda))
         ;; Record function definition
         (record-definition *current-tracker* name
                          :FUNCTION
                          (file parser)
                          :package (current-package-name parser)
                          :exported-p (eq (nth-value 1 
                                        (find-symbol (symbol-name name) package))
                                       :external))
         ;; Analyze lambda list and body
         (analyze-lambda-list parser (cadr function-form))
         (dolist (form (cddr function-form))
           (analyze-definition-form parser form))))
      ;; Handle macro-function
      ((eq accessor 'macro-function)
       (when (listp function-form)  ; Macro function might not use lambda directly
         ;; Record macro definition
         (record-definition *current-tracker* name
                          :MACRO
                          (file parser)
                          :package (current-package-name parser)
                          :exported-p (eq (nth-value 1 
                                        (find-symbol (symbol-name name) package))
                                       :external))
         ;; Analyze the function body
         (dolist (form (cdr function-form))
           (analyze-definition-form parser form)))))))