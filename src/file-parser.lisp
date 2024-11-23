;;;; Filename: file-parser.lisp
;;;
;;; Core file parsing functionality for dependency analysis.
;;; Analyzes source files to track symbol definitions, references,
;;; and package relationships without expanding macros.
;;; Also provides ASDF component parsing coordination.


(in-package #:dep)


(defmethod parse-definitions-in-file ((parser file-parser))
  "First pass parser that records definitions.
   Processes in-package forms to maintain proper package context,
   then records definitions for all other forms."
  (with-slots (file package parsing-files) parser
    (push file parsing-files)
    (with-open-file (stream file :direction :input)
      (let ((*package* package))
        (loop for form = (read stream nil nil)
              while form
              when (consp form)  
              do (if (eq (car form) 'in-package)
                   (analyze-in-package parser form)
                   (analyze-definition-form parser form)))))
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
  "Analyze a form for definitions only - packages, structures, functions, etc."
  (etypecase form
    (list 
     (when (symbolp (car form))
       (let ((operator (car form)))
         (case operator
           (defpackage (analyze-package-definition parser form))
           (defstruct (analyze-struct-definition parser form))
           ((defvar defparameter defconstant) (analyze-variable-definition parser form))
           ((defun defmacro) (analyze-function-or-macro-definition parser form))
           (defgeneric (analyze-generic-definition parser form))
           (defmethod (analyze-method-definition parser form))
           (define-condition (analyze-condition-definition parser form))
           (define-method-combination (analyze-method-combination-definition parser form))
           (define-setf-expander (analyze-setf-expander-definition parser form))
           (define-modify-macro (analyze-modify-macro-definition parser form))
           (define-compiler-macro (analyze-compiler-macro-definition parser form))
           (setf (when (and (cddr form)  ; Must have 3+ elements
                            (listp (second form)))
                   (case (caadr form)
                     ((symbol-value)
                        (analyze-variable-definition parser form))
                     ((symbol-function fdefinition macro-function)
                        (analyze-function-or-macro-definition parser form)))))))))
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
                (dotimes (i (length x))
                  (walk (aref x i) nil)))
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


(defmethod analyze-package-definition ((parser file-parser) form)
  "Record a package definition during first analysis pass.
   Form is (defpackage name options...)"
  (let ((name (normalize-package-name (second form))))
    (record-definition *current-tracker* name
                      :PACKAGE
                      (file parser)
                      :package "KEYWORD"  ; Package names are keywords
                      :exported-p t)))    ; Package names are always accessible


(defun analyze-struct-definition (parser form)
 "Handle structure definitions, recording structure name, slot accessors,
  slot keywords, type member symbols, and any nested definitions.
  Note: Does not analyze references - those are handled in second pass."
 (destructuring-bind (def-op name-and-options &rest slots) form
   (declare (ignore def-op))
   ;; Handle both simple name and (name options...) forms
   (let* ((name (if (and (listp name-and-options) 
                        (symbolp (car name-and-options)))
                    (car name-and-options)
                    name-and-options))
          (options (if (and (listp name-and-options)
                          (symbolp (car name-and-options)))
                      (cdr name-and-options)
                      nil))
          ;; Get real slots list - if first item looks like options, start options list
          (real-slots (if (and (consp (car slots))
                             (keywordp (caar slots)))
                         (progn
                           (setf options (append options (list (car slots))))
                           (cdr slots))
                         slots))
          (package (current-package parser))
          ;; Parse conc-name for accessor generation
          (conc-name-option (find :conc-name options :key #'car))
          (conc-name (cond ((null conc-name-option) 
                           (concatenate 'string (symbol-name name) "-"))
                          ((null (second conc-name-option)) 
                           "")
                          (t 
                           (let ((prefix (second conc-name-option)))
                             (if (symbolp prefix)
                                 (symbol-name prefix)
                                 prefix))))))
     ;; Record the structure type definition
     (record-definition *current-tracker* name
                       :STRUCTURE
                       (file parser)
                       :package (current-package-name parser)
                       :exported-p (eq (nth-value 1 
                                     (find-symbol (symbol-name name)
                                                package))
                                    :external))
     ;; Handle included structure if present - must analyze before slots
     (let ((include (find :include options :key #'car)))
       (when include
         ;; The included structure name might be a defstruct form
         (analyze-definition-form parser (cadr include))))
     ;; Process each slot to record accessor functions and slot keywords
     (dolist (slot real-slots)
       (let* ((slot-name (if (listp slot) (car slot) slot))
              (accessor-name-str (concatenate 'string conc-name (symbol-name slot-name)))
              (accessor-symbol (intern accessor-name-str package)))
         ;; Record accessor function definition
         (record-definition *current-tracker* accessor-symbol
                          :FUNCTION
                          (file parser)
                          :package (current-package-name parser)
                          :exported-p (eq (nth-value 1 
                                        (find-symbol accessor-name-str package))
                                       :external))
         ;; Record slot keyword
         (record-definition *current-tracker* slot-name
                          :FUNCTION  ; Keywords are functions
                          (file parser)
                          :package "KEYWORD"
                          :exported-p t)
         ;; For each slot, look for member type symbols to record and any nested definitions
         (when (listp slot)
           ;; Process slot options
           (loop for (key value) on (cddr slot) by #'cddr
                 when (eq key :type)
                 do (when (listp value)
                      ;; Record any member type symbols
                      (when (eq (car value) 'member)
                        (dolist (member (cdr value))
                          (typecase member
                            ((or symbol string)
                             (record-definition *current-tracker* member
                                             :FUNCTION  ; Keywords/symbols as functions
                                             (file parser)
                                             :package (if (keywordp member)
                                                        "KEYWORD"
                                                        (current-package-name parser))
                                             :exported-p t))))))
                 ;; Look for any nested definitions in slot values or type specs
                 do (analyze-definition-form parser value)))))
     ;; Check for any nested definitions in structure options
     (when (listp name-and-options)
       (dolist (option (cdr name-and-options))
         (analyze-definition-form parser option))))))


(defun analyze-variable-definition (parser form)
  "Record variable definitions and scan initialization forms for nested definitions.
   Handles defvar, defparameter, defconstant, and setf of symbol-value."
  (let ((operator (car form))
        (package (current-package parser)))
    (cond
      ;; Handle defvar/defparameter/defconstant
      ((member operator '(defvar defparameter defconstant))
       (let ((name (second form)))
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
      ;; Handle (setf (symbol-value 'name) value)
      ((eq operator 'setf)
       (let* ((place (second form))
              (sym (typecase (cadr place)
                    (symbol (cadr place))
                    (cons (and (eq (car (cadr place)) 'quote)
                             (cadr (cadr place)))))))
         (when sym
           ;; Record the variable definition
           (record-definition *current-tracker* sym
                            :VARIABLE
                            (file parser)
                            :package (current-package-name parser)
                            :exported-p (eq (nth-value 1 
                                          (find-symbol (symbol-name sym) package))
                                         :external))
           ;; Check for nested definitions in value form
           (analyze-definition-form parser (third form))))))))


(defun analyze-function-or-macro-definition (parser form)
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


(defun analyze-generic-definition (parser form)
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
            ;; Check argument names for potential definitions
            (dolist (arg (cdr option))
              (analyze-definition-form parser arg)))
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


(defun analyze-method-definition (parser form)
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


(defun analyze-condition-definition (parser form)
 "Handle define-condition forms, recording condition name, superclasses,
  slots with accessors, and analyzing for nested definitions in all forms."
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
                        (cadr (find :export name-options :key #'car)))))
     ;; Record the condition definition
     (record-definition *current-tracker* name
                       :CONDITION
                       (file parser)
                       :package (current-package-name parser)
                       :exported-p (or export-p 
                                     (eq (nth-value 1 
                                         (find-symbol (symbol-name name) package))
                                        :external)))
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
                           (otherwise (analyze-definition-form parser value)))))))))


(defun analyze-method-combination-definition (parser form)
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


(defun analyze-setf-expander-definition (parser form)
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


(defun analyze-modify-macro-definition (parser form)
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


(defun analyze-compiler-macro-definition (parser form)
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
