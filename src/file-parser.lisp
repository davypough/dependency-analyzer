;;;; Filename: file-parser.lisp
;;;
;;; Core file parsing functionality for dependency analysis.
;;; Analyzes source files to track symbol definitions, references,
;;; and package relationships without expanding macros.


(in-package #:dep)


(defmethod parse-file ((parser file-parser))
  "Parse a single Lisp source file for definitions and references."
  (with-slots (file package parsing-files) parser
    ;; Record any cycles but continue parsing
    (record-file-dependency-cycle parser file)
    (push file parsing-files)
    
    (with-open-file (stream file :direction :input)
      (let ((*package* package))
        (loop for form = (read stream nil nil)
              while form
              do (analyze-form parser form))))
    
    ;; Remove file from parsing stack
    (pop parsing-files)))


(defmethod analyze-form ((parser file-parser) form)
  "Analyze a form for definitions and references."
  (etypecase form
    (list 
     (when (symbolp (car form))
       (let ((operator (car form)))
         (case operator
           ;; Package context must be handled first
           (in-package (analyze-in-package parser form))
           (defpackage (analyze-package-definition parser form))
           
           ;; Structure and class definitions need special handling
           (defstruct (analyze-struct-definition parser form))
           
           ;; Global setf definitions need special handling
           (setf (analyze-setf-definition parser form))
           
           ;; Basic definitions unified under analyze-basic-definition
           ((defun defmacro defvar defparameter defconstant 
             defgeneric defmethod define-condition)
            (analyze-basic-definition parser form))
           
           ;; All binding forms
           ((let let* flet labels macrolet symbol-macrolet)
            (analyze-binding-form parser form (car form)))
           
           ;; Otherwise, it's a function call 
           (otherwise (analyze-function-call parser form))))))
    (symbol
     (analyze-subform parser form))))


(defun analyze-struct-definition (parser form)
  "Handle structure definitions, including options and slot definitions."
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
           ;; Get real slots list - if first item looks like an option, start options list
           (real-slots (if (and (consp (car slots))
                              (keywordp (caar slots)))
                          (progn
                            (setf options (append options (list (car slots))))
                            (cdr slots))
                          slots))
           (package (current-package parser))
           ;; Parse structure options
           (conc-name-option (find :conc-name options :key #'car))
           (conc-name (cond ((null conc-name-option) 
                            (concatenate 'string (symbol-name name) "-"))
                           ((null (second conc-name-option)) 
                            "")
                           (t 
                            (let ((prefix (second conc-name-option)))
                              (if (symbolp prefix)
                                  (symbol-name prefix)
                                  prefix)))))
           (include (find :include options :key #'car)))
      
      ;; Record the structure type definition
      (record-definition *current-tracker* name
                        :STRUCTURE
                        (file parser)
                        :package (current-package-name parser)
                        :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name)
                                                 package))
                                     :external))
      
      ;; Handle included structure slots if present
      (when include
        (analyze-subform parser (cadr include)))
      
      ;; Process each slot
      (dolist (slot real-slots)
        (let* ((slot-name (if (listp slot) (car slot) slot))
               (accessor-name-str (concatenate 'string conc-name (symbol-name slot-name)))
               (accessor-symbol (intern accessor-name-str package)))
          
          ;; Record accessor function definition (new)
          (record-definition *current-tracker* accessor-symbol
                           :FUNCTION
                           (file parser)
                           :package (current-package-name parser)
                           :exported-p (eq (nth-value 1 
                                         (find-symbol accessor-name-str package))
                                        :external))
          
          ;; Analyze slot options if present
          (when (listp slot)
            (loop for (key value) on (cddr slot) by #'cddr
                  ;; Skip CL types in type declarations
                  unless (and (eq key :type)
                            (symbolp value)
                            (eq (symbol-package value) 
                                (find-package :common-lisp)))
                  when value do (analyze-subform parser value))))))))


(defun analyze-body-with-declarations (parser body)
 "Analyze body forms handling declarations and doc strings."
 (let ((forms body)
       (declarations nil))
   ;; Skip documentation string if present
   (when (and (stringp (car forms)) (cdr forms))
     (pop forms))
   
   ;; Collect declarations
   (loop while (and (consp (car forms))
                    (eq (caar forms) 'declare))
         do (push (pop forms) declarations))
   
   ;; Process declarations
   (when declarations
     (multiple-value-bind (specials ignores types)
         (process-declarations (mapcan #'cdr (nreverse declarations)) 
                             parser)
       (declare (ignore specials ignores types))))
   
   ;; Analyze remaining body forms
   (analyze-rest parser forms)))


(defun analyze-loop-with-clause (parser clause bindings)
 "Handle LOOP WITH clause bindings."
 (typecase clause
   (symbol 
    (push clause bindings))
   (cons
    (cond
      ;; (with var = form)
      ((eq (cadr clause) '=)
       (push (car clause) bindings)
       (analyze-subform parser (caddr clause)))
      ;; (with (var1 var2))
      ((every #'symbolp clause)
       (dolist (var clause)
         (push var bindings)))
      ;; (with (var1 . var2))
      ((consp (car clause))
       (labels ((collect-vars (tree)
                  (cond ((null tree) nil)
                        ((symbolp tree) (push tree bindings))
                        ((consp tree)
                         (collect-vars (car tree))
                         (collect-vars (cdr tree))))))
         (collect-vars (car clause))))))))


(defun analyze-loop-for-as-clause (parser clause bindings)
 "Handle LOOP FOR/AS clause bindings."
 (typecase clause
   (symbol 
    (push clause bindings))
   (cons
    (let ((var (car clause)))
      (typecase var
        ;; Destructuring var
        (cons
         (labels ((collect-vars (tree)
                    (cond ((null tree) nil)
                          ((symbolp tree) (push tree bindings))
                          ((consp tree)
                           (collect-vars (car tree))
                           (collect-vars (cdr tree))))))
           (collect-vars var)))
        ;; Simple var
        (symbol
         (push var bindings)))
      
      ;; Analyze the iteration spec
      (let ((rest (cdr clause)))
        (loop while rest do
          (case (car rest)
            ((in on across)
             (analyze-subform parser (cadr rest))
             (setf rest (cddr rest)))
            ((= being)
             (analyze-subform parser (cadr rest))
             (setf rest (cddr rest)))
            ((from upfrom downfrom to upto downto above below by)
             (analyze-subform parser (cadr rest))
             (setf rest (cddr rest)))
            (t (setf rest nil)))))))))


(defun analyze-let-form (parser form let-type)
  "Handle let and let* forms, establishing proper binding scope."
  (let ((bindings (second form))
        (body (cddr form))
        (old-bound (bound-symbols parser))
        (new-bound nil))
    
    ;; For let*, analyze each binding form in sequence, allowing previous bindings
    ;; to be visible in subsequent initialization forms
    (if (eq let-type 'let*)
        (dolist (binding (if (listp bindings) bindings nil))
          (let ((var (if (listp binding) (car binding) binding))
                (init-form (when (and (listp binding) (cdr binding))
                            (cadr binding))))
            ;; Analyze initialization form in current scope
            (when init-form
              (analyze-subform parser init-form))
            ;; Add this binding to scope before processing next binding
            (push var new-bound)
            (setf (bound-symbols parser)
                  (cons var (bound-symbols parser)))))
        
        ;; For regular let, analyze all initialization forms in original scope
        ;; before adding any new bindings
        (dolist (binding (if (listp bindings) bindings nil))
          (let ((var (if (listp binding) (car binding) binding))
                (init-form (when (and (listp binding) (cdr binding))
                            (cadr binding))))
            (when init-form
              (analyze-subform parser init-form))
            (push var new-bound))))
    
    ;; Add all bindings to scope for body analysis (for regular let)
    (when (eq let-type 'let)
      (setf (bound-symbols parser)
            (append new-bound (bound-symbols parser))))
    
    ;; Analyze the body with bindings in effect
    (analyze-rest parser body)
    
    ;; Restore original bindings
    (setf (bound-symbols parser) old-bound)))


(defun analyze-flet-form (parser form flet-type)
  "Handle flet and labels forms, establishing proper function binding scope."
  (let ((functions (second form))
        (body (cddr form))
        (old-bound (bound-symbols parser))
        (new-bound nil))
    
    ;; For labels, add function names to scope before analyzing any definitions
    (when (eq flet-type 'labels)
      (dolist (func functions)
        (let ((name (car func)))
          (push name new-bound)))
      (setf (bound-symbols parser)
            (append new-bound (bound-symbols parser))))
    
    ;; Analyze each function definition
    (dolist (func functions)
      (destructuring-bind (name lambda-list &rest func-body) func
        (let* ((bound (extract-lambda-bindings lambda-list parser))
               (prev-bound (bound-symbols parser))
               (declarations nil)
               (doc-string nil)
               (forms func-body))

          ;; Extract documentation string if present
          (when (and (stringp (car forms)) (cdr forms))
            (setf doc-string (pop forms)))
          
          ;; Extract declarations that might reference other labels functions
          (loop while (and (consp (car forms))
                          (eq (caar forms) 'declare))
                do (push (pop forms) declarations))

          ;; Add function name binding for flet
          (when (eq flet-type 'flet)
            (push name new-bound))
          
          ;; Process declarations before analyzing body
          (when declarations
            (multiple-value-bind (specials ignores types)
                (process-declarations (mapcan #'cdr (nreverse declarations))
                                    parser)
              (declare (ignore specials ignores types))))

          ;; Analyze function body with parameter bindings
          (setf (bound-symbols parser)
                (append bound (bound-symbols parser)))
          (analyze-rest parser forms)
          
          ;; Restore bindings after function body
          (setf (bound-symbols parser) prev-bound))))
    
    ;; For flet, add function bindings now for the body
    (when (eq flet-type 'flet)
      (setf (bound-symbols parser)
            (append new-bound (bound-symbols parser))))
    
    ;; Analyze the body with function bindings in effect
    (analyze-rest parser body)
    
    ;; Restore original bindings
    (setf (bound-symbols parser) old-bound)))


(defun analyze-macrolet-form (parser form)
  "Handle macrolet forms, establishing proper macro binding scope."
  (let ((macros (second form))
        (body (cddr form))
        (old-bound (bound-symbols parser))
        (new-bound nil))
    
    ;; Add macro names to bound symbols
    (dolist (macro macros)
      (let ((name (car macro)))
        (push name new-bound)))
    
    ;; Analyze macro definitions (just for references, we don't expand macros)
    (dolist (macro macros)
      (destructuring-bind (name lambda-list &rest macro-body) macro
        (declare (ignore lambda-list))
        ;; We analyze the macro body just to catch references, but don't
        ;; try to track macro expansion
        (analyze-rest parser macro-body)))
    
    ;; Add macro bindings for body analysis
    (setf (bound-symbols parser)
          (append new-bound (bound-symbols parser)))
    
    ;; Analyze the body with macro bindings in effect
    (analyze-rest parser body)
    
    ;; Restore original bindings
    (setf (bound-symbols parser) old-bound)))


(defun user-defined-type-p (spec)
  "Return true if spec represents a user-defined type (not from CL package)."
  (typecase spec
    (symbol (not (find-symbol (symbol-name spec) (find-package :cl))))
    (cons (case (car spec)
            ((eql equal equalp member satisfies) nil)  ; Common predicates from CL
            (t t)))  ; Other compound specs might reference user types
    (t nil)))


(defmethod analyze-in-package ((parser file-parser) form)
  "Handle in-package forms by updating the current package context."
  (let* ((name (normalize-package-name (second form)))
         (package (or (find-package name)
                     (make-package name))))
    (setf (current-package parser) package
          (current-package-name parser) (package-name package)
          *package* package)))


(defmethod analyze-basic-definition ((parser file-parser) form)
  "Handle basic definition forms like defun and defvar."
  (let* ((operator (car form))
         (name (second form))
         (type (case operator
                (defun :FUNCTION)
                (defmacro :MACRO)  ;; Added this case
                ((defvar defparameter defconstant) :VARIABLE)
                (defgeneric :GENERIC-FUNCTION)  ;; Added this case 
                (defmethod :METHOD)  ;; Added this case
                (define-condition :CONDITION)))  ;; Added this case
         (package (current-package parser)))
    
    ;; Record the definition
    (record-definition *current-tracker* name
                      type
                      (file parser)
                      :package (current-package-name parser)
                      :exported-p (eq (nth-value 1 (find-symbol (symbol-name name) package))
                                    :external))
    
    ;; For functions, handle the lambda list and declarations before analyzing body
    (if (eq operator 'defun)
        (let* ((lambda-list (third form))
               (body (cdddr form))
               (bound (extract-lambda-bindings lambda-list parser))
               (old-bound (bound-symbols parser))
               (declarations nil)
               (doc-string nil)
               (forms body))
          
          ;; Extract documentation string if present
          (when (and (stringp (car forms)) (cdr forms))
            (setf doc-string (pop forms)))
          
          ;; Extract declarations
          (loop while (and (consp (car forms))
                          (eq (caar forms) 'declare))
                do (push (pop forms) declarations))
          
          ;; Process declarations
          (multiple-value-bind (specials ignores types)
              (process-declarations (mapcan #'cdr (nreverse declarations)) 
                                  parser)
            ;; Add parameter bindings minus ignored ones
            (setf (bound-symbols parser)
                  (append (set-difference bound ignores)
                          (bound-symbols parser)))
            
            ;; Analyze the body with updated bindings
            (analyze-rest parser forms)
            
            ;; Restore old bindings
            (setf (bound-symbols parser) old-bound)))
        
        ;; For other definitions, just analyze the rest
        (analyze-rest parser (cddr form)))))


(defmethod analyze-setf-definition ((parser file-parser) form)
  "Handle setf forms that create global variable, function, or macro definitions."
  (let ((place (second form))
        (value (third form)))
    (when (and (listp place) (= (length place) 2))
      (let ((accessor (first place))
            (name-form (second place)))
        (when (and (symbolp accessor)
                  (member accessor '(symbol-value symbol-function 
                                  macro-function fdefinition)))
          ;; Handle both 'symbol and (quote symbol) forms
          (let ((sym (typecase name-form
                      (symbol name-form)  ; Handle bare symbol
                      (cons (and (eq (car name-form) 'quote)  ; Handle (quote sym)
                               (symbolp (cadr name-form))
                               (cadr name-form)))
                      (t nil))))
            (when sym
              (ecase accessor
                (symbol-value
                 (record-definition *current-tracker* sym
                                  :VARIABLE
                                  (file parser)
                                  :package (current-package-name parser)
                                  :exported-p (eq (nth-value 1 
                                                (find-symbol (symbol-name sym) 
                                                           (current-package parser)))
                                               :external)))
                ((symbol-function fdefinition)
                 (record-definition *current-tracker* sym
                                  :FUNCTION
                                  (file parser)
                                  :package (current-package-name parser)
                                  :exported-p (eq (nth-value 1
                                                (find-symbol (symbol-name sym)
                                                           (current-package parser)))
                                               :external)))
                (macro-function
                 (record-definition *current-tracker* sym
                                  :MACRO
                                  (file parser)
                                  :package (current-package-name parser)
                                  :exported-p (eq (nth-value 1
                                                (find-symbol (symbol-name sym)
                                                           (current-package parser)))
                                               :external))))))))
        ;; Always analyze the value form for references
        (analyze-subform parser value))))


(defmethod analyze-function-call ((parser file-parser) form)
"Handle function call forms by recording reference to operator and analyzing arguments."
(let ((operator (car form)))
  (when (symbolp operator)
    (let* ((pkg (or (symbol-package operator)
                   (current-package parser)))
           (bare-name (symbol-name operator))
           (sym (if (symbol-package operator)
                   operator
                   (or (find-symbol bare-name pkg)
                       (intern bare-name pkg))))
           (visibility (cond
                       ((null (symbol-package operator)) :LOCAL)  ; Uninterned symbol
                       ((eq pkg (current-package parser)) :LOCAL) ; In current package
                       (t (multiple-value-bind (sym status)
                              (find-symbol bare-name (current-package parser))
                            (declare (ignore sym))
                            (case status
                              (:inherited :INHERITED)
                              (:external :IMPORTED)
                              (otherwise :LOCAL)))))))
      ;; Record the reference specifically as a function call
      (record-reference *current-tracker* sym
               :CALL
               (file parser)
               :package (package-name pkg)
               :visibility visibility)))     ; Updated from :local
  ;; Analyze the arguments
  (analyze-rest parser (cdr form))))


(defmethod analyze-package-definition ((parser file-parser) form)
  "Handle package definition forms."
  (let ((name (normalize-package-name (second form))))
    (record-package-definition parser name (cddr form))))


(defmethod analyze-subform ((parser file-parser) form)
 "Analyze a single form for symbol references, recording only references to
  user-defined symbols within project packages."
 (typecase form
   (symbol 
    (let ((pkg (symbol-package form)))
      (unless (or (member form '(nil t))
                  (null pkg) ; uninterned symbols
                  (eq pkg (find-package :common-lisp))
                  (eq pkg (find-package :keyword))
                  (eq pkg (find-package :asdf))
                  (member form '(&optional &rest &body &key &allow-other-keys 
                               &aux &whole &environment))
                  (member form (bound-symbols parser))
                  ;; Skip symbols from system packages - ones that start with "SB-" are SBCL internals
                  (let ((pkg-name (package-name pkg)))
                    (or (string-equal "SB-" pkg-name :end2 (min 3 (length pkg-name)))
                        (member pkg-name '("ASDF" "UIOP" "ALEXANDRIA" "CLOSER-MOP" "CCL" 
                                         "EXTENSIONS" "EXT" "SYSTEM") 
                                :test #'string-equal))))
        (let* ((current-pkg (current-package parser))
               (pkg-name (if pkg 
                          (package-name pkg) 
                          (current-package-name parser)))
               (current-file (file parser))
               (visibility (cond 
                          ((null pkg) :LOCAL)  ; Uninterned symbol
                          ((eq pkg current-pkg) :LOCAL)  ; In current package
                          (t (multiple-value-bind (sym status)
                               (find-symbol (symbol-name form) current-pkg)
                             (declare (ignore sym))
                             (case status
                               (:inherited :INHERITED)
                               (:external :IMPORTED) 
                               (otherwise :LOCAL)))))))
          ;; Only record if symbol is defined in our project
          (when (or (gethash (make-tracking-key form pkg-name) 
                           (slot-value *current-tracker* 'definitions))
                   ;; Or if it's in one of our project packages
                   (gethash pkg-name (slot-value *current-tracker* 'package-uses)))
            (record-reference *current-tracker* form
                           :REFERENCE
                           current-file
                           :package pkg-name
                           :visibility visibility))))))
   (cons
    (analyze-form parser form))
   ;; Skip self-evaluating types
   ((or number character string package pathname array)
    nil)))


(defmethod analyze-rest ((parser file-parser) rest)
  "Analyze a body of code for symbol references."
  (mapc (lambda (form)
          (analyze-subform parser form))
        rest))


(defmethod record-package-definition ((parser file-parser) name options)
  "Record a package definition and handle its options."
  (let* ((pkg-name name)
         (package (or (find-package name)
                     (make-package name :use nil)))
         (current-packages (or (slot-value parser 'parsing-packages) nil)))
    ;; Check for package cycles
    (let ((position (member pkg-name current-packages :test #'string=)))
      (when position
        (let* ((cycle (cons pkg-name (ldiff current-packages position)))
               (chain (format nil "~{~A~^ -> ~}" (reverse cycle))))
          (record-package-cycle chain))))
    (push pkg-name (slot-value parser 'parsing-packages))
    (setf (current-package parser) package
          (current-package-name parser) pkg-name)
    (unwind-protect
        (progn
          ;; Process package options
          (dolist (opt options)
            (when (and (consp opt) (eq (car opt) :use))
              (dolist (used (cdr opt))
                (let ((used-pkg (find-package used)))
                  (when used-pkg
                    (use-package used-pkg package)
                    (record-package-use *current-tracker* pkg-name (package-name used-pkg))
                    (do-external-symbols (sym used-pkg)
                      (multiple-value-bind (s status)
                            (find-symbol (symbol-name sym) package)
                        (when (and s (eq status :INHERITED))
                          (record-reference *current-tracker* sym
                                          (if (and (fboundp sym) 
                                                   (not (macro-function sym))
                                                   (not (special-operator-p sym)))
                                            :CALL
                                            :REFERENCE)
                                          (file parser)
                                          :package (package-name used-pkg)
                                          :visibility :INHERITED)))))))))
          (dolist (opt options)
            (when (and (consp opt) (eq (car opt) :import-from))
              (let ((from-pkg-name (normalize-package-name (second opt)))
                    (from-pkg (find-package (second opt))))
                (when from-pkg
                  (record-package-use *current-tracker* pkg-name from-pkg-name)
                  (dolist (sym (cddr opt))
                    (let* ((name (normalize-symbol-name sym))
                           (from-sym (find-symbol name from-pkg)))
                      (when from-sym
                        (import from-sym package)
                        (record-reference *current-tracker* from-sym
                                        :REFERENCE
                                        (file parser)
                                        :package from-pkg-name
                                        :visibility :IMPORTED))))))))
          (dolist (opt options)
            (when (and (consp opt) (eq (car opt) :export))
              (dolist (sym (cdr opt))
                (let* ((name (normalize-symbol-name sym))
                       (exported-sym (intern name package)))
                  (export exported-sym package)
                  (record-export *current-tracker* pkg-name exported-sym))))))
      ;; Clean up package parsing state
      (pop (slot-value parser 'parsing-packages)))))


(defun record-file-dependency-cycle (parser file-name)
  "Record a file dependency cycle in the tracker."
  (let ((position (member file-name (parsing-files parser) :test #'equal)))
    (when position
      (let* ((cycle (cons file-name (ldiff (parsing-files parser) position)))
             (chain (format nil "~{~A~^ -> ~}" (reverse cycle))))
        ;; Record the cycle in the dependency tracker
        (record-file-cycle chain)))))