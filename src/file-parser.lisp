;;;; Filename: file-parser.lisp
;;;
;;; Core file parsing functionality for dependency analysis.
;;; Analyzes source files to track symbol definitions, references,
;;; and package relationships without expanding macros.


(in-package #:dep)


(defmethod parse-file ((parser file-parser))
  "Parse a single Lisp source file for definitions and references."
  (with-slots (file package parsing-files) parser
    (handler-case
        (progn
          ;; Record any cycles but continue parsing
          (record-file-dependency-cycle parser file)
          (push file parsing-files)
          
          (with-open-file (stream file :direction :input)
            (let ((*package* package))
              (loop for form = (read stream nil nil)
                    while form
                    do (analyze-form parser form))))
          
          ;; Remove file from parsing stack
          (pop parsing-files))
      (error (e)
        (pop parsing-files)
        (error 'file-parse-error
               :file file
               :reason e)))))


(defmethod analyze-form ((parser file-parser) form)
  "Analyze a form for definitions and references. Handles both lists and bare symbols."
  (etypecase form
    (list 
     (when (symbolp (car form))
       (let ((operator (car form)))
         (case operator
           (in-package (analyze-in-package parser form))
           ((defun defvar defparameter defconstant) (analyze-basic-definition parser form))
           (defmacro (analyze-macro-definition parser form))
           (defgeneric (analyze-generic-definition parser form))
           (defmethod (analyze-method-definition parser form))
           (define-condition (analyze-condition-definition parser form)) 
           ((defpackage) (analyze-package-definition parser form))
           (setf (analyze-setf-definition parser form))
           (otherwise (analyze-function-call parser form))))))
    (symbol
     (analyze-subform parser form))))


(defmethod analyze-generic-definition ((parser file-parser) form)
  "Handle generic function definition forms."
  (let* ((name (second form))
         (lambda-list (third form))
         (options (cdddr form))
         (package (current-package parser)))
    ;; Record the generic function definition
    (record-definition *current-tracker* name
                      :generic-function
                      (file parser)
                      :package (current-package-name parser)
                      :exported-p (eq (nth-value 1 (find-symbol (symbol-name name) package))
                                    :external)
                      :context (list :lambda-list lambda-list))
    
    ;; Process method options without analyzing lambda list
    (dolist (option options)
      (when (and (listp option) (eq (car option) :method))
        (analyze-method-definition-internal parser name (cdr option))))))


(defmethod analyze-method-definition ((parser file-parser) form)
  "Handle method definition forms."
  (destructuring-bind (defmethod name &rest rest) form
    ;; Extract qualifiers and lambda-list
    (let ((qualifiers nil)
          (lambda-list nil)
          (current rest))
      ;; Collect qualifiers until we hit the lambda-list
      (loop while (and current (not (listp (car current))))
            do (push (pop current) qualifiers))
      (setf qualifiers (nreverse qualifiers)
            lambda-list (pop current))
      
      (analyze-method-definition-internal parser name qualifiers lambda-list current))))


(defmethod analyze-generic-function-form ((parser file-parser) form)
  "Handle generic function and method definition forms."
  (destructuring-bind (def-type name &rest rest) form
    (let ((package (current-package parser)))
      (ecase def-type
        (defgeneric
         (destructuring-bind (lambda-list &rest options) rest
           ;; Record the generic function definition
           (record-definition *current-tracker* name
                            :generic-function
                            (file parser)
                            :package (current-package-name parser)
                            :exported-p (eq (nth-value 1 (find-symbol (symbol-name name) package))
                                          :external)
                            :context (list :lambda-list lambda-list))
           ;; Process :method options as method definitions
           (dolist (option options)
             (when (and (listp option) (eq (car option) :method))
               (analyze-method-definition-internal parser name (cdr option))))
           ;; Analyze other options for references
           (analyze-rest parser options)))
        (defmethod
         (analyze-method-definition-internal parser name rest))))))


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
                (defun :function)
                ((defvar defparameter defconstant) :variable)))
         (package (current-package parser)))
    (record-definition *current-tracker* name
                      type
                      (file parser)
                      :package (current-package-name parser)
                      :exported-p (eq (nth-value 1 (find-symbol (symbol-name name) package))
                                    :external))
    (analyze-rest parser (cddr form))))


(defmethod analyze-macro-definition ((parser file-parser) form)
  "Handle macro definition forms."
  (let* ((name (second form))
         (body (cdddr form))
         (package (current-package parser)))
    (record-definition *current-tracker* name
                      :macro
                      (file parser)
                      :package (current-package-name parser)
                      :exported-p (eq (nth-value 1 (find-symbol (symbol-name name) package))
                                    :external))
    (record-macro-body-symbols *current-tracker* name body)))


(defmethod analyze-condition-definition ((parser file-parser) form)
  "Handle condition definition forms."
  (let* ((name (second form))
         (package (current-package parser)))
    (record-definition *current-tracker* name
                      :condition
                      (file parser)
                      :package (current-package-name parser)
                      :exported-p (eq (nth-value 1 (find-symbol (symbol-name name) package))
                                    :external))
    (analyze-rest parser (cddr form))))


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
                                  :variable
                                  (file parser)
                                  :package (current-package-name parser)
                                  :exported-p (eq (nth-value 1 
                                                (find-symbol (symbol-name sym) 
                                                           (current-package parser)))
                                               :external)))
                ((symbol-function fdefinition)
                 (record-definition *current-tracker* sym
                                  :function
                                  (file parser)
                                  :package (current-package-name parser)
                                  :exported-p (eq (nth-value 1
                                                (find-symbol (symbol-name sym)
                                                           (current-package parser)))
                                               :external)))
                (macro-function
                 (record-definition *current-tracker* sym
                                  :macro
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
                        (intern bare-name pkg)))))
       ;; Record the reference regardless of what type of function it might be
       (record-reference *current-tracker* sym
                        :reference
                        (file parser)
                        :package (package-name pkg))))
   ;; Analyze the arguments
   (analyze-rest parser (cdr form))))


(defmethod analyze-package-definition ((parser file-parser) form)
  "Handle package definition forms."
  (let ((name (normalize-package-name (second form))))
    (record-package-definition parser name (cddr form))))


(defmethod analyze-subform ((parser file-parser) form)
 "Analyze a single form for symbol references. Handles all Common Lisp form types,
  recursively analyzing structures that might contain symbols."
 (typecase form
   (symbol 
    (unless (or (member form '(nil t))
                (eq (symbol-package form) (find-package :common-lisp))
                (eq (symbol-package form) (find-package :keyword))
                (member form '(&optional &rest &body &key &allow-other-keys 
                             &aux &whole &environment)))
      (let* ((pkg (symbol-package form))
             (bare-name (symbol-name form))
             (pkg-name (if pkg (package-name pkg) (current-package-name parser)))
             (current-file (file parser)))
        ;; Record the reference unless it's bound by an enclosing lambda
        (when (not (member form (bound-symbols parser)))
          (record-reference *current-tracker* form
                          :reference
                          current-file
                          :package pkg-name)
          ;; Only check for undefined symbols if not locally bound
          (let ((def (get-definitions *current-tracker* form)))
            (unless (or def
                       (and pkg 
                            (eq pkg (current-package parser))
                            (equal current-file 
                                   (and def (definition.file def)))))
              (record-anomaly *current-tracker*
                             :undefined-reference
                             :warning
                             current-file
                             (format nil "Reference to undefined symbol ~A::~A" 
                                    pkg-name bare-name)
                             form)))))))
   (cons
    (if (eq (car form) 'lambda)
        ;; Special handling for lambda forms
        (let* ((lambda-list (second form))
               (body (cddr form))
               (bound (extract-lambda-bindings lambda-list))
               (old-bound (bound-symbols parser)))
          ;; Add new bindings to parser state
          (setf (bound-symbols parser)
                (append bound (bound-symbols parser)))
          ;; Analyze the body with updated bindings
          (analyze-rest parser body)
          ;; Restore old bindings
          (setf (bound-symbols parser) old-bound))
        ;; Normal list analysis
        (analyze-form parser form)))
   (array
    (dotimes (i (array-total-size form))
      (analyze-subform parser (row-major-aref form i))))
   (hash-table
    (maphash (lambda (k v)
               (analyze-subform parser k)
               (analyze-subform parser v))
             form))
   (structure-object
    (dolist (slot (closer-mop:class-slots (class-of form)))
      (when (closer-mop:slot-boundp-using-class (class-of form) form slot)
        (analyze-subform parser (closer-mop:slot-value-using-class 
                                (class-of form) form slot)))))
   (standard-object
    (dolist (slot (closer-mop:class-slots (class-of form)))
      (when (closer-mop:slot-boundp-using-class (class-of form) form slot)
        (analyze-subform parser (closer-mop:slot-value-using-class 
                                (class-of form) form slot)))))
   ;; Self-evaluating objects - no analysis needed
   ((or number character string package pathname)
    nil)
   ;; Catch-all for any other types we might encounter
   (t
    nil)))


(defmethod analyze-rest ((parser file-parser) rest)
  "Analyze a body of code for symbol references."
  (mapc (lambda (form)
          (analyze-subform parser form))
        rest))


(defmethod record-macro-body-symbols ((parser file-parser) macro-name body)
  "Record all non-CL symbols in a macro body as potential dependencies."
  (labels ((collect-symbols (form)
             (typecase form
               (symbol 
                (unless (or (member form '(nil t))
                           (eq (symbol-package form) 
                               (find-package :common-lisp)))
                  (record-reference form
                                  :macro-body
                                  (file parser)
                                  :package (current-package-name parser))))
               (cons
                (collect-symbols (car form))
                (collect-symbols (cdr form))))))
    (collect-symbols body)))


(defmethod record-macro-dependencies ((parser file-parser) macro-def form)
  "Record that this code depends on the macro definition."
  (record-reference (definition.symbol macro-def)
                   :macro-expansion
                   (file parser)
                   :package (definition.package macro-def)))


(defmethod record-function-definition ((parser file-parser) name args body)
  "Record a function definition in the tracker."
  (let* ((package (current-package parser))
         (sym-name (normalize-symbol-name name))
         (sym (intern sym-name package)))
    (record-definition sym
                      :function 
                      (file parser)
                      :package (current-package-name parser)
                      :exported-p (eq (nth-value 1 (find-symbol sym-name package)) :external))))


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
                    ;; Record both inherited and call references for exported symbols
                    (do-external-symbols (sym used-pkg)
                      (multiple-value-bind (s status)
                          (find-symbol (symbol-name sym) package)
                        (when (and s (eq status :inherited))
                          ;; Record the inherited relationship
                          (record-reference *current-tracker* sym
                                          :inherited
                                          (file parser)
                                          :package (package-name used-pkg))
                          ;; Also record potential call reference if it's a function
                          (when (and (fboundp sym) 
                                   (not (macro-function sym))
                                   (not (special-operator-p sym)))
                            (record-reference *current-tracker* sym
                                            :call
                                            (file parser)
                                            :package (package-name used-pkg))))))))))
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
                                        :import
                                        (file parser)
                                        :package from-pkg-name))))))))
          (dolist (opt options)
            (when (and (consp opt) (eq (car opt) :export))
              (dolist (sym (cdr opt))
                (let* ((name (normalize-symbol-name sym))
                       (exported-sym (intern name package)))
                  (export exported-sym package)
                  (record-export *current-tracker* pkg-name exported-sym))))))
      ;; Clean up package parsing state
      (pop (slot-value parser 'parsing-packages))))))


(defun record-file-dependency-cycle (parser file-name)
  "Record a file dependency cycle in the tracker."
  (let ((position (member file-name (parsing-files parser) :test #'equal)))
    (when position
      (let* ((cycle (cons file-name (ldiff (parsing-files parser) position)))
             (chain (format nil "~{~A~^ -> ~}" (reverse cycle))))
        ;; Record the cycle in the dependency tracker
        (record-file-cycle chain)))))