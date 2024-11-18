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
           ((defun defvar defparameter defconstant) 
            (analyze-basic-definition parser form))
           ((defgeneric defmethod)
            (if (eq operator 'defgeneric)
                (analyze-generic-definition parser form)
                (analyze-method-definition parser form)))
           (defstruct (analyze-struct-definition parser form))
           (defmacro (analyze-macro-definition parser form))
           (define-condition (analyze-condition-definition parser form)) 
           ((defpackage) (analyze-package-definition parser form))
           (setf (analyze-setf-definition parser form))
           ((let let*) (analyze-let-form parser form operator))
           ((flet labels) (analyze-flet-form parser form operator))
           ((macrolet) (analyze-macrolet-form parser form))
           ((multiple-value-bind)
            (analyze-multiple-value-bind-form parser form))
           ((destructuring-bind)
            (analyze-destructuring-bind-form parser form))
           ((do do*)
            (analyze-do-form parser form operator))
           ((loop)
            (analyze-loop-form parser form))
           ((define-method-combination)
            (analyze-method-combination-definition parser form))
           ((define-symbol-macro)
            (analyze-symbol-macro-definition parser form))
           ((symbol-macrolet)
            (analyze-symbol-macrolet-form parser form))
           ((with-slots)
            (analyze-with-slots-form parser form))
           ((with-accessors)
            (analyze-with-accessors-form parser form))
           ((block)
            (analyze-block-form parser form))
           ((return-from)
            (analyze-return-from-form parser form))
           ((tagbody)
            (analyze-tagbody-form parser form))
           ((go)
            (analyze-go-form parser form))
           (otherwise 
            (analyze-function-call parser form))))))
   (symbol
    (analyze-subform parser form))))


(defun analyze-struct-definition (parser form)
  "Handle structure definitions, including options and slot definitions.
   Records the type definition and all accessor functions that will be created."
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
           (include (find :include options :key #'car))
           (constructor (let ((override (find :constructor options :key #'car)))
                         (if override
                             (or (cadr override)
                                 (symbolicate "MAKE-" name))
                             (symbolicate "MAKE-" name))))
           (copier (let ((override (find :copier options :key #'car)))
                    (if override
                        (cadr override)
                        (symbolicate "COPY-" name)))))
      
      ;; Record the structure type definition
      (record-definition *current-tracker* name
                        :structure
                        (file parser)
                        :package (current-package-name parser)
                        :exported-p (eq (nth-value 1 
                                      (find-symbol (symbol-name name)
                                                 package))
                                     :external))
      
      ;; Record constructor
      (when constructor
        (record-definition *current-tracker* constructor
                          :function
                          (file parser)
                          :package (current-package-name parser)
                          :exported-p (eq (nth-value 1 
                                        (find-symbol (symbol-name constructor)
                                                   package))
                                       :external)))
      
      ;; Record copier
      (when copier
        (record-definition *current-tracker* copier
                          :function
                          (file parser)
                          :package (current-package-name parser)
                          :exported-p (eq (nth-value 1 
                                        (find-symbol (symbol-name copier)
                                                   package))
                                       :external)))
      
      ;; Handle included structure slots if present
      (when include
        (analyze-subform parser (cadr include)))
      
      ;; Process each slot
      (dolist (slot real-slots)
        (let* ((slot-name (if (listp slot) (car slot) slot))
               (accessor-name-str (concatenate 'string conc-name (symbol-name slot-name)))
               (accessor-symbol (intern accessor-name-str package)))
          
          ;; Record accessor function
          (record-definition *current-tracker* accessor-symbol
                           :function
                           (file parser)
                           :package (current-package-name parser)
                           :exported-p (eq (nth-value 1 
                                         (find-symbol (symbol-name accessor-symbol)
                                                    package))
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


(defun analyze-method-combination-definition (parser form)
  "Handle method combination definitions with their complex options."
  (destructuring-bind (def-op name &optional short-form-options &rest body) form
    (declare (ignore def-op))
    (let* ((package (current-package parser)))
      ;; Record the method combination definition
      (record-definition *current-tracker* name
                        :method-combination
                        (file parser)
                        :package (current-package-name parser)
                        :exported-p (eq (nth-value 1 (find-symbol (symbol-name name) package))
                                      :external))

      (if (listp short-form-options)
          ;; Long form definition
          (destructuring-bind (method-group-specs &rest options) body
            ;; Create a lambda-list from group vars and option vars
            (let* ((group-vars (mapcan (lambda (group-spec)
                                       (when (listp group-spec)
                                         (list (car group-spec))))
                                     method-group-specs))
                   (option-vars (mapcan (lambda (option)
                                        (when (eq (car option) :arguments)
                                          (copy-list (cdr option))))
                                      options))
                   (gf-var (cdr (assoc :generic-function options)))
                   (all-vars (append group-vars 
                                   option-vars
                                   (when gf-var (list gf-var))))
                   ;; Construct a defun form for consistent binding handling
                   (defun-form `(defun ,name ,all-vars
                                ,@(cddr body))))
              (analyze-basic-definition parser defun-form)))

          ;; Short form definition - just analyze options and body
          (progn
            (when short-form-options
              (analyze-subform parser short-form-options))
            (analyze-rest parser body))))))


(defun analyze-symbol-macro-definition (parser form)
 "Handle symbol macro definitions."
 (destructuring-bind (def-op name expansion) form
   (declare (ignore def-op))
   (let ((package (current-package parser)))
     ;; Record the symbol macro definition
     (record-definition *current-tracker* name
                       :symbol-macro
                       (file parser)
                       :package (current-package-name parser)
                       :exported-p (eq (nth-value 1 (find-symbol (symbol-name name) package))
                                     :external))
     
     ;; Add to symbol macro bindings for future expansion
     (push (cons name expansion) (symbol-macro-bindings parser))
     
     ;; Analyze the expansion form
     (analyze-subform parser expansion))))


(defun analyze-macro-character-definition (parser form)
 "Handle reader macro character definitions."
 (destructuring-bind (set-op char fn &optional non-terminating-p) form
   (declare (ignore set-op non-terminating-p))
   ;; Analyze the character (though usually literal)
   (analyze-subform parser char)
   
   ;; If the function is a lambda form, analyze it
   (when (and (listp fn) (eq (car fn) 'lambda))
     (destructuring-bind (lambda lambda-list &rest body) fn
       (declare (ignore lambda))
       (let ((old-bound (bound-symbols parser)))
         ;; Add stream and char parameters to bindings
         (setf (bound-symbols parser)
               (append (extract-lambda-bindings lambda-list parser)
                      (bound-symbols parser)))
         
         ;; Analyze the reader macro body
         (analyze-rest parser body)
         
         ;; Restore bindings
         (setf (bound-symbols parser) old-bound))))
   
   ;; If the function is a reference, analyze it
   (when (symbolp fn)
     (analyze-subform parser fn))))


(defun analyze-dispatch-macro-definition (parser form)
 "Handle dispatch macro character definitions."
 (destructuring-bind (set-op disp-char sub-char fn) form
   (declare (ignore set-op))
   ;; Analyze the characters (though usually literal)
   (analyze-subform parser disp-char)
   (analyze-subform parser sub-char)
   
   ;; If the function is a lambda form, analyze it
   (when (and (listp fn) (eq (car fn) 'lambda))
     (destructuring-bind (lambda lambda-list &rest body) fn
       (declare (ignore lambda))
       (let ((old-bound (bound-symbols parser)))
         ;; Add stream, char, and arg parameters to bindings
         (setf (bound-symbols parser)
               (append (extract-lambda-bindings lambda-list parser)
                      (bound-symbols parser)))
         
         ;; Analyze the reader macro body
         (analyze-rest parser body)
         
         ;; Restore bindings
         (setf (bound-symbols parser) old-bound))))
   
   ;; If the function is a reference, analyze it
   (when (symbolp fn)
     (analyze-subform parser fn))))


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


(defun analyze-multiple-value-bind-form (parser form)
 "Handle multiple-value-bind forms, establishing variable bindings."
 (destructuring-bind (mvb vars value-form &rest body) form
   (declare (ignore mvb))
   (let ((old-bound (bound-symbols parser)))
     ;; First analyze the value-producing form
     (analyze-subform parser value-form)
     
     ;; Add variable bindings
     (setf (bound-symbols parser)
           (append vars (bound-symbols parser)))
     
     ;; Analyze body with bindings in effect
     (analyze-rest parser body)
     
     ;; Restore original bindings
     (setf (bound-symbols parser) old-bound))))


(defun analyze-destructuring-bind-form (parser form)
 "Handle destructuring-bind forms, establishing complex bindings."
 (destructuring-bind (db lambda-list value-form &rest body) form
   (declare (ignore db))
   (let ((old-bound (bound-symbols parser)))
     ;; First analyze the value-producing form
     (analyze-subform parser value-form)
     
     ;; Extract and add bindings from lambda list
     (let ((bound (extract-lambda-bindings lambda-list parser)))
       (setf (bound-symbols parser)
             (append bound (bound-symbols parser))))
     
     ;; Analyze body with bindings in effect
     (analyze-rest parser body)
     
     ;; Restore original bindings
     (setf (bound-symbols parser) old-bound))))


(defun analyze-do-form (parser form do-type)
 "Handle do/do* forms, establishing proper binding scope."
 (destructuring-bind (op var-specs end-test &rest body) form
   (declare (ignore op))
   (let ((old-bound (bound-symbols parser))
         (new-bound nil))
   
     ;; For do*, analyze each binding in sequence
     (if (eq do-type 'do*)
         (dolist (spec var-specs)
           (destructuring-bind (var &optional init step) 
               (if (listp spec) spec (list spec))
             ;; Analyze init form in current scope
             (when init
               (analyze-subform parser init))
             ;; Add this binding to scope before next spec
             (push var new-bound)
             (setf (bound-symbols parser)
                   (cons var (bound-symbols parser)))
             ;; Analyze step form with var bound
             (when step
               (analyze-subform parser step))))
         
         ;; For do, analyze all init forms before adding any bindings
         (dolist (spec var-specs)
           (destructuring-bind (var &optional init step)
               (if (listp spec) spec (list spec))
             ;; Analyze init form in original scope
             (when init
               (analyze-subform parser init))
             ;; Collect var for binding
             (push var new-bound)
             ;; Save step form for later analysis
             (when step
               (analyze-subform parser step)))))
     
     ;; For regular do, add all bindings now
     (when (eq do-type 'do)
       (setf (bound-symbols parser)
             (append new-bound (bound-symbols parser))))
     
     ;; Analyze end test
     (when end-test
       (destructuring-bind (test &rest result) end-test
         (analyze-subform parser test)
         (analyze-rest parser result)))
     
     ;; Analyze body with bindings in effect
     (analyze-rest parser body)
     
     ;; Restore original bindings
     (setf (bound-symbols parser) old-bound))))


(defun analyze-symbol-macrolet-form (parser form)
 "Handle symbol-macrolet forms, tracking expansions as special bindings."
 (destructuring-bind (op bindings &body body) form
   (declare (ignore op))
   (let ((old-bound (bound-symbols parser))
         (old-symbol-macros (symbol-macro-bindings parser)))
     ;; Process each binding
     (dolist (binding bindings)
       (destructuring-bind (name expansion) binding
         ;; Record expansion for reference
         (push (cons name expansion) (symbol-macro-bindings parser))
         ;; Analyze the expansion form for references
         (analyze-subform parser expansion)
         ;; Add name to bound symbols to prevent undefined warnings
         (push name (bound-symbols parser))))
     
     ;; Analyze body with bindings in effect
     (analyze-rest parser body)
     
     ;; Restore original state
     (setf (bound-symbols parser) old-bound
           (symbol-macro-bindings parser) old-symbol-macros))))


(defun analyze-with-slots-form (parser form)
 "Handle with-slots forms, tracking slot bindings."
 (destructuring-bind (op slot-entries instance &body body) form
   (declare (ignore op))
   (let ((old-bound (bound-symbols parser))
         (slot-names nil))
     
     ;; Analyze the instance form
     (analyze-subform parser instance)
     
     ;; Process slot specifications
     (dolist (entry slot-entries)
       (if (symbolp entry)
           ;; Simple slot name - same var name as slot
           (push entry slot-names)
           ;; (var-name slot-name) form
           (destructuring-bind (var-name slot-name) entry
             (declare (ignore slot-name))
             (push var-name slot-names))))
     
     ;; Add slot names to bindings
     (setf (bound-symbols parser)
           (append slot-names (bound-symbols parser)))
     
     ;; Analyze body with bindings in effect
     (analyze-rest parser body)
     
     ;; Restore original bindings
     (setf (bound-symbols parser) old-bound))))


(defun analyze-with-accessors-form (parser form)
 "Handle with-accessors forms, tracking accessor bindings."
 (destructuring-bind (op accessor-entries instance &body body) form
   (declare (ignore op))
   (let ((old-bound (bound-symbols parser))
         (accessor-names nil))
     
     ;; Analyze the instance form
     (analyze-subform parser instance)
     
     ;; Process accessor specifications
     (dolist (entry accessor-entries)
       (destructuring-bind (var-name accessor-name) entry
         ;; Record reference to the accessor function
         (analyze-subform parser accessor-name)
         ;; Add variable name to bindings
         (push var-name accessor-names)))
     
     ;; Add accessor names to bindings
     (setf (bound-symbols parser)
           (append accessor-names (bound-symbols parser)))
     
     ;; Analyze body with bindings in effect
     (analyze-rest parser body)
     
     ;; Restore original bindings
     (setf (bound-symbols parser) old-bound))))


(defun analyze-loop-form (parser form)
 "Handle loop forms, analyzing clauses and establishing proper binding scope."
 (let ((old-bound (bound-symbols parser))
       (new-bindings nil))
   (do* ((clauses (cdr form) (cdr clauses))
         (clause (car clauses) (car clauses)))
        ((null clauses))
     (when (and (symbolp clause) (cdr clauses))  ; Named clause
       (case clause
         ((with)
          (analyze-loop-with-clause parser (cadr clauses) new-bindings)
          (pop clauses))
         ((for as)
          (analyze-loop-for-as-clause parser (cadr clauses) new-bindings)
          (pop clauses))
         ((into)
          (push (cadr clauses) new-bindings)
          (pop clauses))
         ;; Skip other loop keywords
         ((initially finally do doing return collect collecting
           append appending nconc nconcing count counting sum summing
           maximize maximizing minimize minimizing if when unless
           while until repeat always never thereis
           in on = across from upfrom downfrom to upto downto above
           below by regarding then else end)
          nil)
         (t 
          ;; If not a keyword, analyze as a form
          (analyze-subform parser clause))))
     (when (consp clause)
       (analyze-subform parser clause)))
   
   ;; Add collected bindings to scope
   (setf (bound-symbols parser)
         (append new-bindings (bound-symbols parser)))
   
   ;; Restore original bindings
   (setf (bound-symbols parser) old-bound)))


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


(defun analyze-block-form (parser form)
 "Handle block forms, establishing block name binding."
 (destructuring-bind (block-op name &rest body) form
   (declare (ignore block-op))
   (let ((old-blocks (block-names parser)))
     ;; Add block name to scope
     (push name (block-names parser))
     
     ;; Analyze body with block name in scope
     (analyze-rest parser body)
     
     ;; Restore original block names
     (setf (block-names parser) old-blocks))))


(defun analyze-return-from-form (parser form)
 "Handle return-from forms, checking block name reference."
 (destructuring-bind (return-op block-name &optional value) form
   (declare (ignore return-op))
   ;; Check if block-name is bound
   (unless (member block-name (block-names parser))
     (record-anomaly *current-tracker*
                    :undefined-block
                    :error
                    (file parser)
                    (format nil "Return from undefined block ~A" block-name)
                    block-name))
   
   ;; Analyze the return value if present
   (when value
     (analyze-subform parser value))))


(defun analyze-tagbody-form (parser form)
 "Handle tagbody forms, establishing tag bindings."
 (let ((old-tags (go-tags parser))
       (new-tags nil))
   ;; First pass: collect all tags
   (dolist (item (cdr form))
     (when (atom item)  ; Tags are atoms (usually symbols or integers)
       (push item new-tags)))
   
   ;; Add tags to scope
   (setf (go-tags parser)
         (append new-tags (go-tags parser)))
   
   ;; Second pass: analyze all non-tag forms
   (dolist (item (cdr form))
     (unless (atom item)
       (analyze-subform parser item)))
   
   ;; Restore original tags
   (setf (go-tags parser) old-tags)))


(defun analyze-go-form (parser form)
 "Handle go forms, checking tag reference."
 (destructuring-bind (go-op tag) form
   (declare (ignore go-op))
   ;; Check if tag is bound
   (unless (member tag (go-tags parser))
     (record-anomaly *current-tracker*
                    :undefined-tag
                    :error
                    (file parser)
                    (format nil "GO to undefined tag ~A" tag)
                    tag))))


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
               (prev-bound (bound-symbols parser)))
          ;; Add function name binding for flet
          (when (eq flet-type 'flet)
            (push name new-bound))
          ;; Analyze function body with its parameter bindings
          (setf (bound-symbols parser)
                (append bound (bound-symbols parser)))
          (analyze-rest parser func-body)
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


(defmethod analyze-generic-definition ((parser file-parser) form)
  "Handle generic function definition forms."
  (destructuring-bind (defgeneric name lambda-list &rest options) form
    (let ((package (current-package parser)))
      ;; Record the generic function definition
      (record-definition *current-tracker* name
                        :generic-function
                        (file parser)
                        :package (current-package-name parser)
                        :exported-p (eq (nth-value 1 (find-symbol (symbol-name name) package))
                                      :external)
                        :context (list :lambda-list lambda-list))
      
      ;; Process main lambda list like a regular function
      (let ((defun-form `(defun ,name ,lambda-list)))
        (analyze-basic-definition parser defun-form))
      
      ;; Process :method options
      (dolist (option options)
        (when (and (listp option) (eq (car option) :method))
          (analyze-method-definition-internal parser name (cdr option)))))))


(defmethod analyze-method-definition ((parser file-parser) form)
  "Handle method definition forms."
  (destructuring-bind (defmethod name &rest rest) form
    (loop while (and rest (not (listp (car rest))))
          collect (pop rest) into qualifiers
          finally (return (analyze-method-definition-internal 
                          parser name qualifiers (car rest) (cdr rest))))))


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
    (let ((expansion (assoc form (symbol-macro-bindings parser))))
      (if expansion
          ;; If symbol has a macro expansion, analyze the expansion
          (analyze-subform parser (cdr expansion))
          ;; Otherwise, process normally if not in the skip list
          (unless (or (member form '(nil t))
                     (eq (symbol-package form) (find-package :common-lisp))
                     (eq (symbol-package form) (find-package :keyword))
                     (member form '(&optional &rest &body &key &allow-other-keys 
                                  &aux &whole &environment))
                     (member form (bound-symbols parser)))
            (let* ((pkg (symbol-package form))
                   (bare-name (symbol-name form))
                   (pkg-name (if pkg 
                               (package-name pkg) 
                               (current-package-name parser)))
                   (current-file (file parser)))
              ;; Only check for undefined symbols if not locally bound
              (let ((def (get-definitions form)))
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
                                form)))
              ;; Record the reference
              (record-reference *current-tracker* form
                              :reference
                              current-file
                              :package pkg-name))))))
   (cons
    (if (eq (car form) 'lambda)
        ;; Special handling for lambda forms
        (let* ((lambda-list (second form))
               (body (cddr form))
               (bound (extract-lambda-bindings lambda-list parser))
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