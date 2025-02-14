;;;; Filename:  reference-analysis.lisp

;;;; Detect references to definitions in the user's project files (except method references).
;;;; Method references are dealt with separately in method-reference-analysis.lisp.


(in-package :dep)


(alexandria:define-constant +def*-forms+
  '(defun defvar defparameter defconstant defclass defstruct defmethod defgeneric 
    define-condition defmacro define-method-combination define-modify-macro
    define-compiler-macro define-symbol-macro defsetf define-setf-expander deftype)
  :test #'equal
  :documentation "All the def* forms allowed in common lisp")


(defun parse-references-in-file (parser)
  "Second pass parser that records references to previously recorded definitions.
   Processes in-package forms to maintain proper package context, 
   then analyzes all forms for references."
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
               (analyze-reference-form parser form)))))


(defun analyze-reference-form (parser top-level-form)
  "Analyze source top-level-forms, recording references to definitions in different files."
  (let (;(gf-top-level-form (copy-list top-level-form))  
        (gf-p nil))  
    (labels ((handle-reference (current-form context parent-context)
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
                 (when (symbolp current-form)
                   ;; Process a symbol reference
                   (unless (or (cl-symbol-p current-form)
                               (def*-name-p current-form context))
                     (if (potential-gf-reference-p current-form context)
                       ;; For potential generic function references in form, just set flag and continue walking
                       (setf gf-p t)
                       ;; For non-generic functions, record reference if it has other-file-defs
                       (let* ((sym-type (get-symbol-reference-type current-form))
                              (sym-pkg (symbol-package current-form))
                              (key (make-tracking-key current-form sym-pkg sym-type))
                              (defs (gethash key (slot-value *current-tracker* 'definitions)))
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
      ;; Then analyze any method refs found
      (when gf-p
        (analyze-method-references parser top-level-form)))))


(defun potential-gf-reference-p (sym context)
  "May include some symbols that masquerade as gf references--eg, bound var in let statement."
  (and (ignore-errors (typep (fdefinition sym) 'generic-function))  ;sym must name a gf
       (not (and (consp context)  ;exclude defgeneric and defmethod name position (but already excluded in def* exclusion earlier)
                 (member (first context) '(defgeneric defmethod))
                 (eq sym (second context))))
       (not (loop for (x y) on context  ;exclude slot method implicit definitions for defclass & define-condition
                  thereis (and (member x '(:reader :writer :accessor))
                               (eq y sym))))))


(defun record-reference (tracker &key name file type package context visibility definitions
                                     qualifiers arguments)
  "Record a name reference in the tracker.
   VISIBILITY is inherited, imported, or local (defaults to local)
   DEFINITIONS is a non-empty list of definitions this reference depends on
   QUALIFIERS tracks method qualifiers in the ref 
   ARGUMENTS is a list of argument values"
  (let* ((pkg-name (etypecase name
                     (string name) 
                     (symbol (when (symbol-package name)
                              (package-name (symbol-package name))))))
         (specializers (when (and (eq type :method) 
                                definitions)
                        (definition.specializers (first definitions))))
         (key (if (eq type :method)
                 (make-tracking-key name pkg-name type qualifiers specializers)
                 (make-tracking-key name pkg-name type)))
         (ref (make-instance 'reference 
                          :name name 
                          :file file
                          :type type
                          :package package 
                          :context context 
                          :visibility visibility
                          :definitions definitions
                          :qualifiers qualifiers
                          :arguments arguments)))
   (pushnew ref (gethash key (slot-value tracker 'references))
            :test #'equalp)
   ref))


(defun def*-name-p (sym context)
  "Return T if sym is the name of a def* definition."
  (and (consp context)
       (member (car context) +def*-forms+)
       (eq (second context) sym)))


(defun get-symbol-reference-type (sym)
  "Derive the reference type of a symbol based on its bindings and usage context."
  (cond
    ;; Note that the order of clauses is important
    ((and (not (macro-function sym))     ; Not a regular macro
          (not (boundp sym))            ; Not a bound variable
          (not (fboundp sym))          ; Not a function
          (multiple-value-bind (expansion expanded-p)
              (ignore-errors (macroexpand-1 sym))
            (declare (ignore expansion))
            expanded-p))                ; Successfully expands
     :symbol-macro)
    ((boundp sym) 
     :variable)
    ((macro-function sym)  ;; Check for macro before function since macros have function bindings
     :macro)
    ((fboundp sym)  ;; Check for function, already excluded symbol-macro, macro, variable
     :function)
    ((ignore-errors (find-class sym))    ;; Check for classes and structures
     :structure/class/condition)
    ((and (subtypep sym t)    ;; Check for type declarations that aren't classes
          (not (ignore-errors (find-class sym)))
          (not (cl-symbol-p sym)))
     :deftype)))


(defun get-visibility (sym current-pkg)
  "Finds the visibility of a symbol--eg, (:inherited #<PACKAGE \"PKG1\">)"
  (let ((sym-pkg (symbol-package sym)))
    (if (eql sym-pkg current-pkg)
      (list :local current-pkg)
      (let ((usedp (member sym-pkg (package-use-list current-pkg)))
            (importedp (or (member sym (package-internal-symbols current-pkg))
                           (member sym (package-external-symbols current-pkg)))))
        (cond ((and usedp (not importedp)) (list :inherited current-pkg))
              ((and (not usedp) importedp) (list :imported current-pkg))
              ((and usedp importedp)       (list :inherited :imported current-pkg))
              (t                           (list :not-visible current-pkg)))))))


(defun package-external-symbols (pkg)
  "Return a list of all external symbols in PKG."
  (let (acc)
    (do-external-symbols (sym pkg)
      (push sym acc))
    (nreverse acc)))


(defun package-internal-symbols (pkg)
  "Return a list of all internal symbols in PKG.
By 'internal' here we mean symbols accessible from PKG but not exported.
DO-SYMBOLS iterates over all symbols accessible from PKG, including inherited ones."
  (let (acc)
    (do-symbols (sym pkg)
      ;; Check that the symbol is actually home to PKG (so we don't pick up inherited ones),
      ;; and that it is not exported.
      (when (and (eq (symbol-package sym) pkg)
                 (not (symbol-exported-p sym pkg)))
        (push sym acc)))
    (nreverse acc)))


(defun symbol-exported-p (sym pkg)
  "Return T if SYM is exported from PKG; otherwise NIL."
  (eq (nth-value 1 (find-symbol (symbol-name sym) pkg)) :external))