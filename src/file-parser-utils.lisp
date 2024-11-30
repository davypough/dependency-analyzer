;;;; Filename: file-parser-utils.lisp
;;;
;;; Utility functions for parsing, package handling, and cycle detection.
;;; Contains pure functions for name normalization, symbol lookup,
;;; package option processing, and dependency cycle detection.

(in-package #:dep)


;;; Name and Symbol Utils


(defun normalize-package-name (designator)
  "Convert a package designator to a normalized string form.
   Handles package objects, symbols (interned or uninterned), and strings uniformly.
   Examples:
     'my-package  -> \"MY-PACKAGE\"
     #:my-package -> \"MY-PACKAGE\"
     \"MY-PACKAGE\" -> \"MY-PACKAGE\"
     #<PACKAGE MY-PACKAGE> -> \"MY-PACKAGE\""
  (typecase designator
    (package 
      (package-name designator))
    (string
      (string-upcase designator))
    (symbol  
      ;; Works for both interned and uninterned symbols
      (string-upcase (symbol-name designator)))
    (t
      (error "Invalid package designator: ~S" designator))))


(defun extract-lambda-bindings (lambda-list &optional parser)
  "Extract bindings from lambda list and optionally analyze initialization forms.
   Handles:
   - Regular function parameters
   - Generic function parameters 
   - Method specialized parameters ((var specializer))
   - Complex lambda lists with destructuring
   - &optional, &rest, &key parameters with defaults
   - &aux bindings
   
   Returns just the binding symbols, stripping:
   - Lambda list keywords
   - Specializer forms
   - Default value forms (which are analyzed if parser provided)"
  (let ((bindings nil)
        (state :required))
    (dolist (item lambda-list)
      (case item
        ((&optional &rest &key &aux)
         (setf state item))
        (&allow-other-keys
         nil)
        ((&whole &environment)
         (setf state item))
        (t (case state
             (:required 
              (setf bindings 
                    (append bindings
                            (typecase item
                              (symbol (list item))
                              (cons
                               (let ((var (if (and (listp (car item)) 
                                                  (= (length (car item)) 2))
                                            ;; Handle method specializers (var type)
                                            (caar item)
                                            (car item))))
                                 ;; Handle initialization form if present
                                 (when (and parser (cddr item))
                                   (analyze-subform parser (third item)))
                                 (extract-destructuring-bindings var)))))))
             (&optional
              (when (and parser (listp item) (cddr item))
                (analyze-subform parser (third item)))
              (setf bindings
                    (append bindings
                            (if (listp item)
                                (extract-destructuring-bindings (car item))
                                (list item)))))
             (&rest
              (typecase item
                (symbol 
                 (unless (member item lambda-list-keywords)
                   (push item bindings)))
                (cons 
                 (push (car item) bindings))))
             (&key
              (when (and parser (listp item) (cddr item))
                (analyze-subform parser (third item)))
              (setf bindings
                    (append bindings
                            (cond ((symbolp item)
                                   (list item))
                                  ((listp item)
                                   (if (listp (car item))
                                       (extract-destructuring-bindings (cadar item))
                                       (extract-destructuring-bindings (car item))))))))
             (&aux
              (when (and parser (listp item) (cdr item))
                (analyze-subform parser (second item)))
              (setf bindings
                    (append bindings
                            (if (listp item)
                                (extract-destructuring-bindings (car item))
                                (list item)))))
             ((&whole &environment)
              (push item bindings)
              (setf state :required))))))
    (remove-duplicates bindings)))


(defun extract-destructuring-bindings (form)
  "Extract all bindings from a destructuring form, handling nested structures.
   Handles:
   - Regular symbols
   - Cons cells and lists
   - Dotted lists
   - Lambda-list keywords
   - Method parameter specializers ((var type))
   
   Returns only the binding symbols, stripping:
   - Lambda list keywords
   - Method specializers"
  (typecase form
    (symbol 
     (unless (member form lambda-list-keywords)
       (list form)))
    (cons
     (case (car form)
       (&optional (extract-destructuring-bindings (cdr form)))
       (&rest (extract-destructuring-bindings (cdr form)))
       (&key (extract-destructuring-bindings (cdr form)))
       (&aux (extract-destructuring-bindings (cdr form)))
       (otherwise
        (if (consp (car form))
            ;; Handle both nested destructuring and method specializers
            (if (and (listp (car form)) 
                    (= (length (car form)) 2))
                ;; Method specializer case - just take the variable
                (cons (first (car form))
                      (extract-destructuring-bindings (cdr form)))
                ;; Regular nested destructuring
                (append (extract-destructuring-bindings (car form))
                       (extract-destructuring-bindings (cdr form))))
            ;; Handle dotted list case and regular cons
            (let ((bindings (if (symbolp (car form))
                               (list (car form))
                               (extract-destructuring-bindings (car form)))))
              (when (cdr form)
                (if (symbolp (cdr form))
                    (push (cdr form) bindings)
                    (setf bindings 
                          (append bindings 
                                  (extract-destructuring-bindings (cdr form))))))
              bindings)))))
    (t nil)))


(defun process-declarations (declarations parser)
  "Process declarations, returning list of:
   - special variables that should be treated as references
   - ignored symbols that should not be treated as undefined references
   - type declarations for documentation"
  (let ((specials nil)
        (ignores nil)
        (types nil))
    (dolist (decl declarations)
      (when (listp decl)
        (case (car decl)
          ((special)
           ;; Special vars are added to specials list and analyzed as references
           (dolist (var (cdr decl))
             (pushnew var specials)
             (analyze-subform parser var)))
          
          ((ignore ignored)
           ;; Ignored vars should not trigger undefined warnings
           (dolist (var (cdr decl))
             (pushnew var ignores)))
          
          ((type ftype)
           ;; Process type declarations
           (dolist (var (cddr decl))
             (push (cons var (second decl)) types)))
          
          ((optimize)
           ;; Optimize declarations don't affect dependency analysis
           nil)
          
          (otherwise
           ;; Handle (type var) shorthand declarations
           (when (and (symbolp (car decl))
                     (find-symbol (symbol-name (car decl)) 
                                (find-package :common-lisp)))
             (dolist (var (cdr decl))
               (push (cons var (car decl)) types)))))))
    (values specials ignores types)))


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
   ;; Analyze remaining body forms directly
   (mapc (lambda (form)
           (analyze-subform parser form))
         forms)))


(defun collect-file-references (tracker source-file target-file)
  "Collect all symbols in SOURCE-FILE that reference definitions in TARGET-FILE.
   Returns a list of symbols that create the dependency relationship."
  (let ((refs ())
        (target-defs (get-file-definitions tracker target-file)))
    ;; Build hash table of symbols defined in target file for quick lookup
    (let ((target-symbols (make-hash-table :test 'equal)))
      (dolist (def target-defs)
        (setf (gethash (definition.name def) target-symbols) t))
      ;; Check all references in source file to see if they reference target symbols
      (maphash (lambda (key refs-list)
                 (dolist (ref refs-list)
                   (when (and (equal (reference.file ref) source-file)
                            (gethash (reference.symbol ref) target-symbols))
                     (pushnew (reference.symbol ref) refs :test #'equal))))
               (slot-value tracker 'references)))
    ;; Return sorted list of referenced symbols
    (sort refs #'string< :key #'symbol-name)))


(defmethod record-definition ((tracker dependency-tracker) symbol type file 
                           &key package exported-p)
  "Record a symbol definition in the tracker."
  (let* ((key (make-tracking-key symbol package))
         (def (make-definition :name symbol
                             :type type
                             :file file
                             :package package
                             :exported-p exported-p)))
    (setf (gethash key (slot-value tracker 'definitions)) def)
    (push def (gethash file (slot-value tracker 'file-map)))
    (when exported-p
      (record-export tracker package symbol))
    def))


(defmethod record-reference ((tracker dependency-tracker) symbol type file 
                          &key package visibility definition)
  "Record a symbol reference in the tracker.
   TYPE is call or reference
   VISIBILITY is inherited, imported, or local (defaults to local)"
  (let* ((key (make-tracking-key symbol (when (symbol-package symbol)
                                        (package-name (symbol-package symbol)))))
         (ref (make-reference :symbol symbol
                            :type type
                            :file file
                            :package package
                            :visibility (or visibility :LOCAL)
                            :definition definition)))
    (push ref (gethash key (slot-value tracker 'references)))
    ref))


(defmethod record-package-use ((tracker dependency-tracker) using-package used-package)
  "Record that one package uses another package."
  (pushnew used-package 
           (gethash using-package (slot-value tracker 'package-uses))
           :test #'string=))


(defmethod record-export ((tracker dependency-tracker) package-name symbol)
  "Record a symbol as being exported from a package.
   Both package-name and symbol can be either strings or symbols."
  (let ((pkg (find-package (string package-name))))
    (when pkg
      (let ((exported-sym (etypecase symbol
                           (string (intern symbol pkg))
                           (symbol (intern (symbol-name symbol) pkg)))))
        (pushnew exported-sym 
                 (gethash (string package-name) (slot-value tracker 'package-exports))
                 :test #'eq)))))


(defmethod record-macro-body-symbols ((tracker dependency-tracker) macro-name symbols)
  "Record the non-CL symbols used in a macro's body."
  (setf (gethash (make-tracking-key macro-name) 
                 (slot-value tracker 'macro-bodies))
        symbols))


(defmethod record-project-cycle ((tracker dependency-tracker) cycle-chain)
  "Record a project dependency cycle."
  (pushnew cycle-chain (project-cycles tracker) :test #'string=))

(defmethod record-file-cycle ((tracker dependency-tracker) cycle-chain)
  "Record a file dependency cycle."
  (pushnew cycle-chain (file-cycles tracker) :test #'string=))

(defmethod record-package-cycle ((tracker dependency-tracker) cycle-chain)
  "Record a package dependency cycle."
  (pushnew cycle-chain (package-cycles tracker) :test #'string=))

(defmethod get-project-cycles (&optional tracker)
  "Get all recorded project dependency cycles."
  (project-cycles (ensure-tracker tracker)))

(defmethod get-file-cycles (&optional tracker)
  "Get all recorded file dependency cycles."
  (file-cycles (ensure-tracker tracker)))

(defmethod get-package-cycles (&optional tracker)
  "Get all recorded package dependency cycles."
  (package-cycles (ensure-tracker tracker)))


(defmethod get-macro-body-symbols (&optional (tracker nil tracker-provided-p) macro-name)
  "Get all recorded symbols used in a macro's body."
  (let ((actual-tracker (if tracker-provided-p tracker (ensure-tracker))))
    (gethash (make-tracking-key macro-name) 
             (slot-value actual-tracker 'macro-bodies))))


(defun process-package-use-option (package used-pkg-name options parser)
  "Process a :use package option, recording package dependencies and inherited symbols.
   Handles all package name forms consistently using string normalization.
   
   PACKAGE - The package object being defined
   USED-PKG-NAME - Name of package being used (string, already normalized)
   OPTIONS - Full defpackage options list
   PARSER - Current file parser for context"
  (let* ((used-pkg (find-package used-pkg-name))
         (using-pkg-name (normalize-package-name package)))
    (when used-pkg
      (use-package used-pkg package)
      (record-package-use *current-tracker* 
                         using-pkg-name
                         used-pkg-name)
      ;; Record references for all exported symbols from used package
      (do-external-symbols (sym used-pkg)
        (multiple-value-bind (s status)
            (find-symbol (symbol-name sym) package)
          (when (and s (eq status :INHERITED))
            ;; Record basic reference with inherited visibility
            (record-reference *current-tracker* sym
                            :VALUE
                            (file parser)
                            :package used-pkg-name
                            :visibility :INHERITED)
            ;; If it's a function, also record potential call reference
            (when (and (fboundp sym) 
                      (not (macro-function sym))
                      (not (special-operator-p sym)))
              (record-reference *current-tracker* sym
                              :OPERATOR
                              (file parser)
                              :package used-pkg-name
                              :visibility :INHERITED))))))))


(defun process-package-import-option (package from-pkg-name pkg-name parser symbol)
  "Process an :import-from package option for a single symbol.
   Records dependencies between packages and the imported symbol reference.
   
   PACKAGE - The package object being defined
   FROM-PKG-NAME - Package name to import from (string, already normalized)
   PKG-NAME - Name of package being defined (string, already normalized)
   PARSER - Current file parser for context
   SYMBOL - Symbol to import (can be string, symbol, or uninterned symbol)"
  (let* ((from-pkg (find-package from-pkg-name))
         (sym-name (normalize-symbol-name symbol))
         (from-sym (and from-pkg (find-symbol sym-name from-pkg))))
    (when (and from-pkg from-sym)
      (import from-sym package)
      (record-package-use *current-tracker* pkg-name from-pkg-name)
      (record-reference *current-tracker* from-sym
                       :VALUE
                       (file parser)
                       :package from-pkg-name
                       :visibility :IMPORTED))))


(defun process-package-export-option (package pkg-name symbol)
  "Process an :export package option for a single symbol.
   Records the symbol as being exported from the package.
   
   PACKAGE - The package object being defined  
   PKG-NAME - Name of package being defined (string, already normalized)
   SYMBOL - Symbol to export (can be string, symbol, or uninterned symbol)
   
   Returns the exported symbol if successful."
  (let* ((sym-name (normalize-symbol-name symbol))
         (exported-sym (and sym-name 
                           (intern sym-name package))))
    (when exported-sym
      (export exported-sym package)
      (record-export *current-tracker* pkg-name exported-sym)
      exported-sym)))


(defun detect-package-cycle (pkg-name current-packages)
  "Check for and record any package dependency cycles.
   A cycle exists if the package being processed appears in the current package stack.
   
   PKG-NAME - Name of package being checked (string, already normalized)
   CURRENT-PACKAGES - Stack of packages currently being processed (list of strings)
   
   Example cycle: A -> B -> C -> A would be recorded as \"A -> B -> C -> A\"
   
   Records both:
   1. The cycle chain in the tracker's package-cycles list
   2. An anomaly record with severity ERROR"
  (let ((position (member pkg-name current-packages :test #'string=)))
    (when position
      (let* ((cycle (cons pkg-name (ldiff current-packages position)))
             (chain (format nil "~{~A~^ -> ~}" (reverse cycle))))
        ;; Record as both a cycle and an anomaly
        (record-package-cycle *current-tracker* chain)
        (record-anomaly *current-tracker*
                       :package-cycle
                       :ERROR
                       pkg-name
                       (format nil "Package dependency cycle detected: ~A" chain)
                       cycle)))))


(defmethod analyze-subform ((parser file-parser) form)
  "Analyze a single form for symbol references, recording only references to
   symbols that have definition records. Recursively examines all subforms including
   array elements and hash tables."
  (typecase form
    (symbol 
     (let* ((pkg (symbol-package form))
            (current-pkg (current-package parser))
            (pkg-name (if pkg 
                       (package-name pkg) 
                       (current-package-name parser)))
            (current-file (file parser))
            (visibility (cond 
                       ((null pkg) :LOCAL)
                       ((eq pkg current-pkg) :LOCAL)
                       (t (multiple-value-bind (sym status)
                            (find-symbol (symbol-name form) current-pkg)
                          (declare (ignore sym))
                          (case status
                            (:inherited :INHERITED)
                            (:external :IMPORTED) 
                            (otherwise :LOCAL)))))))
       (when (gethash (make-tracking-key form pkg-name) 
                     (slot-value *current-tracker* 'definitions))
         (record-reference *current-tracker* form
                         :VALUE
                         current-file
                         :package pkg-name
                         :visibility visibility))))
    (cons
     (analyze-form parser form))
    (array
     (dotimes (i (array-total-size form))
       (analyze-subform parser (row-major-aref form i))))
    (hash-table
     (maphash (lambda (k v)
                (analyze-subform parser k)
                (analyze-subform parser v))
              form))
    (t nil)))


(defun record-helper-functions (parser body)
 "Record helper functions defined within a body using defun/flet/labels.
  PARSER - The file parser for context
  BODY - List of body forms to scan for function definitions
  Returns list of helper function names recorded."
 (let ((helper-functions nil)
       (file (file parser))
       (pkg-name (current-package-name parser))
       (pkg (current-package parser)))
   ;; Scan body for function definitions
   (dolist (form body)
     (when (and (listp form)
                (member (car form) '(defun flet labels)))
       (push (if (eq (car form) 'defun)
                (second form)
                (caar (cdr form)))
             helper-functions)))
   ;; Record each helper function
   (dolist (func helper-functions)
     (record-definition *current-tracker* func
                      :FUNCTION
                      file
                      :package pkg-name
                      :exported-p nil))
   helper-functions))


(defun preceding-forms-in-body (form parser)
 "Find forms that precede the given form in the same body context.
  Used to find relevant declarations for symbol-value and symbol-function forms.
  Returns a list of preceding forms in reverse order (most recent first)."
 (let* ((file (file parser))
        (preceding nil))
   (with-open-file (stream file :direction :input)
     (loop with found = nil
           for current-form = (read stream nil nil)
           while (and current-form (not found))
           do (if (equal current-form form)
                 (setf found t)
                 (push current-form preceding))
           finally (return (remove-if-not 
                          ;; Only keep declarations and type specs
                          (lambda (f)
                            (and (listp f)
                                 (or (eq (car f) 'declare)
                                     (and (eq (car f) 'type)
                                          (= (length f) 3)))))
                          preceding))))))


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


(defun extract-spec-symbols-from-slot (slot-spec)
 "Extract potential definition-reference symbols from a slot specification.
  Only collects symbols that could be user-defined, ignoring CL symbols."
 (let ((symbols nil))
   (when (listp slot-spec)
     (destructuring-bind (name &optional initform &rest slot-options) slot-spec
       (declare (ignore name))
       ;; Check initform for non-quoted symbols
       (when (and initform (not (quoted-form-p initform)))
         (collect-non-cl-symbols initform symbols))
       ;; Process slot options
       (loop for (option value) on slot-options by #'cddr
             do (case option
                  (:type (collect-non-cl-symbols value symbols))))))
   symbols))


(defun cl-symbol-p (symbol)
 "Return true if symbol is from the common-lisp package."
 (and (symbolp symbol)
      (eq (symbol-package symbol) 
          (find-package :common-lisp))))


(defun collect-non-cl-symbols (form symbols)
  "Recursively collect all non-CL symbols from form into symbols list.
   Handles nested lists and ignores quoted forms."
  (typecase form
    (symbol 
     (unless (or (null form) (cl-symbol-p form))
       (push form symbols)))
    (cons
     (unless (quoted-form-p form)
       (setf symbols (collect-non-cl-symbols (car form) symbols))
       (setf symbols (collect-non-cl-symbols (cdr form) symbols)))))
  symbols)


(defun quoted-form-p (form)
 "Return true if form is quoted with quote or function quote."
 (and (consp form)
      (or (eq (car form) 'quote)
          (eq (car form) 'function))))


(defun symbol-qualified-p (symbol parser)
  "Return t if symbol appears to have been package-qualified in source.
   This is inferred when symbol's package differs from parser's current package."
  (and (symbol-package symbol)  ; Has a package
       (not (eq (symbol-package symbol) 
                (current-package parser)))))


(defun determine-symbol-visibility (symbol parser)
  "Determine how symbol is visible in current package context.
   Returns :LOCAL, :INHERITED, or :IMPORTED."
  (let ((sym-pkg (symbol-package symbol))
        (cur-pkg (current-package parser)))
    (cond 
      ((null sym-pkg) :LOCAL)  ; Uninterned symbol
      ((eq sym-pkg cur-pkg) :LOCAL)  ; In current package
      (t (multiple-value-bind (sym status)
             (find-symbol (symbol-name symbol) cur-pkg)
           (declare (ignore sym))
           (case status
             (:inherited :INHERITED)
             (:external :IMPORTED)
             (otherwise :LOCAL)))))))


(defun extract-ignorable-symbols (form)
  "Return list of symbols that should be ignored when tracking references.
   This includes:
   - Lambda list keywords
   - Local bindings from lambda lists
   - Structure/class slot names being defined
   - Local variables from let/flet/labels forms
   Returns list of symbols to ignore."
  (let ((ignore-syms nil))
    (labels ((handle-lambda-list (lambda-list)
               ;; Add lambda keywords and bindings from lambda lists
               (dolist (item lambda-list)
                 (typecase item
                   (symbol 
                    (when (member item lambda-list-keywords)
                      (push item ignore-syms))
                    (push item ignore-syms))
                   (cons  ; Only recurse into cons that are destructuring forms
                    (unless (member (car item) '(:documentation :method-combination))
                      (handle-lambda-list item))))))
             
             (handle-defstruct (form)
               ;; Add slot names from defstruct forms
               (when (and (>= (length form) 2)
                         (listp (second form)))
                 ;; Handle (defstruct (name options...) slots...)
                 (dolist (slot (cddr form))
                   (if (listp slot)
                       (push (car slot) ignore-syms)
                       (push slot ignore-syms))))
               (when (and (>= (length form) 2)
                         (symbolp (second form)))
                 ;; Handle (defstruct name slots...)
                 (dolist (slot (cddr form))
                   (if (listp slot)
                       (push (car slot) ignore-syms)
                       (push slot ignore-syms)))))
             
             (handle-defclass (form)
               ;; Add slot names from defclass forms
               (when (>= (length form) 4)
                 (dolist (slot (fourth form))
                   (when (listp slot)
                     (push (car slot) ignore-syms)))))
             
             (walk (form)
               (when (listp form)
                 (case (car form)
                   ((lambda) 
                    (when (>= (length form) 2)
                      (handle-lambda-list (second form))))
                   ((let let* flet labels)
                    (when (>= (length form) 2)
                      (dolist (binding (second form))
                        (if (listp binding)
                            (push (car binding) ignore-syms)
                            (push binding ignore-syms)))))
                   ((defstruct) (handle-defstruct form))
                   ((defclass) (handle-defclass form))
                   ((defun defmacro)
                    (when (>= (length form) 3)
                      (handle-lambda-list (third form))))
                   ((defmethod)
                    (let ((rest (cddr form)))
                      ;; Skip over qualifiers to get to lambda list
                      (loop while (and rest (atom (car rest)))
                            do (pop rest))
                      (when rest
                        (handle-lambda-list (car rest)))))
                   ((defgeneric)
                    (when (>= (length form) 3)
                      (handle-lambda-list (third form)))))
                 ;; Recurse into all subforms
                 (dolist (subform (cdr form))
                   (walk subform)))))
      
      (walk form)
      (remove-duplicates ignore-syms))))


(defun skip-reference-p (symbol)
  "Return T if symbol should be skipped during reference analysis.
   Skips:
   - NIL and keywords
   - Common Lisp package symbols  
   - Lambda list keywords"
  (or (null symbol)
      (keywordp symbol) 
      (and (symbol-package symbol)
           (eq (symbol-package symbol) 
               (find-package :common-lisp)))
      (member symbol lambda-list-keywords)))


(defun definition-name-p (symbol context operator-position)
  "Return T if symbol appears to be the name in a definition form.
   These names should be ignored during reference analysis since they are being defined.
   
   SYMBOL - The symbol being checked
   CONTEXT - The parent form containing the symbol
   OPERATOR-POSITION - Whether symbol appears in function call position"
  (and (not operator-position)  ; Not in operator position 
       (consp context)          ; Inside a compound form
       (symbolp (car context))  ; Has symbol operator
       (let ((op-name (symbol-name (car context))))
         ;; Check if it's a definition operator
         (or (alexandria:starts-with-subseq "DEF" op-name)
             (alexandria:starts-with-subseq "DEFINE-" op-name)))
       ;; Symbol must be in name position (second element)
       (eql symbol (cadr context))))


(defun check-package-reference (symbol parser tracker)
  "Check package visibility and cross-package references for a symbol.
   Returns two values:
   1. Package visibility (:LOCAL, :INHERITED, or :IMPORTED)
   2. T if an anomaly was recorded, NIL otherwise
   
   SYMBOL - The symbol being checked
   PARSER - The current file parser
   TRACKER - The dependency tracker"
  (let* ((sym-pkg (symbol-package symbol))
         (cur-pkg (current-package parser))
         (pkg-name (if sym-pkg 
                      (package-name sym-pkg)
                      (current-package-name parser)))
         (visibility (determine-symbol-visibility symbol parser))
         (anomaly-p nil))
    ;; Check for unqualified cross-package reference while in CL-USER
    (when (and (eq cur-pkg (find-package :common-lisp-user))
               (not (symbol-qualified-p symbol parser))
               (not (eq sym-pkg (find-package :common-lisp-user))))
      (record-anomaly tracker
                      :missing-in-package
                      :WARNING
                      (file parser)
                      (format nil "Unqualified reference to ~A from package ~A without in-package declaration"
                              symbol pkg-name))
      (setf anomaly-p t))
    (values visibility anomaly-p)))


(defun record-symbol-reference (symbol parser tracker ref-type)
  "Record a symbol reference and any related anomalies.
   Returns true if a reference was recorded.
   
   SYMBOL - The symbol being referenced
   PARSER - The current file parser
   TRACKER - The dependency tracker
   REF-TYPE - Either :OPERATOR or :VALUE"
  (multiple-value-bind (visibility anomaly-p)
      (check-package-reference symbol parser tracker)
    (let* ((pkg (or (symbol-package symbol)
                    (current-package parser)))
           (pkg-name (intern (package-name pkg) :keyword))
           (key (make-tracking-key symbol (package-name pkg)))
           (def (gethash key (slot-value tracker 'definitions))))
      ;; Record reference if definition exists
      (when def
        ;; Pass as regular arguments rather than keyword args
        (let ((ref (make-reference :symbol symbol
                                 :type ref-type
                                 :file (file parser)
                                 :package pkg-name
                                 :visibility visibility
                                 :definition def)))
          (push ref (gethash key (slot-value tracker 'references)))
          t))
      ;; Record undefined reference anomaly if no definition found
      (when (not def)
        (record-anomaly tracker
                        :undefined-reference
                        :ERROR
                        (file parser)
                        (format nil "~A ~A has no definition"
                                (if (eq ref-type :OPERATOR) "Function" "Symbol")
                                symbol)
                        (format nil "Referenced in ~A position"
                                (if (eq ref-type :OPERATOR) "operator" "value")))
        nil))))


(defun walk-form (form handler &key (log-stream *trace-output*) (log-depth t))
  "Walk a form calling HANDLER on each subform with position and context info.
   FORM - The form to analyze
   HANDLER - Function taking (form op-position context depth)
   LOG-STREAM - Where to send debug output (nil for no logging)
   LOG-DEPTH - Whether to track and log nesting depth"
  (labels ((walk (x op-position context depth)
             ;; Log form being processed
             (when log-stream
               (let ((indent (if log-depth 
                               (make-string (* depth 2) :initial-element #\Space)
                               "")))
                 (format log-stream "~&~AForm: ~S~%" indent x)
                 (when context
                   (format log-stream "~A Context: ~S~%" indent context))
                 (format log-stream "~A In ~:[value~;operator~] position~%" 
                         indent op-position)))
             
             ;; Process form with handler
             (funcall handler x op-position context depth)
             
             ;; Recursively process subforms
             (typecase x
               (cons
                (when log-stream
                  (format log-stream "~A Processing list form~%" 
                          (if log-depth
                              (make-string (* depth 2) :initial-element #\Space)
                              "")))
                (walk (car x) t (cons (car x) (cdr x)) (1+ depth)) 
                (walk (cdr x) nil x (1+ depth)))
               
               (array
                (when log-stream
                  (format log-stream "~A Processing array~%"
                          (if log-depth
                              (make-string (* depth 2) :initial-element #\Space)
                              "")))
                (dotimes (i (array-total-size x))
                  (walk (row-major-aref x i) nil x (1+ depth))))
               
               (hash-table
                (when log-stream
                  (format log-stream "~A Processing hash table~%"
                          (if log-depth
                              (make-string (* depth 2) :initial-element #\Space)
                              "")))
                (maphash (lambda (k v)
                          (walk k nil x (1+ depth))
                          (walk v nil x (1+ depth)))
                        x)))))
    
    ;; Start walking at top level
    (let ((*print-circle* nil)     ; Prevent circular printing issues
          (*print-length* 10)      ; Limit list output
          (*print-level* 5))       ; Limit nesting output
      (walk form nil nil 0))))


(defun extract-spec-symbols-from-slot (slot-spec)
  "Extract potential definition-reference symbols from a slot specification,
   separating them by type/value vs method/function.
   Returns (values type-value-symbols method-function-symbols).
   Type/value symbols come from :type and initform.
   Method/function symbols come from :reader, :writer, :accessor."
  (let ((type-value-syms nil)
        (method-fn-syms nil))
    (when (listp slot-spec)
      (multiple-value-bind (ref-type symbols)
          (classify-slot-reference slot-spec)
        (if (eq ref-type :METHOD-FUNCTION)
            (setf method-fn-syms symbols)  
            (setf type-value-syms symbols))))
    (values type-value-syms method-fn-syms)))


(defun classify-slot-reference (slot-spec)
  "Classify a slot specification as :TYPE-VALUE or :METHOD-FUNCTION.
   Returns (values type symbols) where:
   type is :TYPE-VALUE or :METHOD-FUNCTION
   symbols is list of referenced symbols.
   
   A slot is :METHOD-FUNCTION if it has :reader,:writer,:accessor options.
   Otherwise it is :TYPE-VALUE for regular slots with just type/value refs."
  (let ((symbols nil)
        (ref-type :TYPE-VALUE))
    (when (listp slot-spec)
      (destructuring-bind (name &optional initform &rest slot-options) slot-spec
        (declare (ignore name))
        ;; Check initform for non-quoted symbols
        (when (and initform (not (quoted-form-p initform)))
          (collect-non-cl-symbols initform symbols))
        ;; Process slot options
        (loop for (option value) on slot-options by #'cddr
              do (case option
                   (:type (collect-non-cl-symbols value symbols))
                   ((:reader :writer :accessor)
                    (setf ref-type :METHOD-FUNCTION)
                    (collect-non-cl-symbols value symbols))))))
    (values ref-type (remove-duplicates symbols))))

