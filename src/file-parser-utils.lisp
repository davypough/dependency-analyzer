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
  "Extract bindings from lambda list, handling destructuring and specializers." 
  (let ((bindings nil)
        (state :required))
    (dolist (item lambda-list)
      (case item  
        ((&optional &rest &key &aux &whole &environment)
         (setf state item))
        (t (case state
             (:required 
              (typecase item
                (symbol (push item bindings))
                (cons   ; Handle (var init) or ((var type))
                  (if (and (listp (car item)) (= (length (car item)) 2))
                      (push (caar item) bindings)  ; (var type) specializer
                      (push (car item) bindings)))))
             (&optional
              (typecase item  
                (symbol (push item bindings))
                (cons (push (car item) bindings))))
             (&rest
              (when (symbolp item)
                (push item bindings)))
             (&key
              (typecase item
                (symbol (push item bindings))
                (cons   ; Handle both ((kw var)) and (var) forms
                  (let ((var (if (listp (car item))
                                (if (listp (cadar item))
                                    (caadar item)  ; (((:kw var)) init)
                                    (cadar item))  ; ((:kw var) init) 
                                (car item))))     ; ((var) init)
                    (push var bindings)))))
             (&aux
              (typecase item
                (symbol (push item bindings))
                (cons (push (car item) bindings))))))))
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
  "Collect all references in SOURCE-FILE that reference definitions in TARGET-FILE.
   Returns a list of reference objects."
  (let ((refs ())
        (target-defs (get-file-definitions tracker target-file)))
    ;; Build hash table of symbols defined in target file for quick lookup
    (let ((target-symbols (make-hash-table :test 'equal)))
      (dolist (def target-defs)
        (setf (gethash (definition.name def) target-symbols) t))
      ;; Check all references in source file to see if they reference target symbols
      (maphash (lambda (key refs-list)
                 (declare (ignore key))
                 (dolist (ref refs-list)
                   (when (and (equal (reference.file ref) source-file)
                            (gethash (reference.symbol ref) target-symbols))
                     (pushnew ref refs :test #'equal))))
               (slot-value tracker 'references)))
    ;; Return sorted list of references
    (sort refs #'string< :key (lambda (r) (symbol-name (reference.symbol r))))))


(defun record-definition (tracker symbol type file &key package exported-p)
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


(defun record-reference (tracker symbol file &key package context visibility definition)
  "Record a symbol reference in the tracker.
   VISIBILITY is inherited, imported, or local (defaults to local)"
  (let* ((key (make-tracking-key symbol (when (symbol-package symbol)
                                        (package-name (symbol-package symbol)))))
         (ref (make-reference :symbol symbol
                            :file file
                            :package package
                            :context context
                            :visibility (or visibility :LOCAL)
                            :definition definition)))
    (pushnew ref (gethash key (slot-value tracker 'references))
         :test (lambda (a b)
                 (and (equal (reference.symbol a) (reference.symbol b))
                      (equal (reference.file a) (reference.file b))
                      (equal (reference.package a) (reference.package b)))))
    ref))


(defun record-package-use (tracker using-package used-package)
  "Record that one package uses another package."
  (pushnew used-package 
           (gethash using-package (slot-value tracker 'package-uses))
           :test #'string=))


(defun record-export (tracker package-name symbol)
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


(defun record-macro-body-symbols (tracker macro-name symbols)
  "Record the non-CL symbols used in a macro's body."
  (setf (gethash (make-tracking-key macro-name) 
                 (slot-value tracker 'macro-bodies))
        symbols))


(defun record-project-cycle (tracker cycle-chain)
  "Record a project dependency cycle."
  (pushnew cycle-chain (project-cycles tracker) :test #'string=))

(defun record-file-cycle (tracker cycle-chain)
  "Record a file dependency cycle."
  (pushnew cycle-chain (file-cycles tracker) :test #'string=))

(defun record-package-cycle (tracker cycle-chain)
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


(defun skip-item-p (item)
  "Return T if item should be skipped during reference analysis.
   Skips:
   - NIL and keywords
   - Common Lisp package symbols"
  (or (null item)
      (numberp item)
      (stringp item)
      (keywordp item) 
      (and (symbolp item)
           (symbol-package item)
           (eq (symbol-package item) 
               (find-package :common-lisp)))))


(defun definition-name-p (symbol context)  
  "Return T if symbol appears to be the name in a definition form.
   These names should be ignored during reference analysis since they are being defined.
   
   SYMBOL - The symbol being checked
   CONTEXT - The parent form containing the symbol"
  (and (consp context)          ; Inside a compound form
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
         (anomaly-p nil)
         (package-ops '(defpackage in-package use-package make-package 
                       rename-package delete-package)))
    ;; Check for unqualified cross-package reference while in CL-USER
    ;; But ignore references in package setup forms
    (when (and (eq cur-pkg (find-package :common-lisp-user))
               (not (symbol-qualified-p symbol parser))
               (not (eq sym-pkg (find-package :common-lisp-user)))
               ;; Add check for non-package-related forms
               (not (member symbol '(:use cl common-lisp)))  ; Allow standard package names
               (not (member (car (car (bound-symbols parser))) package-ops))) ; Skip package ops
      (record-anomaly tracker
                      :missing-in-package
                      :WARNING
                      (file parser)
                      (format nil "Unqualified reference to ~A from package ~A without in-package declaration"
                              symbol pkg-name))
      (setf anomaly-p t))
    (values visibility anomaly-p)))


(defun walk-form (form handler &key (log-stream *trace-output*) (log-depth t))
  "Walk a form calling HANDLER on each subform with context and depth info.
   FORM - The form to analyze
   HANDLER - Function taking (form context depth)
   LOG-STREAM - Where to send debug output (nil for no logging)
   LOG-DEPTH - Whether to track and log nesting depth"
  (labels ((walk (x context depth)
             ;; Log form being processed
             (unless (skip-item-p x) 
              (when log-stream
               (let ((indent (if log-depth 
                               (make-string (* depth 2) :initial-element #\Space)
                               "")))
                 (format log-stream "~&~AForm: ~S~%" indent x)
                 (when context
                   (format log-stream "~A Context: ~S~%" indent context))
                 (format log-stream "~A~%" indent))))
             
             ;; Process form with handler
             (funcall handler x context depth)
             
             ;; Recursively process subforms
             (typecase x
               (cons
                (when log-stream
                  (format log-stream "~A Processing list form~%" 
                          (if log-depth
                              (make-string (* depth 2) :initial-element #\Space)
                              "")))
                (walk (car x) (cons (car x) (cdr x)) (1+ depth)) 
                (walk (cdr x) x (1+ depth)))
               
               (array
                (when log-stream
                  (format log-stream "~A Processing array~%"
                          (if log-depth
                              (make-string (* depth 2) :initial-element #\Space)
                              "")))
                (dotimes (i (array-total-size x))
                  (walk (row-major-aref x i) x (1+ depth))))
               
               (hash-table
                (when log-stream
                  (format log-stream "~A Processing hash table~%"
                          (if log-depth
                              (make-string (* depth 2) :initial-element #\Space)
                              "")))
                (maphash (lambda (k v)
                          (walk k x (1+ depth))
                          (walk v x (1+ depth)))
                        x)))))
    
    ;; Start walking at top level
    (let ((*print-circle* nil)     ; Prevent circular printing issues
          (*print-length* 10)      ; Limit list output
          (*print-level* 5))       ; Limit nesting output
      (walk form nil 0))))


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


(defun limit-form-size (form pkg-name &key (max-depth 8) (max-elements 20))
  "Limit the size of a form for use as reference context.
   Returns form if within limits, otherwise returns truncated version.
   Strips package prefixes that match pkg-name exactly."
  (let ((element-count 0))
    (labels ((clean-symbol (sym)
               (let* ((actual-sym (if (and (listp sym) 
                                         (eq (car sym) 'quote))
                                    (cadr sym)
                                    sym))
                      (pkg (and (symbolp actual-sym)
                               (symbol-package actual-sym)))
                      (sym-pkg-name (and pkg (package-name pkg))))
                 (cond ((not (symbolp actual-sym)) actual-sym)
                       ((null pkg) actual-sym)  ; Uninterned stays uninterned
                       ((string= sym-pkg-name pkg-name) ; Exact package match
                        (intern (symbol-name actual-sym) (find-package :dep)))
                       (t actual-sym))))  ; Keep other package prefixes
             (truncate-form (f depth)
               (when (> (incf element-count) max-elements)
                 (return-from limit-form-size 
                   (typecase f 
                     (cons (if (member (car f) '(quote function))
                              (list (car f) (clean-symbol (cadr f)))
                              (list (clean-symbol (car f)) (make-instance 'dots-object))))  ; '|...|)))
                     (symbol (clean-symbol f))
                     (t f))))
               (typecase f
                 (cons
                  (cond ((>= depth max-depth)
                         (if (member (car f) '(quote function))
                             (list (car f) (clean-symbol (cadr f)))
                             (list (clean-symbol (car f)) (make-instance 'dots-object))))  ;'|...|)))
                        ((member (car f) '(quote function))
                         (list (car f) (clean-symbol (cadr f))))
                        (t (let ((car (truncate-form (car f) (1+ depth)))
                               (cdr (truncate-form (cdr f) (1+ depth))))
                             (cons car cdr)))))
                 (symbol (clean-symbol f))
                 (t f))))
      (truncate-form form 0))))