;;;; Filename: file-parser-utils.lisp
;;;
;;; Utility functions for parsing, package handling, and cycle detection.
;;; Contains pure functions for name normalization, symbol lookup,
;;; package option processing, and dependency cycle detection.

(in-package #:dep)


;;; Name and Symbol Utils

(defun normalize-package-name (designator)
  "Convert a package designator to a normalized string form."
  (typecase designator
    (string
      designator)
    (symbol
      (symbol-name designator))
    (t
      (princ-to-string designator))))


(defun normalize-symbol-name (designator)
  "Convert a symbol designator to a normalized string form."
  (typecase designator
    (string
      designator)
    (symbol
      (symbol-name designator))
    (t
      (princ-to-string designator))))


(defun find-exported-symbol (name package-name &optional current-package)
  "Find a symbol in a package, checking exports and inherited symbols."
  (let* ((target-pkg (find-package package-name))
         (current-pkg (or current-package *package*)))
    (declare (ignore current-pkg))
    (when target-pkg
      (multiple-value-bind (sym status)
          (find-symbol (normalize-symbol-name name) target-pkg)
        (when (and sym (member status '(:external :inherited)))
          (values sym (package-name (symbol-package sym))))))))


(defun extract-method-specializers (lambda-list)
  "Extract the specializer types from a method lambda list."
  (loop for param in lambda-list
        until (member param lambda-list-keywords)
        collect (if (and (listp param) (= (length param) 2))
                   (second param)    ; (var type) -> type
                   t)))             ; var -> t


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


(defun collect-file-references (tracker source-file target-file)
  "Collect all symbols in SOURCE-FILE that reference definitions in TARGET-FILE.
   Returns a list of symbols that create the dependency relationship."
  (let ((refs ())
        (target-defs (get-file-definitions tracker target-file)))
    ;; Build hash table of symbols defined in target file for quick lookup
    (let ((target-symbols (make-hash-table :test 'equal)))
      (dolist (def target-defs)
        (setf (gethash (definition.symbol def) target-symbols) t))
      ;; Check all references in source file to see if they reference target symbols
      (maphash (lambda (key refs-list)
                 (dolist (ref refs-list)
                   (when (and (equal (reference.file ref) source-file)
                            (gethash (reference.symbol ref) target-symbols))
                     (pushnew (reference.symbol ref) refs :test #'equal))))
               (slot-value tracker 'references)))
    ;; Return sorted list of referenced symbols
    (sort refs #'string< :key #'symbol-name)))


(defun method-qualifiers-match-p (recorded-qualifiers target-qualifiers)
  "Compare two lists of method qualifiers for equality.
   Handles both ordering-sensitive (e.g., :before vs :after)
   and ordering-insensitive (e.g., :method :around) qualifiers."
  (and (= (length recorded-qualifiers) (length target-qualifiers))
       (or (equal recorded-qualifiers target-qualifiers)
           ;; For qualifiers where order doesn't matter
           (and (intersection recorded-qualifiers target-qualifiers)
                (subsetp recorded-qualifiers target-qualifiers)
                (subsetp target-qualifiers recorded-qualifiers)))))


(defun specializers-match-p (recorded-specializers target-specializers)
  "Compare two lists of method specializers for equality."
  (and (= (length recorded-specializers) (length target-specializers))
       (every #'eq recorded-specializers target-specializers)))


(defun analyze-method-definition-internal (parser name qualifiers lambda-list body)
  "Process the common parts of method definition from both defmethod and defgeneric :method options."
  ;; Record the method definition with its qualifiers and specializers
  (record-definition *current-tracker* name
                    :method
                    (file parser)
                    :package (current-package-name parser)
                    :exported-p (eq (nth-value 1 
                                   (find-symbol (symbol-name name)
                                              (current-package parser)))
                                  :external)
                    :context (list :qualifiers qualifiers
                                 :specializers (extract-method-specializers lambda-list)))
                         
  ;; Reuse existing function analysis logic
  (let ((defun-form `(defun ,name ,lambda-list ,@body)))
    (analyze-basic-definition parser defun-form)))


(defun find-method-definition (tracker gf-name qualifiers specializers)
  "Find a method definition matching the given generic function name,
   qualifiers, and specializers."
  (let* ((key (make-tracking-key gf-name))
         (methods (remove :method (gethash key (slot-value tracker 'definitions))
                         :key #'definition.type :test-not #'eq)))
    (find-if (lambda (def)
               (let ((context (definition.context def)))
                 (and (method-qualifiers-match-p 
                       (getf context :qualifiers) qualifiers)
                      (specializers-match-p
                       (getf context :specializers) specializers))))
             methods)))


(defmethod record-definition ((tracker dependency-tracker) symbol type file 
                           &key position package exported-p context)
  "Record a symbol definition in the tracker."
  (let* ((key (make-tracking-key symbol package))
         (def (make-definition :symbol symbol
                             :type type
                             :file file
                             :package package
                             :position position
                             :exported-p exported-p
                             :context context)))
    (setf (gethash key (slot-value tracker 'definitions)) def)
    (push def (gethash file (slot-value tracker 'file-map)))
    (when exported-p
      (record-export tracker package symbol))
    def))


(defmethod record-reference ((tracker dependency-tracker) symbol type file 
                          &key position context package)
  "Record a symbol reference in the tracker."
  (let* ((key (make-tracking-key symbol (when (symbol-package symbol)
                                        (package-name (symbol-package symbol)))))
         (ref (make-reference :symbol symbol
                            :type type
                            :file file
                            :position position
                            :context context
                            :package package)))
    (push ref (gethash key (slot-value tracker 'references)))
    ref))



(defmethod record-package-use ((tracker dependency-tracker) using-package used-package)
  "Record that one package uses another package."
  (pushnew used-package 
           (gethash using-package (slot-value tracker 'package-uses))
           :test #'string=))

(defmethod record-export ((tracker dependency-tracker) package-name symbol)
  "Record a symbol as being exported from a package."
  (let ((pkg (find-package (string package-name))))
    (when pkg
      (let ((exported-sym (intern (symbol-name symbol) pkg)))
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

(defmethod lookup-macro-definition (&optional (tracker nil tracker-provided-p) symbol)
  "Find a macro's definition if it exists."
  (let* ((actual-tracker (if tracker-provided-p tracker (ensure-tracker)))
         (current-pkg (symbol-package symbol))
         (sym-name (symbol-name symbol)))
    (when current-pkg
      (let ((def (gethash (make-tracking-key symbol (package-name current-pkg))
                         (slot-value actual-tracker 'definitions))))
        (when (and def (eq (definition.type def) :macro))
          (return-from lookup-macro-definition def)))
      (dolist (used-pkg (package-use-list current-pkg))
        (multiple-value-bind (used-sym status)
            (find-symbol sym-name used-pkg)
          (when (and used-sym (member status '(:external :inherited)))
            (let* ((used-key (make-tracking-key used-sym (package-name used-pkg))))
              (let ((def (gethash used-key (slot-value actual-tracker 'definitions))))
                (when (and def (eq (definition.type def) :macro))
                  (return-from lookup-macro-definition def)))))))
      (let ((def (gethash sym-name (slot-value actual-tracker 'definitions))))
        (when (and def (eq (definition.type def) :macro))
          def)))))


;;; Package Option Processing Utils

(defun process-package-use-option (package used-pkg-name options parser)
  "Process a :use package option."
  (let ((used-pkg (find-package used-pkg-name)))
    (when used-pkg
      (use-package used-pkg package)
      (record-package-use *current-tracker* 
                         (package-name package) 
                         (package-name used-pkg))
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
                              :package (package-name used-pkg)))))))))


(defun process-package-import-option (package from-pkg-name pkg-name parser sym)
  "Process an :import-from package option for a single symbol."
  (let* ((from-pkg (find-package from-pkg-name))
         (name (normalize-symbol-name sym))
         (from-sym (find-symbol name from-pkg)))
    (when (and from-pkg from-sym)
      (import from-sym package)
      (record-package-use *current-tracker* pkg-name from-pkg-name)
      (record-reference *current-tracker* from-sym
                       :import
                       (file parser)
                       :package from-pkg-name))))


(defun process-package-export-option (package pkg-name sym)
  "Process an :export package option for a single symbol."
  (let* ((name (normalize-symbol-name sym))
         (exported-sym (intern name package)))
    (export exported-sym package)
    (record-export *current-tracker* pkg-name exported-sym)))


;;; Cycle Detection Utils

(defun record-file-dependency-cycle (parser file-name)
  "Record a file dependency cycle in the tracker.
   A cycle exists if we encounter a file that is already in our parsing stack."
  (let ((position (member file-name (parsing-files parser) :test #'equal)))
    (when position
      (let* ((cycle (cons file-name (ldiff (parsing-files parser) position)))
             (chain (format nil "~{~A~^ -> ~}" cycle)))
        ;; Record the cycle in the dependency tracker
        (record-file-cycle chain)
        (record-anomaly *current-tracker*
                       :file-cycle
                       :error
                       file-name
                       (format nil "File dependency cycle detected: ~A" chain)
                       cycle)))))


(defun detect-package-cycle (pkg-name current-packages)
  "Check for and record any package dependency cycles."
  (let ((position (member pkg-name current-packages :test #'string=)))
    (when position
      (let* ((cycle (cons pkg-name (ldiff current-packages position)))
             (chain (format nil "~{~A~^ -> ~}" (reverse cycle))))
        ;; Record as both a cycle and an anomaly
        (record-package-cycle chain)
        (record-anomaly *current-tracker*
                       :package-cycle
                       :error
                       pkg-name
                       (format nil "Package dependency cycle detected: ~A" chain)
                       cycle)))))