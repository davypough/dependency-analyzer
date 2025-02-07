;;;; Filename: file-parser-utils.lisp

;;; Utility functions for parsing, package handling, and cycle detection.
;;; Contains pure functions for name normalization, symbol lookup,
;;; package option processing, and dependency cycle detection.

(in-package #:dep)


(alexandria:define-constant +comparison-functions+ 
  '((sort . 1)           ; predicate in position 1 (0-based)
    (stable-sort . 1)    
    (merge . 3))         ; predicate in position 3 for merge
  :test #'equal
  :documentation "Maps comparison functions to the position of their predicate argument")


(alexandria:define-constant +mapping-functions+
  '((map . 1)            ; function in position 1 after result-type
    (mapcar . 0)         ; function in position 0
    (mapcan . 0)
    (mapc . 0)
    (maplist . 0)
    (mapcon . 0)
    (some . 0)
    (every . 0)
    (notany . 0)
    (notevery . 0))
  :test #'equal
  :documentation "Maps mapping functions to position of their function argument")


(alexandria:define-constant +sequence-functions+
  '(;; Basic sequence functions - sequence in position 1
    (remove . 1) (remove-if . 1) (remove-if-not . 1)
    (delete . 1) (delete-if . 1) (delete-if-not . 1)
    (substitute . 2) (substitute-if . 2) (substitute-if-not . 2)
    (find . 1) (find-if . 1) (find-if-not . 1)
    (position . 1) (position-if . 1) (position-if-not . 1) 
    (count . 1) (count-if . 1) (count-if-not . 1)
    (member . 1) (member-if . 1) (member-if-not . 1)
    ;; List functions
    (reduce . 1)
    (assoc . 1) (rassoc . 1)
    ;; Set functions - first list in position 0
    (adjoin . 0)
    (union . 0) (intersection . 0) (set-difference . 0))
  :test #'equal)


(defmacro pushit (call result-sym)
  "Push function calls onto RESULT-SYM while preserving the call's value.
   For atomic forms, pushes (form value).
   For direct function calls, pushes (fn arg1 arg2...).
   For higher-order functions:
   - Comparison functions (sort etc): Uses sequence element twice (fn elem elem)  
   - Mapping functions (mapcar etc): Uses first element from each sequence
   - Sequence/list functions with :key/:test/:test-not: Uses appropriate elements"
  (if (atom call)
      `(let ((val ,call))
         (push (list ',call val) ,result-sym)
         val)
    (destructuring-bind (fn &rest args) call
      (let ((fn-pos (or (cdr (assoc fn +comparison-functions+))
                        (cdr (assoc fn +mapping-functions+)))))
        (cond
          ;; Direct funcall/apply - extract actual function and args
          ((member fn '(funcall apply))
           (let ((actual-fn (second call))
                 (actual-args (cddr call))  
                 (vals (mapcar (lambda (_) (declare (ignore _))
                                 (gensym "arg"))
                               (cddr call)))
                 (the-value (gensym "value")))
             `(let* (,@(mapcar (lambda (tmp arg) 
                                `(,tmp ,arg))
                              vals actual-args))
                (let ((,the-value (,fn ,actual-fn ,@vals)))
                  (push (cons ,(cond ((and (consp actual-fn)
                                           (eq (car actual-fn) 'function))
                                      `(fdefinition ',(cadr actual-fn)))
                                     ((symbolp actual-fn)
                                      `(fdefinition ',actual-fn))
                                     (t actual-fn))
                              (mapcar #'identity (list ,@vals)))
                        ,result-sym)
                  ,the-value))))

          ;; Comparison function (sort, stable-sort, merge)
          ((assoc fn +comparison-functions+)
           (let ((pred-arg (nth fn-pos args))
                 (vals (mapcar (lambda (_) (declare (ignore _)) (gensym "arg")) args))
                 (the-value (gensym "value")))
             `(let* (,@(mapcar (lambda (tmp arg) `(,tmp ,arg)) vals args))
                (let ((,the-value (,fn ,@vals)))
                  (when (and (typep ,(nth (if (eq fn 'merge) 1 0) vals) 'sequence)
                             (plusp (length ,(nth (if (eq fn 'merge) 1 0) vals))))
                    (let ((elem (elt ,(nth (if (eq fn 'merge) 1 0) vals) 0)))
                      (push (cons ,(cond 
                                     ;; Handle #'fn form
                                     ((and (consp pred-arg)
                                           (eq (car pred-arg) 'function))
                                     `(fdefinition ',(cadr pred-arg)))
                                     ;; Handle (eval '#'fn) form  
                                     ((and (consp pred-arg)
                                           (eq (car pred-arg) 'eval)
                                           (consp (cadr pred-arg))
                                           (eq (car (cadr pred-arg)) 'quote)
                                           (consp (cadr (cadr pred-arg)))
                                           (eq (car (cadr (cadr pred-arg))) 'function))
                                      `(fdefinition ',(cadr (cadr (cadr pred-arg)))))
                                     ;; Handle bare symbol
                                     ((symbolp pred-arg)
                                      `(fdefinition ',pred-arg))
                                     ;; Handle direct function object
                                     (t pred-arg))
                                  (list elem elem))
                            ,result-sym)))
                  ,the-value))))

          ;; Mapping function (mapcar etc)
          ((assoc fn +mapping-functions+)
           (let ((fn-arg (nth fn-pos args))          
                 (vals (mapcar (lambda (_) (declare (ignore _)) (gensym "arg")) args))
                 (the-value (gensym "value")))
             `(let* (,@(mapcar (lambda (tmp arg) `(,tmp ,arg)) vals args))
                (let ((,the-value (,fn ,@vals)))
                  (when (every (lambda (seq)
                                 (and (typep seq 'sequence)
                                      (plusp (length seq))))
                               (list ,@(mapcar (lambda (v) v)
                                                 (nthcdr (1+ fn-pos) vals))))
                    (push (cons ,(cond ((and (consp fn-arg)
                                             (eq (car fn-arg) 'function))
                                        `(fdefinition ',(cadr fn-arg)))
                                       ((and (consp fn-arg)  
                                             (eq (car fn-arg) 'quote))
                                        `(fdefinition ',(cadr fn-arg)))
                                       ;; Skip lambda case - no dependencies to track
                                       (t nil))
                                (mapcar (lambda (seq)
                                          (elt seq 0))
                                        (list ,@(mapcar (lambda (v) v)
                                                          (nthcdr (1+ fn-pos) vals)))))
                          ,result-sym))
                  ,the-value))))

          ;; Sequence/List functions with keyword function args
((assoc fn +sequence-functions+)
 (let* ((seq-pos (cdr (assoc fn +sequence-functions+)))
        (vals (mapcar (lambda (_) (declare (ignore _)) 
                       (gensym "arg"))
                     args))
        (the-value (gensym "value")))
   `(let* (,@(mapcar (lambda (tmp arg) 
                       `(,tmp ,arg))
                     vals args))
      (let ((,the-value (,fn ,@vals)))
        (when-let ((seq ,(nth seq-pos vals)))
          (when (and (typep seq 'sequence)
                    (plusp (length seq)))
            (let ((elem `',(elt seq 0)))
              (let ((required-args ,(1+ seq-pos)))
                (do* ((orig-args (nthcdr required-args (list ,@args)))
                      (remaining-args orig-args (cddr remaining-args))
                      (keyword (car remaining-args) (car remaining-args))
                      (orig-val (cadr remaining-args) (cadr remaining-args)))
                     ((null remaining-args))
                  (when (and (keywordp keyword)
                            (member keyword '(:key :test :test-not)))
                    (push (cons orig-val
                              (if (eq keyword :key)
                                  (list elem)
                                  (list elem elem)))
                          ,result-sym)))))))
        ,the-value))))

          ;; Normal function call  
          (t
           (let ((vals (mapcar (lambda (_)
                                (declare (ignore _))
                                (gensym "arg"))
                              args))
                 (the-value (gensym "value")))
             `(let* (,@(mapcar (lambda (tmp arg)
                                `(,tmp ,arg))
                              vals args))
                (let ((,the-value (,fn ,@vals)))
                  (push (cons (fdefinition ',fn)
                            (mapcar #'identity (list ,@vals)))
                        ,result-sym)
                  ,the-value)))))))))


(defun symbol-status (sym pkg)
  "Determines the status of a symbol in a package, :internal, :external, :inherited, :nil."
  (let ((name (if (and (listp sym) (eq (car sym) 'setf))
                  (format nil "(SETF ~A)" (symbol-name (cadr sym)))
                  (symbol-name sym))))
    (if-let (status (nth-value 1 (find-symbol name pkg)))
       status
       :nil)))


(defun get-file-definitions (&optional (tracker nil tracker-provided-p) file)
  "Get all definitions in a file."
  (let ((actual-tracker (if tracker-provided-p tracker (ensure-tracker))))
    (gethash file (slot-value actual-tracker 'file-map))))


(defun collect-file-references (tracker source-file target-file)
  "Collect all references in SOURCE-FILE that reference definitions in TARGET-FILE.
   Returns a list of reference objects."
  (let ((refs nil))
    ;; Build hash table of symbols defined in target file for quick lookup
    (let ((target-symbols (make-hash-table :test 'equal)))
      (dolist (def (get-file-definitions tracker target-file))
        (push def (gethash (definition.name def) target-symbols)))
      ;; Check all references in source file to see if they reference target symbols
      (maphash (lambda (key refs-list)
                 (declare (ignore key))
                 (dolist (ref refs-list)
                   (when (and (equal (reference.file ref) source-file)
                            (gethash (reference.name ref) target-symbols))
                     (pushnew ref refs :test #'equal))))
               (slot-value tracker 'references)))
    ;; Return sorted list of references
    (sort refs #'string< 
          :key (lambda (r) 
                (with-output-to-string (s)
                  (format s "~A~{~A~}~{~A~}" 
                         (reference.name r)
                         (reference.qualifiers r)
                         (reference.arguments r)))))))


(defun record-definition (tracker &key name type file package status context qualifiers specializers)
  "Record a name definition in the tracker. For methods, qualifiers and specializers
   are used to create a specialized lookup key. Records anomalies for duplicate definitions."
  (let* ((def (make-instance 'definition 
                            :name name
                            :type type 
                            :file file
                            :package package
                            :status status
                            :context context
                            :qualifiers qualifiers
                            :specializers specializers))
         ;; Generate appropriate key based on definition type
         (key (if (eq type :METHOD)
                 (make-tracking-key name package type qualifiers specializers)
                 (make-tracking-key name package type)))
         ;; Look for existing definitions with this key
         (existing-defs (gethash key (slot-value tracker 'definitions)))
         ;; Filter out definitions from same file
         (other-file-defs (remove-if (lambda (d)
                                       (equal (definition.file d) file))
                                     existing-defs)))
    
    ;; Check for duplicates and record anomaly if found in other files
    (when other-file-defs
      (let ((def-files (mapcar #'definition.file other-file-defs)))
        (record-anomaly tracker
                       :type :duplicate-definition
                       :severity :WARNING
                       :file file
                       :description (format nil "~A also defined in ~A" name def-files)
                       :package package
                       :context (definition.context def))))
    
    ;; Store definition under the computed key
    (setf (gethash key (slot-value tracker 'definitions))
      (cons def existing-defs))
    ;; Add to file map
    (setf (gethash file (slot-value tracker 'file-map))
      (cons def (gethash file (slot-value tracker 'file-map))))
    ;; Record exports for non-package definitions
    (unless (eq type :package)
      (when (eq status :external)
        (record-export tracker package name)))
    def))


(defun record-reference (tracker &key name file type package context visibility definitions
                                     qualifiers arguments)
  "Record a name reference in the tracker.
   VISIBILITY is inherited, imported, or local (defaults to local)
   DEFINITIONS is a non-empty list of definitions this reference depends on
   QUALIFIERS tracks method qualifiers in the call 
   ARGUMENTS is a list of argument values"
  (declare (special log-stream))
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
   (format log-stream "~%~A~%" ref)
   ref))


(defun record-anomaly (tracker &key type severity file description package context)
  "Record a new anomaly in the dependency tracker.
   For :duplicate-definition type, files must be provided as list of all definition locations."
  (let ((anomaly (make-instance 'anomaly 
                               :type type 
                               :severity severity 
                               :file file
                               :description description
                               :package package
                               :context context)))
    (pushnew anomaly (gethash type (anomalies tracker))
                     :test #'equal :key #'anomaly.file)
    anomaly))


(defun record-package-use (tracker using-package used-package)
  "Record that one package uses another package."
  (pushnew used-package 
           (gethash using-package (slot-value tracker 'package-uses))
           :test #'string=))


(defun record-export (tracker package-name name)
  "Record a name as being exported from a package.
   Both package-name and name can be either strings or symbols."
  (when-let* ((pkg (find-package (string package-name)))
              (exported-sym (etypecase name
                              (string (intern name pkg))
                              (symbol (intern (symbol-name name) pkg)))))
    (pushnew exported-sym (gethash (string package-name) (slot-value tracker 'package-exports))
             :test #'eq)))


(defun record-package-cycle (tracker cycle-chain)
  "Record a package dependency cycle."
  (pushnew cycle-chain (package-cycles tracker) :test #'string=))


(defun get-file-cycles (&optional tracker)
  "Get all recorded file dependency cycles."
  (file-cycles (ensure-tracker tracker)))


(defun get-package-cycles (&optional tracker)
  "Get all recorded package dependency cycles."
  (package-cycles (ensure-tracker tracker)))


(defun detect-package-cycle (pkg-name current-packages)
  "Check for and record any package dependency cycles.
   A cycle exists if the package being processed appears in the current package stack.
   
   PKG-NAME - Name of package being checked (string, already normalized)
   CURRENT-PACKAGES - Stack of packages currently being processed (list of strings)
   
   Example cycle: A -> B -> C -> A would be recorded as \"A -> B -> C -> A\"
   
   Records both:
   1. The cycle chain in the tracker's package-cycles list
   2. An anomaly record with severity ERROR"
  (when-let* ((position (member pkg-name current-packages :test #'string=))
              (cycle (cons pkg-name (ldiff current-packages position)))
              (chain (format nil "~{~A~^ -> ~}" (reverse cycle))))
    ;; Record as both a cycle and an anomaly
    (record-package-cycle *current-tracker* chain)
    (record-anomaly *current-tracker*
                :type :package-cycle
                :severity :WARNING
                :package chain
                :description (format nil "Package dependency cycle detected: ~A" chain)
                :context cycle)))


(defun cl-symbol-p (item)
 "Return true if item is from the common-lisp package."
 (and (symbolp item)
      (eq (symbol-package item) 
          (find-package :common-lisp))))


(defun quoted-symbol-p (item)
 "Return true if item is quoted with quote (but not function quote);
  eg, 'var, (quote var). Function quotes (#'foo, (function foo)) return nil."
 (and (consp item)
      (eq (car item) 'quote)))


(defun symbol-qualified-p (symbol parser)
  "Return t if symbol appears to have been package-qualified in source.
   This is inferred when symbol's package differs from parser's current package."
  (and (symbol-package symbol)  ; Has a package
       (not (eq (symbol-package symbol) 
                (current-package parser)))))


(defun skip-item-p (form)
  "Return T if form should be skipped during definition and reference analysis."
  (or (null form)
      (numberp form) 
      (stringp form)
      (keywordp form)
      (cl-symbol-p form)
      (quoted-symbol-p form)))


(defun skip-definition-form (form)
  "Return T if form should be skipped during reference analysis."
  (and (consp form)
       (member (car form) '(defun defvar defparameter defconstant 
                            defclass defstruct defmethod defgeneric 
                            define-condition defmacro define-method-combination
                            define-modify-macro define-compiler-macro
                            define-symbol-macro defsetf define-setf-expander
                            deftype))))


(defun walk-form (form handler)
  "Walk a form calling HANDLER on each subform with context and parent context info.
   FORM - The form to analyze
   HANDLER - Function taking (form context parent-context)"
  (declare (special log-stream))
  (labels ((walk (x context parent-context)
             (format log-stream "~&WALK(ING):~%  Expression: ~S~%  Context: ~S~%  Parent: ~S~%" 
                              x context parent-context)
             (unless (skip-item-p x)  ;skip non-referring items
               (funcall handler x context parent-context)  
               ;; Recursively process subforms
               (typecase x
                 (cons 
                   (dolist (element x)
                     (walk element x context)))
                 (array 
                   (dotimes (i (array-total-size x))
                     (walk (row-major-aref x i) x context)))
                 (hash-table 
                   (maphash (lambda (k v)
                             (walk k x context)
                             (walk v x context))
                           x))))))
    
    ;; Start walking at top level
    (let ((*print-circle* nil)     ; Prevent circular printing issues
          (*print-length* 10)      ; Limit list length 
          (*print-level* 5))       ; Limit nesting output
      (format log-stream "~&-------------------------------~%")
      (walk form form form))))


(defun package-context-p (context parent-context)
  "Return T if the context indicates a form that accepts package designator references.
   Such contexts include:
   - Direct package forms (defpackage, in-package, use-package etc)  
   - Package options in defpackage (:use, :import-from, :nicknames)"
  (and (consp context)
       (or ;; Direct package forms
           (member (car context) '(defpackage in-package use-package 
                                   make-package rename-package delete-package))
           ;; Package options
           (and (consp parent-context)
                (eq (car parent-context) 'defpackage)
                (keywordp (car context))
                (member (car context) '(:use :import-from :nicknames))))))


(defun not-interned-p (symbol)
  (not (symbol-package symbol)))


(defun find-method-call-definitions (parser form)
  "Find method and generic function definitions for a method call form."
  (let* ((name (car form))
         (args (mapcar (lambda (arg)
                        (ignore-errors (eval arg))) 
                      (cdr form)))
         (gf (ignore-errors (fdefinition name))))
    (when (typep gf 'generic-function)
      (let* ((methods (ignore-errors (compute-applicable-methods gf args)))
             (pkg-name (current-package-name parser))
             (gf-key (make-tracking-key name pkg-name :GENERIC-FUNCTION))
             (gf-defs (gethash gf-key (slot-value *current-tracker* 'definitions)))
             (method-defs (loop for method in methods
                               for specs = (mapcar #'class-name 
                                                 (c2mop:method-specializers method))
                               for method-key = (make-tracking-key name pkg-name :METHOD 
                                                                 nil  ; qualifiers 
                                                                 specs)
                               append (gethash method-key (slot-value *current-tracker* 'definitions)))))
        (values (car gf-defs) method-defs)))))


(defun type-compatible-p (specializer arg parser)
  "Test if argument could match specializer using runtime type info.
   Uses only standard CL type operations for compatibility checks."
  (flet ((get-make-name (type)
           (intern (concatenate 'string "MAKE-" (string type))
                  (if (symbolp type)
                      (symbol-package type)
                      *package*))))
    
    (cond
      ;; T specializer matches anything
      ((or (eq specializer t)
           (eq specializer 'cl:t)
           (string= (princ-to-string specializer) "T"))
       t)
      
      ;; EQL specializer
      ((and (listp specializer)
           (eq (car specializer) 'eql))
       (let ((eql-val (cadr specializer)))
         (cond
           ;; Constant value - direct comparison
           ((or (numberp arg) (characterp arg) 
                (stringp arg) (pathnamep arg))
            (equal eql-val arg))
           ;; Symbol comparison
           ((symbolp arg)
            (eq eql-val arg))
           ;; Non-constant - assume potentially compatible
           (t t))))
      
      ;; Constructor form 
      ((and (or (and (consp arg)
                     (symbolp (car arg))
                     (eq (car arg) (get-make-name specializer)))
                (and (symbolp arg)
                     (eq arg (get-make-name specializer))))
            (find-package (current-package-name parser)))
       (let ((*package* (find-package (current-package-name parser))))
         (handler-case
             (if (consp arg)
                 ;; Verify constructor exists and returns compatible type
                 (when (fboundp (car arg))
                   (or 
                    ;; Check if specializer names a class
                    (find-class specializer nil)
                    ;; Or if it's a type specifier
                    (ignore-errors
                      (subtypep specializer t))))
                 ;; Just verify function existence for symbol
                 (fboundp arg))
           (error () t))))
      
      ;; Class/type specializer
      ((symbolp specializer)
       (multiple-value-bind (subtype-p valid-p)
           (ignore-errors 
             (cond
               ;; Known literal type
               ((or (numberp arg) (characterp arg)
                    (stringp arg) (pathnamep arg))
                (subtypep (type-of arg) specializer))
               ;; make-instance form
               ((and (consp arg)
                     (eq (car arg) 'make-instance))
                (ignore-errors
                  (and (find-class (second arg) nil)
                       (subtypep (second arg) specializer))))
               ;; Symbol - check if names a type
               ((symbolp arg)
                (or 
                 ;; Check as class name
                 (and (find-class arg nil)
                      (subtypep arg specializer))
                 ;; Check as type specifier
                 (ignore-errors
                   (subtypep arg specializer))))
               ;; Other forms - assume compatible
               (t t)))
         (if valid-p
             subtype-p
             t)))
      
      ;; Unknown/complex specializer - conservative
      (t t))))


(defun analyze-call-qualifiers (name-form)
  "Analyze a method call name form to extract name and qualifiers.
   Handles multiple qualifier lists correctly."
  (etypecase name-form
    (symbol (list name-form))
    (cons (if (every #'keywordp (butlast name-form))
              (list* (car (last name-form)) (butlast name-form))
              (list (car name-form))))))


(defun specializer-compatible-p (specializer arg)
  "Test if an argument form could match a method specializer.
   Uses runtime type information when available.
   Returns T if potentially compatible, NIL if definitely incompatible."
  (labels ((parse-eql-spec (spec)
             "Parse EQL specializer form, handling string/cons formats"
             (typecase spec
               (cons 
                (when (eq (car spec) 'eql)
                  (cadr spec)))
               (string
                (ignore-errors
                  (let ((form (read-from-string spec)))
                    (when (and (consp form)
                              (eq (car form) 'eql))
                      (cadr form)))))))
           
           (get-constructor-name (type-name)
             "Get constructor function name for a type"
             (intern (concatenate 'string "MAKE-" 
                                (string type-name))
                    (if (symbolp type-name)
                        (symbol-package type-name)
                        *package*)))
           
           (constant-value-p (form)
             "Test if form represents a constant value"
             (or (numberp form)
                 (characterp form)
                 (stringp form)
                 (pathnamep form)
                 (and (symbolp form)
                      (not (boundp form)))))
           
           (valid-type-p (type)
             "Test if type specifier names a valid type"
             (or (find-class type nil)
                 (ignore-errors
                   (subtypep type t)))))
    
    (cond
      ;; T specializer - matches anything
      ((or (eq specializer t)
           (eq specializer 'cl:t)
           (string= (princ-to-string specializer) "T"))
       t)
      
      ;; EQL specializer
      ((or (and (consp specializer)
                (eq (car specializer) 'eql))
           (and (stringp specializer)
                (> (length specializer) 5)
                (string= (subseq specializer 0 5) "(EQL ")))
       (cond
           ;; Constant value comparison
           ((constant-value-p arg)
            (equal (parse-eql-spec specializer) arg))
           ;; Variable or complex form - assume potentially compatible
           (t t)))
      
      ;; Constructor form for class
      ((and (consp arg)
            (symbolp (car arg))
            (eq (car arg)
                (get-constructor-name specializer)))
       (if (valid-type-p specializer)
           ;; Verify constructor exists
           (fboundp (get-constructor-name specializer))
           t))
      
      ;; Non-constant compound form
      ((consp arg)
       (or 
        ;; make-instance form check
        (and (eq (car arg) 'make-instance)
             (valid-type-p (second arg))
             (subtypep (second arg) specializer))
        ;; Other forms - conservative
        t))
      
      ;; Simple type specializer with constant
      ((and (not (consp arg))
            (valid-type-p specializer))
       (if (constant-value-p arg)
           (typep arg specializer)
           t))
      
      ;; Unknown/complex case - conservative
      (t t))))


(defun analyze-function-call-context (symbol context)
  "Analyze if a symbol appears in a function call context.
   Returns (values call-p name args caller-type), where:
   - call-p: T if symbol is being called as a function
   - name: The function name being called
   - args: List of argument forms
   - caller-type: One of :direct :funcall :apply :method :macro"
  (labels ((quoted-p (form)
             "Test if form is quoted"
             (and (consp form)
                  (member (car form) '(quote function))))
           
           (function-name-p (sym)
             "Test if symbol can be a function name"
             (and sym
                  (symbolp sym)
                  (not (keywordp sym))
                  (not (special-operator-p sym))
                  (not (macro-function sym))))
           
           (valid-function-p (sym)
             "Test if symbol names a valid function"
             (or (cl-symbol-p sym)  ; CL package functions always valid
                 (and (function-name-p sym)
                      (fboundp sym)))))
    
    (when (and context (listp context))
      (cond
        ;; Direct function call 
        ((and (eq symbol (car context))
              (not (quoted-p context))
              (valid-function-p symbol))
         (values t 
                 symbol 
                 (cdr context)
                 :direct))
        
        ;; Funcall with #'function form
        ((and (eq (car context) 'funcall)
              (listp (second context))
              (eq (car (second context)) 'function)
              (eq (cadr (second context)) symbol)
              (valid-function-p symbol))
         (values t 
                 symbol 
                 (cddr context)
                 :funcall))
        
        ;; Apply with #'function form
        ((and (eq (car context) 'apply)
              (listp (second context))
              (eq (car (second context)) 'function)
              (eq (cadr (second context)) symbol)
              (valid-function-p symbol))
         (values t 
                 symbol 
                 (cddr context)
                 :apply))
        
        ;; Funcall/apply with symbol
        ((and (member (car context) '(funcall apply))
              (eq (second context) symbol)
              (valid-function-p symbol))
         (values t 
                 symbol 
                 (cddr context)
                 (if (eq (car context) 'funcall)
                     :funcall
                     :apply)))
        
        ;; Method combination forms
        ((and (member symbol '(call-next-method next-method-p make-method))
              (eq symbol (car context)))
         (values t 
                 symbol 
                 (cdr context)
                 :method))
        
        ;; Not a function call
        (t (values nil nil nil nil))))))


(defun slot-definition-p (symbol context)
  "Returns true if symbol appears as a struct/class slot definition."
  (and (consp context)
       (or (and (eq (car context) 'defstruct)
                (consp (cdr context))
                (or (symbolp (cadr context))
                    (and (consp (cadr context))
                         (symbolp (caadr context)))))
       (member symbol (cddr context) :key (lambda (x)
                                          (if (consp x) (car x) x))))))


(defun extract-specializers (lambda-list)
  "Extract specializer types from a method lambda list.
   Returns list of type specializers as symbols or lists for (eql ...) forms.
   Returns (T) for non-method lambda lists.
   Examples: 
    ((data string) x &key k) -> (STRING T)
    (a (b number) (c (eql 42))) -> (T NUMBER (EQL 42))"
  (loop for param in (loop for param in lambda-list
                          until (and (symbolp param) 
                                   (member param lambda-list-keywords))
                          collect param)
        collect (if (listp param)
                   (second param) 
                   t)))


(defun lambda-list-p (form)
  "Test if form appears to be a method lambda list."
  (and (listp form)
       (every (lambda (x)
                (or (symbolp x)
                    (and (listp x)
                         (= (length x) 2)
                         (symbolp (car x)))))
              form)))


(defun analyze-method-specializers (parser lambda-list)
  "Analyze specializer types in method lambda list."
  (dolist (param lambda-list)
    (when (and (listp param)
               (= (length param) 2))
      (let ((specializer (second param)))
        (typecase specializer
          (cons
           (when (eq (car specializer) 'eql)
             (analyze-definition-form parser (cadr specializer))))
          (symbol  ; Class/type specializer
           (analyze-definition-form parser specializer)))))))


(defun get-defstruct-conc-name (name current-form)
  "Extract the :conc-name prefix for a defstruct.
   Returns string to prepend to slot names."
  (if (and (consp (second current-form))           ; Has options list
           (consp (cadr current-form)))            ; Not just a name
    (let* ((options (cdadr current-form))        ; Skip struct name
           (conc-option (find :conc-name options :key #'car)))
      (if conc-option
        (let ((prefix (cadr conc-option)))
          (cond ((null prefix) "")       ; (:conc-name nil)
                ((stringp prefix) prefix)
                (t (string prefix))))    ; Symbol case
        (format nil "~A-" (string name))))
    (format nil "~A-" (string name))))      ; Default case


(defun record-slot-accessors (parser name class current-form)
  "Record all slot accessor definitions for a class/struct/condition"
  (declare (special log-stream))
  (let ((pkg (current-package parser)))
    (if (typep class 'structure-class)
        ;; Structure slots use naming pattern with conc-name
        (let ((prefix (get-defstruct-conc-name name current-form)))
          (dolist (slot (c2mop:class-slots class))
            (let ((accessor-name 
                   (intern (format nil "~A~A" 
                                 prefix
                                 (symbol-name (c2mop:slot-definition-name slot)))
                           pkg)))
              (when (fboundp accessor-name)
                (record-definition *current-tracker*
                                 :name accessor-name
                                 :type :function   ; Structures use regular functions
                                 :file (file parser)
                                 :package pkg
                                 :status (symbol-status accessor-name pkg)
                                 :context current-form)))))
        ;; CLOS class slots use MOP info
        (progn 
          (c2mop:finalize-inheritance class)
          (dolist (slot (c2mop:class-direct-slots class))
            ;; Record reader methods
            (dolist (reader (c2mop:slot-definition-readers slot))
              (when (fboundp reader)
                (record-definition *current-tracker*
                                 :name reader
                                 :type :method    ; CLOS readers are methods
                                 :file (file parser)
                                 :package pkg
                                 :status (symbol-status reader pkg)
                                 :context current-form
                                 :specializers (list name))))
            ;; Record writer methods
            (dolist (writer (c2mop:slot-definition-writers slot))
              (when (fboundp writer)
                (record-definition *current-tracker*
                                 :name writer
                                 :type :method    ; CLOS writers are methods
                                 :file (file parser)
                                 :package pkg
                                 :status (symbol-status writer pkg)
                                 :context current-form
                                 :specializers (list name)))))))))


(defun record-defstruct-functions (parser name class current-form)
  "Record constructor, copier, and predicate functions for structures"
  (declare (special log-stream) (ignorable class))
  (let ((pkg (current-package parser)))
    ;; Constructor
    (let ((make-name (intern (format nil "MAKE-~A" (symbol-name name)) pkg)))
      (when (fboundp make-name)
        (record-definition *current-tracker*
                          :name make-name
                          :type :function
                          :file (file parser)
                          :package pkg
                          :status (symbol-status make-name pkg)
                          :context current-form)))
    ;; Copier
    (let ((copy-name (intern (format nil "COPY-~A" (symbol-name name)) pkg)))
      (when (fboundp copy-name)
        (record-definition *current-tracker*
                          :name copy-name
                          :type :function
                          :file (file parser)
                          :package pkg
                          :status (symbol-status copy-name pkg)
                          :context current-form)))
    ;; Predicate
    (let ((pred-name (intern (format nil "~A-P" (symbol-name name)) pkg)))
      (when (fboundp pred-name)
        (record-definition *current-tracker*
                          :name pred-name
                          :type :function
                          :file (file parser)
                          :package pkg
                          :status (symbol-status pred-name pkg)
                          :context current-form)))))


(defun analyze-defclass/defstruct/define-condition (parser name form)
  "Analyze a defclass, defstruct, or define-condition form.
   Records the primary type definition and all implicitly defined functions."
  (declare (special log-stream))
  (let ((def-op (first form))
        (class (find-class name nil)))
    ;; Common accessor analysis
    (record-slot-accessors parser name class form)
    ;; Only defstruct has additional implicit functions
    (when (eq def-op 'defstruct)
      (record-defstruct-functions parser name class form))))


(defun analyze-defpackage/make-package (parser name current-form)
  "Handle defpackage/make-package form recording nicknames as definitions.
   Uses runtime package info when available, falls back to form parsing
   for make-package when package doesn't yet exist."
  (declare (special log-stream))
  (let ((file (file parser))
        (context current-form)
        (form-type (car current-form))
        (package (find-package name)))
    
    ;; Try runtime first for both forms
    (if package
        ;; Use runtime info when available
        (dolist (nickname (package-nicknames package))
          (record-definition *current-tracker*
                           :name nickname
                           :type :PACKAGE  
                           :file file
                           :context context))
        
        ;; Fall back to parsing for make-package only
        (when (eq form-type 'make-package)
          (loop for (key val) on (cddr current-form) by #'cddr
                when (eq key :nicknames)
                do (dolist (nick (ensure-list (if (and (consp val)
                                                      (eq (car val) 'quote))
                                               (cadr val)
                                               val)))
                     (record-definition *current-tracker*
                                      :name nick
                                      :type :PACKAGE
                                      :file file
                                      :context context)))))
    
    ;; Process exports only when package exists
    (when package
      (do-external-symbols (sym package)
        (cond 
          ((boundp sym)  ; Variable binding
           (record-definition *current-tracker*
                            :name sym
                            :type :VARIABLE
                            :file file
                            :package package
                            :status :EXTERNAL
                            :context context))
          ((macro-function sym)  ; Macro binding - check before fboundp
           (record-definition *current-tracker*
                            :name sym
                            :type :MACRO
                            :file file
                            :package package
                            :status :EXTERNAL
                            :context context))
          ((fboundp sym)  ; Function binding (not macro)
           (if (typep (symbol-function sym) 'standard-generic-function)
               (record-definition *current-tracker*
                                :name sym
                                :type :GENERIC-FUNCTION
                                :file file
                                :package package
                                :status :EXTERNAL
                                :context context)
               (record-definition *current-tracker*
                                :name sym
                                :type :FUNCTION
                                :file file
                                :package package
                                :status :EXTERNAL
                                :context context)))
          ((find-class sym nil)  ; Class definition
           (record-definition *current-tracker*
                            :name sym
                            :type :STRUCTURE/CLASS/CONDITION
                            :file file
                            :package package
                            :status :EXTERNAL
                            :context context))
          ((ignore-errors (subtypep nil sym))  ; Type definition
           (record-definition *current-tracker*
                            :name sym
                            :type :DEFTYPE
                            :file file
                            :package package
                            :status :EXTERNAL
                            :context context)))))))


(defun get-symbol-reference-type (sym &optional context)
  "Derive the reference type of a symbol based on its bindings and usage context."
  (cond 
    ;; When symbol appears in function call position
    ((and context 
          (consp context)
          (eq (car context) sym)
          (fboundp sym))
     (let ((fn (fdefinition sym)))
       (when (typep fn 'standard-generic-function)
         :generic-function)))

    ;; When symbol is used in funcall/apply
    ((and context
          (consp context)
          (member (car context) '(funcall apply))
          (consp (second context))
          (member (car (second context)) '(quote function))
          (eq (cadr (second context)) sym)
          (fboundp sym))
     (let ((fn (fdefinition sym)))
       (if (typep fn 'standard-generic-function)
           :generic-function
           :function)))
    
    ;; Check variable binding first since functions can also be bound as values
    ((boundp sym) 
     :variable)
    
    ;; Check for macro before function since macros have function bindings
    ((macro-function sym) 
     :macro)
    
    ;; Generic functions and regular functions
    ((fboundp sym)
     (let ((fn (fdefinition sym)))
       (if (typep fn 'standard-generic-function)
           :generic-function
           :function)))
    
    ;; Classes and structures
    ((ignore-errors (find-class sym))
     :structure/class/condition)
    
    ;; Type declarations that aren't classes
    ((and (subtypep sym t)
          (not (ignore-errors (find-class sym)))
          (not (cl-symbol-p sym)))
     :deftype)
    
    ;; Symbol macros last since they're least common
    ((nth-value 1 (macroexpand-1 sym))
     :symbol-macro)))


(defun compatible-method-p (sym context top-level-form)
 "Test if symbol is being called as a method. Returns matching method if found."
 (when (fboundp sym) 
   (let ((fn (fdefinition sym)))
     (when (typep fn 'standard-generic-function)
       (let* ((args (rest context))
              ;; Evaluate whole form but wrap target expr in type-of
              (type-form (subst `(list (type-of ,(car args)) ,(car args))
                               (car args)
                               (copy-tree top-level-form)))
              (type-result (handler-case (eval type-form)
                            (error () nil))))
         ;; If we got the type, find matching method
         (when type-result  
           (let ((arg-type (car type-result)))
             (when arg-type
               (first (compute-applicable-methods 
                       fn (list arg-type)))))))))))


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


(defun extract-function-name (form)
  "Extract function name from quoted function forms like #'name or (function name)"
  (when (and (consp form)
             (member (car form) '(quote function)))
    (cadr form)))