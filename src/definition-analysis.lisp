;;;; Filename:  definition-analysis.lisp

;;;; Analyze & record lisp definitions in the user's project files.


(in-package :dep)


(defun parse-definitions-in-file (parser)
  "First pass parser that records definitions. Analyzes all forms recursively,
   maintaining package context during traversal. Logs analysis progress to provided log-stream."
  (with-slots (file parsing-files) parser
    ;; Reset to CL-USER before processing each file
    (setf (current-package parser) (find-package :common-lisp-user)
          (current-package-name parser) "COMMON-LISP-USER"
          *package* (find-package :common-lisp-user))
    (push file parsing-files)
    (with-open-file (stream file :direction :input)
      (loop for form = (read stream nil :eof)
            until (eq form :eof)
            do (when (and (consp form) (eq (first form) 'in-package))
                 (eval form)
                 (analyze-in-package parser form))
               (analyze-definition-form parser form)))
    (pop parsing-files)))


(defun analyze-definition-form (parser form)
  "Analyze raw form from source file for definitions, recording location and basic info."
  (walk-form form
    (lambda (current-form context parent-context &optional form)
      (declare (ignorable context parent-context form))
      (when (and (consp current-form)  
                 (symbolp (first current-form))
                 (not (quoted-symbol-p (first current-form))))
          (let ((head (first current-form)))
            (cond
              ;; Variable definition 
              ((member head '(defvar defparameter defconstant))
               (let ((name (second current-form)))
                 (record-definition *current-tracker*
                                    :name name
                                    :type :variable 
                                    :file (file parser)
                                    :package (current-package parser)  ;from last in-package, not runtime lookup--could be in several
                                    :status (symbol-status name (current-package parser))  ;(symbol-package name))
                                    :context current-form)))

              ;; Function definition
              ((eq head 'defun)
               (let* ((full-name (second current-form))
                      (base-name (if (and (listp full-name) (eq (car full-name) 'setf))
                                   (cadr full-name)
                                   full-name)))
                 (record-definition *current-tracker*
                                    :name full-name
                                    :type :function
                                    :file (file parser)
                                    :package (current-package parser)
                                    :status (symbol-status base-name (current-package parser))
                                    :context current-form)))

              ;; Macro definition
              ((eq head 'defmacro)
               (record-definition *current-tracker*
                                  :name (second current-form)
                                  :type :macro
                                  :file (file parser)
                                  :package (current-package parser)
                                  :status (symbol-status (second current-form) (current-package parser))
                                  :context current-form))

              ;; Generic function definition
              ((eq head 'defgeneric)
               (record-definition *current-tracker*
                                  :name (second current-form)
                                  :type :generic-function
                                  :file (file parser)
                                  :package (current-package parser)
                                  :status (symbol-status (second current-form) (current-package parser))
                                  :context current-form))

              ;; Method definition
              ((eq head 'defmethod)
               (let* ((rest (cddr current-form))  ; Skip defmethod and name
                      (qualifiers (loop while (and rest (keywordp (car rest)))
                                        collect (pop rest)))
                      (lambda-list (car rest)))
                 (record-definition *current-tracker*
                                    :name (second current-form)
                                    :type :method
                                    :file (file parser)
                                    :package (current-package parser)
                                    :status (symbol-status (second current-form) (current-package parser))
                                    :context current-form
                                    :qualifiers qualifiers 
                                    :specializers (extract-specializers lambda-list))))

              ;; Structure/class definition
              ((member head '(defclass defstruct define-condition))
               (let ((name (if (and (eq head 'defstruct)  ;handle defstruct options
                                    (consp (second current-form)))
                             (caadr current-form)               
                             (second current-form))))
                 (record-definition *current-tracker*
                                    :name name
                                    :type :structure/class/condition
                                    :file (file parser)
                                    :package (current-package parser)
                                    :status (symbol-status name (current-package parser))
                                    :context current-form)
                 (analyze-defclass/defstruct/define-condition parser name current-form)))

              ;; Type definition
              ((eq head 'deftype)
               (record-definition *current-tracker*
                                  :name (second current-form)
                                  :type :deftype
                                  :file (file parser)
                                  :package (current-package parser)
                                  :status (symbol-status (second current-form) (current-package parser))
                                  :context current-form))

              ;; Package system
              ((member head '(defpackage make-package))
               (record-definition *current-tracker*
                                  :name (second current-form)  ;eg, "PKG", :pkg, #:pkg
                                  :type :package
                                  :file (file parser)
                                  :context current-form)
               (analyze-defpackage/make-package parser (second current-form) current-form))

              ;; Setf form definitions  eg, (setf (symbol-value 'sym-var) 0)
              ((and (eq head 'setf) 
                    (consp (second current-form))
                    (symbolp (caadr current-form))
                    (member (caadr current-form) '(symbol-value symbol-function macro-function))
                    (consp (cdadr current-form))
                    (consp (cadadr current-form))
                    (eq (first (cadadr current-form)) 'quote))
               (let ((accessor (caadr current-form))
                     (name (second (cadadr current-form))))
                 (record-definition *current-tracker*
                                    :name name
                                    :type (case accessor
                                            (symbol-value :variable)
                                            (symbol-function :function)
                                            (macro-function :macro))
                                    :file (file parser)
                                    :package (current-package parser)
                                    :status (symbol-status name (current-package parser))
                                    :context current-form)))

              ;; Extended definition forms
              ((member head '(defsetf define-setf-expander))
               (record-definition *current-tracker*
                                :name (second current-form)
                                :type :setf
                                :file (file parser) 
                                :package (current-package parser)
                                :status (symbol-status (second current-form) (current-package parser))
                                :context current-form))

              ((eq head 'define-symbol-macro)
               (record-definition *current-tracker*
                                :name (second current-form)
                                :type :symbol-macro
                                :file (file parser)
                                :package (current-package parser)
                                :status (symbol-status (second current-form) (current-package parser))
                                :context current-form))

              ((member head '(define-modify-macro define-compiler-macro))
               (record-definition *current-tracker*
                                :name (second current-form)
                                :type :macro
                                :file (file parser)
                                :package (current-package parser)
                                :status (symbol-status (second current-form) (current-package parser))
                                :context current-form))

              ((eq head 'define-method-combination)
               (record-definition *current-tracker*
                                :name (second current-form)
                                :type :method-combination
                                :file (file parser)
                                :package (current-package parser)
                                :status (symbol-status (second current-form) (current-package parser))
                                :context current-form)))))))
  form)


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
        (record-export tracker (package-name package) name)))
    def))


(defun record-export (tracker package-designator symbol)
  "Record a symbol as being exported from a package.
   Both package-designator and symbol can be either strings or symbols."
  (let* ((pkg-name (etypecase package-designator
                     (string package-designator)
                     (package (package-name package-designator))
                     (symbol (string package-designator))))
         (pkg (find-package pkg-name))
         (sym (etypecase symbol
                (string (intern symbol pkg))
                (symbol (intern (symbol-name symbol) pkg)))))
    (when pkg
      (pushnew sym 
               (gethash pkg-name 
                       (slot-value tracker 'package-exports))
               :test #'eq))))


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
  (declare (ignorable class))
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
  (let ((def-op (first form))
        (class (find-class name nil)))
    ;; Common accessor analysis
    (record-slot-accessors parser name class form)
    ;; Only defstruct has additional implicit functions
    (when (eq def-op 'defstruct)
      (record-defstruct-functions parser name class form))))


(defun analyze-defpackage/make-package (parser name current-form)
  "Handle defpackage/make-package form recording nicknames as definitions.
   Uses runtime info when available, falls back to form parsing
   for make-package when package doesn't yet exist."
  (let ((file (file parser))
        (context current-form)
        (form-type (car current-form)))
    
    ;; For make-package forms, just analyze the form structure
    (if (eq form-type 'make-package)
        (loop for (key val) on (cddr current-form) by #'cddr
              when (member key '(:nicknames :use))
              do (when (and (consp val) 
                           (member (car val) '(list quote)))
                   (dolist (item (if (eq (car val) 'list)
                                    (cdr val)    ; (list item1 item2)
                                    (cadr val))) ; (quote (item1 item2))
                     (when (or (symbolp item) (stringp item))
                       (when (eq key :nicknames)
                         (record-definition *current-tracker*
                                          :name item
                                          :type :package
                                          :file file
                                          :context context))))))
        
        ;; For defpackage, use runtime info when available
        (when-let (package (find-package name))
          ;; Record nicknames
          (dolist (nickname (package-nicknames package))
            (record-definition *current-tracker*
                             :name nickname
                             :type :package 
                             :file file
                             :context context))
          
          ;; Record exports but don't create definitions
          (do-external-symbols (sym package)
            (record-export *current-tracker* package sym))))))
