;;;; Filename:  utils.lisp

;;;; Code which is shared among the dependency analyzer files plus utilities.


(in-package :dep)


(defparameter *current-tracker* nil
  "The currently active dependency tracker instance.")


(defun project-pathname (pathname)
  "Convert a pathname to a string representation relative to project root.
   Returns a path starting with / that is relative to the project root.
   E.g., /source/file.lisp instead of /path/to/project/source/file.lisp"
  (when pathname
    (if-let (project-root (slot-value *current-tracker* 'project-root))
      (let ((relative (enough-namestring pathname project-root)))
        (if (char= (char relative 0) #\/)
          relative
          (concatenate 'string "/" relative)))
      (namestring pathname))))


(defun package-designator-to-string (designator)
  "Convert a package designator to its string name, preserving case.
   Valid designators are: package, string, symbol, or character.
   Signals type-error if designator is invalid."
  (etypecase designator
    (package (package-name designator))
    (string designator)
    (symbol (string designator))
    (character (string designator))))


(defun make-tracking-key (designator &optional package-alias type qualifiers specializers)
  "Create a lookup key for a designator with optional package name, type & method info.
   For methods and generic functions, always includes qualifiers and specializers parts, even if nil.
   DESIGNATOR can be symbol, package designator, or (setf symbol) form.
   PACKAGE-ALIAS is a package designator--eg, \"FOO\", :foo, #:foo, or package object.
   TYPE is one of +valid-definition-types+ of DESIGNATOR.
   QUALIFIERS is a list of method qualifiers in their significant order.
   SPECIALIZERS is a list of specializer types."
  (let ((name (format nil "~A" designator))
        (pkg-name (typecase package-alias
                    (package (package-name package-alias))
                    (null nil)
                    (t (or (package-name package-alias) (string package-alias))))))
    
    (when (and type (not (member type +valid-definition-types+)))
      (error "Invalid definition type in make-tracking-key~%  designator: ~S~%  package-alias: ~S~%  type: ~S"
             designator package-alias type))
    
    (let ((key (format nil "~A|~A|~A" name (or pkg-name "") (or type ""))))
      (when (or (member type '(:METHOD :GENERIC-FUNCTION)) qualifiers specializers)
        ;; Preserve qualifier order as it is significant
        (setf key (format nil "~A|(~{~A~^ ~})" key (or qualifiers nil)))
        ;; Sort specializers using printed representation for comparison
        (let ((sorted-specs (sort (copy-list (or specializers nil)) 
                                  #'string<
                                  :key (lambda (spec)
                                         (format nil "~S" spec)))))
          (setf key (format nil "~A|(~{~A~^ ~})" key sorted-specs))))
      key)))


(defun ensure-tracker (&optional tracker)
  "Return TRACKER if provided, otherwise return *CURRENT-TRACKER*.
   Signals an error if no tracker is available."
  (or tracker *current-tracker*
      (error "No tracker is currently bound. Please use 'with-dependency-tracker' to bind one.")))


(defun analyze-in-package (parser form)
  "Handle in-package forms by updating the current package context.
   For uninterned symbols, updates parser state but skips package validation."
  (let* ((pkg-designator (second form))
         (pkg (and (or (symbolp pkg-designator) (stringp pkg-designator))
                  (find-package (string pkg-designator)))))
    (cond 
      ;; Found existing package
      (pkg
       (setf (current-package parser) pkg
             (current-package-name parser) (package-name pkg)))
      ;; Uninterned symbol - just update parser state
      ((and (symbolp pkg-designator) 
            (not (symbol-package pkg-designator)))
       (setf (current-package parser) (find-package :common-lisp-user)
             (current-package-name parser) (symbol-name pkg-designator)))
      ;; Invalid package reference  
      (t
       (format t "~2%Cannot accurately analyze dependencies:~%~
                 File: ~A~%~
                 Form: ~S~%~
                 References undefined package: ~A~2%~
                 Please ensure all package definitions compile and load successfully before analysis.~2%"
                (project-pathname (file parser)) form pkg-designator)))))


(defun walk-form (form handler)
  "Walk a form calling HANDLER on each subform with context and parent context info.
   FORM - The form to analyze
   HANDLER - Function taking (form context parent-context)"
  (labels ((walk (x context parent-context)
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
      (walk form form form))))


(defun symbol-status (sym pkg)
  "Determines the status of a symbol in a package, :internal, :external, :inherited, :nil.
   For setf functions, checks the status of the base name."
  (let ((base-sym (if (and (listp sym) (eq (car sym) 'setf))
                      (cadr sym)  ; Extract base symbol from (setf sym)
                      sym)))
    (if-let (status (nth-value 1 (find-symbol (symbol-name base-sym) pkg)))
       status
       :nil)))


(defun cl-symbol-p (item)
 "Return true if item is from the common-lisp package."
 (and (symbolp item)
      (eq (symbol-package item) 
          (find-package :common-lisp))))


(defun skip-item-p (form)
  "Return T if form should be skipped during reference analysis."
  (or (null form)
      (numberp form) 
      (stringp form)
      (keywordp form)
      (cl-symbol-p form)
      (and (symbolp form)  ;skip uninterned symbols
           (not (symbol-package form)))))


(defun quoted-symbol-p (item)
 "Return true if item is quoted with quote (but not function quote);
  eg, 'var, (quote var). Function quotes (#'foo, (function foo)) return nil."
 (and (consp item)
      (eq (car item) 'quote)))


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


(defun get-file-definitions (&optional (tracker nil tracker-provided-p) file)
  "Get all definitions in a file."
  (let ((actual-tracker (if tracker-provided-p tracker (ensure-tracker))))
    (gethash file (slot-value actual-tracker 'file-map))))


(defun gensym-form-p (form)
  "Detects if a form starts with gensym."
  (and (consp form) (eq (car form) 'gensym)))


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
    (pushnew anomaly (gethash type (slot-value tracker 'anomalies))
                     :test #'equal :key #'anomaly.file)
    anomaly))