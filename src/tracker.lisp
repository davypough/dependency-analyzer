;;;; Filename: tracker.lisp

;;; Core dependency tracking functionality.
;;; Provides the main data structures and operations for recording and querying
;;; dependencies between files, symbols, and packages.

(in-package #:dep)

(defparameter *current-tracker* nil
  "The currently active dependency tracker instance.")

(defmacro with-dependency-tracker ((&optional (tracker-form '(make-instance 'dependency-tracker :project-name "test-project"))) &body body)
  "Execute BODY with *CURRENT-TRACKER* bound to the result of TRACKER-FORM.
   If TRACKER-FORM is not provided, creates a new tracker instance."
  `(let ((tracker ,tracker-form))
     (setf *current-tracker* tracker)
     (let ((*current-tracker* tracker))
       ,@body)))

(defun ensure-tracker (&optional tracker)
  "Return TRACKER if provided, otherwise return *CURRENT-TRACKER*.
   Signals an error if no tracker is available."
  (or tracker *current-tracker*
      (error "No tracker is currently bound. Please use 'with-dependency-tracker' to bind one.")))

(defclass dependency-tracker ()
  ((definitions 
    :initform (make-hash-table :test 'equal)
    :documentation "Maps symbols to their definition locations")
   (references
    :initform (make-hash-table :test 'equal)
    :documentation "Maps symbols to where they are used")
   (file-map
    :initform (make-hash-table :test 'equal)
    :documentation "Maps files to their contained definitions")
   (package-uses
    :initform (make-hash-table :test 'equal)
    :documentation "Maps packages to the packages they use")
   (package-exports
    :initform (make-hash-table :test 'equal)
    :documentation "Maps packages to their exported symbols")
   (macro-bodies
    :initform (make-hash-table :test 'equal)
    :documentation "Maps macro names to symbols used in their bodies")
   (project-cycles
    :initform nil
    :accessor project-cycles
    :documentation "List of detected project dependency cycles")
   (file-cycles
    :initform nil
    :accessor file-cycles
    :documentation "List of detected file dependency cycles")
   (package-cycles
    :initform nil
    :accessor package-cycles
    :documentation "List of detected package dependency cycles")
   (project-name
    :initarg :project-name
    :reader project.name
    :documentation "Name of the ASDF project being analyzed"))
  (:documentation 
   "Main data structure for tracking dependencies between files and symbols."))


(defun make-tracking-key (symbol &optional package)
  "Create a lookup key for a symbol, optionally in a specific package context."
  (let ((result (if package
                    (format nil "~A::~A" package (symbol-name symbol))
                    (symbol-name symbol))))
    result))

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

(defmethod record-definition ((tracker dependency-tracker) symbol type file 
                           &key position package exported-p)
  "Record a symbol definition in the tracker."
  (let* ((key (make-tracking-key symbol package))
         (def (make-definition :symbol symbol
                             :type type
                             :file file
                             :package package
                             :position position
                             :exported-p exported-p)))
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

(defmethod get-definitions (&optional tracker symbol)
  "Get all recorded definitions of a symbol.
   If only one argument is provided, treat it as the symbol and use the current tracker."
  (when (and tracker (null symbol))
    ;; If only one arg provided, it's the symbol
    (setf symbol tracker
          tracker nil))
  (let* ((actual-tracker (or tracker (ensure-tracker)))
         (pkg-name (if (symbol-package symbol)
                      (package-name (symbol-package symbol))
                      "COMMON-LISP-USER"))
         (key (make-tracking-key symbol pkg-name)))
    (or (gethash key (slot-value actual-tracker 'definitions))
        ;; Try without package context as fallback
        (gethash (symbol-name symbol) (slot-value actual-tracker 'definitions)))))

(defmethod get-references (&optional (tracker nil tracker-provided-p) symbol)
  "Get all recorded references to a symbol."
  (let* ((actual-tracker (if tracker-provided-p tracker (ensure-tracker)))
         (key (if (symbol-package symbol)
                 (make-tracking-key symbol (package-name (symbol-package symbol)))
                 (symbol-name symbol))))
    (gethash key (slot-value actual-tracker 'references))))

(defmethod get-file-definitions (&optional (tracker nil tracker-provided-p) file)
  "Get all definitions in a file."
  (let ((actual-tracker (if tracker-provided-p tracker (ensure-tracker))))
    (gethash file (slot-value actual-tracker 'file-map))))

(defmethod get-package-uses (&optional (tracker nil tracker-provided-p) package-name)
  "Get all packages that a given package uses."
  (let ((actual-tracker (if tracker-provided-p tracker (ensure-tracker))))
    (gethash package-name (slot-value actual-tracker 'package-uses))))

(defmethod get-package-exports (&optional (tracker nil tracker-provided-p) package-name)
  "Get all symbols exported by a package."
  (let ((actual-tracker (if tracker-provided-p tracker (ensure-tracker))))
    (mapcar #'(lambda (sym)
                (let ((pkg (find-package (string package-name))))
                  (if pkg
                      (intern (symbol-name sym) pkg)
                      sym)))
            (gethash (string package-name) (slot-value actual-tracker 'package-exports)))))

(defmethod file-dependencies (&optional (tracker nil tracker-provided-p) file)
  "Get all files that this file depends on."
  (let* ((actual-tracker (if tracker-provided-p tracker (ensure-tracker)))
         (deps ())
         (refs-seen (make-hash-table :test 'equal)))
    ;; First collect all references in this file
    (maphash (lambda (key refs)
               (declare (ignore key))
               (dolist (ref refs)
                 (when (equal (reference.file ref) file)
                   ;; Track each referenced symbol with its package
                   (let* ((sym (reference.symbol ref))
                          (pkg (reference.package ref))
                          (key (make-tracking-key sym pkg)))
                     (setf (gethash key refs-seen) t)))))
             (slot-value actual-tracker 'references))
    ;; Then look up the definitions for each referenced symbol
    (maphash (lambda (key _)
               (declare (ignore _))
               (let ((def (gethash key (slot-value actual-tracker 'definitions))))
                 (when def
                   (let ((def-file (definition.file def)))
                     (unless (equal def-file file)
                       (pushnew def-file deps :test #'equal))))))
             refs-seen)
    deps))

(defmethod file-dependents (&optional (tracker nil tracker-provided-p) file)
  "Get all files that depend on this file."
  (let* ((actual-tracker (if tracker-provided-p tracker (ensure-tracker)))
         (deps ()))
    (dolist (def (get-file-definitions actual-tracker file))
      (let ((sym (definition.symbol def)))
        (dolist (ref (get-references actual-tracker sym))
          (let ((ref-file (reference.file ref)))
            (unless (equal ref-file file)
              (pushnew ref-file deps :test #'equal))))))
    deps))

(defmethod package-depends-on-p (&optional (tracker nil tracker-provided-p) package1 package2)
  "Check if package1 depends on package2 (directly or indirectly)."
  (let ((actual-tracker (if tracker-provided-p tracker (ensure-tracker))))
    (labels ((check-deps (pkg visited)
               (when (member pkg visited :test #'string=)
                 (return-from check-deps nil))
               (let ((uses (get-package-uses actual-tracker pkg)))
                 (or (member package2 uses :test #'string=)
                     (some (lambda (p)
                            (check-deps p (cons pkg visited)))
                          uses)))))
      (check-deps package1 nil))))

(defmethod clear-tracker (&optional (tracker nil tracker-provided-p))
  "Clear all recorded information from the tracker."
  (let ((actual-tracker (if tracker-provided-p tracker (ensure-tracker))))
    (with-slots (definitions references file-map package-uses 
                 package-exports macro-bodies) actual-tracker
      (clrhash definitions)
      (clrhash references)
      (clrhash file-map)
      (clrhash package-uses)
      (clrhash package-exports)
      (clrhash macro-bodies))))

(defmethod clear-tracker :after (&optional (tracker nil tracker-provided-p))
  "Clear all recorded information including cycles from the tracker."
  (let ((actual-tracker (if tracker-provided-p tracker (ensure-tracker))))
    (setf (project-cycles actual-tracker) nil)
    (setf (file-cycles actual-tracker) nil)
    (setf (package-cycles actual-tracker) nil)))

(defmethod print-object ((tracker dependency-tracker) stream)
  "Print a human-readable representation of the tracker."
  (print-unreadable-object (tracker stream :type t)
    (format stream "~A: ~D definitions, ~D files"
            (project.name tracker)
            (hash-table-count (slot-value tracker 'definitions))
            (hash-table-count (slot-value tracker 'file-map)))))