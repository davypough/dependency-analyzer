;;;; Filename: tracker.lisp

;;; Core dependency tracking functionality.
;;; Provides the main data structures and operations for recording and querying
;;; dependencies between files, symbols, and packages.

(in-package #:dep)


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
   (system-cycles
    :initform nil
    :accessor system-cycles
    :documentation "List of detected system dependency cycles")
   (file-cycles
    :initform nil
    :accessor file-cycles
    :documentation "List of detected file dependency cycles")
   (package-cycles
    :initform nil
    :accessor package-cycles
    :documentation "List of detected package dependency cycles")
   (system-name
    :initarg :system-name
    :reader system.name
    :documentation "Name of the ASDF system being analyzed"))
  (:documentation 
   "Main data structure for tracking dependencies between files and symbols."))


(defstruct (definition (:conc-name definition.))
  (symbol nil :type symbol :read-only t)
  (type nil :type keyword :read-only t)
  (file nil :type (or string pathname) :read-only t)
  (package nil :type (or string symbol) :read-only t)
  (position nil :type (or null integer))
  (exported-p nil :type boolean))


(defstruct (reference (:conc-name reference.))
  (symbol nil :type symbol :read-only t)
  (type nil :type keyword :read-only t)
  (file nil :type (or string pathname) :read-only t)
  (position nil :type (or null integer))
  (context nil :type t)
  (package nil :type (or string symbol null)))


(defun make-tracking-key (symbol &optional package)
  "Create a lookup key for a symbol, optionally in a specific package context."
  (if package
      (format nil "~A::~A" package (symbol-name symbol))
      (symbol-name symbol)))


(defmethod record-system-cycle ((tracker dependency-tracker) cycle-chain)
  "Record a system dependency cycle."
  (pushnew cycle-chain (system-cycles tracker) :test #'string=))


(defmethod record-file-cycle ((tracker dependency-tracker) cycle-chain)
  "Record a file dependency cycle."
  (pushnew cycle-chain (file-cycles tracker) :test #'string=))


(defmethod record-package-cycle ((tracker dependency-tracker) cycle-chain)
  "Record a package dependency cycle."
  (pushnew cycle-chain (package-cycles tracker) :test #'string=))


(defmethod get-system-cycles ((tracker dependency-tracker))
  "Get all recorded system dependency cycles."
  (system-cycles tracker))


(defmethod get-file-cycles ((tracker dependency-tracker))
  "Get all recorded file dependency cycles."
  (file-cycles tracker))


(defmethod get-package-cycles ((tracker dependency-tracker))
  "Get all recorded package dependency cycles."
  (package-cycles tracker))


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


(defmethod get-macro-body-symbols ((tracker dependency-tracker) macro-name)
  "Get all recorded symbols used in a macro's body."
  (gethash (make-tracking-key macro-name) 
           (slot-value tracker 'macro-bodies)))


(defmethod lookup-macro-definition ((tracker dependency-tracker) symbol)
  "Find a macro's definition if it exists."
  (let* ((current-pkg (symbol-package symbol))
         (sym-name (symbol-name symbol)))
    (when current-pkg
      (let ((def (gethash (make-tracking-key symbol (package-name current-pkg))
                         (slot-value tracker 'definitions))))
        (when (and def (eq (definition.type def) :macro))
          (return-from lookup-macro-definition def)))
      (dolist (used-pkg (package-use-list current-pkg))
        (multiple-value-bind (used-sym status)
            (find-symbol sym-name used-pkg)
          (when (and used-sym (member status '(:external :inherited)))
            (let* ((used-key (make-tracking-key used-sym (package-name used-pkg))))
              (let ((def (gethash used-key (slot-value tracker 'definitions))))
                (when (and def (eq (definition.type def) :macro))
                  (return-from lookup-macro-definition def)))))))
      (let ((def (gethash sym-name (slot-value tracker 'definitions))))
        (when (and def (eq (definition.type def) :macro))
          def)))))


(defmethod get-definitions ((tracker dependency-tracker) symbol)
  "Get all recorded definitions of a symbol."
  (let* ((pkg-name (if (symbol-package symbol)
                       (package-name (symbol-package symbol))
                       "COMMON-LISP-USER"))
         (key (make-tracking-key symbol pkg-name)))
    (or (gethash key (slot-value tracker 'definitions))
        ;; Try without package context as fallback
        (gethash (symbol-name symbol) (slot-value tracker 'definitions)))))


(defmethod get-references ((tracker dependency-tracker) symbol)
  "Get all recorded references to a symbol."
  (let ((key (if (symbol-package symbol)
                 (make-tracking-key symbol (package-name (symbol-package symbol)))
                 (symbol-name symbol))))
    (gethash key (slot-value tracker 'references))))


(defmethod get-file-definitions ((tracker dependency-tracker) file)
  "Get all definitions in a file."
  (gethash file (slot-value tracker 'file-map)))


(defmethod get-package-uses ((tracker dependency-tracker) package-name)
  "Get all packages that a given package uses."
  (gethash package-name (slot-value tracker 'package-uses)))


(defmethod get-package-exports ((tracker dependency-tracker) package-name)
  "Get all symbols exported by a package."
  (mapcar #'(lambda (sym)
              (let ((pkg (find-package (string package-name))))
                (if pkg
                    (intern (symbol-name sym) pkg)
                    sym)))
          (gethash (string package-name) (slot-value tracker 'package-exports))))


(defmethod file-dependencies ((tracker dependency-tracker) file)
  "Get all files that this file depends on."
  (let ((deps ())
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
             (slot-value tracker 'references))
    ;; Then look up the definitions for each referenced symbol
    (maphash (lambda (key _)
               (declare (ignore _))
               (let ((def (gethash key (slot-value tracker 'definitions))))
                 (when def
                   (let ((def-file (definition.file def)))
                     (unless (equal def-file file)
                       (pushnew def-file deps :test #'equal))))))
             refs-seen)
    deps))


(defmethod file-dependents ((tracker dependency-tracker) file)
  "Get all files that depend on this file."
  (let ((deps ()))
    (dolist (def (get-file-definitions tracker file))
      (let ((sym (definition.symbol def)))
        (dolist (ref (get-references tracker sym))
          (let ((ref-file (reference.file ref)))
            (unless (equal ref-file file)
              (pushnew ref-file deps :test #'equal))))))
    deps))


(defmethod package-depends-on-p ((tracker dependency-tracker) package1 package2)
  "Check if package1 depends on package2 (directly or indirectly)."
  (labels ((check-deps (pkg visited)
             (when (member pkg visited :test #'string=)
               (return-from check-deps nil))
             (let ((uses (get-package-uses tracker pkg)))
               (or (member package2 uses :test #'string=)
                   (some (lambda (p)
                          (check-deps p (cons pkg visited)))
                        uses)))))
    (check-deps package1 nil)))


(defmethod clear-tracker ((tracker dependency-tracker))
  "Clear all recorded information from the tracker."
  (with-slots (definitions references file-map package-uses 
               package-exports macro-bodies) tracker
    (clrhash definitions)
    (clrhash references)
    (clrhash file-map)
    (clrhash package-uses)
    (clrhash package-exports)
    (clrhash macro-bodies)))


(defmethod clear-tracker :after ((tracker dependency-tracker))
  "Clear all recorded information including cycles from the tracker."
  (setf (system-cycles tracker) nil)
  (setf (file-cycles tracker) nil)
  (setf (package-cycles tracker) nil))


(defmethod print-object ((tracker dependency-tracker) stream)
  "Print a human-readable representation of the tracker."
  (print-unreadable-object (tracker stream :type t)
    (format stream "~A: ~D definitions, ~D files"
            (system.name tracker)
            (hash-table-count (slot-value tracker 'definitions))
            (hash-table-count (slot-value tracker 'file-map)))))