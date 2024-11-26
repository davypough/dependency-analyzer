;;;; Filename: tracker.lisp
;;;
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


(defun make-tracking-key (designator &optional package)
  "Create a lookup key for a symbol or package name, optionally in a specific package context.
   DESIGNATOR can be either a symbol or a string.
   For symbols: Creates a key based on the symbol name and optional package
   For strings: Uses the string directly as a name
   Returns a string key that uniquely identifies the entity."
  (let ((name (etypecase designator
                (string designator)
                (symbol (symbol-name designator)))))
    (if package
        (format nil "~A::~A" package name)
        name)))


(defmethod get-definitions (&optional tracker symbol)
  "Get all recorded definitions of a symbol or string.
   If only one arg provided, treat it as the symbol/string and use the current tracker."
  (when (and tracker (null symbol))
    ;; If only one arg provided, it's the symbol/string
    (setf symbol tracker
          tracker nil))
  (let* ((actual-tracker (or tracker (ensure-tracker)))
         (pkg-name (etypecase symbol
                    (string "COMMON-LISP-USER")  
                    (symbol (if (symbol-package symbol)
                              (package-name (symbol-package symbol))
                              "COMMON-LISP-USER"))))
         (key (make-tracking-key symbol pkg-name)))
    (or (gethash key (slot-value actual-tracker 'definitions))
        ;; Try without package context as fallback
        (gethash (make-tracking-key symbol) (slot-value actual-tracker 'definitions)))))


(defmethod get-references (&optional (tracker nil tracker-provided-p) symbol)
  "Get all recorded references to a symbol."
  (let* ((actual-tracker (if tracker-provided-p tracker (ensure-tracker)))
         (key (if (symbol-package symbol)
                 (make-tracking-key symbol (package-name (symbol-package symbol)))
                 (make-tracking-key symbol))))
    (gethash key (slot-value actual-tracker 'references))))


(defmethod get-file-definitions (&optional (tracker nil tracker-provided-p) file)
  "Get all definitions in a file."
  (let ((actual-tracker (if tracker-provided-p tracker (ensure-tracker))))
    (gethash file (slot-value actual-tracker 'file-map))))


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


(defmethod record-system-dependencies ((tracker dependency-tracker) system-name depends-on)
  "Record ASDF system dependencies in the tracker's subsystems table.
   SYSTEM-NAME - Name of the system (string or symbol)
   DEPENDS-ON - List of dependencies from ASDF system definition
                Can include direct names or feature expressions:
                - Direct: \"system-name\" or :system-name
                - Feature: (:feature :os-type :system-name)
   Dependencies are normalized to strings for consistent tracking."
  (flet ((normalize-dep (dep)
           "Convert dependency spec to normalized string form, handling:
            - String dependencies: \"system-name\" -> \"SYSTEM-NAME\"
            - Symbol dependencies: :system-name -> \"SYSTEM-NAME\"
            - Feature conditionals: (:feature :os :name) -> \"NAME\""
           (typecase dep
             (string (string-upcase dep))  
             (symbol (string-upcase dep))
             (cons 
              (if (eq (first dep) :feature)
                  ;; Feature conditional - extract system name
                  (normalize-dep (third dep))
                  ;; Other list form - take first element
                  (normalize-dep (first dep))))
             (t nil))))  ; Skip invalid specs
    
    ;; Convert system name to normalized string
    (let ((sys-name (normalize-dep system-name)))
      ;; Process and record each valid dependency
      (setf (gethash sys-name (slot-value tracker 'subsystems))
            (loop for dep in depends-on
                  for normalized = (normalize-dep dep)
                  when normalized 
                  collect normalized)))))