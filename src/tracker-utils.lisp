;;;; Filename: tracker-utils.lisp
;;;
;;; Utility functions for dependency tracking functionality.
;;; Contains helper functions for working with dependency tracking,
;;; including key generation, tracker management, and common operations.


(in-package #:dep)


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
