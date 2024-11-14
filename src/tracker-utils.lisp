;;;; Filename: tracker-utils.lisp
;;;
;;; Utility functions for dependency tracking functionality.
;;; Contains helper functions for working with dependency tracking,
;;; including key generation, tracker management, and common operations.

(in-package #:dep)


(defmacro with-dependency-tracker ((&optional (tracker-form '(make-instance 'dependency-tracker :system-name "test-system"))) &body body)
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


(defun make-tracking-key (symbol &optional package)
  "Create a lookup key for a symbol, optionally in a specific package context."
  (let ((result (if package
                    (format nil "~A::~A" package (symbol-name symbol))
                    (symbol-name symbol))))
    result))