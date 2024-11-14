;;;; Filename: base-definitions.lisp
;;;
;;; Base class and structure definitions for the dependency analyzer.
;;; Contains fundamental type definitions needed by other modules.

(in-package #:dep)


(defparameter *current-tracker* nil
  "The currently active dependency tracker instance.")


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