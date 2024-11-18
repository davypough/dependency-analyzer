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
   (anomalies
    :initform (make-hash-table :test 'equal)
    :accessor anomalies
    :documentation "Maps anomaly types to lists of detected anomalies")
   (project-name
    :initarg :project-name
    :reader project.name
    :documentation "Name of the ASDF project being analyzed")
   (project-root
    :initarg :project-root
    :reader project-root
    :documentation "Root directory of the project being analyzed"))
  (:documentation 
   "Main data structure for tracking dependencies between files and symbols."))


(defstruct (anomaly (:conc-name anomaly.))
  "Data structure for recording dependency analysis anomalies"
  (type nil :type keyword :read-only t)
  (severity nil :type keyword :read-only t)
  (location nil :type (or string pathname) :read-only t)
  (description nil :type string :read-only t)
  (context nil :type t :read-only t))


(defclass project-parser ()
  ((project 
    :initarg :project 
    :reader project
    :documentation "The ASDF project being analyzed")
   (parsing-projects
    :initform nil
    :accessor parsing-projects
    :documentation "Stack of projects being parsed for cycle reporting"))
  (:documentation
   "Parser for analyzing an ASDF project and its components."))


(defclass file-parser ()
  ((file 
    :initarg :file 
    :reader file
    :documentation "The source file being parsed")
   (package 
    :initarg :package
    :initform (find-package :common-lisp-user)
    :accessor current-package
    :documentation "The current package context while parsing")
   (current-package-name
    :initarg :package-name
    :initform "COMMON-LISP-USER"
    :accessor current-package-name
    :documentation "String name of the current package")
   (parsing-files
    :initform nil
    :accessor parsing-files
    :documentation "Stack of files being parsed for cycle reporting")
   (parsing-packages
    :initform nil
    :accessor parsing-packages
    :documentation "Stack of packages being parsed for cycle detection")
   (bound-symbols
    :initform nil
    :accessor bound-symbols
    :documentation "Stack of symbols bound by enclosing lambda forms")
  (block-names
   :initform nil
   :accessor block-names
   :documentation "Stack of currently bound block names")
  (go-tags
   :initform nil
   :accessor go-tags
   :documentation "Stack of currently bound go tags")
  (symbol-macro-bindings
   :initform nil
   :accessor symbol-macro-bindings
   :documentation "Association list of symbol-macro expansions"))
 (:documentation
  "Parser for analyzing a single Lisp source file."))


(defstruct (definition (:conc-name definition.))
  "Data structure holding info about a lisp definition--eg, defun, defvar"
  (symbol nil :type symbol :read-only t)
  (type nil :type keyword :read-only t)
  (file nil :type (or string pathname) :read-only t)
  (package nil :type (or string symbol) :read-only t)
  (position nil :type (or null integer))
  (exported-p nil :type boolean)
  (context nil :type t))


(defstruct (reference (:conc-name reference.))
  "Data structure holding info about a lisp reference to a definition"
  (symbol nil :type symbol :read-only t)
  (type nil :type keyword :read-only t)
  (file nil :type (or string pathname) :read-only t)
  (position nil :type (or null integer))
  (context nil :type t)
  (package nil :type (or string symbol null)))