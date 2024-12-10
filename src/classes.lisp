;;;; Filename: base-definitions.lisp
;;;
;;; Base class and structure definitions for the dependency analyzer.
;;; Contains fundamental type definitions needed by other modules.


(in-package #:dep)


;(defconstant +valid-definition-types+ 
;  '(:STRUCTURE/CLASS/CONDITION :VARIABLE :FUNCTION :MACRO
;    :GENERIC-FUNCTION :METHOD :SYMBOL-MACRO :TYPE :PACKAGE)
;  "Valid types for dependency definitions. Used for type checking and reference matching.")


(alexandria:define-constant +valid-definition-types+ 
  '(:STRUCTURE/CLASS/CONDITION :VARIABLE :FUNCTION :MACRO
    :GENERIC-FUNCTION :METHOD :SYMBOL-MACRO :TYPE :PACKAGE)
  :test #'equal
  :documentation "Valid types for dependency definitions. Used for type checking and reference matching.")


(defclass definition ()
  ((name :initarg :name :reader definition.name :type (or symbol string))
   (context :initarg :context :reader definition.context)
   (type :initarg :type :reader definition.type
         :type (member . #.+valid-definition-types+))
   (file :initarg :file :reader definition.file :type (or string pathname))
   (package :initarg :package :reader definition.package
            :type (or string symbol))
   (exported-p :initarg :exported-p :reader definition.exported-p
               :type boolean)
   (qualifiers :initarg :qualifiers :reader definition.qualifiers 
               :type list)
   (specializers :initarg :specializers :reader definition.specializers
                :type list))
  (:default-initargs :name nil :type nil :file nil :package nil
                     :exported-p nil :context nil :qualifiers nil :specializers nil)
  (:documentation "Data structure holding info about a lisp definition;
                   eg, defun, defvar, or package"))


(defmethod print-object ((def definition) stream)  
  (print-unreadable-object (def stream :type t)
    (format stream "Name: ~S~%  Context: ~S~%  Type: ~S~%  File: ~A~%  Package: ~S~%  Exported: ~A~A~A"
            (definition.name def) 
            (definition.context def) 
            (definition.type def)
            (project-pathname (definition.file def)) 
            (definition.package def) 
            (definition.exported-p def)
            (if (definition.qualifiers def)
                (format nil "~%  Qualifiers: ~S" (definition.qualifiers def))
                "")
            (if (definition.specializers def)
                (format nil "~%  Specializers: ~S" (definition.specializers def)) 
                ""))))


(defclass reference ()
  ((symbol :initarg :symbol :reader reference.symbol :type symbol)
   (context :initarg :context :reader reference.context)
   (file :initarg :file :reader reference.file :type (or string pathname))
   (package :initarg :package :reader reference.package
            :type (or string symbol null))
   (visibility :initarg :visibility :reader reference.visibility
               :type (member :LOCAL :INHERITED :IMPORTED)) 
   (definition :initarg :definition :reader reference.definition
               :type (or null (and standard-object definition))))
  (:default-initargs :symbol nil :context nil :file nil :package nil
                     :visibility nil :definition nil)
  (:documentation "Data structure holding info about a lisp reference
                   to a definition"))


(defmethod print-object ((ref reference) stream)
  (print-unreadable-object (ref stream :type t)
    (format stream "Symbol: ~A~%  Context: ~S~%  File: ~A~%  Package: ~S~%  Visibility: ~A~%  Definition: ~A"
            (reference.symbol ref) (reference.context ref) (project-pathname (reference.file ref))
            (reference.package ref) (reference.visibility ref)
            (project-pathname (definition.file (reference.definition ref))))))


(defclass anomaly ()
  ((type :initarg :type :reader anomaly.type :type keyword)
   (severity :initarg :severity :reader anomaly.severity
             :type (member :ERROR :WARNING :INFO))
   (location :initarg :location :reader anomaly.location
             :type (or string pathname))
   (description :initarg :description :reader anomaly.description
                :type string)
   (context :initarg :context :reader anomaly.context))
  (:default-initargs :type nil :severity nil :location nil :description nil
                     :context nil)
  (:documentation "Data structure for recording dependency analysis
                   anomalies"))


(defmethod print-object ((anom anomaly) stream)
  (print-unreadable-object (anom stream :type t)
    (format stream "Type: ~S~%  Severity: ~A~%  Location: ~A~%  Description: ~A~%  Context: ~S"
            (anomaly.type anom) (anomaly.severity anom) (project-pathname (anomaly.location anom))
            (anomaly.description anom) (anomaly.context anom))))


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
    :documentation "Name of the project being analyzed")
   (project-root
    :initarg :project-root
    :reader project-root
    :documentation "Root directory of the project being analyzed")))


(defmethod print-object ((tracker dependency-tracker) stream)
  "Print a human-readable representation of the tracker."
  (print-unreadable-object (tracker stream :type t)
    (format stream "~A: ~D definitions, ~D references, ~D files"
            (project.name tracker)
            (hash-table-count (slot-value tracker 'definitions))
            (hash-table-count (slot-value tracker 'references))
            (hash-table-count (slot-value tracker 'file-map)))))


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
    :documentation "Stack of packages being parsed for cycle detection"))
  (:documentation "Parser for analyzing a single Lisp source file."))


(defclass dots-object () ())  ;special object that will always print as ...


(defmethod print-object ((obj dots-object) stream)
  "Prints ... without quotes, used in limit-form-size"
  (write-string "..." stream))

