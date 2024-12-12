;;;; Filename: base-definitions.lisp
;;;
;;; Base class and structure definitions for the dependency analyzer.
;;; Contains fundamental type definitions needed by other modules.


(in-package #:dep)


(alexandria:define-constant +valid-definition-types+ 
  '(:STRUCTURE/CLASS/CONDITION :VARIABLE :FUNCTION :MACRO
    :GENERIC-FUNCTION :METHOD :SYMBOL-MACRO :TYPE :PACKAGE)
  :test #'equal
  :documentation "Valid types for dependency definitions. Used for type checking and reference matching.")


(defun format-if (stream control indent value)
  "Print an indented slot NAME and VALUE, only if VALUE is non-nil."
  (when value
    (format stream control indent value)))


(defclass definition ()
  ((name :initarg :name :reader definition.name :type (or symbol string))
   (context :initarg :context :reader definition.context :type cons)
   (type :initarg :type :reader definition.type :type (member . #.+valid-definition-types+))
   (file :initarg :file :reader definition.file :type (or string pathname))
   (package :initarg :package :reader definition.package :type (or string symbol))
   (exported-p :initarg :exported-p :reader definition.exported-p :type boolean)
   (qualifiers :initarg :qualifiers :reader definition.qualifiers :type list)
   (specializers :initarg :specializers :reader definition.specializers :type list))
  (:default-initargs :name nil :context nil :type nil :file nil :package nil
                     :exported-p nil :qualifiers nil :specializers nil)
  (:documentation "Data structure holding info about a lisp definition; eg, defun, defvar, or package"))


(defmethod print-object ((object definition) stream)
  (print-definition object stream 0))


(defun print-definition (def &optional (stream *standard-output*) (indent 0))
  (let ((indent-str (make-string indent :initial-element #\Space)))
    (format stream "~&~ADEFINITION> Name: ~A~%" indent-str (definition.name def))
    (format stream    "~A           Context: ~A~%" indent-str (definition.context def))
    (format stream    "~A           Type: ~S~%" indent-str (definition.type def))
    (format stream    "~A           File: ~A~%" indent-str (project-pathname (definition.file def)))
    (format stream    "~A           Package: ~S~%" indent-str (definition.package def))
    (format stream    "~A           Exported-p: ~S~%" indent-str (definition.exported-p def))
    (format-if stream "~A           Qualifiers: ~S~%" indent-str (definition.qualifiers def))
    (format-if stream "~A           Specializers: ~S~%" indent-str (definition.specializers def))))


(defclass reference ()
  ((name :initarg :name :reader reference.name :type (or string symbol))
   (context :initarg :context :reader reference.context :type (or symbol list))
   (file :initarg :file :reader reference.file :type (or string pathname))
   (package :initarg :package :reader reference.package :type (or string symbol))
   (visibility :initarg :visibility :reader reference.visibility :type (member :LOCAL :INHERITED :IMPORTED)) 
   (definition :initarg :definition :reader reference.definition :type (or null (and standard-object definition))))
  (:default-initargs :name nil :context nil :file nil :package nil :visibility nil :definition nil)
  (:documentation "Data structure holding info about a lisp reference to a definition"))


(defmethod print-object ((object reference) stream)
  (print-reference object stream 0))


(defun print-reference (ref &optional (stream *standard-output*) (indent 0))
  (let ((indent-str (make-string indent :initial-element #\Space)))
    (format stream "~&~AREFERENCE> Name: ~A~%" indent-str (reference.name ref))
    (format stream   "~A           Context: ~A~%" indent-str (reference.context ref))
    (format stream   "~A           File: ~A~%" indent-str (project-pathname (reference.file ref)))
    (format stream   "~A           Package: ~S~%" indent-str (reference.package ref))
    (format stream   "~A           Visibility: ~A~%" indent-str (reference.visibility ref))
    (format stream   "~A           Definition: " indent-str)
    (print-definition (reference.definition ref) stream (+ indent 28))))


(defclass anomaly ()
  ((type :initarg :type :reader anomaly.type :type keyword)
   (severity :initarg :severity :reader anomaly.severity :type (member :ERROR :WARNING :INFO))
   (file :initarg :file :reader anomaly.location :type (or string pathname))
   (description :initarg :description :reader anomaly.description :type string)
   (context :initarg :context :reader anomaly.context :type (or symbol list)))
  (:default-initargs :type nil :severity nil :file nil :description nil
                     :context nil)
  (:documentation "Data structure for recording dependency analysis anomalies"))


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
    (format stream "~A: ~D definitions, ~D references, ~D anomalies, ~D files"
            (project.name tracker)
            (hash-table-count (slot-value tracker 'definitions))  ;need to collapse to actual definitions
            (hash-table-count (slot-value tracker 'references))
            (hash-table-count (slot-value tracker 'anomalies))
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

