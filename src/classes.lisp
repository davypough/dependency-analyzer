;;;; Filename: base-definitions.lisp
;;;
;;; Base class and structure definitions for the dependency analyzer.
;;; Contains fundamental type definitions needed by other modules.


(in-package #:dep)


(alexandria:define-constant +valid-definition-types+ 
  '(:STRUCTURE/CLASS/CONDITION :VARIABLE :FUNCTION :MACRO
    :GENERIC-FUNCTION :METHOD :SYMBOL-MACRO :TYPE :PACKAGE)  ;use :deftype instead of :type
  :test #'equal
  :documentation "Valid types for dependency definitions. Used for type checking and reference matching.")


(defun format-if (stream control indent value)
  "Print an indented slot name and value only if VALUE is non-nil."
  (when value
    (format stream "~&~A" indent)
    (format stream control value)))


(defclass definition ()
  ((name :initarg :name :reader definition.name :type (or symbol string))
   (context :initarg :context :reader definition.context :type cons)
   (type :initarg :type :reader definition.type :type (member . #.+valid-definition-types+))
   (file :initarg :file :reader definition.file :type (or string pathname))
   (package :initarg :package :reader definition.package :type package)
   (status :initarg :status :reader definition.status :type (member :internal :external :inherited :nil))
   (qualifiers :initarg :qualifiers :reader definition.qualifiers :type list)
   (specializers :initarg :specializers :reader definition.specializers :type list))
  (:default-initargs :name nil :context nil :type nil :file nil :package nil
                    :status nil :qualifiers nil :specializers nil)
  (:documentation "Data structure holding info about a lisp definition. For methods, specializers slot holds the method's parameter specializer types."))


(defmethod print-object ((object definition) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name context type file package status qualifiers specializers) object
      (format-if stream "    :Name ~A"       " " name)
      (format-if stream "    :Context ~A"    " " context)
      (format-if stream "    :Type ~S"       " " type)
      (format-if stream "    :File ~A"       " " (and file (project-pathname file)))
      (format-if stream "    :Package ~S"    " " package)
      (format-if stream "    :Status ~S"     " " status)
      (format-if stream "    :Qualifiers ~S" " " qualifiers)
      (format-if stream "    :Specializers ~S" " " specializers))))


(defun print-definition (def &optional (stream *standard-output*) (indent 0))
  "Print a readable representation of a definition. Similar to print-object,
   but typically used for logging to a file. Omit null slots. For methods,
   prints qualifiers and specializers."
  (let ((indent-str (make-string indent :initial-element #\Space)))
    (format stream "~&~ADEFINITION>" indent-str)
    (format-if stream "    :Name ~A" indent-str (definition.name def))
    (format-if stream "    :Context ~A" indent-str (definition.context def))
    (format-if stream "    :Type ~S" indent-str (definition.type def))
    (format-if stream "    :File ~A" indent-str (and (definition.file def)
                                                    (project-pathname (definition.file def))))
    (format-if stream "    :Package ~S" indent-str (definition.package def))
    (format-if stream "    :Status ~S" indent-str (definition.status def))
    (format-if stream "    :Qualifiers ~S" indent-str (definition.qualifiers def))
    (format-if stream "    :Specializers ~S" indent-str (definition.specializers def))
    (format stream "~%")))


(defclass reference ()
  ((name :initarg :name :reader reference.name :type (or string symbol))
   (context :initarg :context :reader reference.context :type (or symbol list))
   (file :initarg :file :reader reference.file :type (or string pathname))
   (type :initarg :type :reader reference.type :type (member . #.+valid-definition-types+))
   (package :initarg :package :reader reference.package :type package)
   (visibility :initarg :visibility :reader reference.visibility :type (member :LOCAL :INHERITED :IMPORTED))
   (definitions :initarg :definitions :reader reference.definitions :type cons)
   (qualifiers :initarg :qualifiers :initform nil :reader reference.qualifiers :type list)
   (arguments :initarg :arguments :initform nil :reader reference.arguments :type list))
  (:default-initargs :name nil :context nil :file nil :type nil :package nil :visibility nil
                     :definitions nil :qualifiers nil :arguments nil)
  (:documentation "Data structure holding info about a lisp reference to a definition"))


(defmethod print-object ((object reference) stream)
  "Print a reference object, omitting slots that are nil."
  (print-unreadable-object (object stream :type t)
    (with-slots (name context type file package visibility definitions qualifiers arguments) object
      (format-if stream "    :Name ~A" "" name)
      (format-if stream "    :Context ~A" "" context)
      (format-if stream "    :Type ~S" "" type)
      (format-if stream "    :File ~A" "" (and file (project-pathname file)))
      (format-if stream "    :Package ~S" "" package)
      (format-if stream "    :Visibility ~A" "" visibility)
      (format-if stream "    :Qualifiers ~S" "" qualifiers)
      (format-if stream "    :Arguments ~S" "" arguments)
      (format-if stream "    :Definitions~A" "" "")
      (dolist (def definitions)
        (print-definition def stream 20)))))


(defun print-reference (ref &optional (stream *standard-output*) (indent 0))
  "Print a readable representation of a reference, omitting null slots.
   Shows method qualifiers and arguments when present."
  (let ((indent-str (make-string indent :initial-element #\Space)))
    (format stream "~&~AREFERENCE>" indent-str)
    (format-if stream "    :Name ~A" indent-str (reference.name ref))
    (format-if stream "    :Context ~A" indent-str (reference.context ref))
    (format-if stream "    :Type ~S" indent-str (reference.type ref))
    (format-if stream "    :File ~A" indent-str (and (reference.file ref)
                                                  (project-pathname (reference.file ref))))
    (format-if stream "    :Package ~S" indent-str (reference.package ref))
    (format-if stream "    :Visibility ~A" indent-str (reference.visibility ref))
    (format-if stream "    :Qualifiers ~S" indent-str (reference.qualifiers ref))
    (format-if stream "    :Arguments ~S" indent-str (reference.arguments ref))
    (format-if stream "    :Definitions~A" indent-str "")
    (dolist (def (reference.definitions ref))
      (print-definition def stream (+ indent 20)))))


(defclass anomaly ()
  ((name :initarg :name :reader anomaly.name :type (or symbol string))
   (type :initarg :type :reader anomaly.type :type keyword)
   (severity :initarg :severity :reader anomaly.severity :type (member :ERROR :WARNING :INFO))
   (primary-location :initarg :file :reader anomaly.location :type (or string pathname))
   (description :initarg :description :reader anomaly.description :type string)
   (context :initarg :context :reader anomaly.context :type (or symbol list))
   (file :initarg :file :reader anomaly.file :type list)
   (package :initarg :package :reader anomaly.package :type (or string symbol package null) :initform nil))
  (:default-initargs :name nil :type nil :severity nil :file nil :description nil
                    :context nil :package nil)
  (:documentation "Data structure for recording dependency analysis anomalies."))


(defmethod print-object ((object anomaly) stream)
  "Print an anomaly object, omitting slots that are nil."
  (print-unreadable-object (object stream :type t)
    (with-slots (type name severity primary-location description context files package) object
      (format-if stream "    :Name ~A" "" name)
      (format-if stream "    :Type ~S" "" type)
      (format-if stream "    :Context ~A" "" context) 
      (format-if stream "    :Severity ~S" "" severity)
      (format-if stream "    :File ~A" "" (and primary-location (project-pathname primary-location)))
      ;(format-if stream "    :Files ~{~A~^, ~}" "" (let ((mapped (mapcar #'project-pathname files)))
      ;                                             (and mapped (not (null mapped)) mapped)))
      (format-if stream "    :Package ~S" "" package)
      (format-if stream "    :Description ~S" "" description))))


(defun print-anomaly (anom &optional (stream *standard-output*) (indent 0))
  "Print a readable representation of an anomaly record, skipping null slots."
  (let ((indent-str (make-string indent :initial-element #\Space)))
    (format stream "~&~AANOMALY>" indent-str)
    (format-if stream "    :Name ~A" indent-str (anomaly.name anom))
    (format-if stream "    :Type ~S" indent-str (anomaly.type anom))
    (format-if stream "    :Context ~A" indent-str (anomaly.context anom))
    (format-if stream "    :Severity ~S" indent-str (anomaly.severity anom))
    (format-if stream "    :File ~A" indent-str (and (anomaly.file anom)
                                                    (project-pathname (anomaly.file anom))))
    ;(format-if stream "    :Location ~A" indent-str (and (anomaly.location anom)
    ;                                                  (project-pathname (anomaly.location anom))))
    ;(format-if stream "    :Files ~{~A~^, ~}" indent-str (let ((mapped (mapcar #'project-pathname (anomaly.files anom))))
    ;                                                    (and mapped (not (null mapped)) mapped)))
    (format-if stream "    :Package ~S" indent-str (anomaly.package anom))
    (format-if stream "    :Description ~S" indent-str (anomaly.description anom))
    ;; Optional: a blank line after printing
    (format stream "~2%")))


(defclass dependency-tracker ()
  ((definitions 
    :initform (make-hash-table :test 'equal)
    :documentation "Maps symbols to their definition locations")
   (references
    :initform (make-hash-table :test 'equal)
    :documentation "Maps symbols to where they are used")
   (anomalies
    :initform (make-hash-table :test 'equal)
    :accessor anomalies
    :documentation "Maps anomaly types to lists of detected anomalies")
   (project-name
    :initarg :project-name
    :reader project.name
    :documentation "Name of the project being analyzed")
   (project-package
    :initarg :project-package 
    :reader project-package
    :documentation "Package being analyzed for dependencies")
   (project-root
    :initarg :project-root
    :reader project-root
    :documentation "Root directory of the project being analyzed")
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
    :documentation "List of detected package dependency cycles"))
  (:documentation "Tracker for analyzing a user's project files."))


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
