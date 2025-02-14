;;;; Filename: base-definitions.lisp
;;;
;;; Base class and structure definitions for the dependency analyzer.
;;; Contains fundamental type definitions needed by other modules.


(in-package #:dep)


(alexandria:define-constant +valid-definition-types+ 
  '(:STRUCTURE/CLASS/CONDITION :VARIABLE :FUNCTION :MACRO
    :GENERIC-FUNCTION :METHOD :SYMBOL-MACRO :DEFTYPE :PACKAGE)  ;use :deftype instead of :type
  :test #'equal
  :documentation "Valid types for dependency definitions. Used for type checking and reference matching.")


(defun format-if (stream control indent value)
  "Print an indented slot name and value only if VALUE is non-nil."
  (when value
    (format stream "~&~A" indent)
    (format stream control value)))


(defclass definition ()
  ((name :initarg :name :reader definition.name :type (or symbol string))
   (context :initarg :context :reader definition.context :type (or symbol list))
   (type :initarg :type :reader definition.type :type (member . #.+valid-definition-types+))
   (file :initarg :file :reader definition.file :type (or string pathname))
   (package :initarg :package :reader definition.package :type package)
   (status :initarg :status :reader definition.status :type (member :internal :external :inherited :nil))
   (qualifiers :initarg :qualifiers :reader definition.qualifiers :type list)
   (specializers :initarg :specializers :reader definition.specializers :type list))
  (:documentation "Data structure holding info about a lisp definition. For methods, specializers slot holds the method's parameter specializer types."))


(defmethod print-object ((def definition) stream)
  (print-definition def stream))


(defun print-definition (def &optional (stream *standard-output*) (indent 0))
  "Print a readable representation of a definition. Similar to print-object,
   but typically used for logging to a file. Omit null slots. For methods,
   prints qualifiers and specializers."
  (let ((indent-str (make-string indent :initial-element #\Space)))
    (with-slots (name context type file package status qualifiers specializers) def
      (format stream "~&~ADEFINITION>" indent-str)
      (format-if stream "    :Name ~S" indent-str name)
      (format-if stream "    :Context ~S" indent-str context)
      (format-if stream "    :Type ~S" indent-str type)
      (format-if stream "    :File ~A" indent-str (and file (project-pathname file)))
      (format-if stream "    :Package ~S" indent-str package)
      (format-if stream "    :Status ~S" indent-str status)
      (format-if stream "    :Qualifiers ~S" indent-str qualifiers)
      (format-if stream "    :Specializers ~S" indent-str specializers)
      (format stream "~%"))))


(defclass reference ()
  ((name :initarg :name :reader reference.name :type (or string symbol))
   (context :initarg :context :reader reference.context :type (or symbol list))
   (file :initarg :file :reader reference.file :type (or string pathname))
   (type :initarg :type :reader reference.type :type (member . #.+valid-definition-types+))
   (package :initarg :package :reader reference.package :type package)
   (visibility :initarg :visibility :reader reference.visibility :type cons)  ;eg, (:inherited #<PACKAGE "PKG1">)
   (definitions :initarg :definitions :reader reference.definitions :type cons)
   (qualifiers :initarg :qualifiers :initform nil :reader reference.qualifiers :type list)
   (arguments :initarg :arguments :initform nil :reader reference.arguments :type list))
  (:documentation "Data structure holding info about a lisp reference to a definition"))


(defmethod print-object ((ref reference) stream)
  (print-reference ref stream))


(defun print-reference (ref &optional (stream *standard-output*) (indent 0))
  "Print a readable representation of a reference, omitting null slots.
   Shows method qualifiers and arguments when present."
  (let ((indent-str (make-string indent :initial-element #\Space)))
    (with-slots (name context type file package visibility definitions qualifiers arguments) ref
      (format stream "~&~AREFERENCE>" indent-str)
      (format-if stream "    :Name ~S" indent-str name)
      (format-if stream "    :Context ~S" indent-str context)
      (format-if stream "    :Type ~S" indent-str type)
      (format-if stream "    :File ~A" indent-str (and file (project-pathname file)))
      (format-if stream "    :Package ~S" indent-str package)
      (format-if stream "    :Visibility ~S" indent-str visibility)
      (format-if stream "    :Qualifiers ~S" indent-str qualifiers)
      (format-if stream "    :Arguments ~S" indent-str arguments)
      (format-if stream "    :Definitions~A" indent-str "")
      (dolist (def definitions)
        (print-definition def stream (+ indent 20))))))


(defclass anomaly ()
  ((description :initarg :description :reader anomaly.description :type string)
   (type :initarg :type :reader anomaly.type :type keyword)
   (severity :initarg :severity :reader anomaly.severity :type (member :ERROR :WARNING :INFO))
   (context :initarg :context :reader anomaly.context :type (or symbol list))
   (file :initarg :file :reader anomaly.file :type list)
   (package :initarg :package :reader anomaly.package :type (or string symbol package null) :initform nil))
  (:documentation "Data structure for recording dependency analysis anomalies."))


(defmethod print-object ((anom anomaly) stream)
  (print-anomaly anom stream))


(defun print-anomaly (anom &optional (stream *standard-output*) (indent 0))
  "Print a readable representation of an anomaly record, skipping null slots."
  (let ((indent-str (make-string indent :initial-element #\Space)))
    (with-slots (description type severity context file package) anom
      (format stream "~&~AANOMALY>" indent-str)
      (format-if stream "    :Description ~S" indent-str (anomaly.description anom))
      (format-if stream "    :Type ~S" indent-str (anomaly.type anom))
      (format-if stream "    :Severity ~S" indent-str (anomaly.severity anom))
      (format-if stream "    :Context ~A" indent-str (anomaly.context anom))
      (format-if stream "    :File ~A" indent-str (and (anomaly.file anom)
                                                      (project-pathname (anomaly.file anom))))
      (format-if stream "    :Package ~S" indent-str (anomaly.package anom))
      (format stream "~2%"))))


(defclass dependency-tracker ()
  ((definitions 
    :initform (make-hash-table :test 'equal)
    :documentation "Maps symbols to their definition locations")
   (references
    :initform (make-hash-table :test 'equal)
    :documentation "Maps symbols to where they are used")
   (anomalies
    :initform (make-hash-table :test 'equal)
    :documentation "Maps anomaly types to lists of detected anomalies")
   (project-name
    :initarg :project-name
    :documentation "Name of the project being analyzed")
   (project-packages
    :initform nil
    :documentation "List of all packages defined, used, or imported from by the project")
   (project-owned-packages
    :initform nil
    :documentation "List of packages newly created by the project")
   (project-root
    :initarg :project-root
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
    :documentation "List of detected file dependency cycles")
   (package-cycles
    :initform nil
    :documentation "List of detected package dependency cycles")
   (subsystems
    :initform nil
    :documentation "ASDF :depends-on system dependencies determined from runtime analysis")
   (pushit-symbols
    :initform nil
    :documentation "List of detected referring symbols in a form analyzed by pushit-transform"))
  (:documentation "Tracker for analyzing a user's project files."))


(defmethod print-object ((tracker dependency-tracker) stream)
  "Print a human-readable representation of the tracker."
  (print-unreadable-object (tracker stream :type t)
    (format stream "~A: ~D definitions, ~D references, ~D anomalies, ~D files"
            (slot-value tracker 'project-name)
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
