;;;; Filename:  globals.lisp

;;;; Global variables for the dependency analyzer project.


(in-package :dep)


;; Constant used in classes.lisp & shared.lisp
(alexandria:define-constant +valid-definition-types+ 
  '(:STRUCTURE/CLASS/CONDITION :VARIABLE :FUNCTION :MACRO :GENERIC-FUNCTION 
    :METHOD :METHOD-COMBINATION :SYMBOL-MACRO :DEFTYPE :PACKAGE)  ;use :deftype instead of :type
  :test #'equal
  :documentation "Valid types for dependency definitions. Used for type checking and reference matching.")


;; Constant used in reference-analysis.lisp
(alexandria:define-constant +def*-forms+
  '(defun defvar defparameter defconstant defclass defstruct defmethod defgeneric 
    define-condition defmacro define-method-combination define-modify-macro
    define-compiler-macro define-symbol-macro defsetf define-setf-expander deftype)
  :test #'equal
  :documentation "All the def* forms allowed in common lisp")


;; Constants used in method-reference-analysis.lisp
(alexandria:define-constant +comparison-functions+ 
  '((sort . 1)           ; predicate in position 1 (0-based)
    (stable-sort . 1)    
    (merge . 3))         ; predicate in position 3 for merge
  :test #'equal
  :documentation "Maps comparison functions to the position of their predicate argument")


(alexandria:define-constant +mapping-functions+
  '((map . 1)            ; function in position 1 after result-type
    (mapcar . 0)         ; function in position 0
    (mapcan . 0)
    (mapc . 0)
    (maplist . 0)
    (mapcon . 0)
    (some . 0)
    (every . 0)
    (notany . 0)
    (notevery . 0))
  :test #'equal
  :documentation "Maps mapping functions to position of their function argument")


(alexandria:define-constant +sequence-functions+
  '(;; Basic sequence functions - sequence in position 1
    (remove . 1) (remove-if . 1) (remove-if-not . 1)
    (delete . 1) (delete-if . 1) (delete-if-not . 1)
    (substitute . 2) (substitute-if . 2) (substitute-if-not . 2)
    (find . 1) (find-if . 1) (find-if-not . 1)
    (position . 1) (position-if . 1) (position-if-not . 1) 
    (count . 1) (count-if . 1) (count-if-not . 1)
    (member . 1) (member-if . 1) (member-if-not . 1)
    ;; List functions
    (reduce . 1)
    (assoc . 1) (rassoc . 1)
    ;; Set functions - first list in position 0
    (adjoin . 0)
    (union . 0) (intersection . 0) (set-difference . 0))
  :test #'equal)