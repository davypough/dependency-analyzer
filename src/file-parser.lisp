;;;; Filename: file-parser.lisp
;;;
;;; Core file parsing functionality for dependency analysis.
;;; Analyzes source files to track symbol definitions, references,
;;; and package relationships without expanding macros.


(in-package #:dep)


(defmethod parse-file ((parser file-parser))
  "Parse a single Lisp source file for definitions and references."
  (with-slots (file package parsing-files) parser
    (handler-case
        (progn
          ;; Record any cycles but continue parsing
          (record-file-dependency-cycle parser file)
          (push file parsing-files)
          
          (with-open-file (stream file :direction :input)
            (let ((*package* package))
              (loop for form = (read stream nil nil)
                    while form
                    do (analyze-form parser form))))
          
          ;; Remove file from parsing stack
          (pop parsing-files))
      (error (e)
        (pop parsing-files)
        (error 'file-parse-error
               :file file
               :reason e)))))


(defmethod analyze-form ((parser file-parser) form)
  "Analyze a form for definitions and references. Handles both lists and bare symbols."
  (etypecase form
    (list 
     (when (symbolp (car form))
       (let ((operator (car form)))
         (case operator
           (in-package (analyze-in-package parser form))
           ((defun defvar defparameter defconstant) (analyze-basic-definition parser form))
           (defmacro (analyze-macro-definition parser form))
           (define-condition (analyze-condition-definition parser form)) 
           ((defpackage) (analyze-package-definition parser form))
           (otherwise (analyze-function-call parser form))))))
    (symbol
     (analyze-subform parser form))))


(defmethod analyze-in-package ((parser file-parser) form)
  "Handle in-package forms by updating the current package context."
  (let* ((name (normalize-package-name (second form)))
         (package (or (find-package name)
                     (make-package name))))
    (setf (current-package parser) package
          (current-package-name parser) (package-name package)
          *package* package)))


(defmethod analyze-basic-definition ((parser file-parser) form)
  "Handle basic definition forms like defun and defvar."
  (let* ((operator (car form))
         (name (second form))
         (type (case operator
                (defun :function)
                ((defvar defparameter defconstant) :variable)))
         (package (current-package parser)))
    (record-definition *current-tracker* name
                      type
                      (file parser)
                      :package (current-package-name parser)
                      :exported-p (eq (nth-value 1 (find-symbol (symbol-name name) package))
                                    :external))
    (analyze-rest parser (cddr form))))


(defmethod analyze-macro-definition ((parser file-parser) form)
  "Handle macro definition forms."
  (let* ((name (second form))
         (body (cdddr form))
         (package (current-package parser)))
    (record-definition *current-tracker* name
                      :macro
                      (file parser)
                      :package (current-package-name parser)
                      :exported-p (eq (nth-value 1 (find-symbol (symbol-name name) package))
                                    :external))
    (record-macro-body-symbols *current-tracker* name body)))


(defmethod analyze-condition-definition ((parser file-parser) form)
  "Handle condition definition forms."
  (let* ((name (second form))
         (package (current-package parser)))
    (record-definition *current-tracker* name
                      :condition
                      (file parser)
                      :package (current-package-name parser)
                      :exported-p (eq (nth-value 1 (find-symbol (symbol-name name) package))
                                    :external))
    (analyze-rest parser (cddr form))))


(defmethod analyze-function-call ((parser file-parser) form)
  "Handle general function call forms."
  (let ((operator (car form)))
    (when (symbolp operator)
      (let* ((pkg (or (symbol-package operator)
                     (current-package parser)))
             (bare-name (symbol-name operator))
             (sym (if (symbol-package operator)
                     operator
                     (or (find-symbol bare-name pkg)
                         (intern bare-name pkg)))))
        (record-reference *current-tracker* sym
                         :call
                         (file parser)
                         :package (package-name pkg))))
    (analyze-rest parser (cdr form))))


(defmethod analyze-package-definition ((parser file-parser) form)
  "Handle package definition forms."
  (let ((name (normalize-package-name (second form))))
    (record-package-definition parser name (cddr form))))


(defmethod analyze-subform ((parser file-parser) form)
  "Analyze a single form for symbol references."
  (typecase form
    (symbol 
     (unless (or (member form '(nil t))
                 (eq (symbol-package form) 
                     (find-package :common-lisp))
                 (eq (symbol-package form)
                     (find-package :keyword)))
       (let ((pkg (symbol-package form)))
         (record-reference *current-tracker* form
                          :reference
                          (file parser)
                          :package (package-name pkg)))))
    (cons
     (analyze-form parser form))))


(defmethod analyze-rest ((parser file-parser) rest)
  "Analyze a body of code for symbol references."
  (mapc (lambda (form)
          (analyze-subform parser form))
        rest))


(defmethod record-macro-body-symbols ((parser file-parser) macro-name body)
  "Record all non-CL symbols in a macro body as potential dependencies."
  (labels ((collect-symbols (form)
             (typecase form
               (symbol 
                (unless (or (member form '(nil t))
                           (eq (symbol-package form) 
                               (find-package :common-lisp)))
                  (record-reference form
                                  :macro-body
                                  (file parser)
                                  :package (current-package-name parser))))
               (cons
                (collect-symbols (car form))
                (collect-symbols (cdr form))))))
    (collect-symbols body)))


(defmethod record-macro-dependencies ((parser file-parser) macro-def form)
  "Record that this code depends on the macro definition."
  (record-reference (definition.symbol macro-def)
                   :macro-expansion
                   (file parser)
                   :package (definition.package macro-def)))


(defmethod record-function-definition ((parser file-parser) name args body)
  "Record a function definition in the tracker."
  (let* ((package (current-package parser))
         (sym-name (normalize-symbol-name name))
         (sym (intern sym-name package)))
    (record-definition sym
                      :function 
                      (file parser)
                      :package (current-package-name parser)
                      :exported-p (eq (nth-value 1 (find-symbol sym-name package)) :external))))


(defmethod record-package-definition ((parser file-parser) name options)
  "Record a package definition and handle its options."
  (let* ((pkg-name name)
         (package (or (find-package name)
                     (make-package name :use nil)))
         (current-packages (or (slot-value parser 'parsing-packages) nil)))
    ;; Check for package cycles
    (let ((position (member pkg-name current-packages :test #'string=)))
      (when position
        (let* ((cycle (cons pkg-name (ldiff current-packages position)))
               (chain (format nil "~{~A~^ -> ~}" (reverse cycle))))
          (record-package-cycle chain))))
    (push pkg-name (slot-value parser 'parsing-packages))
    (setf (current-package parser) package
          (current-package-name parser) pkg-name)
    (unwind-protect
        (progn
          ;; Process package options
          (dolist (opt options)
            (when (and (consp opt) (eq (car opt) :use))
              (dolist (used (cdr opt))
                (let ((used-pkg (find-package used)))
                  (when used-pkg
                    (use-package used-pkg package)
                    (record-package-use *current-tracker* pkg-name (package-name used-pkg))
                    ;; Record both inherited and call references for exported symbols
                    (do-external-symbols (sym used-pkg)
                      (multiple-value-bind (s status)
                          (find-symbol (symbol-name sym) package)
                        (when (and s (eq status :inherited))
                          ;; Record the inherited relationship
                          (record-reference *current-tracker* sym
                                          :inherited
                                          (file parser)
                                          :package (package-name used-pkg))
                          ;; Also record potential call reference if it's a function
                          (when (and (fboundp sym) 
                                   (not (macro-function sym))
                                   (not (special-operator-p sym)))
                            (record-reference *current-tracker* sym
                                            :call
                                            (file parser)
                                            :package (package-name used-pkg))))))))))
          (dolist (opt options)
            (when (and (consp opt) (eq (car opt) :import-from))
              (let ((from-pkg-name (normalize-package-name (second opt)))
                    (from-pkg (find-package (second opt))))
                (when from-pkg
                  (record-package-use *current-tracker* pkg-name from-pkg-name)
                  (dolist (sym (cddr opt))
                    (let* ((name (normalize-symbol-name sym))
                           (from-sym (find-symbol name from-pkg)))
                      (when from-sym
                        (import from-sym package)
                        (record-reference *current-tracker* from-sym
                                        :import
                                        (file parser)
                                        :package from-pkg-name))))))))
          (dolist (opt options)
            (when (and (consp opt) (eq (car opt) :export))
              (dolist (sym (cdr opt))
                (let* ((name (normalize-symbol-name sym))
                       (exported-sym (intern name package)))
                  (export exported-sym package)
                  (record-export *current-tracker* pkg-name exported-sym))))))
      ;; Clean up package parsing state
      (pop (slot-value parser 'parsing-packages))))))


(defun record-file-dependency-cycle (parser file-name)
  "Record a file dependency cycle in the tracker."
  (let ((position (member file-name (parsing-files parser) :test #'equal)))
    (when position
      (let* ((cycle (cons file-name (ldiff (parsing-files parser) position)))
             (chain (format nil "~{~A~^ -> ~}" (reverse cycle))))
        ;; Record the cycle in the dependency tracker
        (record-file-cycle chain)))))