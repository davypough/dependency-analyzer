;;;; Filename: file-parser-utils.lisp
;;;
;;; Utility functions for parsing, package handling, and cycle detection.
;;; Contains pure functions for name normalization, symbol lookup,
;;; package option processing, and dependency cycle detection.

(in-package #:dep)


;;; Name and Symbol Utils

(defun normalize-package-name (designator)
  "Convert a package designator to a normalized string form."
  (typecase designator
    (string
      designator)
    (symbol
      (symbol-name designator))
    (t
      (princ-to-string designator))))


(defun normalize-symbol-name (designator)
  "Convert a symbol designator to a normalized string form."
  (typecase designator
    (string
      designator)
    (symbol
      (symbol-name designator))
    (t
      (princ-to-string designator))))


(defun find-exported-symbol (name package-name &optional current-package)
  "Find a symbol in a package, checking exports and inherited symbols."
  (let* ((target-pkg (find-package package-name))
         (current-pkg (or current-package *package*)))
    (declare (ignore current-pkg))
    (when target-pkg
      (multiple-value-bind (sym status)
          (find-symbol (normalize-symbol-name name) target-pkg)
        (when (and sym (member status '(:external :inherited)))
          (values sym (package-name (symbol-package sym))))))))


(defmethod record-definition ((tracker dependency-tracker) symbol type file 
                           &key position package exported-p)
  "Record a symbol definition in the tracker."
  (let* ((key (make-tracking-key symbol package))
         (def (make-definition :symbol symbol
                             :type type
                             :file file
                             :package package
                             :position position
                             :exported-p exported-p)))
    (setf (gethash key (slot-value tracker 'definitions)) def)
    (push def (gethash file (slot-value tracker 'file-map)))
    (when exported-p
      (record-export tracker package symbol))
    def))


(defmethod record-reference ((tracker dependency-tracker) symbol type file 
                          &key position context package)
  "Record a symbol reference in the tracker."
  (let* ((key (make-tracking-key symbol (when (symbol-package symbol)
                                        (package-name (symbol-package symbol)))))
         (ref (make-reference :symbol symbol
                            :type type
                            :file file
                            :position position
                            :context context
                            :package package)))
    (push ref (gethash key (slot-value tracker 'references)))
    ref))



(defmethod record-package-use ((tracker dependency-tracker) using-package used-package)
  "Record that one package uses another package."
  (pushnew used-package 
           (gethash using-package (slot-value tracker 'package-uses))
           :test #'string=))

(defmethod record-export ((tracker dependency-tracker) package-name symbol)
  "Record a symbol as being exported from a package."
  (let ((pkg (find-package (string package-name))))
    (when pkg
      (let ((exported-sym (intern (symbol-name symbol) pkg)))
        (pushnew exported-sym 
                 (gethash (string package-name) (slot-value tracker 'package-exports))
                 :test #'eq)))))

(defmethod record-macro-body-symbols ((tracker dependency-tracker) macro-name symbols)
  "Record the non-CL symbols used in a macro's body."
  (setf (gethash (make-tracking-key macro-name) 
                 (slot-value tracker 'macro-bodies))
        symbols))


(defmethod record-project-cycle ((tracker dependency-tracker) cycle-chain)
  "Record a project dependency cycle."
  (pushnew cycle-chain (project-cycles tracker) :test #'string=))

(defmethod record-file-cycle ((tracker dependency-tracker) cycle-chain)
  "Record a file dependency cycle."
  (pushnew cycle-chain (file-cycles tracker) :test #'string=))

(defmethod record-package-cycle ((tracker dependency-tracker) cycle-chain)
  "Record a package dependency cycle."
  (pushnew cycle-chain (package-cycles tracker) :test #'string=))

(defmethod get-project-cycles (&optional tracker)
  "Get all recorded project dependency cycles."
  (project-cycles (ensure-tracker tracker)))

(defmethod get-file-cycles (&optional tracker)
  "Get all recorded file dependency cycles."
  (file-cycles (ensure-tracker tracker)))

(defmethod get-package-cycles (&optional tracker)
  "Get all recorded package dependency cycles."
  (package-cycles (ensure-tracker tracker)))


(defmethod get-macro-body-symbols (&optional (tracker nil tracker-provided-p) macro-name)
  "Get all recorded symbols used in a macro's body."
  (let ((actual-tracker (if tracker-provided-p tracker (ensure-tracker))))
    (gethash (make-tracking-key macro-name) 
             (slot-value actual-tracker 'macro-bodies))))

(defmethod lookup-macro-definition (&optional (tracker nil tracker-provided-p) symbol)
  "Find a macro's definition if it exists."
  (let* ((actual-tracker (if tracker-provided-p tracker (ensure-tracker)))
         (current-pkg (symbol-package symbol))
         (sym-name (symbol-name symbol)))
    (when current-pkg
      (let ((def (gethash (make-tracking-key symbol (package-name current-pkg))
                         (slot-value actual-tracker 'definitions))))
        (when (and def (eq (definition.type def) :macro))
          (return-from lookup-macro-definition def)))
      (dolist (used-pkg (package-use-list current-pkg))
        (multiple-value-bind (used-sym status)
            (find-symbol sym-name used-pkg)
          (when (and used-sym (member status '(:external :inherited)))
            (let* ((used-key (make-tracking-key used-sym (package-name used-pkg))))
              (let ((def (gethash used-key (slot-value actual-tracker 'definitions))))
                (when (and def (eq (definition.type def) :macro))
                  (return-from lookup-macro-definition def)))))))
      (let ((def (gethash sym-name (slot-value actual-tracker 'definitions))))
        (when (and def (eq (definition.type def) :macro))
          def)))))


;;; Package Option Processing Utils

(defun process-package-use-option (package used-pkg-name options parser)
  "Process a :use package option."
  (let ((used-pkg (find-package used-pkg-name)))
    (when used-pkg
      (use-package used-pkg package)
      (record-package-use *current-tracker* 
                         (package-name package) 
                         (package-name used-pkg))
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
                              :package (package-name used-pkg)))))))))


(defun process-package-import-option (package from-pkg-name pkg-name parser sym)
  "Process an :import-from package option for a single symbol."
  (let* ((from-pkg (find-package from-pkg-name))
         (name (normalize-symbol-name sym))
         (from-sym (find-symbol name from-pkg)))
    (when (and from-pkg from-sym)
      (import from-sym package)
      (record-package-use *current-tracker* pkg-name from-pkg-name)
      (record-reference *current-tracker* from-sym
                       :import
                       (file parser)
                       :package from-pkg-name))))


(defun process-package-export-option (package pkg-name sym)
  "Process an :export package option for a single symbol."
  (let* ((name (normalize-symbol-name sym))
         (exported-sym (intern name package)))
    (export exported-sym package)
    (record-export *current-tracker* pkg-name exported-sym)))


;;; Cycle Detection Utils

(defun record-file-dependency-cycle (parser file-name)
  "Record a file dependency cycle in the tracker."
  (let ((position (member file-name (parsing-files parser) :test #'equal)))
    (when position
      (let* ((cycle (cons file-name (ldiff (parsing-files parser) position)))
             (chain (format nil "~{~A~^ -> ~}" (reverse cycle))))
        ;; Record the cycle in the dependency tracker
        (record-file-cycle chain)))))


(defun detect-package-cycle (pkg-name current-packages)
  "Check for and record any package dependency cycles."
  (let ((position (member pkg-name current-packages :test #'string=)))
    (when position
      (let* ((cycle (cons pkg-name (ldiff current-packages position)))
             (chain (format nil "~{~A~^ -> ~}" (reverse cycle))))
        (record-package-cycle chain)))))