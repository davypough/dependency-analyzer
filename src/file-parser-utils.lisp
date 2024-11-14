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