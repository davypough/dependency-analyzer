;;;; Filename: macro-tests.lisp

;;; Tests for macro dependencies and expansion tracking.

(in-package #:dep/tests)


(in-suite symbol-tests)


(def-fixture with-test-macros ()
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (file (create-temp-file
                 "(defpackage #:test-macros
                    (:use #:cl)
                    (:export #:with-wrapper #:helper-func #:call-wrapped))
                  (in-package #:test-macros)
                  (defun helper-func (x)
                    (1+ x))
                  (defmacro with-wrapper (&body body)
                    `(let ((result (progn ,@body)))
                       (helper-func result)))
                  (defun call-wrapped ()
                    (with-wrapper (+ 1 2)))"))
         (parser (make-instance 'dep:file-parser :tracker tracker :file file)))
    (unwind-protect
        (progn
          (dep:parse-file parser)
          (&body))
      (delete-temp-file file)
      (dep:clear-tracker tracker))))


(test test-macro-expansion-symbols
  "Test tracking of symbols used in macro expansions."
  (with-fixture with-test-macros ()
    (let* ((macro-def (dep:get-definitions tracker 'test-macros::with-wrapper))
           (helper-refs (dep:get-references tracker 'test-macros::helper-func)))
      (is (not (null macro-def))
          "Macro with-wrapper should be defined~%Current definition: ~A~%All definitions in file: ~{~%  ~A~}"
          macro-def (dep:get-file-definitions tracker file))
      (is (find :macro-body (mapcar #'dep:reference.type helper-refs))
          "Should record macro body symbol references~%References found: ~{~%  Type: ~A~%  File: ~A~%  Context: ~A~%  Package: ~A~%~}"
          (mapcar (lambda (ref)
                   (list (dep:reference.type ref)
                         (dep:reference.file ref)
                         (dep:reference.context ref)
                         (dep:reference.package ref)))
                 helper-refs)))))


(test test-nested-macro-dependencies
  "Test dependencies from nested macro expansions."
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (file (create-temp-file
                 "(defpackage #:test-nested
                    (:use #:cl)
                    (:export #:outer #:inner #:use-macros))
                  (in-package #:test-nested)
                  (defmacro inner (x)
                    `(1+ ,x))
                  (defmacro outer (x)
                    `(inner ,x))
                  (defun use-macros (x)
                    (outer x))"))
         (parser (make-instance 'dep:file-parser :tracker tracker :file file)))
    (unwind-protect
        (progn
          (dep:parse-file parser)
          (let* ((inner-def (dep:get-definitions tracker 'test-nested::inner))
                 (outer-def (dep:get-definitions tracker 'test-nested::outer))
                 (inner-refs (dep:get-references tracker 'test-nested::inner)))
            (is (not (null inner-def))
                "Inner macro should be defined~%Current definition: ~A~%All definitions: ~{~%  ~A~}"
                inner-def (dep:get-file-definitions tracker file))
            (is (not (null outer-def))
                "Outer macro should be defined~%Current definition: ~A~%All definitions: ~{~%  ~A~}"
                outer-def (dep:get-file-definitions tracker file))
            (is (find :macro-body (mapcar #'dep:reference.type inner-refs))
                "Should record nested macro reference~%References found: ~{~%  Type: ~A~%  File: ~A~%  Context: ~A~%~}"
                (mapcar (lambda (ref)
                         (list (dep:reference.type ref)
                               (dep:reference.file ref)
                               (dep:reference.context ref)))
                       inner-refs))))
      (delete-temp-file file)
      (dep:clear-tracker tracker))))


(test test-macro-helper-function-dependencies
  "Test dependencies on helper functions used by macros."
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (file (create-temp-file
                 "(defpackage #:test-helpers
                    (:use #:cl)
                    (:export #:define-wrapper))
                  (in-package #:test-helpers)
                  (defun generate-wrapper-name (name)
                    (intern (format nil \"~A-WRAPPER\" name)))
                  (defmacro define-wrapper (name args)
                    `(defun ,(generate-wrapper-name name) ,args
                       (format t \"Wrapping ~A~%\" ',name)))"))
         (parser (make-instance 'dep:file-parser :tracker tracker :file file)))
    (unwind-protect
        (progn
          (dep:parse-file parser)
          (let* ((helper-def (dep:get-definitions 
                            tracker 'test-helpers::generate-wrapper-name))
                 (macro-def (dep:get-definitions 
                           tracker 'test-helpers::define-wrapper))
                 (helper-refs (dep:get-references 
                             tracker 'test-helpers::generate-wrapper-name)))
            (is (not (null helper-def))
                "Helper function should be defined~%Current definition: ~A~%All definitions: ~{~%  ~A~}"
                helper-def (dep:get-file-definitions tracker file))
            (is (not (null macro-def))
                "Macro should be defined~%Current definition: ~A~%All definitions: ~{~%  ~A~}"
                macro-def (dep:get-file-definitions tracker file))
            (is (find :macro-body (mapcar #'dep:reference.type helper-refs))
                "Should record helper function reference~%References found: ~{~%  Type: ~A~%  File: ~A~%  Package: ~A~%  Context: ~A~%~}"
                (mapcar (lambda (ref)
                         (list (dep:reference.type ref)
                               (dep:reference.file ref)
                               (dep:reference.package ref)
                               (dep:reference.context ref)))
                       helper-refs))))
      (delete-temp-file file)
      (dep:clear-tracker tracker))))


(test test-macro-runtime-dependencies
  "Test dependencies for symbols used at macro runtime."
  (with-fixture with-test-macros ()
    (let* ((helper-def (dep:get-definitions tracker 'test-macros::helper-func))
           (wrapper-def (dep:get-definitions tracker 'test-macros::with-wrapper))
           (call-def (dep:get-definitions tracker 'test-macros::call-wrapped))
           (helper-refs (dep:get-references tracker 'test-macros::helper-func)))
      (is (not (null helper-def))
          "Helper function should be defined~%Current definition: ~A~%All definitions: ~{~%  ~A~}"
          helper-def (dep:get-file-definitions tracker file))
      (is (not (null wrapper-def))
          "Macro should be defined~%Current definition: ~A~%All definitions: ~{~%  ~A~}"
          wrapper-def (dep:get-file-definitions tracker file))
      (is (not (null call-def))
          "Calling function should be defined~%Current definition: ~A~%All definitions: ~{~%  ~A~}"
          call-def (dep:get-file-definitions tracker file))
      (is (find :macro-body (mapcar #'dep:reference.type helper-refs))
          "Should record runtime helper reference~%References found: ~{~%  Type: ~A~%  File: ~A~%  Package: ~A~%~}"
          (mapcar (lambda (ref)
                   (list (dep:reference.type ref)
                         (dep:reference.file ref)
                         (dep:reference.package ref)))
                 helper-refs)))))


(test test-macro-expansion-time-dependencies
  "Test dependencies for symbols used at macro expansion time."
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (file (create-temp-file
                 "(defpackage #:test-expand
                    (:use #:cl)
                    (:export #:with-counter))
                  (in-package #:test-expand)
                  (let ((counter 0))
                    (defmacro with-counter (&body body)
                      (incf counter)
                      `(progn ,@body)))"))
         (parser (make-instance 'dep:file-parser :tracker tracker :file file)))
    (unwind-protect
        (progn
          (dep:parse-file parser)
          (let* ((macro-def (dep:get-definitions 
                           tracker 'test-expand::with-counter))
                 (sym-refs (dep:get-references 
                          tracker 'test-expand::with-counter)))
            (is (not (null macro-def))
                "Macro should be defined~%Current definition: ~A~%All definitions: ~{~%  ~A~}"
                macro-def (dep:get-file-definitions tracker file))
            (is (intersection '(:macro :macro-body)
                            (mapcar #'dep:reference.type sym-refs)
                            :test #'eq)
                "Should record expansion-time symbol usage~%References found: ~{~%  Type: ~A~%  File: ~A~%  Package: ~A~%  Context: ~A~%~}"
                (mapcar (lambda (ref)
                         (list (dep:reference.type ref)
                               (dep:reference.file ref)
                               (dep:reference.package ref)
                               (dep:reference.context ref)))
                       sym-refs))))
      (delete-temp-file file)
      (dep:clear-tracker tracker))))