;;;; Filename: indirect-file-tests.lisp

;;; Tests for indirect file dependencies through transitive references.

(in-package #:dep/tests)


(in-suite file-tests)


(def-fixture with-file-chain ()
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (file-a (create-temp-file
                   "(defpackage #:test-a (:use #:cl) (:export #:func-a))
                    (in-package #:test-a)
                    (defun func-a (x) (* x 2))"))
         (file-b (create-temp-file
                   "(defpackage #:test-b (:use #:cl #:test-a) (:export #:func-b))
                    (in-package #:test-b)
                    (defun func-b (x) (func-a (1+ x)))"))
         (file-c (create-temp-file
                   "(defpackage #:test-c (:use #:cl #:test-b))
                    (in-package #:test-c)
                    (defun func-c (x) (func-b x))"))
         (parser-a (make-instance 'dep:file-parser :tracker tracker :file file-a))
         (parser-b (make-instance 'dep:file-parser :tracker tracker :file file-b))
         (parser-c (make-instance 'dep:file-parser :tracker tracker :file file-c)))
    (unwind-protect
        (progn
          (dep:parse-file parser-a)
          (dep:parse-file parser-b)
          (dep:parse-file parser-c)
          (&body))
      (delete-temp-file file-a)
      (delete-temp-file file-b)
      (delete-temp-file file-c)
      (dep:clear-tracker tracker))))


(test test-transitive-file-dependencies
  "Test detection of file dependencies through intermediate files."
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (file-a (create-temp-file
                   "(defpackage #:test-a (:use #:cl) (:export #:func-a))
                    (in-package #:test-a)
                    (defun func-a (x) (* x 2))"))
         (file-b (create-temp-file
                   "(defpackage #:test-b (:use #:cl #:test-a) (:export #:func-b))
                    (in-package #:test-b)
                    (defun func-b (x) (func-a (1+ x)))"))
         (file-c (create-temp-file
                   "(defpackage #:test-c (:use #:cl #:test-b))
                    (in-package #:test-c)
                    (defun func-c (x) (func-b x))"))
         (parser-a (make-instance 'dep:file-parser :tracker tracker :file file-a))
         (parser-b (make-instance 'dep:file-parser :tracker tracker :file file-b))
         (parser-c (make-instance 'dep:file-parser :tracker tracker :file file-c)))
    (unwind-protect
        (progn
          ;; Parse all files to establish dependencies
          (dep:parse-file parser-a)
          (dep:parse-file parser-b)
          (dep:parse-file parser-c)
          
          ;; Test direct dependencies B -> A
          (let ((deps-b (dep:file-dependencies tracker file-b)))
            (is (member file-a deps-b :test #'equal)
                "File ~A should depend on ~A~%Dependencies found: ~{~%  ~A~}~%Symbol references:~{~%  ~A~}"
                file-b file-a deps-b
                (mapcar (lambda (ref) 
                         (format nil "~A in package ~A" 
                                 (dep:reference.symbol ref)
                                 (dep:reference.package ref)))
                       (dep:get-references tracker 'test-a::func-a))))
          
          ;; Test direct dependencies C -> B
          (let ((deps-c (dep:file-dependencies tracker file-c)))
            (is (member file-b deps-c :test #'equal)
                "File ~A should depend on ~A~%Dependencies found: ~{~%  ~A~}~%Symbol references:~{~%  ~A~}"
                file-c file-b deps-c
                (mapcar (lambda (ref)
                         (format nil "~A in package ~A"
                                 (dep:reference.symbol ref)
                                 (dep:reference.package ref)))
                       (dep:get-references tracker 'test-b::func-b))))
          
          ;; Test transitive dependencies C -> A
          (let ((deps-c (dep:file-dependencies tracker file-c)))
            (is (member file-a deps-c :test #'equal)
                "File ~A should transitively depend on ~A through ~A~%Dependencies found: ~{~%  ~A~}~%Full dependency chain:~{~%  ~A~}"
                file-c file-a file-b deps-c
                (list
                  (format nil "~A -> ~A (direct)" file-c file-b)
                  (format nil "~A -> ~A (transitive)" file-b file-a)))))
      ;; Cleanup
      (progn
        ;; Delete temp files
        (delete-temp-file file-a)
        (delete-temp-file file-b)
        (delete-temp-file file-c)
        ;; Delete test packages
        (when (find-package "TEST-A")
          (delete-package "TEST-A"))
        (when (find-package "TEST-B")
          (delete-package "TEST-B"))
        (when (find-package "TEST-C")
          (delete-package "TEST-C"))
        (dep:clear-tracker tracker)))))


(test test-file-dependency-cycle-detection
  "Test detection of circular dependencies between files."
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (file-x (create-temp-file
                   "(defpackage #:test-x (:use #:cl) (:export #:func-x))
                    (in-package #:test-x)
                    (defun func-x () (test-y:func-y))"))
         (file-y (create-temp-file
                   "(defpackage #:test-y (:use #:cl) (:export #:func-y))
                    (in-package #:test-y)
                    (defun func-y () (test-x:func-x))"))
         (parser-x (make-instance 'dep:file-parser :tracker tracker :file file-x))
         (parser-y (make-instance 'dep:file-parser :tracker tracker :file file-y)))
    (unwind-protect
        (progn
          ;; Parse both files to establish cyclic dependencies
          (dep:parse-file parser-x)
          (dep:parse-file parser-y)
          
          ;; Test file dependencies in both directions
          (let ((deps-x (dep:file-dependencies tracker file-x))
                (deps-y (dep:file-dependencies tracker file-y)))
            ;; Verify X -> Y dependency
            (is (member file-y deps-x :test #'equal)
                "File ~A should record dependency on ~A~%Dependencies found: ~{~%  ~A~}~%Symbol references:~{~%  ~A~}"
                file-x file-y deps-x
                (mapcar (lambda (ref)
                         (format nil "~A in package ~A" 
                                 (dep:reference.symbol ref)
                                 (dep:reference.package ref)))
                       (dep:get-references tracker 'test-y::func-y)))
            
            ;; Verify Y -> X dependency
            (is (member file-x deps-y :test #'equal)
                "File ~A should record dependency on ~A~%Dependencies found: ~{~%  ~A~}~%Symbol references:~{~%  ~A~}"
                file-y file-x deps-y
                (mapcar (lambda (ref)
                         (format nil "~A in package ~A"
                                 (dep:reference.symbol ref)
                                 (dep:reference.package ref)))
                       (dep:get-references tracker 'test-x::func-x)))
            
            ;; Test cycle detection
            (let* ((cycles (dep:get-file-cycles tracker))
                   (expected-cycle (format nil "~A -> ~A -> ~A" 
                                         file-x file-y file-x)))
              (is (= 1 (length cycles))
                  "Should detect exactly one cycle~%Cycles found (~D): ~{~%  ~A~}"
                  (length cycles) cycles)
              (is (find expected-cycle cycles :test #'string=)
                  "Should detect the correct dependency cycle~%Expected: ~A~%Found cycles: ~{~%  ~A~}~%Symbol dependencies:~{~%  ~A -> ~A~}"
                  expected-cycle 
                  cycles
                  (list
                    (list 'test-x::func-x 'test-y::func-y)
                    (list 'test-y::func-y 'test-x::func-x))))))
      ;; Cleanup
      (progn
        ;; Delete temp files
        (delete-temp-file file-x)
        (delete-temp-file file-y)
        ;; Delete test packages
        (when (find-package "TEST-X")
          (delete-package "TEST-X"))
        (when (find-package "TEST-Y")
          (delete-package "TEST-Y"))
        (dep:clear-tracker tracker)))))


(test test-dependency-order-requirements
  "Test detection of file loading order requirements."
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (base-file (create-temp-file
                     "(defpackage #:test-base 
                        (:use #:cl)
                        (:export #:*config*))
                      (in-package #:test-base)
                      (defvar *config* '(:debug t :log-level :info))"))
         (util-file (create-temp-file
                     "(defpackage #:test-utils 
                        (:use #:cl #:test-base)
                        (:export #:with-config))
                      (in-package #:test-utils)
                      (defmacro with-config (&body body)
                        `(let ((*config* *config*))
                           ,@body))"))
         (app-file (create-temp-file
                    "(defpackage #:test-app 
                       (:use #:cl #:test-base #:test-utils))
                     (in-package #:test-app)
                     (defun run ()
                       (with-config 
                         (getf *config* :log-level)))"))
         (parser-base (make-instance 'dep:file-parser 
                                    :tracker tracker :file base-file))
         (parser-util (make-instance 'dep:file-parser 
                                    :tracker tracker :file util-file))
         (parser-app (make-instance 'dep:file-parser 
                                   :tracker tracker :file app-file)))
    (unwind-protect
        (progn
          ;; Parse files in dependency order
          (dep:parse-file parser-base)
          (dep:parse-file parser-util)
          (dep:parse-file parser-app)
          
          ;; Test base file dependencies
          (let ((base-deps (dep:file-dependencies tracker base-file)))
            (is (null base-deps)
                "Base file should have no dependencies~%Found dependencies: ~{~%  ~A~}~%Definitions in base file:~{~%  ~A~}"
                base-deps
                (mapcar (lambda (def)
                         (format nil "~A (~A)" 
                                 (dep:definition.symbol def)
                                 (dep:definition.type def)))
                       (dep:get-file-definitions tracker base-file))))
          
          ;; Test util file dependencies
          (let ((util-deps (dep:file-dependencies tracker util-file)))
            (is (member base-file util-deps :test #'equal)
                "Util file ~A should depend on base file ~A~%Dependencies found: ~{~%  ~A~}~%References to base symbols:~{~%  ~A~}"
                util-file base-file util-deps
                (mapcar (lambda (ref)
                         (format nil "~A (~A in ~A)" 
                                 (dep:reference.symbol ref)
                                 (dep:reference.type ref)
                                 (dep:reference.package ref)))
                       (dep:get-references tracker 'test-base::*config*))))
          
          ;; Test app file dependencies
          (let ((app-deps (dep:file-dependencies tracker app-file)))
            (is (and (member base-file app-deps :test #'equal)
                    (member util-file app-deps :test #'equal))
                "App file ~A should depend on both base (~A) and util (~A) files~%Dependencies found: ~{~%  ~A~}~%References:~{~%  ~A~}"
                app-file base-file util-file app-deps
                (append
                  (mapcar (lambda (ref)
                           (format nil "~A (~A in ~A)" 
                                   (dep:reference.symbol ref)
                                   (dep:reference.type ref)
                                   (dep:reference.package ref)))
                         (dep:get-references tracker 'test-base::*config*))
                  (mapcar (lambda (ref)
                           (format nil "~A (~A in ~A)"
                                   (dep:reference.symbol ref)
                                   (dep:reference.type ref)
                                   (dep:reference.package ref)))
                         (dep:get-references tracker 'test-utils::with-config))))))
      ;; Cleanup
      (progn
        ;; Delete temp files
        (delete-temp-file base-file)
        (delete-temp-file util-file)
        (delete-temp-file app-file)
        ;; Delete test packages
        (when (find-package "TEST-BASE")
          (delete-package "TEST-BASE"))
        (when (find-package "TEST-UTILS")
          (delete-package "TEST-UTILS"))
        (when (find-package "TEST-APP")
          (delete-package "TEST-APP"))
        (dep:clear-tracker tracker)))))