;;;; Filename: tracker-tests.lisp

;;; Test suite for dependency tracker functionality.
;;; Tests cover tracker creation, definition and reference recording,
;;; dependency tracking, and package relationship management.

(in-package #:dep/tests)


(in-suite tracker-tests)


(test test-create-tracker
  "Test basic tracker instantiation and initialization."
  (let ((tracker (make-instance 'dep:dependency-tracker 
                               :system-name "test-system")))
    (is (typep tracker 'dep:dependency-tracker)
        "Should create a dependency-tracker instance~%Got: ~A" tracker)
    (is (string= (dep:system.name tracker) "test-system")
        "Should set correct system name~%Expected: test-system~%Got: ~A"
        (dep:system.name tracker))))


(test test-record-basic-definition
  "Test recording and retrieving a basic function definition."
  (let ((tracker (make-instance 'dep:dependency-tracker 
                               :system-name "test-system")))
    (unwind-protect
        (progn
          ;; Ensure test package exists
          (unless (find-package "TEST-PACKAGE")
            (make-package "TEST-PACKAGE" :use '(:cl)))
          
          ;; Create symbol in test package
          (let ((my-func (intern "MY-FUNCTION" "TEST-PACKAGE")))
            ;; Record definition and verify
            (dep:record-definition tracker my-func 
                                 :function "test.lisp"
                                 :package "TEST-PACKAGE")
            (let ((def (dep:get-definitions tracker my-func)))
              (is (typep def 'dep:definition)
                  "Should record a definition object~%Got: ~A~%All recorded definitions: ~{~%  ~A~}"
                  def (dep:get-file-definitions tracker "test.lisp"))
              (is (eq (dep:definition.symbol def) my-func)
                  "Should record correct symbol~%Expected: ~A~%Got: ~A"
                  my-func (dep:definition.symbol def))
              (is (eq (dep:definition.type def) :function)
                  "Should record correct type~%Expected: :FUNCTION~%Got: ~A"
                  (dep:definition.type def))
              (is (string= (dep:definition.file def) "test.lisp")
                  "Should record correct file~%Expected: test.lisp~%Got: ~A"
                  (dep:definition.file def))
              (is (string= (dep:definition.package def) "TEST-PACKAGE")
                  "Should record correct package~%Expected: TEST-PACKAGE~%Got: ~A"
                  (dep:definition.package def)))))
      ;; Cleanup
      (when (find-package "TEST-PACKAGE")
        (delete-package "TEST-PACKAGE")))))


(test test-record-exported-definition
  "Test recording a definition with package and export information."
  (let ((tracker (make-instance 'dep:dependency-tracker 
                               :system-name "test-system")))
    (unwind-protect
        (progn
          ;; Ensure test package exists and create symbol in it
          (unless (find-package "TEST-PACKAGE")
            (make-package "TEST-PACKAGE" :use '(:cl)))
          (let ((exported-func (intern "EXPORTED-FUNC" "TEST-PACKAGE")))
            ;; Record definition and verify
            (dep:record-definition tracker exported-func
                                 :function "test.lisp"
                                 :package "TEST-PACKAGE" 
                                 :exported-p t)
            (let ((def (dep:get-definitions tracker exported-func)))
              (is (typep def 'dep:definition)
                  "Should record a definition object~%Got: ~A~%All definitions in file: ~{~%  ~A~}"
                  def (dep:get-file-definitions tracker "test.lisp"))
              (is (string= (dep:definition.package def) "TEST-PACKAGE")
                  "Should record correct package~%Expected: TEST-PACKAGE~%Got: ~A"
                  (dep:definition.package def))
              (is-true (dep:definition.exported-p def)
                       "Should record export status~%Definition details:~%  Symbol: ~A~%  Package: ~A~%  Type: ~A~%  File: ~A"
                       (dep:definition.symbol def)
                       (dep:definition.package def)
                       (dep:definition.type def)
                       (dep:definition.file def)))))
      ;; Cleanup
      (when (find-package "TEST-PACKAGE")
        (delete-package "TEST-PACKAGE")))))


(test test-record-reference
  "Test recording and retrieving function references."
  (let ((tracker (make-instance 'dep:dependency-tracker 
                               :system-name "test-system")))
    (unwind-protect
        (progn
          ;; Ensure test package exists
          (unless (find-package "TEST-PACKAGE")
            (make-package "TEST-PACKAGE" :use '(:cl)))
          
          ;; Create symbol in test package
          (let ((my-func (intern "MY-FUNCTION" "TEST-PACKAGE")))
            ;; Record reference and verify
            (dep:record-reference tracker my-func 
                                :call "other.lisp"
                                :package "TEST-PACKAGE")
            (let ((refs (dep:get-references tracker my-func)))
              (is (= 1 (length refs))
                  "Should record exactly one reference~%References found: ~{~%  ~A~}"
                  refs)
              (let ((ref (first refs)))
                (is (eq (dep:reference.symbol ref) my-func)
                    "Should record correct symbol~%Expected: ~A~%Got: ~A"
                    my-func (dep:reference.symbol ref))
                (is (eq (dep:reference.type ref) :call)
                    "Should record correct reference type~%Expected: :CALL~%Got: ~A"
                    (dep:reference.type ref))
                (is (string= (dep:reference.file ref) "other.lisp")
                    "Should record correct file~%Expected: other.lisp~%Got: ~A"
                    (dep:reference.file ref))
                (is (string= (dep:reference.package ref) "TEST-PACKAGE")
                    "Should record correct package~%Expected: TEST-PACKAGE~%Got: ~A"
                    (dep:reference.package ref))))))
      ;; Cleanup
      (when (find-package "TEST-PACKAGE")
        (delete-package "TEST-PACKAGE")))))


(test test-file-dependencies
  "Test tracking bidirectional dependencies between files."
  (let ((tracker (make-instance 'dep:dependency-tracker 
                               :system-name "test-system")))
    (unwind-protect
        (progn
          ;; Ensure test package exists
          (unless (find-package "TEST-PACKAGE")
            (make-package "TEST-PACKAGE" :use '(:cl)))
          ;; Create symbol in test package
          (let ((base-func (intern "BASE-FUNC" "TEST-PACKAGE")))
            ;; Record definition and reference with package context
            (dep:record-definition tracker base-func 
                                 :function "base.lisp"
                                 :package "TEST-PACKAGE")
            (dep:record-reference tracker base-func 
                                :call "dependent.lisp"
                                :package "TEST-PACKAGE")
            ;; Test forward dependencies
            (let ((deps (dep:file-dependencies tracker "dependent.lisp")))
              (is (= 1 (length deps))
                  "Should record exactly one dependency~%Dependencies found: ~{~%  ~A~}"
                  deps)
              (is (string= (first deps) "base.lisp")
                  "Should record correct dependency~%Expected: base.lisp~%Got: ~A"
                  (first deps)))
            ;; Test reverse dependencies
            (let ((deps (dep:file-dependents tracker "base.lisp")))
              (is (= 1 (length deps))
                  "Should record exactly one dependent~%Dependents found: ~{~%  ~A~}"
                  deps)
              (is (string= (first deps) "dependent.lisp")
                  "Should record correct dependent~%Expected: dependent.lisp~%Got: ~A"
                  (first deps)))))
      ;; Cleanup
      (when (find-package "TEST-PACKAGE")
        (delete-package "TEST-PACKAGE")))))


(test test-clear-tracker
  "Test clearing all tracker data."
  (let ((tracker (make-instance 'dep:dependency-tracker 
                               :system-name "test-system")))
    (unwind-protect
        (progn
          ;; Ensure test package exists
          (unless (find-package "TEST-PACKAGE")
            (make-package "TEST-PACKAGE" :use '(:cl)))
          ;; Create symbols in test package
          (let ((func1 (intern "FUNC1" "TEST-PACKAGE"))
                (func2 (intern "FUNC2" "TEST-PACKAGE")))
            ;; Record some test data
            (dep:record-definition tracker func1 :function "file1.lisp" 
                                 :package "TEST-PACKAGE")
            (dep:record-definition tracker func2 :function "file2.lisp" 
                                 :package "TEST-PACKAGE")
            (dep:record-reference tracker func1 :call "file2.lisp" 
                                :package "TEST-PACKAGE")
            ;; Verify data is recorded
            (is (not (null (dep:get-definitions tracker func1)))
                "Should record func1 definition before clearing~%Current definitions: ~{~%  ~A~}"
                (dep:get-file-definitions tracker "file1.lisp"))
            (is (not (null (dep:get-definitions tracker func2)))
                "Should record func2 definition before clearing~%Current definitions: ~{~%  ~A~}"
                (dep:get-file-definitions tracker "file2.lisp"))
            (is (not (null (dep:get-references tracker func1)))
                "Should record func1 reference before clearing~%Current references: ~{~%  ~A~}"
                (dep:get-references tracker func1))
            ;; Clear and verify
            (dep:clear-tracker tracker)
            (is-false (dep:get-definitions tracker func1)
                     "Should clear func1 definition~%Remaining definitions in file1.lisp: ~{~%  ~A~}"
                     (dep:get-file-definitions tracker "file1.lisp"))
            (is-false (dep:get-definitions tracker func2)
                     "Should clear func2 definition~%Remaining definitions in file2.lisp: ~{~%  ~A~}"
                     (dep:get-file-definitions tracker "file2.lisp"))
            (is-false (dep:get-references tracker func1)
                     "Should clear func1 references~%Remaining references: ~{~%  ~A~}"
                     (dep:get-references tracker func1))))
      ;; Cleanup
      (when (find-package "TEST-PACKAGE")
        (delete-package "TEST-PACKAGE")))))