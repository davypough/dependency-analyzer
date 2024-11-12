;;;; Filename: function-tests.lisp

;;; Tests for function-to-function dependencies and symbol references.

(in-package #:dep/tests)


(in-suite symbol-tests)


(def-fixture with-test-functions ()
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (file (create-temp-file
                 "(defpackage #:test-funcs 
                    (:use #:cl)
                    (:export #:base-op #:wrapped-op #:call-ops))
                  (in-package #:test-funcs)
                  (defun base-op (x &key (scale 1))
                    (* x scale))
                  (defun wrapped-op (x &key (transform #'base-op))
                    (funcall transform x))
                  (defun call-ops (x)
                    (wrapped-op x :transform #'base-op))"))
         (parser (make-instance 'dep:file-parser :tracker tracker :file file)))
    (unwind-protect
        (progn
          (dep:parse-file parser)
          (&body))
      (delete-temp-file file)
      (dep:clear-tracker tracker))))


(test test-direct-function-call
  "Test dependencies from direct function calls."
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (file (create-temp-file
                 "(defpackage #:test-funcs 
                    (:use #:cl)
                    (:export #:base-op #:wrapped-op #:call-ops))
                  (in-package #:test-funcs)
                  (defun base-op (x &key (scale 1))
                    (* x scale))
                  (defun wrapped-op (x &key (transform #'base-op))
                    (funcall transform x))
                  (defun call-ops (x)
                    (wrapped-op x :transform #'base-op))"))
         (parser (make-instance 'dep:file-parser :tracker tracker :file file)))
    (unwind-protect
        (progn
          ;; Parse file and verify function relationships
          (dep:parse-file parser)
          (let* ((callee (dep:get-definitions tracker 'test-funcs::base-op))
                 (caller (dep:get-definitions tracker 'test-funcs::call-ops))
                 (refs (dep:get-references tracker 'test-funcs::base-op)))

            ;; Verify callee definition
            (is (not (null callee))
                "Called function base-op should be defined~%Current definition: ~A~%All definitions: ~{~%  ~A~}"
                callee 
                (mapcar (lambda (def)
                         (format nil "~A (~A in ~A)"
                                 (dep:definition.symbol def)
                                 (dep:definition.type def)
                                 (dep:definition.package def)))
                       (dep:get-file-definitions tracker file)))

            ;; Verify caller definition
            (is (not (null caller))
                "Calling function call-ops should be defined~%Current definition: ~A~%All definitions: ~{~%  ~A~}"
                caller 
                (mapcar (lambda (def)
                         (format nil "~A (~A in ~A)"
                                 (dep:definition.symbol def)
                                 (dep:definition.type def)
                                 (dep:definition.package def)))
                       (dep:get-file-definitions tracker file)))

            ;; Verify function call references
            (let ((call-refs (remove-if-not 
                             (lambda (ref) (eq (dep:reference.type ref) :call))
                             refs)))
              (is (not (null call-refs))
                  "Should record function call references~%References found: ~{~%  ~A~}~%All reference types: ~{~%  ~A~}"
                  (mapcar (lambda (ref)
                           (format nil "~A (~A in ~A from ~A)"
                                   (dep:reference.symbol ref)
                                   (dep:reference.type ref)
                                   (dep:reference.package ref)
                                   (dep:reference.file ref)))
                         call-refs)
                  (mapcar #'dep:reference.type refs)))

            ;; Verify reference context
            (let ((ref-contexts (mapcar #'dep:reference.context refs)))
              (is (some #'null ref-contexts)
                  "Should include direct function calls~%Reference contexts: ~{~%  ~A~}"
                  (mapcar (lambda (ref)
                           (format nil "Context: ~A (~A in ~A)"
                                   (dep:reference.context ref)
                                   (dep:reference.type ref)
                                   (dep:reference.package ref)))
                         refs)))))
      ;; Cleanup
      (progn
        (delete-temp-file file)
        (when (find-package "TEST-FUNCS")
          (delete-package "TEST-FUNCS"))
        (dep:clear-tracker tracker)))))


(test test-function-argument-dependencies
  "Test dependencies from function arguments."
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (file (create-temp-file
                 "(defpackage #:test-args
                    (:use #:cl)
                    (:export #:process-data 
                            #:data-item
                            #:data-item-value
                            #:make-data-item))
                  (in-package #:test-args)
                  (defstruct data-item 
                    \"Structure representing a data item\"
                    value)
                  (defun process-data (items)
                    \"Process a list of data items\"
                    (mapcar #'data-item-value items))"))
         (parser (make-instance 'dep:file-parser :tracker tracker :file file)))
    (unwind-protect
        (progn
          ;; Parse file
          (dep:parse-file parser)
          ;; Test type definition recording
          (let ((type-def (dep:get-definitions tracker 'test-args::data-item)))
            (is (not (null type-def))
                "Argument type data-item should be defined~%Current definition: ~A~%All definitions: ~{~%  ~A~}"
                type-def 
                (mapcar (lambda (def)
                         (format nil "~A (~A in ~A)"
                                 (dep:definition.symbol def)
                                 (dep:definition.type def)
                                 (dep:definition.package def)))
                       (dep:get-file-definitions tracker file))))
          ;; Test function definition recording
          (let ((fn-def (dep:get-definitions tracker 'test-args::process-data)))
            (is (not (null fn-def))
                "Function process-data should be defined~%Current definition: ~A~%All definitions: ~{~%  ~A~}"
                fn-def
                (mapcar (lambda (def)
                         (format nil "~A (~A in ~A)"
                                 (dep:definition.symbol def)
                                 (dep:definition.type def)
                                 (dep:definition.package def)))
                       (dep:get-file-definitions tracker file))))
          ;; Test accessor reference recording
          (let* ((accessor-refs (dep:get-references tracker 'test-args::data-item-value))
                 (fn-refs (remove-if-not 
                          (lambda (ref) (eq (dep:reference.type ref) :reference))
                          accessor-refs)))
            (is (not (null fn-refs))
                "Should record accessor reference~%References found: ~{~%  ~A~}~%All references by type:~{~%  ~A: ~D refs~}"
                (mapcar (lambda (ref)
                         (format nil "~A (~A in ~A, context: ~A)"
                                 (dep:reference.symbol ref)
                                 (dep:reference.type ref)
                                 (dep:reference.package ref)
                                 (dep:reference.context ref)))
                       fn-refs)
                (loop for type in (remove-duplicates 
                                 (mapcar #'dep:reference.type accessor-refs))
                      collect (cons type 
                                  (count type accessor-refs 
                                        :key #'dep:reference.type))
                      collect ": "
                      collect (count type accessor-refs 
                                   :key #'dep:reference.type))))
          ;; Test automatic definition creation
          (let ((accessor-def (dep:get-definitions tracker 'test-args::data-item-value)))
            (is (not (null accessor-def))
                "Accessor function should be automatically defined~%Current definition: ~A~%All definitions with their exports:~{~%  ~A~}"
                accessor-def
                (mapcar (lambda (def)
                         (format nil "~A (~A in ~A, exported: ~A)"
                                 (dep:definition.symbol def)
                                 (dep:definition.type def)
                                 (dep:definition.package def)
                                 (dep:definition.exported-p def)))
                       (dep:get-file-definitions tracker file)))))
      ;; Cleanup
      (progn
        (delete-temp-file file)
        (when (find-package "TEST-ARGS")
          (delete-package "TEST-ARGS"))
        (dep:clear-tracker tracker)))))


(test test-keyword-parameter-dependencies
  "Test dependencies from keyword parameters."
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (file (create-temp-file
                 "(defpackage #:test-funcs 
                    (:use #:cl)
                    (:export #:base-op #:wrapped-op #:call-ops))
                  (in-package #:test-funcs)
                  (defun base-op (x &key multiplier)
                    \"Base operation with optional multiplier\"
                    (* x (or multiplier 1)))
                  (defun wrapped-op (x &key (transform #'base-op))
                    \"Wrapper with customizable transform function\"
                    (funcall transform x))
                  (defun call-ops (x)
                    \"Demonstrate different ways of calling wrapped-op\"
                    (list
                      (wrapped-op x)
                      (wrapped-op x :transform #'base-op)))"))
         (parser (make-instance 'dep:file-parser :tracker tracker :file file)))
    (unwind-protect
        (progn
          ;; Parse file
          (dep:parse-file parser)
          ;; Test function definition
          (let ((fn-def (dep:get-definitions tracker 'test-funcs::wrapped-op)))
            (is (not (null fn-def))
                "Function wrapped-op should be defined~%Current definition: ~A~%All definitions in file: ~{~%  ~A~}"
                fn-def 
                (mapcar (lambda (def)
                         (format nil "~A (~A in ~A, exported: ~A)"
                                 (dep:definition.symbol def)
                                 (dep:definition.type def)
                                 (dep:definition.package def)
                                 (dep:definition.exported-p def)))
                       (dep:get-file-definitions tracker file))))
          ;; Test reference recording
          (let* ((refs (dep:get-references tracker 'test-funcs::base-op))
                 (ref-by-type (make-hash-table)))
            ;; Collect references by type
            (dolist (ref refs)
              (push ref (gethash (dep:reference.type ref) ref-by-type)))
            ;; Test different reference types
            (is (gethash :reference ref-by-type)
                "Should record function reference in keyword arg~%References found: ~{~%  ~A~}"
                (mapcar (lambda (ref)
                         (format nil "~A (~A in ~A, context: ~A)"
                                 (dep:reference.symbol ref)
                                 (dep:reference.type ref)
                                 (dep:reference.package ref)
                                 (dep:reference.context ref)))
                       refs))
            ;; Verify reference contexts
            (let ((contexts (mapcar #'dep:reference.context refs)))
              (is (some (lambda (ctx) 
                         (and ctx (consp ctx) (eq (car ctx) :key)))
                      contexts)
                  "Should include keyword parameter context~%Reference contexts: ~{~%  ~A~}"
                  (mapcar (lambda (ref)
                           (format nil "~A: ~A"
                                   (dep:reference.type ref)
                                   (dep:reference.context ref)))
                         refs)))
            ;; Test usage statistics
            (is (> (hash-table-count ref-by-type) 1)
                "Should record multiple reference types~%Reference type counts:~{~%  ~A: ~D refs~}"
                (loop for type being the hash-keys of ref-by-type
                        using (hash-value type-refs)
                      collect type
                      collect (length type-refs)))))
      ;; Cleanup
      (progn
        (delete-temp-file file)
        (when (find-package "TEST-FUNCS")
          (delete-package "TEST-FUNCS"))
        (dep:clear-tracker tracker)))))


(test test-function-return-value-dependencies
  "Test dependencies from return value types."
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (file (create-temp-file
                "(defpackage #:test-returns
                   (:use #:cl)
                   (:export #:result
                           #:make-result 
                           #:result-value
                           #:get-result
                           #:get-wrapped-result))
                 (in-package #:test-returns)
                 (defstruct (result (:constructor make-result)
                                  (:conc-name result-))
                   \"Structure representing a computation result\"
                   (value nil :type t)
                   (timestamp (get-universal-time) :type integer))
                 (defun get-result (x)
                   \"Create a basic result\"
                   (make-result :value x))
                 (defun get-wrapped-result (x)
                   \"Create a result wrapped in another result\"
                   (make-result :value (get-result x)))"))
         (parser (make-instance 'dep:file-parser :tracker tracker :file file)))
    (unwind-protect
        (progn
          ;; Parse file
          (dep:parse-file parser)
          ;; Test type definition
          (let ((type-def (dep:get-definitions tracker 'test-returns::result)))
            (is (not (null type-def))
                "Return type result should be defined~%Current definition: ~A~%All definitions: ~{~%  ~A~}"
                type-def
                (mapcar (lambda (def)
                         (format nil "~A (~A in ~A, exported: ~A)"
                                 (dep:definition.symbol def)
                                 (dep:definition.type def)
                                 (dep:definition.package def)
                                 (dep:definition.exported-p def)))
                       (dep:get-file-definitions tracker file))))
          ;; Test function definition and return type
          (let ((fn-def (dep:get-definitions tracker 'test-returns::get-result)))
            (is (not (null fn-def))
                "Function get-result should be defined~%Current definition: ~A~%All definitions: ~{~%  ~A~}"
                fn-def
                (mapcar (lambda (def)
                         (format nil "~A (~A in ~A, exported: ~A)"
                                 (dep:definition.symbol def)
                                 (dep:definition.type def)
                                 (dep:definition.package def)
                                 (dep:definition.exported-p def)))
                       (dep:get-file-definitions tracker file))))
          ;; Test constructor references
          (let* ((ctor-refs (dep:get-references tracker 'test-returns::make-result))
                 (ref-by-type (make-hash-table)))
            ;; Group references by type
            (dolist (ref ctor-refs)
              (push ref (gethash (dep:reference.type ref) ref-by-type)))
            ;; Test for constructor calls
            (is (gethash :call ref-by-type)
                "Should record constructor call references~%References found: ~{~%  ~A~}"
                (mapcar (lambda (ref)
                         (format nil "~A (~A in ~A, from: ~A)"
                                 (dep:reference.symbol ref)
                                 (dep:reference.type ref)
                                 (dep:reference.package ref)
                                 (dep:reference.file ref)))
                       ctor-refs))
            ;; Test usage patterns
            (is (> (hash-table-count ref-by-type) 0)
                "Should record constructor usage patterns~%Reference counts by type:~{~%  ~A: ~D calls~}"
                (loop for type being the hash-keys of ref-by-type
                        using (hash-value refs)
                      collect type
                      collect (length refs))))
          ;; Test nested dependencies
          (let* ((nested-fn (dep:get-definitions tracker 'test-returns::get-wrapped-result))
                 (nested-refs (dep:get-references tracker 'test-returns::get-result)))
            (is (not (null nested-fn))
                "Nested function should be defined~%Definition: ~A"
                nested-fn)
            (is (not (null nested-refs))
                "Should record nested function calls~%References found: ~{~%  ~A~}"
                (mapcar (lambda (ref)
                         (format nil "~A (~A in ~A, context: ~A)"
                                 (dep:reference.symbol ref)
                                 (dep:reference.type ref)
                                 (dep:reference.package ref)
                                 (dep:reference.context ref)))
                       nested-refs))))
      ;; Cleanup
      (progn
        (delete-temp-file file)
        (when (find-package "TEST-RETURNS")
          (delete-package "TEST-RETURNS"))
        (dep:clear-tracker tracker)))))


(test test-method-specializer-dependencies
  "Test dependencies from method specializers."
  (let* ((tracker (make-instance 'dep:dependency-tracker :system-name "test"))
         (file (create-temp-file
                "(defpackage #:test-methods
                   (:use #:cl)
                   (:export #:processor
                           #:make-processor
                           #:processor-state
                           #:process
                           #:stateful-processor))
                 (in-package #:test-methods)
                 
                 (defclass processor ()
                   ((state :initform nil
                          :accessor processor-state
                          :documentation \"Current processor state\"))
                   (:documentation \"Base processor class\"))
                 
                 (defclass stateful-processor (processor)
                   ((history :initform nil
                            :accessor processor-history
                            :documentation \"Processing history\"))
                   (:documentation \"Processor with history tracking\"))
                 
                 (defgeneric process (processor input)
                   (:documentation \"Process input through processor\"))
                 
                 (defmethod process ((proc processor) input)
                   \"Base processing method\"
                   (with-slots (state) proc
                     (setf state input)))
                 
                 (defmethod process :after ((proc stateful-processor) input)
                   \"Additional history tracking for stateful processor\"
                   (with-slots (history) proc
                     (push input history)))"))
         (parser (make-instance 'dep:file-parser :tracker tracker :file file)))
    (unwind-protect
        (progn
          ;; Parse file
          (dep:parse-file parser)
          
          ;; Test class definition
          (let* ((class-def (dep:get-definitions tracker 'test-methods::processor))
                 (class-refs (dep:get-references tracker 'test-methods::processor)))
            (is (not (null class-def))
                "Specializer class processor should be defined~%Current definition: ~A~%All definitions: ~{~%  ~A~}"
                class-def
                (mapcar (lambda (def)
                         (format nil "~A (~A in ~A, exported: ~A)"
                                 (dep:definition.symbol def)
                                 (dep:definition.type def)
                                 (dep:definition.package def)
                                 (dep:definition.exported-p def)))
                       (dep:get-file-definitions tracker file)))
            
            ;; Test class reference types
            (let ((ref-types (mapcar #'dep:reference.type class-refs)))
              (is (member :reference ref-types)
                  "Should record class references~%Reference types found: ~{~A~^, ~}~%All references: ~{~%  ~A~}"
                  ref-types
                  (mapcar (lambda (ref)
                           (format nil "~A (~A in ~A, context: ~A)"
                                   (dep:reference.symbol ref)
                                   (dep:reference.type ref)
                                   (dep:reference.package ref)
                                   (dep:reference.context ref)))
                         class-refs))))
          
          ;; Test generic function definition
          (let ((gen-def (dep:get-definitions tracker 'test-methods::process)))
            (is (not (null gen-def))
                "Generic function process should be defined~%Current definition: ~A~%All definitions: ~{~%  ~A~}"
                gen-def
                (mapcar (lambda (def)
                         (format nil "~A (~A in ~A, exported: ~A)"
                                 (dep:definition.symbol def)
                                 (dep:definition.type def)
                                 (dep:definition.package def)
                                 (dep:definition.exported-p def)))
                       (dep:get-file-definitions tracker file))))
          
          ;; Test specializer references
          (let* ((spec-refs (dep:get-references tracker 'test-methods::processor))
                 (method-refs (remove-if-not 
                             (lambda (ref)
                               (and (eq (dep:reference.type ref) :reference)
                                    (equal (dep:reference.context ref) :specializer)))
                             spec-refs)))
            (is (not (null method-refs))
                "Should record specializer references~%Method references: ~{~%  ~A~}~%All references: ~{~%  ~A~}"
                (mapcar (lambda (ref)
                         (format nil "~A (~A in ~A, context: ~A)"
                                 (dep:reference.symbol ref)
                                 (dep:reference.type ref)
                                 (dep:reference.package ref)
                                 (dep:reference.context ref)))
                       method-refs)
                (mapcar (lambda (ref)
                         (format nil "~A (~A in ~A, context: ~A)"
                                 (dep:reference.symbol ref)
                                 (dep:reference.type ref)
                                 (dep:reference.package ref)
                                 (dep:reference.context ref)))
                       spec-refs)))
          
          ;; Test slot references
          (let* ((slot-refs (remove-if-not 
                            (lambda (ref)
                              (and (eq (dep:reference.type ref) :reference)
                                   (member (dep:reference.context ref) 
                                         '(:slot :accessor) :test #'equal)))
                            (dep:get-references tracker 'test-methods::state))))
            (is (not (null slot-refs))
                "Should record slot references~%Slot references: ~{~%  ~A~}"
                (mapcar (lambda (ref)
                         (format nil "~A (~A in ~A, context: ~A)"
                                 (dep:reference.symbol ref)
                                 (dep:reference.type ref)
                                 (dep:reference.package ref)
                                 (dep:reference.context ref)))
                       slot-refs))))
      ;; Cleanup
      (progn
        (delete-temp-file file)
        (when (find-package "TEST-METHODS")
          (delete-package "TEST-METHODS"))
        (dep:clear-tracker tracker)))))