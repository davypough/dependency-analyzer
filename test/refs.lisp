;;;; File: refs.lisp

;;;; Test file containing minimal examples of references to all Lisp definition forms
;;;; that can be processed by the dependency analyzer. Forms are grouped by their
;;;; reference type as specified in +valid-definition-types+.


;; First establish package context for remaining references, same as defs.lisp
(in-package #:user-pkg)


;; VARIABLE REFERENCES (to :VARIABLE type)
;; Simple references in different contexts
(list *special1* *param1* +const1+)          ;simple variable refs
(defparameter *param4* (list *param3*))      ;ref in initialization
(+ 1 *special1*)                             ;ref in arithmetic expression
(defun fun1 (&optional (x *param1*))         ;ref in lambda list
  (print x)
  *param3*)                                  ;ref in function body
;; Complex expressions and nested contexts
(setf *special1* (+ (floor (/ *param1* 2)) *param3*))  ;multiple refs in nested forms
(when (> *param1* 0)                                   ;ref in conditional test
  (let ((scaled (+ *special1* *param3*)))              ;refs in let binding
    (values scaled *param4*)))                         ;ref in multiple value return
;; Modifying forms 
(incf *special1* 10)                                   ;ref in modification macro
(push *param3* *param4*)                               ;ref in both value and place
(setf (car *param4*) (1+ *special1*))                  ;refs in complex setf form
;; References in iteration constructs
(dotimes (i *param1*)                                  ;ref as iteration limit
  (list *special1*))                                  ;ref in loop body
(loop for x from 1 to +const1+                         ;ref as iteration limit
      for y = *special1* then (+ y 1)                  ;ref in iteration variable binding
      collect (list x y *param3*))                     ;ref in collection form
;; References in data structures
(make-array *param1* :initial-element *special1*)      ;refs in array creation
(let ((hash (make-hash-table)))
  (setf (gethash '*param1* hash) *param1*              ;ref as key and value
        (gethash '+const1+ hash) +const1+)             ;ref as key and value
  hash)
;; References in special forms
(if (zerop *special1*)                                 ;ref in condition form
    (or *param3* *param4*)                             ;refs in or form
    (and *param1* (not (zerop *special1*))))           ;refs in and form
;; References in backquoted forms
`(,*special1* ,*param1* ,@*param4*)                    ;refs inside backquote with unquote/splice
;; Reflective and runtime references
(symbol-value '*special1*)                             ;ref through reflection
(find-symbol "*PARAM3*" "USER-PKG")                    ;ref through lookup by name
(mapcar #'(lambda (sym) (when (boundp sym) (symbol-value sym)))  ;refs in higher-order function
        '(*special1* *param1* *param3*))
;; References in docstrings and metadata
(defun test-vars ()
  "Function that uses *SPECIAL1* and *PARAM1* variables."  ;refs in docstring
  (list *special1* *param1*))                          ;refs in function body
;; References in type declarations
(let ((x *param1*))
  (declare (type (integer 0 *) x))                     ;implicit ref in type declaration
  (+ x *special1*))                                    ;direct ref in expression
;; References in function application
(funcall #'+ *special1* *param1* +const1+)             ;refs as function arguments
(apply #'list (list *special1* *param3*))   ;refs in apply list

;; PACKAGE REFERENCES (:PACKAGE type)
;; Direct package references with different designators
(find-package :test1-pkg)                   ; Keyword symbol
(find-package (make-symbol "TEST1-PKG"))    ; Uninterned symbol
(find-package "TEST1-PKG")                  ; String, uppercase
(find-package "test1-pkg")                  ; String, lowercase
(find-package :t1pkg)                       ; Reference via nickname (keyword)
(find-package (make-symbol "T1PKG"))        ; Reference via nickname (uninterned)
(find-package "T1PKG")                      ; Reference via nickname (string)
;; References to the indirect packages
(find-package :test3-pkg)                   ; Package with import-from
(find-package :test4-pkg)                   ; Package with shadowing
(find-package :test5-pkg)                   ; Package with intern/export
;; Package references in function/macro arguments
(package-name (find-package :test1-pkg))    ; Package as argument
;; Testing imported symbol references
(let ((*package* (find-package :user-pkg)))
  (let ((sym (find-symbol "VAR1" :test3-pkg))) ; Reference to imported symbol
    sym))
;; Testing shadowed symbol references
(let ((*package* (find-package :user-pkg)))
  (let ((sym1 (find-symbol "VAR1" :test1-pkg))  ; Original symbol
        (sym2 (find-symbol "VAR1" :test4-pkg))) ; Shadowed symbol
    (eq sym1 sym2)))  ; Should be NIL - different symbols
;; Testing package relationships
(let ((pkg3 (find-package :test3-pkg)))
  (member (find-package :test1-pkg)         ; Test import-from relationship
          (package-use-list pkg3)))
;; Package references in unusual contexts
(defun package-test (&optional (pkg :test3-pkg)) ; Reference in lambda list
  (format nil "~A has ~D symbols~%" 
          pkg                               ; Reference in function body
          (loop for sym being the symbols of (find-package pkg)
                count t)))
;; Package references in let bindings and conditionals
(let ((pkg-name :test4-pkg))                ; Reference in variable binding
  (when (find-package pkg-name)             ; Reference in condition
    (format nil "Package ~A exists~%" pkg-name)))
;; Package references in complex expressions
(mapcar #'find-package 
        (list :test1-pkg :test3-pkg :test5-pkg))  ; References in data structure
;; Testing multiple package relationships
(let ((all-pkgs (list :test1-pkg :test2-pkg :test3-pkg :test4-pkg :test5-pkg)))
  (mapcar (lambda (pkg-name)
            (let ((pkg (find-package pkg-name)))
              (list pkg-name 
                    (package-use-list pkg)
                    (package-used-by-list pkg))))
          all-pkgs))


;; FUNCTION REFERENCES (to :FUNCTION type)
;; Simple function calls in different contexts
(fn1 42)                                      ;direct function call
(mapcar #'fn1 (list 1 2 3))                   ;function object as higher-order function arg
(funcall #'fn1 10)                            ;explicit funcall
(apply #'fn1 (list 5))                        ;explicit apply
(fn2a 10 20)                                  ;function with optional argument
(fn2b 30 :z 40)                               ;function with keyword argument
;; setf function usage
(let ((lst (list 0)))
  (setf (fn1 lst) 50)                         ;setf function reference
  lst)
;; Complex expressions and nested function calls
(fn2a (fn1 5) (fn2b 6 :z 7))                  ;nested function calls
(fn3 (+ (fn1 8) (fn1 9)))                     ;composition with arithmetic
;; Higher-order function handling
(map 'list #'fn1 '(10 20 30))                 ;higher-order function 
(let ((numbers (list 3 1 4 2)))
  (setf numbers (sort numbers #'< :key #'fn1)))
(remove-if #'(lambda (x) (zerop (fn1 x)))     ;lambda using defined function
           (list 0 1 2))
;; Macro-expanded forms that result in function calls
(with-test (test-val)                         ;macro expanding to function call
  (when test-val (fn1 100)))
;; Compile-time vs runtime function references
(symbol-function 'fn1)                        ;runtime function lookup
(fdefinition 'fn1)                            ;another runtime function lookup
(function-lambda-expression #'fn1)            ;introspection of function
;; Partial application and composition
(reduce #'(lambda (a b) 
            (if (fn2a a (fn1 b))
                (+ a b)
                (* a b)))
        (list 1 2 3))
;; Calls with complex argument expressions
(fn2a (if (> (fn1 5) 2)
         10
         30)
      (case (fn1 4)
        (4 40)
        (t 50)))


;; MACRO REFERENCES (to :MACRO type)
;; Simple macro usage
(with-test (test-var)                         ;basic macro call
  (list test-var))
(my-macro 42)                                 ;direct macro call
(incf-by *special1* 5)                        ;modify-macro reference
;; Nested macro forms
(with-test (outer-var)                        ;nested macro usage
  (with-test (inner-var)
    (list outer-var inner-var)))
;; Macro expansion and evaluation
(macroexpand '(with-test (x) (list x)))       ;explicit macro expansion
(macroexpand-1 '(with-test (x) (list x)))     ;single-level expansion
(eval (macroexpand '(with-test (var) var)))   ;expansion and evaluation
;; Compiler-macro usage
(my-plus 2 3)                                 ;direct call that compiler may optimize
(let ((a 2) (b 3))
  (my-plus a b))                              ;runtime values for compiler-macro
;; Complex macro contexts
(let ((forms '(1 2 3)))
  (with-test (test-flag)                      ;macro with runtime computed forms
    (mapcar #'(lambda (x) 
                (if test-flag (my-macro x) x))
            forms)))
;; Reflective macro operations
(macro-function 'with-test)                   ;runtime access to macro function
(when (macro-function 'my-macro)              ;conditional based on macro existence
  (my-macro :available))
;; Macro edge cases
(eval-when (:compile-toplevel :load-toplevel :execute)
  (my-macro :eval-when-context))              ;macro in special evaluation context


;; GENERIC FUNCTION REFERENCES (to :GENERIC-FUNCTION type)
;; Simple method invocation
(gf1 42)                                      ;call with number - dispatches to specialized method
(gf1 "hello")                                 ;call with string - different method dispatch
(gf2 10 20)                                   ;call with multiple arguments
;; Setf generic function usage
(let ((test-obj (cons 1 2)))
  (setf (gf3 test-obj) 100)                   ;setf generic function reference
  test-obj)
;; Method combination and qualifiers
(let ((num 5))
  (gf2 num 30))                           ;triggers :before method for number
;; Runtime method selection
(let ((values-to-test (list 42 "text" (cons 1 2))))
  (mapcar #'gf1 values-to-test))              ;dynamic dispatch based on runtime types
;; Complex dispatch scenarios
(let ((test-value 10))
  (gf1 (if (> *special1* 5)                   ;conditional affecting method dispatch
          "string-case"
          test-value)))
;; Edge cases for method dispatch
(gf1 (make-instance 'named-point :name "origin" :u 0 :v 0)) ;dispatch on user-defined class
(handler-case
    (setf (gf3 "invalid") 42)                 ;should trigger method's error condition
  (error (c) c))
;; Reflective generic function operations
(find-method #'gf1 nil (list (find-class 'number))) ;runtime method lookup
(c2mop:generic-function-methods #'gf1)              ;introspection of all methods


;; METHOD REFERENCES (to :METHOD type)
;; Note: Method references occur through generic function calls
;; and are tracked by method specializer matching
;; References to specialized methods with various argument types
(gf1 42)                                      ;call with number - references number method
(gf1 "hello")                                 ;call with string - references string method
(gf2 3.14 10)                             ;references :before method for (number t)
;; Setf method references
(let ((pair (cons 1 2)))
  (setf (gf3 pair) 50)                        ;references setf method for cons objects
  pair)
;; Method combination
(let ((num 5))
  (gf2 num 10)                                ;references both primary and :before methods
  (gf2 20 num))                            ;only references primary method (no :before)
;; EQL specializer references
(process-var '*special1*)                      ;references method specialized on (eql '*special1*)
;; Method inheritance and overriding
(defclass test-point (monitored-point) ())    ;create subclass for testing
(let ((pt (make-instance 'test-point 
                        :name "test"
                        :u 10 :v 20)))
  (point-name pt)                             ;inherited accessor method
  (point-timestamp pt))                       ;specialized accessor method
;; CLOS built-in method references
(find-method #'test-method nil (list (find-class 'string)))


;; SYMBOL MACRO REFERENCES (to :SYMBOL-MACRO type)
;; Simple symbol macro usage
today                                         ;expands to (get-universal-time)
pi/2                                          ;expands to (/ pi 2)
empty-list                                    ;expands to nil
;; Symbol macros in complex expressions
(+ pi/2 (sin pi/2))                           ;arithmetic with symbol macro
(list pi/2 today empty-list)                  ;list with multiple symbol macros
;; Symbol macros in binding contexts
(let ((current-time today))                   ;binding with symbol macro value
  (list current-time pi/2))
;; Symbol macros in iteration
(loop for i from 1 to 3
      collect (+ i pi/2))                     ;symbol macro in loop body
;; Conditional forms with symbol macros
(when (> today 0)                             ;symbol macro in condition
  (list empty-list))
(if (numberp pi/2)                            ;symbol macro in test form
    pi/2
    empty-list)                               ;symbol macro in result form
;; Nested expressions with symbol macros
(cons (+ pi/2 (cos pi/2))                     ;nested usage
      (list empty-list today))
;; Symbol macro expansion
(macroexpand-1 'today)                        ;explicit expansion
(macroexpand-1 'pi/2)
;; Runtime handling of symbol macros
(let ((macros (list 'today 'pi/2 'empty-list)))
  (mapcar #'(lambda (sym)
              (list sym (eval sym)))
          macros))


;; STRUCTURE/CLASS/CONDITION REFERENCES (to :STRUCTURE/CLASS/CONDITION type)
;; Structure usage
(let ((pt (make-point 10 20)))               ;structure constructor reference
  (list (point-x pt)                         ;accessor references
        (point-y pt)
        (point-p pt)                         ;predicate reference
        (copy-point pt)))                    ;copy constructor reference

;; CLOS class usage
(let ((named-pt (make-instance 'named-point  ;class reference in make-instance
                              :name "origin"
                              :u 0 :v 0)))
  (list (point-name named-pt)                ;accessor method references 
        (point-u named-pt)
        (point-v named-pt)))

;; Condition usage
(handler-case
    (error 'invalid-point                    ;condition reference in error
           :reason "Coordinates out of bounds"
           :value '(100 100))
  (invalid-point (c)                         ;condition reference in handler clause
    (list (point-error-reason c)            ;accessor references
          (point-error-value c))))

;; Type testing and dispatching
(typep (make-point 1 2) 'point)             ;type reference in typep
(typecase (make-instance 'named-point)      ;class references in typecase
  (named-point :named)
  (point :point)
  (t :other))

;; Class relationships
(let ((mon-pt (make-instance 'monitored-point
                            :name "tracked"
                            :u 5 :v 10)))
  (list (typep mon-pt 'named-point)         ;superclass reference
        (point-name mon-pt)                 ;inherited method reference
        (point-timestamp mon-pt)))          ;specialized method reference

;; Class reflection
(find-class 'point)                         ;runtime class lookup
(find-class 'named-point)
(find-class 'monitored-point)
(find-class 'invalid-point)
(c2mop:class-direct-superclasses            ;MOP reference to class relationships
 (find-class 'monitored-point))


;; DEFTYPE REFERENCES (to :DEFTYPE type)
;; Simple type declarations and checks
(typep 10 'pos-integer)                      ;type check reference
(typep "hello" 'string-or-symbol)           ;union type reference
(typep 'hello 'string-or-symbol)            ;another union type reference

;; Type references in declarations
(locally
  (let ((x 5)
        (vec (make-array 3 :element-type 'number)))
    (declare (type pos-integer x)          ;type reference in declaration
             (type (sized-vector number 3) vec))     ;parameterized type reference
    (list x vec)))

;; Type references in complex expressions
(flet ((check-types (val)
         (list (typep val 'pos-integer)
               (typep val 'string-or-symbol)
               (if (typep val 'vector)
                   (typep val '(sized-vector t 2))
                   nil))))
  (mapcar #'check-types                     ;multiple type references
          (list 5 "test" 'symbol (vector 1 2))))

;; Reflection and runtime type operations
(subtypep 'pos-integer 'integer)           ;subtype relationship check
(subtypep 'string-or-symbol 'string)
(subtypep 'string-or-symbol 'symbol)

;; Type references in function interfaces
(defun test-sized-vector (vec)
  (declare (type (sized-vector t *) vec))  ;parameterized type in declaration
  (array-dimensions vec))

;; Type references in pattern matching contexts
(typecase 42
  (pos-integer :positive)                  ;user type in typecase
  (string-or-symbol :stringy)
  (t :other))