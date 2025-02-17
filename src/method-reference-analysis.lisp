;;;; Filename:  method-reference-analysis.lisp

;;;; Analyze and record references to method definitions in the user's project files.


(in-package :dep)


(defun analyze-method-references (parser gf-top-level-form) 
  "Analyze generic functions to track method references."
  (let* ((context-form (build-eval-context gf-top-level-form))
         (refs-sym (gensym "REFS"))
         (pushit-form (pushit-transform context-form #'user-gf-designator-p refs-sym))
         (method-refs (extract-method-refs pushit-form refs-sym)))
    (dolist (ref method-refs)
      (let ((gf (first ref))
            (args (rest ref)))
        (record-method-reference parser gf-top-level-form gf args)))))


(defun build-eval-context (form)
  "Build let form to establish bindings for typed parameters in definition forms."
    (if (consp form)
        (case (car form)
          ;; Method definition
          ((defmethod)
           (multiple-value-bind (name qualifiers lambda-list body)
               (extract-method-components form)
             (declare (ignore name qualifiers))
             (let ((bindings (process-lambda-list lambda-list)))
               ;(print `(:debug-method-bindings :bindings ,bindings))
               (let ((result-form 
                      (if bindings
                          `(let ,(reverse bindings)
                             ,@body)
                          `(progn ,@body))))
                 result-form))))
          
          ;; Function definitions with type declarations
          ((flet labels) 
           (let* ((name (second form))
                  (lambda-list (third form))
                  (body (cdddr form))
                  (declarations (when (and (consp (car body))
                                         (eq (caar body) 'declare))
                                (pop body)))
                  (bindings nil))
             ;; Process type declarations
             (when declarations
               (dolist (decl (cdr declarations))
                 (when (eq (car decl) 'type)
                   (push `(,(third decl) (create-mock-instance ',(second decl)))
                         bindings))))
             (list* (car form) name lambda-list
                     (append (when declarations (list declarations))
                             (if bindings
                                 `((let ,(reverse bindings)
                                     ,@body))
                                 body)))))
          ;; No case matched - return original form
          (otherwise form))
        ;; Not a cons - error
        (error "Form ~A is not consp" form)))


(defun extract-method-components (form)
  "Extract components from a defmethod form.
   Returns: (values name qualifiers lambda-list body)"
  (let* ((rest (cddr form))
         (qualifiers (loop while (and rest (keywordp (car rest)))
                           collect (pop rest)))
         (lambda-list (car rest))
         (body (cdr rest)))
    (values (second form) qualifiers lambda-list body)))


(defun process-lambda-list (lambda-list)
  "Build mock instance bindings for specialized parameters.
   Returns: list of bindings for let form"
  (let ((bindings nil))
    (loop for param in lambda-list
          until (and (symbolp param)
                    (member param lambda-list-keywords))
          when (consp param)
          do (let ((param-name (car param))
                   (param-type (second param)))
               (when param-type
                 (let ((binding `(,param-name (create-mock-instance ',param-type))))
                   (push binding bindings)))))
    bindings))


(defun create-mock-instance (specializer)
  "Create a minimal instance sufficient for method dispatch.
   SPECIALIZER - A type specifier from method parameter list.
   Returns instance that satisfies typep for the specializer."
  
  (cond
    ;; Handle EQL specializer: (eql value)
    ((and (consp specializer)
          (eq (car specializer) 'eql))
     (cadr specializer))  ; Return the actual value
    
    ;; Handle built-in types with specific constructors
    ((eq specializer 'string) "")
    ((eq specializer 'number) 0)
    ((eq specializer 'vector) (vector))
    ((eq specializer 'array) (make-array 0))
    ((eq specializer 'hash-table) (make-hash-table))
    ((eq specializer 'package) (find-package :common-lisp))
    ((eq specializer 'symbol) 'mock-symbol)
    
    ;; Handle CLOS classes through instantiation
    (t
     (let ((class (find-class specializer nil)))
       (when class
         ;; For known types, create minimal instance
         (handler-case
             ;; Try direct instantiation first
             (make-instance specializer)
           (error ()
             ;; On failure, try allocate-instance
             (allocate-instance class))))))))


(defmacro pushit (ref result-sym)
  "Push function refs onto RESULT-SYM while preserving the ref's value.
   For atomic forms, pushes (form value).
   For direct function refs, pushes (fn arg1 arg2...).
   For higher-order functions:
   - Comparison functions (sort etc): Uses sequence element twice (fn elem elem)  
   - Mapping functions (mapcar etc): Uses first element from each sequence
   - Sequence/list functions with :key/:test/:test-not: Uses appropriate elements"
  (if (atom ref)
      `(let ((val ,ref))
         (push (list ',ref val) ,result-sym)
         val)
    (destructuring-bind (fn &rest args) ref
      (let ((fn-pos (or (cdr (assoc fn +comparison-functions+))
                        (cdr (assoc fn +mapping-functions+)))))
        (cond
          ;; Direct funcall/apply - extract actual function and args
          ((member fn '(funcall apply))
           (let ((actual-fn (second ref))
                 (actual-args (cddr ref))  
                 (vals (mapcar (lambda (_) (declare (ignore _))
                                 (gensym "arg"))
                               (cddr ref)))
                 (the-value (gensym "value")))
             `(let* (,@(mapcar (lambda (tmp arg) 
                                `(,tmp ,arg))
                              vals actual-args))
                (let ((,the-value (,fn ,actual-fn ,@vals)))
                  (push (cons ,(cond ((and (consp actual-fn)
                                           (eq (car actual-fn) 'function))
                                      `(fdefinition ',(cadr actual-fn)))
                                     ((symbolp actual-fn)
                                      `(fdefinition ',actual-fn))
                                     (t actual-fn))
                              (if (eq ',fn 'apply)
                                ,(car (last actual-args))  ; Just use apply's list arg directly
                                (mapcar #'identity (list ,@vals))))  ; Keep funcall args as-is
                        ,result-sym)
                  ,the-value))))

          ;; Comparison function (sort, stable-sort, merge)
          ((assoc fn +comparison-functions+)
           (let ((pred-arg (nth fn-pos args))
                 (vals (mapcar (lambda (_) (declare (ignore _)) (gensym "arg")) args))
                 (the-value (gensym "value")))
             `(let* (,@(mapcar (lambda (tmp arg) `(,tmp ,arg)) vals args))
                (let ((,the-value (,fn ,@vals)))
                  (when (and (typep ,(nth (if (eq fn 'merge) 1 0) vals) 'sequence)
                             (plusp (length ,(nth (if (eq fn 'merge) 1 0) vals))))
                    (let ((elem (elt ,(nth (if (eq fn 'merge) 1 0) vals) 0)))
                      (push (cons ,(cond 
                                     ;; Handle #'fn form
                                     ((and (consp pred-arg)
                                           (eq (car pred-arg) 'function))
                                     `(fdefinition ',(cadr pred-arg)))
                                     ;; Handle (eval '#'fn) form  
                                     ((and (consp pred-arg)
                                           (eq (car pred-arg) 'eval)
                                           (consp (cadr pred-arg))
                                           (eq (car (cadr pred-arg)) 'quote)
                                           (consp (cadr (cadr pred-arg)))
                                           (eq (car (cadr (cadr pred-arg))) 'function))
                                      `(fdefinition ',(cadr (cadr (cadr pred-arg)))))
                                     ;; Handle bare symbol
                                     ((symbolp pred-arg)
                                      `(fdefinition ',pred-arg))
                                     ;; Handle direct function object
                                     (t pred-arg))
                                  (list elem elem))
                            ,result-sym)))
                  ,the-value))))

          ;; Mapping function (mapcar etc)
          ((assoc fn +mapping-functions+)
           (let ((fn-arg (nth fn-pos args))          
                 (vals (mapcar (lambda (_) (declare (ignore _)) (gensym "arg")) args))
                 (the-value (gensym "value")))
             `(let* (,@(mapcar (lambda (tmp arg) `(,tmp ,arg)) vals args))
                (let ((,the-value (,fn ,@vals)))
                  (when (every (lambda (seq)
                                 (and (typep seq 'sequence)
                                      (plusp (length seq))))
                               (list ,@(mapcar (lambda (v) v)
                                                 (nthcdr (1+ fn-pos) vals))))
                    (push (cons ,(cond ((and (consp fn-arg)
                                             (eq (car fn-arg) 'function))
                                        `(fdefinition ',(cadr fn-arg)))
                                       ((and (consp fn-arg)  
                                             (eq (car fn-arg) 'quote))
                                        `(fdefinition ',(cadr fn-arg)))
                                       ;; Skip lambda case - no dependencies to track
                                       (t nil))
                                (mapcar (lambda (seq)
                                          (elt seq 0))
                                        (list ,@(mapcar (lambda (v) v)
                                                          (nthcdr (1+ fn-pos) vals)))))
                          ,result-sym))
                  ,the-value))))

          ;; Sequence/List functions with keyword function args
          ((assoc fn +sequence-functions+)
           (let* ((seq-pos (cdr (assoc fn +sequence-functions+)))
                  (vals (mapcar (lambda (_) (declare (ignore _)) 
                                  (gensym "arg"))
                                args))
                  (the-value (gensym "value")))
             `(let* (,@(mapcar (lambda (tmp arg) 
                                 `(,tmp ,arg))
                               vals args))
                (let ((,the-value (,fn ,@vals)))
                  (when-let ((seq ,(nth seq-pos vals)))
                    (when (and (typep seq 'sequence)
                               (plusp (length seq)))
                      (let ((elem `',(elt seq 0)))
                        (let ((required-args ,(1+ seq-pos)))
                          (do* ((orig-args (nthcdr required-args (list ,@args)))
                                (remaining-args orig-args (cddr remaining-args))
                                (keyword (car remaining-args) (car remaining-args))
                                (orig-val (cadr remaining-args) (cadr remaining-args)))
                               ((null remaining-args))
                            (when (and (keywordp keyword)
                                       (member keyword '(:key :test :test-not)))
                              (push (cons orig-val
                                          (if (eq keyword :key)
                                            (list elem)
                                            (list elem elem)))
                                    ,result-sym)))))))
                  ,the-value))))

          ;; Normal function ref  
          (t
           (let ((vals (mapcar (lambda (_)
                                (declare (ignore _))
                                (gensym "arg"))
                              args))
                 (the-value (gensym "value")))
             `(let* (,@(mapcar (lambda (tmp arg)
                                `(,tmp ,arg))
                              vals args))
                (let ((,the-value (,fn ,@vals)))
                  (push (cons (fdefinition ',fn)
                            (mapcar #'identity (list ,@vals)))
                        ,result-sym)
                  ,the-value)))))))))


(defun pushit-transform (form predicate result-sym)
  "Recursively walk input CONTEXT-FORM.  
  If (atom FORM) is recognized by PREDICATE, replace it with (PUSHIT 'atom RESULT).
  If any element of FORM is a method ref (recognized by PREDICATE),
  replace the entire list with (PUSHIT form RESULT).
  Otherwise preserve structure and recurse."
  (cond
    ((atom form)
     (when (funcall predicate form)
       (push form (slot-value *current-tracker* 'pushit-symbols)))
     form)
    ((and (consp form)
          (not (member (first form) '(defgeneric defmethod)))
          (or (funcall predicate (car form))  ; Function position
              (some (lambda (arg)             ; Any argument position
                     (or (funcall predicate arg)
                         (and (consp arg)     ; Handle #'fn forms
                              (member (car arg) '(quote function))
                              (funcall predicate (cadr arg)))))
                   (cdr form))))
     (if (assoc (car form) +comparison-functions+)
         `(pushit (,(car form) ,@(mapcar (lambda (x) `(eval ',x)) (cdr form))) ,result-sym)
         `(pushit ,form ,result-sym)))
    (t
     (cons (pushit-transform (car form) predicate result-sym)
           (mapcar (lambda (x)
                    (pushit-transform x predicate result-sym))
                  (cdr form))))))


(defun user-gf-designator-p (item)
  (and (function-designator-p item)
       (not (cl-symbol-p item))
       (if (functionp item)
           (typep item 'generic-function)
           (typep (fdefinition (if (listp item)
                                  (if (eq (first item) 'function)
                                      (second item)
                                      item)
                                  item))
                 'generic-function))))


(defun function-designator-p (item)
  (or (functionp item)
      (and (symbolp item)
           (fboundp item))
      (and (listp item)
           (case (first item)
             (lambda t)
             (function (function-designator-p (second item)))
             (setf (and (= (length item) 2)
                       (symbolp (second item))
                       (fboundp `(setf ,(second item)))))))))


(defun extract-method-refs (transformed-form refs-sym)
  "Extract actual method refs from a transformed form."
  (let ((wrapped-form
          `(let ((,refs-sym nil))
             ,transformed-form
             ,refs-sym)))
    (handler-case (eval wrapped-form)
      (error (c)
        (warn "Error evaluating method refs: ~A" c)
        '()))))


(defun record-method-reference (parser form gf args)
  "Record reference from method ref to its matching definition.
   PARSER - File parser instance for current file context
   FORM - Original source form containing the ref 
   GF - Generic function being called
   ARGS - List of argument values/mock instances to match"
  (let ((gf-name (c2mop:generic-function-name gf))
        (pkg (current-package parser)))

    ;; Record reference for each matching method
    (dolist (method (compute-applicable-methods gf args))
      (let* ((specs (mapcar #'class-name (c2mop:method-specializers method)))
             (quals (method-qualifiers method))
             ;; Build lookup key to find method definition
             (key (make-tracking-key gf-name
                                   (symbol-package gf-name)
                                   :method 
                                   quals 
                                   specs))
             (defs (gethash key (slot-value *current-tracker* 'definitions)))
             ;; Filter out definitions from same file before recording reference
             (other-file-defs (remove-if (lambda (def)
                                           (equal (definition.file def) 
                                                  (file parser)))
                                          defs)))
        (when other-file-defs  ; Only record if we have defs from other files
          (record-reference *current-tracker*
                            :name gf-name
                            :type :method
                            :file (file parser)
                            :context form
                            :package pkg
                            :qualifiers quals
                            :arguments args 
                            :definitions other-file-defs))))))
