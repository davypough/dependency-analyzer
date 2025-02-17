;;;; Filename:  code-quality-analysis.lisp

;;;; Functions to detect quality issues in the user's project files.


(in-package :dep)


#+ignore (defun detect-unused-definitions (tracker)  ;redo later, too many ways to reference a definition
  "Find user-created definitions that are never referenced."
  (let ((used-defs (make-hash-table :test 'equal)))
    ;; Mark all referenced definitions  
    (maphash (lambda (key refs)
               (declare (ignore key))
               (dolist (ref refs)
                 (dolist (def (reference.definitions ref))
                   (setf (gethash def used-defs) t))))
             (slot-value tracker 'references))
    
    ;; Find unused definitions
    (maphash (lambda (key defs)
               (declare (ignore key))
               (dolist (def defs)
                 (when (and (not (gethash def used-defs))
                           (not (eq (definition.type def) :package))
                           (user-defined-p def))
                   (record-anomaly tracker
                    :type :unused-definition
                    :severity :warning 
                    :file (definition.file def)
                    :package (definition.package def)
                    :description (format nil "~A ~A is never referenced"
                                      (definition.type def)
                                      (definition.name def))))))
             (slot-value tracker 'definitions))))


#+ignore (defun user-defined-p (def)  ;keep to use later for detecting unused definitions
  "Check if definition appears to be explicitly defined by user rather than auto-generated."
  (let ((name (string (definition.name def))))
    (case (definition.type def)
      (:function 
       ;; Filter out structure accessors/predicates/copiers
       (not (or (search "-P" name :from-end t) ; predicates
                (search "COPY-" name))))          ; copiers
      (t t))))  ; Keep all other types of definitions


(defun detect-redundant-package-uses (tracker)
  "Find packages that are used but none of their symbols are referenced."
  (maphash (lambda (using-pkg used-pkgs)
             (dolist (used used-pkgs)
               (unless (eq used "CL") ; Skip common-lisp package
                 (let ((used-symbols nil))
                   ;; Check all references from using-pkg
                   (maphash (lambda (key refs)
                             (declare (ignore key))
                             (dolist (ref refs)
                               (when (and (eq (reference.package ref) 
                                            (find-package using-pkg))
                                        (equal (package-name 
                                               (symbol-package (reference.name ref)))
                                              used))
                                 (setf used-symbols t))))
                           (slot-value tracker 'references))
                   (unless used-symbols
                     (record-anomaly tracker
                       :type :redundant-package-use  
                       :severity :warning
                       :file using-pkg
                       :package using-pkg
                       :description (format nil "Package ~A uses but never references symbols from ~A"
                                         using-pkg used)))))))
           (slot-value tracker 'package-uses)))


(defun detect-multiple-package-definitions (tracker)
  "Find packages defined in multiple places (defpackage/make-package).
   Records :info anomaly suggesting consolidation when appropriate.
   Detects multiple definitions both within and across files."
  (let ((pkg-defs (make-hash-table :test 'equal)))
    ;; Group package definitions by normalized name 
    (maphash (lambda (key def-list)
               (declare (ignore key))
               (dolist (def def-list)
                 (when (and (eq (definition.type def) :package)
                          (not (gensym-form-p (definition.name def))))
                   (let ((pkg-name (package-designator-to-string (definition.name def))))
                     (push def (gethash pkg-name pkg-defs))))))
             (slot-value tracker 'definitions))
    
    ;; Record anomaly for packages with multiple definitions
    (maphash (lambda (pkg-name defs)
               (when (> (length defs) 1)
                 (let ((def-files (mapcar #'definition.file defs))
                       (def-forms (mapcar #'definition.context defs)))
                   (record-anomaly tracker
                     :type :multiple-package-definitions
                     :severity :info
                     :file def-files  
                     :package pkg-name
                     :context def-forms
                     :description 
                     (format nil "Package ~S has ~D definitions (~{~A~^, ~}). Consider consolidating."
                             pkg-name
                             (length defs)
                             (mapcar #'project-pathname def-files))))))
             pkg-defs)))


(defun detect-suboptimal-package-placement (tracker)
  "Analyze symbol usage patterns to detect definitions that might be
   better placed in different packages based on their usage patterns.
   Detects:
   1. Symbols exclusively used in non-home packages
   2. Symbols consistently referenced with package qualification"
  (let ((usage-table (make-hash-table :test 'equal))
        (project-packages (slot-value tracker 'project-owned-packages)))
    
    ;; Build usage patterns from definitions
    (maphash 
     (lambda (key def-list)
       (declare (ignore key))
       (dolist (def def-list)
         (let* ((sym (definition.name def))
                (pkg (definition.package def)))
           ;; Only analyze project package symbols
           (when (and pkg 
                     (member pkg project-packages)
                     (not (eq pkg (find-package :common-lisp))))
             (let ((usage-key (format nil "~A:~A" 
                                    (package-name pkg)
                                    (if (symbolp sym)
                                        (symbol-name sym)
                                        sym))))
               (setf (gethash usage-key usage-table)
                     (cons def (make-hash-table :test 'equal))))))))
     (slot-value tracker 'definitions))
    
    ;; Analyze reference patterns
    (maphash 
     (lambda (key refs)
       (declare (ignore key))
       (dolist (ref refs)
         (let* ((sym (reference.name ref))
                (sym-pkg (when (symbolp sym)
                           (symbol-package sym)))
                (usage-key (format nil "~A:~A" 
                                   (if sym-pkg
                                     (package-name sym-pkg)
                                     "NIL")          ; Safe fallback for uninterned symbols
                                   (if (symbolp sym)
                                     (symbol-name sym)
                                     sym))))
           (when-let ((usage (gethash usage-key usage-table)))
             (let ((refs-by-pkg (cdr usage)))
               (push ref (gethash (package-name (reference.package ref))
                                refs-by-pkg)))))))
     (slot-value tracker 'references))
    
    ;; Record anomalies for suspicious patterns
    (maphash 
     (lambda (key usage)
       (declare (ignore key))
       (let* ((def (car usage))
              (refs-by-pkg (cdr usage))
              (sym (definition.name def))
              (home-pkg (definition.package def)))
         
         ;; Skip certain definition types
         (unless (member (definition.type def)
                        '(:package :method :generic-function))
           
           ;; Check exclusive usage in non-home package
           (when (= (hash-table-count refs-by-pkg) 1)
             (let* ((using-pkg-name (car (alexandria:hash-table-keys refs-by-pkg)))
                    (ref-count (length (gethash using-pkg-name refs-by-pkg))))
               (when (and (not (string= using-pkg-name (package-name home-pkg)))
                         (> ref-count 2))
                 (record-anomaly tracker
                   :type :suboptimal-package-placement
                   :severity :info
                   :file (definition.file def)
                   :package home-pkg
                   :context (definition.context def)
                   :description 
                   (format nil "Symbol ~A:~A is only used in package ~A (~D references)"
                           (package-name home-pkg) sym using-pkg-name ref-count)))))
           
           ;; Check for consistent package qualification
           (let ((qualified-count 0)
                 (unqualified-count 0))
             (maphash 
              (lambda (pkg-name refs)
                (declare (ignore pkg-name))
                (dolist (ref refs)
                  (if (eq (car (reference.visibility ref)) :inherited)
                      (incf unqualified-count)
                      (incf qualified-count))))
              refs-by-pkg)
             
             (when (and (> (+ qualified-count unqualified-count) 3)
                       (zerop unqualified-count)
                       (> (hash-table-count refs-by-pkg) 1))
               (record-anomaly tracker
                 :type :consistent-package-qualification
                 :severity :info
                 :file (definition.file def)
                 :package home-pkg
                 :context (definition.context def)
                 :description
                 (format nil "Symbol ~A:~A is always referenced with package qualifier (~D refs)"
                         (package-name home-pkg) sym qualified-count)))))))
     usage-table)))


(defun detect-shadowed-definitions (tracker)
  "Find definitions that shadow inherited symbols from other packages.
   Excludes valid CLOS method definitions that extend inherited generic functions."
  (maphash 
    (lambda (key defs)
      (declare (ignore key))
      (dolist (def defs)
        (let ((sym-name (definition.name def))
              (pkg (definition.package def)))
          (when (and pkg (symbolp sym-name))
            (multiple-value-bind (found-sym status)
                (find-symbol (string sym-name) pkg)
              (when (and found-sym 
                        (eq status :inherited)
                        (not (eq (definition.type def) :package))
                        (not (and (eq (definition.type def) :method)
                                 (fboundp sym-name)
                                 (typep (fdefinition sym-name) 
                                       'generic-function))))
                (record-anomaly tracker
                  :type :shadowed-definition
                  :severity :warning
                  :file (definition.file def)
                  :package pkg
                  :description (format nil 
                                "Definition of ~A shadows inherited symbol from ~A"
                                sym-name 
                                (package-name (symbol-package found-sym))))))))))
    (slot-value tracker 'definitions)))


(defun detect-qualified-internal-references (tracker)
  "Find package-qualified references to internal symbols."
  (maphash (lambda (key refs)
             (declare (ignore key))
             (dolist (ref refs)
               (let ((sym (reference.name ref))
                     (pkg (reference.package ref)))
                 (when (and (symbolp sym) 
                          pkg
                          (not (eq pkg (symbol-package sym))) ; Different package = qualified
                          (eq (nth-value 1 (find-symbol (string sym) 
                                                      (symbol-package sym)))
                              :internal))
                   (record-anomaly tracker
                     :type :qualified-internal-reference
                     :severity :warning
                     :file (reference.file ref)
                     :package pkg
                     :description (format nil "Package-qualified reference to internal symbol ~A:~A"
                                       (package-name (symbol-package sym))
                                       sym))))))
           (slot-value tracker 'references)))


(defun detect-circular-type-dependencies (tracker)
  "Find type definitions that form circular dependencies through slots/superclasses."
  (let ((visited (make-hash-table :test 'equal))
        (path nil))
    (labels ((get-type-deps (class-name)
               ;; Get dependencies from slots and superclasses
               (when-let (class (find-class class-name nil))
                 (union 
                   ;; Direct slot types
                   (loop for slot in (c2mop:class-direct-slots class)
                         for slot-type = (c2mop:slot-definition-type slot)
                         when (and slot-type (find-class slot-type nil))
                         collect slot-type)
                   ;; Direct superclasses 
                   (c2mop:class-direct-superclasses class))))
             
             (detect-cycle (class)
               (let ((status (gethash class visited)))
                 (cond (status               ; Already visited
                        (when (eq status :in-progress)
                          ;; Found cycle - record the path
                          (let* ((cycle-start (position class path))
                                 (cycle (subseq path cycle-start)))
                            (record-anomaly tracker
                              :type :circular-type-dependency
                              :severity :warning
                              :file (car cycle)
                              :description (format nil "Type dependency cycle detected: ~{~A~^ -> ~}" cycle)
                              :context cycle)))
                        nil)
                       (t                    ; New node
                        (setf (gethash class visited) :in-progress)
                        (push class path)
                        (dolist (dep (get-type-deps class))
                          (detect-cycle dep))
                        (pop path)
                        (setf (gethash class visited) t)
                        nil)))))
      
      ;; Check each defined type
      (maphash (lambda (key defs)
                 (declare (ignore key))
                 (dolist (def defs)
                   (when (eq (definition.type def) :structure/class/condition)
                     (detect-cycle (definition.name def)))))
               (slot-value tracker 'definitions)))))


(defun detect-inline-package-references (tracker)
  "Find direct references to package names in code.
   This detects strings and keywords used as package designators,
   which makes package renaming harder than using package variables.
   Excludes references in own package definition contexts."
  (maphash 
    (lambda (key refs)
      (declare (ignore key))
      (dolist (ref refs)
        (when (and (reference.name ref)  ; Check we have a name
                  (or (stringp (reference.name ref))
                      (keywordp (reference.name ref))))
          ;; Only flag if reference isn't in its own package definition
          (let ((context (reference.context ref)))
            (unless (and (consp context)
                        (member (first context) '(defpackage in-package make-package delete-package))
                        ;; For package definition forms, check if defining same package
                        (or (not (eq (first context) 'defpackage))
                            (equal (string (second context))
                                   (string (reference.name ref)))))
              (record-anomaly tracker
                :type :inline-package-reference
                :severity :info
                :file (reference.file ref)
                :package (reference.package ref)
                :description (format nil "Package name ~S referenced directly. Consider using a package variable instead"
                                   (reference.name ref))
                :context context))))))
    (slot-value tracker 'references)))


(defun detect-indirect-slot-access (tracker)
  "Find slot-value calls that could use accessors instead."
  (maphash 
    (lambda (key refs)
      (declare (ignore key))
      (dolist (ref refs)
        ;; Look for slot-value in function call contexts
        (when (and (eq (reference.name ref) 'slot-value)
                  (consp (reference.context ref)))
          (let* ((call (reference.context ref))
                 (slot-name (third call))
                 (object (second call)))
            (when (and slot-name 
                      (symbolp slot-name)
                      object)
              ;; Try to determine object's class
              (let ((class-name (cond
                                  ((symbolp object) 
                                   (ignore-errors 
                                     (type-of (symbol-value object))))
                                  ((and (consp object)
                                        (eq (car object) 'make-instance))
                                        (second object))
                                  (t nil))))
                ;; Check if accessor exists for this slot
                (when (and class-name
                         (find-class class-name nil)
                         (c2mop:slot-definition-readers 
                           (find slot-name
                                (c2mop:class-slots (find-class class-name))
                                :key #'c2mop:slot-definition-name)))
                  (record-anomaly tracker
                    :type :indirect-slot-access
                    :severity :info
                    :file (reference.file ref)
                    :package (reference.package ref)
                    :description 
                      (format nil 
                             "Indirect slot access (slot-value ~A '~A). Consider using accessor instead"
                             object slot-name)
                    :context (reference.context ref)))))))))
    (slot-value tracker 'references)))


(defun record-anomaly (tracker &key type severity file description package context)
  "Record a new anomaly in the dependency tracker.
   For :duplicate-definition type, files must be provided as list of all definition locations."
  (let ((anomaly (make-instance 'anomaly 
                               :type type 
                               :severity severity 
                               :file file
                               :description description
                               :package package
                               :context context)))
    (pushnew anomaly (gethash type (slot-value tracker 'anomalies))
                     :test #'equal :key #'anomaly.file)
    anomaly))


