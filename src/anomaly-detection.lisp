;;;; Filename:  anomaly-detection.lisp

;;;; Functions to detect anomalies in the user's project files.


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


(defun detect-shadowed-definitions (tracker)
  "Find definitions that shadow inherited symbols from other packages."
  (maphash (lambda (key defs)
             (declare (ignore key))
             (dolist (def defs)
               (let ((sym-name (definition.name def))
                     (pkg (definition.package def)))
                 (when (and pkg (symbolp sym-name))
                   (multiple-value-bind (inherited status)
                       (find-symbol (string sym-name) pkg)
                     (when (and inherited 
                               (eq status :inherited)
                               (not (eq (definition.type def) :package)))
                       (record-anomaly tracker
                         :type :shadowed-definition
                         :severity :warning
                         :file (definition.file def)
                         :package pkg
                         :description (format nil "Definition of ~A shadows inherited symbol from ~A"
                                           sym-name 
                                           (package-name (symbol-package inherited))))))))))
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
   which makes package renaming harder than using package variables."
  (maphash 
    (lambda (key refs)
      (declare (ignore key))
      (dolist (ref refs)
        (when (and (reference.name ref)  ; Check we have a name
                  (or (stringp (reference.name ref))
                      (keywordp (reference.name ref)))
                  ;; Ignore references in package definition contexts
                  (not (member (first (reference.context ref))
                             '(defpackage in-package make-package delete-package))))
          (record-anomaly tracker
            :type :inline-package-reference
            :severity :warning
            :file (reference.file ref)
            :package (reference.package ref)
            :description (format nil "Package name ~S referenced directly. Consider using a package variable instead"
                               (reference.name ref))
            :context (reference.context ref)))))
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
                    :severity :warning
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


