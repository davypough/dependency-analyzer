;;;; Filename: report-utils.lisp

;;; Utility functions for report generation and formatting.
;;; Contains helper functions for path handling, tree building,
;;; and other report-related operations.


(in-package #:dep)


(defun source-file-name (pathname)
  "Extract just the source file name from a pathname."
  (if pathname
      (file-namestring pathname)
      nil))


(defun build-file-dependency-tree (tracker)
  "Build a tree structure representing file dependencies."
  (let ((nodes (make-hash-table :test 'equal))
        (roots nil)
        (is-dependency (make-hash-table :test 'equal)))  ; Track files that are dependencies
    
    ;; First create nodes for all files
    (maphash (lambda (file definitions)
               (declare (ignore definitions))
               (setf (gethash file nodes)
                     (list :name (source-file-name file)
                           :full-name file
                           :children nil)))
             (slot-value tracker 'file-map))
    
    ;; Then establish dependencies and track which files are dependencies
    (maphash (lambda (file node)
               ;; Get files this one depends on
               (dolist (dep (file-dependencies tracker file))
                 (when-let ((dep-node (gethash dep nodes)))
                   (pushnew dep-node (getf node :children)
                           :test #'equal
                           :key (lambda (n) (getf n :full-name)))
                   ;; Mark this dep as being a dependency
                   (setf (gethash dep is-dependency) t))))
             nodes)
    
    ;; Find root nodes (files that aren't dependencies of any other file)
    (maphash (lambda (file node)
               (unless (gethash file is-dependency)
                 (push node roots)))
             nodes)
    
    (sort roots #'string< :key (lambda (node) (getf node :name)))))


(defun build-package-dependency-tree (tracker)
  "Build a tree structure representing package dependencies.
   Includes project packages, their runtime dependencies, and system dependencies.
   Returns (values roots cycles)."
  (let ((nodes (make-hash-table :test 'equal))
        (roots nil)
        (cycles nil))
    
    ;; First add CL package as it's always a root
    (setf (gethash "COMMON-LISP" nodes)
          (list :name "COMMON-LISP"
                :children nil
                :parents nil))
    
    ;; Add nodes for packages defined in our source
    (maphash (lambda (pkg-name def-form)
               (declare (ignore def-form))
               (when-let* ((pkg (find-package pkg-name))
                          (primary-name (package-name pkg)))
                 (setf (gethash primary-name nodes)
                       (list :name primary-name
                            :children nil
                            :parents nil))))
             (slot-value tracker 'defined-packages))
    
    ;; Add nodes for all runtime dependencies
    (maphash (lambda (pkg-name _)
               (declare (ignore _))
               (when-let ((pkg (find-package pkg-name)))
                 ;; Get full package dependency closure
                 (dolist (dep-pkg (package-use-list pkg))
                   (let ((dep-name (package-name dep-pkg)))
                     (unless (gethash dep-name nodes)
                       (setf (gethash dep-name nodes)
                             (list :name dep-name
                                  :children nil
                                  :parents nil)))))))
             (slot-value tracker 'defined-packages))
    
    ;; Establish parent-child relationships
    (maphash (lambda (_ node)
               (declare (ignore _))
               (let* ((pkg-name (getf node :name))
                      (pkg (find-package pkg-name)))
                 (when pkg  
                   (dolist (used-pkg (package-use-list pkg))
                     (let* ((used-name (package-name used-pkg))
                            (used-node (gethash used-name nodes)))
                       (when used-node
                         ;; Check for cycles
                         (when (member pkg (package-use-list used-pkg))
                           (pushnew (format nil "~A ↔ ~A" pkg-name used-name)
                                   cycles :test #'string=))
                         ;; Record dependency
                         (unless (member pkg (package-use-list used-pkg))
                           (pushnew used-node (getf node :parents)
                                   :test #'equal
                                   :key (lambda (n) (getf n :name))))))))))
             nodes)
    
    ;; Find root packages - only COMMON-LISP and packages with no other parents
    (maphash (lambda (_ node)
               (declare (ignore _))
               (let ((parents (getf node :parents))
                     (name (getf node :name)))
                 ;; A package is a root if it's COMMON-LISP or has no parents
                 (when (or (string= name "COMMON-LISP")
                          (null parents))
                   (push node roots))))
             nodes)
    
    ;; Calculate children relationships
    (maphash (lambda (_ node)
               (declare (ignore _))
               (dolist (parent (getf node :parents))
                 (pushnew node (getf parent :children)
                         :test #'equal
                         :key (lambda (n) (getf n :name)))))
             nodes)
    
    (values (sort roots #'string< :key (lambda (node) (getf node :name)))
            (sort cycles #'string<))))


(defun build-type-dependency-tree (tracker)
  "Build a tree structure representing type inheritance relationships.
   Includes CLOS classes, structures, conditions and types.
   Returns (values roots cycles)."
  (let ((nodes (make-hash-table :test 'equal))
        (roots nil)
        (cycles nil))
    
    ;; First create nodes for all type definitions
    (maphash (lambda (key def-list)
               (declare (ignore key))
               (dolist (def def-list)
                 (when (eq (definition.type def) :structure/class/condition)
                   (let* ((type-name (definition.name def))
                          (class (find-class type-name nil)))
                     (when (and class (typep class 'standard-class))
                       (setf (gethash (class-name class) nodes)
                             (list :name (class-name class)
                                   :children nil
                                   :parents nil)))))))
             (slot-value tracker 'definitions))
    
    ;; Establish parent-child relationships 
    (maphash (lambda (_ node)
               (declare (ignore _))
               (let* ((type-name (getf node :name))
                      (class (find-class type-name nil)))
                 (when (and class (typep class 'standard-class))
                   (dolist (super (c2mop:class-direct-superclasses class))
                     (let* ((super-name (class-name super))
                            (super-node (gethash super-name nodes)))
                       (when (and super-node
                                 (not (eq super-name 'standard-object)))
                         ;; Check for cycles
                         (when (member class (c2mop:class-direct-subclasses super))
                           (pushnew (format nil "~A ↔ ~A" type-name super-name)
                                   cycles :test #'string=))
                         ;; Record inheritance
                         (pushnew super-node (getf node :parents)
                                 :test #'equal
                                 :key (lambda (n) (getf n :name)))))))))
             nodes)
    
    ;; Find root types - those with no parents or only standard-object parent
    (maphash (lambda (_ node)
               (declare (ignore _))
               (let ((parents (getf node :parents)))
                 (when (null parents)
                   (push node roots))))
             nodes)
    
    ;; Calculate children relationships
    (maphash (lambda (_ node)
               (declare (ignore _))
               (dolist (parent (getf node :parents))
                 (pushnew node (getf parent :children)
                         :test #'equal
                         :key (lambda (n) (getf n :name)))))
             nodes)
    
    (values (sort roots #'string< :key (lambda (node) (getf node :name)))
            (sort cycles #'string<))))


(defun print-ascii-tree (stream roots &optional (prefix "") (last-child-p nil))
  "Print an ASCII representation of a dependency tree."
  (declare (ignore last-child-p))
  (dolist (node (butlast roots))
    (format stream "~&~A-- ~A~%" prefix (getf node :name))
    (print-ascii-tree stream 
                      (sort (getf node :children) #'string< 
                            :key (lambda (n) (getf n :name)))
                      (concatenate 'string prefix "   ")
                      nil))
  (when-let ((last-node (car (last roots))))
    (format stream "~&~A-- ~A~%" prefix (getf last-node :name))
    (print-ascii-tree stream 
                      (sort (getf last-node :children) #'string< 
                            :key (lambda (n) (getf n :name)))
                      (concatenate 'string prefix "   ")
                      t)))


(defmethod get-package-uses (&optional (tracker nil tracker-provided-p) package-name)
  "Get all packages that a given package uses."
  (let ((actual-tracker (if tracker-provided-p tracker (ensure-tracker))))
    (gethash package-name (slot-value actual-tracker 'package-uses))))


(defmethod get-package-exports (&optional (tracker nil tracker-provided-p) pkg)
  "Get all symbols exported by a package.
   PKG: package object or designator"
  (let* ((actual-tracker (if tracker-provided-p tracker (ensure-tracker)))
         (pkg-obj (if (packagep pkg) pkg (find-package pkg))))
    (when pkg-obj  ; Verify valid package
      (mapcar #'(lambda (sym)
                  (intern (symbol-name sym) pkg-obj))
              (gethash (package-name pkg-obj)  ; Keep string key for exports table
                      (slot-value actual-tracker 'package-exports))))))


(defun file-dependencies (&optional (tracker nil tracker-provided-p) file)
  "Get all files that this file depends on."
  (let ((actual-tracker (if tracker-provided-p tracker (ensure-tracker)))
        (deps nil)
        (refs-seen (make-hash-table :test 'equal)))
  
    ;; First collect all references in this file
    (maphash (lambda (key refs)
               (declare (ignore key))
               (dolist (ref refs)
                 (when (equal (reference.file ref) file)
                   ;; Track each referenced name with its package
                   (let* ((sym (reference.name ref))
                          (pkg (reference.package ref))
                          (type (reference.type ref))
                          (key (make-tracking-key sym pkg type)))
                 
                     ;; Look up matching definition
                     (setf (gethash key refs-seen) t)))))
             (slot-value actual-tracker 'references))
    
    ;; Then look up the definitions for each referenced name
    (maphash (lambda (key _)
               (declare (ignore _))
               (let ((defs (gethash key (slot-value actual-tracker 'definitions))))
                 (dolist (def defs)
                   (let ((def-file (definition.file def)))
                     (unless (equal def-file file)
                       (pushnew def-file deps :test #'equal))))))
             refs-seen)
    
    ;; Return sorted list of dependencies
    (let ((sorted-deps (sort deps #'string< 
                            :key (lambda (path)
                                  (if path 
                                      (namestring path)
                                      "")))))
      sorted-deps)))


(defmethod get-system-cycles (&optional (tracker nil tracker-provided-p))
  "Get all recorded system dependency cycles from the tracker."
  (let ((actual-tracker (if tracker-provided-p tracker (ensure-tracker)))
        (cycles nil))
    (maphash (lambda (type anomaly-list)
               (when (eq type :system-cycle)
                 (dolist (a anomaly-list)
                   (push (anomaly.context a) cycles))))
             (slot-value actual-tracker 'anomalies))
    (sort cycles #'string< :key (lambda (cycle)
                                 (format nil "~{~A~^->~}" cycle)))))


(defun build-class-dependency-tree (tracker)
  "Build a tree structure representing CLOS class inheritance relationships.
   Only includes classes of type standard-class, filtering out structures and conditions.
   No longer performs cycle detection as that's handled by analyze-class-hierarchies."
  (let ((nodes (make-hash-table :test 'equal))
        (roots nil))
    
    ;; First create nodes only for CLOS classes
    (maphash (lambda (key def-list)
               (declare (ignore key))
               (dolist (def def-list)
                 (when (eq (definition.type def) :structure/class/condition)
                   (let* ((type-name (definition.name def))
                          (class (find-class type-name nil)))
                     ;; Only process if it's a standard-class
                     (when (and class (typep class 'standard-class))
                       (setf (gethash (class-name class) nodes)
                             (list :name (class-name class)
                                   :children nil
                                   :parents nil)))))))
             (slot-value tracker 'definitions))
    
    ;; Establish parent-child relationships only between CLOS classes 
    (maphash (lambda (_ node)
               (declare (ignore _))
               (let* ((type-name (getf node :name))
                      (class (find-class type-name nil)))
                 (when (and class (typep class 'standard-class))
                   (dolist (super (c2mop:class-direct-superclasses class))
                     (let* ((super-name (class-name super))
                            (super-node (gethash super-name nodes)))
                       ;; Only establish relationship if superclass is in our nodes
                       (when (and super-node 
                                 (not (eq super-name 'standard-object)))
                         ;; Record inheritance without checking for cycles
                         (pushnew super-node (getf node :parents)
                                 :test #'equal
                                 :key (lambda (n) (getf n :name)))))))))
             nodes)
    
    ;; Find root classes - those with no parents in our set
    (maphash (lambda (_ node)
               (declare (ignore _))
               (let ((parents (getf node :parents)))
                 (when (null parents)
                   (push node roots))))
             nodes)
    
    ;; Calculate children relationships
    (maphash (lambda (_ node)
               (declare (ignore _))
               (dolist (parent (getf node :parents))
                 (pushnew node (getf parent :children)
                         :test #'equal
                         :key (lambda (n) (getf n :name)))))
             nodes)
    
    ;; Return just the roots - cycle detection now handled elsewhere
    (values (sort roots #'string< :key (lambda (node) (getf node :name))) nil)))


(defun build-structure-dependency-tree (tracker)
  "Build a tree structure representing structure inheritance relationships.
   Only includes types of structure-class, filtering out CLOS classes and conditions.
   Returns (values roots cycles) though cycles should never occur due to 
   structure's single inheritance model."
  (let ((nodes (make-hash-table :test 'equal))
        (roots nil))
    
    ;; First create nodes only for structure types
    (maphash (lambda (key def-list)
               (declare (ignore key))
               (dolist (def def-list)
                 (when (eq (definition.type def) :structure/class/condition)
                   (let* ((type-name (definition.name def))
                          (class (find-class type-name nil)))
                     ;; Only process if it's a structure-class
                     (when (and class (typep class 'structure-class))
                       (setf (gethash (class-name class) nodes)
                             (list :name (class-name class)
                                   :children nil
                                   :parents nil)))))))
             (slot-value tracker 'definitions))
    
    ;; Establish parent-child relationships
    (maphash (lambda (_ node)
               (declare (ignore _))
               (let* ((type-name (getf node :name))
                      (class (find-class type-name nil)))
                 (when (and class (typep class 'structure-class))
                   ;; Structures have exactly one direct superclass
                   (let* ((super (first (c2mop:class-direct-superclasses class)))
                          (super-name (when super (class-name super)))
                          (super-node (when super-name 
                                      (gethash super-name nodes))))
                     ;; Only establish relationship if superclass is in our nodes
                     ;; (ignoring structure-object which is the root of all structures)
                     (when (and super-node 
                               (not (eq super-name 'structure-object)))
                       ;; Record inheritance - no need to check for cycles
                       ;; since structures use single inheritance
                       (pushnew super-node (getf node :parents)
                               :test #'equal
                               :key (lambda (n) (getf n :name))))))))
             nodes)
    
    ;; Find root structures - those with no parents except structure-object
    (maphash (lambda (_ node)
               (declare (ignore _))
               (let ((parents (getf node :parents)))
                 (when (null parents)
                   (push node roots))))
             nodes)
    
    ;; Calculate children relationships
    (maphash (lambda (_ node)
               (declare (ignore _))
               (dolist (parent (getf node :parents))
                 (pushnew node (getf parent :children)
                         :test #'equal
                         :key (lambda (n) (getf n :name)))))
             nodes)
    
    ;; Structures can't have cycles, so return nil for cycles
    (values (sort roots #'string< :key (lambda (node) (getf node :name)))
            nil)))


(defun build-condition-dependency-tree (tracker)
  "Build a tree structure representing condition inheritance relationships.
   Only includes conditions (types that inherit from condition), filtering out
   other CLOS classes and structures. Shows how user-defined conditions relate
   to each other through inheritance.
   Returns (values roots cycles)."
  (let ((nodes (make-hash-table :test 'equal))
        (roots nil)
        (cycles nil))
    
    ;; First create nodes only for condition types
    (maphash (lambda (key def-list)
               (declare (ignore key))
               (dolist (def def-list)
                 (when (eq (definition.type def) :structure/class/condition)
                   (let* ((type-name (definition.name def))
                          (class (find-class type-name nil)))
                     ;; Process if it's a condition - check for inheritance from condition class
                     (when (and class 
                               (not (eq (class-name class) 'condition))  ; Skip condition base class
                               (subtypep type-name 'condition))
                       (setf (gethash (class-name class) nodes)
                             (list :name (class-name class)
                                   :children nil
                                   :parents nil)))))))
             (slot-value tracker 'definitions))
    
    ;; Establish parent-child relationships between conditions
    (maphash (lambda (_ node)
               (declare (ignore _))
               (let* ((type-name (getf node :name))
                      (class (find-class type-name nil)))
                 (when class  ; Should always be true here
                   (dolist (super (c2mop:class-direct-superclasses class))
                     (let* ((super-name (class-name super))
                            (super-node (gethash super-name nodes)))
                       ;; Only establish relationship if superclass is one of our condition nodes
                       ;; This filters out non-condition parents
                       (when super-node
                         ;; Check for cycles (conditions can have multiple inheritance)
                         (when (member class (c2mop:class-direct-subclasses super))
                           (pushnew (format nil "~A ↔ ~A" type-name super-name)
                                   cycles :test #'string=))
                         ;; Record inheritance
                         (pushnew super-node (getf node :parents)
                                 :test #'equal
                                 :key (lambda (n) (getf n :name)))))))))
             nodes)
    
    ;; Find root conditions - those with no parents in our nodes
    ;; (they'll connect to built-in condition hierarchies)
    (maphash (lambda (_ node)
               (declare (ignore _))
               (let ((parents (getf node :parents)))
                 (when (null parents)
                   (push node roots))))
             nodes)
    
    ;; Calculate children relationships
    (maphash (lambda (_ node)
               (declare (ignore _))
               (dolist (parent (getf node :parents))
                 (pushnew node (getf parent :children)
                         :test #'equal
                         :key (lambda (n) (getf n :name)))))
             nodes)
    
    (values (sort roots #'string< :key (lambda (node) (getf node :name)))
            (sort cycles #'string<))))


(defun build-deftype-dependency-tree (tracker)
  "Build a tree structure representing relationships between types defined with deftype.
   Unlike class hierarchies which show inheritance, this shows how user-defined types
   are constructed from other types through type specifiers. For example:
   - (deftype pos-int () '(integer 1 *)) creates a refinement of integer
   - (deftype str-or-sym () '(or string symbol)) creates a union type
   Returns (values roots cycles), though cycles indicates type interdependencies
   rather than inheritance cycles."
  (let ((nodes (make-hash-table :test 'equal))
        (roots nil)
        (cycles nil))
    
    ;; First pass: Create nodes for all deftype definitions
    (maphash (lambda (key def-list)
               (declare (ignore key))
               (dolist (def def-list)
                 (when (eq (definition.type def) :deftype)
                   (let ((type-name (definition.name def)))
                     ;; Create node even if we can't determine relationships
                     (setf (gethash type-name nodes)
                           (list :name type-name
                                 :children nil
                                 :parents nil))))))
             (slot-value tracker 'definitions))
    
    ;; Second pass: Try to establish type relationships
    (maphash (lambda (key def-list)
               (declare (ignore key))
               (dolist (def def-list)
                 (when (eq (definition.type def) :deftype)
                   (let* ((type-name (definition.name def))
                          (node (gethash type-name nodes))
                          (context (definition.context def))
                          (type-expr (third context))) ; Get the type expression
                     ;; Extract base type from type specifier
                     (when (and (listp type-expr)
                               (eq (car type-expr) 'quote))
                       (let ((spec (cadr type-expr)))
                         ;; Handle different type specifier forms
                         (typecase spec
                           (cons
                            (case (car spec)
                              ;; Union types - all component types are "parents"
                              ((or)
                               (dolist (parent-type (cdr spec))
                                 (when-let ((parent-node (gethash parent-type nodes)))
                                   (pushnew parent-node (getf node :parents)
                                           :test #'equal
                                           :key (lambda (n) (getf n :name))))))
                              ;; Refinement types - base type is parent
                              ((integer rational float real number)
                               (when-let ((parent-node (gethash (car spec) nodes)))
                                 (pushnew parent-node (getf node :parents)
                                         :test #'equal
                                         :key (lambda (n) (getf n :name))))))))))))))
             (slot-value tracker 'definitions))
    
    ;; Find root types - those with no established relationships
    (maphash (lambda (_ node)
               (declare (ignore _))
               (let ((parents (getf node :parents)))
                 (when (null parents)
                   (push node roots))))
             nodes)
    
    ;; Build children relationships and detect interdependencies
    (maphash (lambda (_ node)
               (declare (ignore _))
               (let ((type-name (getf node :name)))
                 (dolist (parent (getf node :parents))
                   ;; Record child relationship
                   (pushnew node (getf parent :children)
                           :test #'equal
                           :key (lambda (n) (getf n :name)))
                   ;; Check for type interdependencies
                   (let ((parent-name (getf parent :name)))
                     (when (member parent-name (getf node :children)
                                 :test #'equal
                                 :key (lambda (n) (getf n :name)))
                       (pushnew (format nil "~A ↔ ~A" type-name parent-name)
                               cycles :test #'string=))))))
             nodes)
    
    (values (sort roots #'string< :key (lambda (node) (getf node :name)))
            (sort cycles #'string<))))