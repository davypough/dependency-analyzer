;;;; Filename:  code-quality-analysis.lisp

;;;; Functions to detect quality issues in the user's project files.


(in-package :dep)


(defun analyze-package-dependencies (tracker)
  "Analyze package relationships and metrics for the entire project ecosystem.
   This includes packages we define, system packages we use, and external
   dependencies. All relationships are verified using runtime information."
  
  ;; Data structures for our analysis
  (let ((package-metrics (make-hash-table :test 'eq))
        (package-graph (make-hash-table :test 'equal))
        (package-details (make-hash-table :test 'equal))
        (all-project-packages (make-hash-table :test 'eq)))
    
    ;; First identify all packages involved in our project
    (maphash (lambda (pkg-name def-form)
               (declare (ignore def-form))
               ;; Add each package we define
               (when-let ((pkg (find-package pkg-name)))
                 (setf (gethash pkg all-project-packages) t)
                 ;; Also add all packages it uses
                 (dolist (used (package-use-list pkg))
                   (setf (gethash used all-project-packages) t))))
             (slot-value tracker 'defined-packages))
    
    ;; Now analyze metrics for each package in our ecosystem
    (maphash (lambda (pkg _)
               (declare (ignore _))
               ;; Calculate comprehensive metrics
               (let* ((used-packages (package-use-list pkg))
                      (local-symbols (count-if 
                                    (lambda (s) 
                                      (eq (symbol-package s) pkg))
                                    (list-all-symbols pkg)))
                      (inherited-symbols (count-if 
                                        (lambda (s)
                                          (eq (nth-value 1 
                                               (find-symbol (symbol-name s) pkg))
                                              :inherited))
                                        (list-all-symbols pkg)))
                      ;; Track exports and their usage
                      (exports (loop for s being the external-symbols of pkg
                                   collect s))
                      (export-users (count-if 
                                   (lambda (p) 
                                     (intersection exports 
                                                 (package-use-list p)))
                                   (list-all-packages))))
                 
                 ;; Record complete package metrics
                 (setf (gethash pkg package-metrics)
                       (list :local-symbols local-symbols
                             :inherited-symbols inherited-symbols
                             :used-packages (length used-packages)
                             :exported-symbols (length exports)
                             :export-users export-users))
                 
                 ;; Record dependency relationships
                 (setf (gethash (package-name pkg) package-graph)
                       (mapcar #'package-name used-packages))
                 
                 ;; Record detailed symbol usage patterns
                 (setf (gethash (package-name pkg) package-details)
                       (analyze-package-usage tracker pkg))))
             all-project-packages)
    
    ;; Analyze for dependency cycles between packages
    (let ((cycles (detect-dependency-cycles package-graph)))
      (when cycles
        (dolist (cycle cycles)
          (record-anomaly tracker
            :type :package-cycle
            :severity :warning
            :file (mapcar (lambda (pkg-name)
                           (find-package-definition-file 
                            tracker pkg-name))
                         cycle)
            :description 
            (format nil 
                    "Package dependency cycle detected: ~{~A~^ -> ~}" 
                    cycle)
            :context cycle))))
    
    ;; Update tracker with analysis results
    (setf (slot-value tracker 'package-metrics) package-metrics
          (slot-value tracker 'package-graph) package-graph
          (slot-value tracker 'package-details) package-details)))


(defun analyze-package-usage (tracker pkg)
  "Analyze detailed symbol usage patterns for a package.
   Returns alist of (symbol-name . usage-info) for reporting."
  (let ((usage-data nil))
    (do-symbols (sym pkg)
      (multiple-value-bind (symbol status) 
          (find-symbol (symbol-name sym) pkg)
        (when symbol ; Skip unbound symbols
          (push 
           (list (symbol-name symbol)
                 :status status
                 :references (count-symbol-references tracker symbol pkg)
                 :exports (when (eq status :external)
                           (collect-export-references tracker symbol)))
           usage-data))))
    usage-data))


(defun count-symbol-references (tracker symbol pkg)
  "Count references to a symbol from within a specific package.
   Returns total number of references from the package to that symbol.
   
   Parameters:
   tracker - The dependency tracker instance
   symbol - The symbol to count references for
   pkg - The package to check references from"
  (let ((ref-count 0))
    ;; Look through all references in the tracker
    (maphash (lambda (key refs)
               (declare (ignore key))
               ;; For each reference from the target package
               (dolist (ref refs)
                 (when (and (eq (reference.package ref) pkg)
                          (equal (reference.name ref) symbol))
                   (incf ref-count))))
             (slot-value tracker 'references))
    ref-count))


(defun collect-export-references (tracker symbol)
  "Collect information about where an exported symbol is referenced.
   Returns list of packages that reference this exported symbol.
   
   Parameters:
   tracker - The dependency tracker instance
   symbol - The exported symbol to analyze"
  (let ((referencing-packages nil))
    ;; Examine all references
    (maphash (lambda (key refs)
               (declare (ignore key))
               (dolist (ref refs)
                 ;; When reference matches our symbol
                 (when (equal (reference.name ref) symbol)
                   ;; Record the referencing package
                   (pushnew (package-name (reference.package ref))
                           referencing-packages 
                           :test #'string=))))
             (slot-value tracker 'references))
    ;; Return sorted list of referencing packages
    (sort referencing-packages #'string<)))


(defun find-package-definition-file (tracker pkg-name)
  "Find the file containing the defpackage form for a package."
  (let ((def-key (make-tracking-key pkg-name nil :package)))
    (when-let ((defs (gethash def-key (slot-value tracker 'definitions))))
      (definition.file (first defs)))))


(defun detect-dependency-cycles (graph)
  "Detect cycles in a dependency graph using depth-first search.
   Returns list of cycles found in the graph.
   
   Parameters:
   graph - Hash table mapping nodes to their dependencies
   
   Returns:
   List of cycles, where each cycle is a list of nodes in cycle order.
   
   Algorithm:
   1. Maintain visited and path sets during DFS traversal
   2. When a back edge is found (visited node in current path), cycle detected
   3. Record complete cycle path for reporting"
  (let ((cycles nil)                          ; Accumulate detected cycles
        (visited (make-hash-table :test 'equal)) ; Track all visited nodes
        (path nil))                           ; Current DFS path
    
    (labels ((visit (node)
               ;; Check node's status in current traversal
               (let ((status (gethash node visited)))
                 (cond
                   ;; Already fully explored - no cycles here
                   ((eq status :completed) nil)
                   
                   ;; Found node in current path - cycle detected
                   ((eq status :in-progress)
                    (let* ((cycle-start (position node path :test #'equal))
                           (cycle (reverse (cons node (subseq path 0 cycle-start)))))
                      (pushnew cycle cycles :test #'equal)))
                   
                   ;; New node - explore it
                   (t
                    (setf (gethash node visited) :in-progress)
                    (push node path)
                    ;; Recursively visit each dependency
                    (dolist (dep (gethash node graph))
                      (visit dep))
                    (pop path)
                    (setf (gethash node visited) :completed))))))
      
      ;; Start DFS from each unvisited node
      (maphash (lambda (node deps)
                 (declare (ignore deps))
                 (unless (gethash node visited)
                   (visit node)))
               graph))
    
    ;; Return detected cycles sorted for consistent reporting
    (sort cycles #'string< 
          :key (lambda (cycle)
                 (format nil "~{~A~^->~}" cycle)))))


(defun list-all-symbols (pkg)
  "List all symbols accessible in a package, including inherited ones.
   Returns a fresh list to avoid package lock issues.
   
   Parameters:
   pkg - A package designator (name or package object)
   
   Returns:
   List of all symbols accessible in the package, with duplicates removed.
   
   Notes:
   1. Includes internal, external, and inherited symbols
   2. Handles both present and shadowed symbols
   3. Returns fresh list to avoid modifying package data
   4. Processes package designator safely"
  
  (let ((package (etypecase pkg
                   (package pkg)
                   (string (find-package pkg))
                   (symbol (find-package pkg))))
        (symbols nil))
    
    (unless package
      (error "Invalid package designator: ~A" pkg))
    
    ;; Collect all accessible symbols
    (do-symbols (sym package)
      (push sym symbols))
    
    ;; Remove duplicates and sort for consistent output
    (sort (remove-duplicates symbols) #'string< :key #'symbol-name)))


(defun analyze-package-exports (tracker)
  "Analyze runtime export patterns between packages.
   Focuses on post-compilation metrics and relationships that inform
   the architectural report. Records patterns that suggest potential
   package organization improvements."
  (let ((export-usage (make-hash-table :test 'equal))    ; Maps exports to reference patterns
        (pkg-relationships (make-hash-table :test 'equal))) ; Maps packages to export dependencies
    
    ;; Phase 1: Build export usage patterns
    (maphash (lambda (key refs)
               (declare (ignore key))
               (dolist (ref refs)
                 (let* ((sym (reference.name ref))
                        (home-pkg (symbol-package sym))
                        (using-pkg (reference.package ref)))
                   ;; Only track references to exported symbols
                   (when (and home-pkg using-pkg
                            (not (eq home-pkg using-pkg))
                            (eq (nth-value 1 (find-symbol (symbol-name sym) home-pkg))
                                :external))
                     ;; Record which packages use exports from which other packages
                     (pushnew (list (package-name home-pkg)     ; Source package
                                   (package-name using-pkg)      ; Using package  
                                   (symbol-name sym))            ; Symbol used
                              (gethash (package-name home-pkg) export-usage)
                              :test #'equal)))))
             (slot-value tracker 'references))
    
    ;; Phase 2: Analyze usage patterns to detect package relationships
    (maphash (lambda (source-pkg usages)
               ;; Group usages by target package
               (let ((pkg-usage (make-hash-table :test 'equal)))
                 (dolist (usage usages)
                   (destructuring-bind (source user sym) usage
                     (declare (ignore source))
                     (push sym (gethash user pkg-usage))))
                 
                 ;; Record significant relationships (using multiple exports)
                 (maphash (lambda (user-pkg symbols)
                           (when (> (length symbols) 2)  ; Using more than 2 exports suggests relationship
                             (push (list user-pkg symbols)
                                   (gethash source-pkg pkg-relationships))))
                         pkg-usage)))
             export-usage)
    
    ;; Phase 3: Update package metrics with export analysis
    (maphash (lambda (pkg metrics)
               (let ((exported-syms (get-package-exports tracker pkg))
                     (dependent-pkgs 0)
                     (export-refs 0))
                 ;; Count packages depending on our exports
                 (when-let (relationships (gethash pkg pkg-relationships))
                   (setf dependent-pkgs (length relationships)
                         export-refs (reduce #'+ relationships 
                                           :key (lambda (r) (length (second r)))))
                 ;; Update metrics
                 (setf (gethash pkg (slot-value tracker 'package-metrics))
                       (list :local-symbols (getf metrics :local-symbols)
                             :inherited-symbols (getf metrics :inherited-symbols)
                             :used-packages (getf metrics :used-packages)
                             :exported-symbols (length exported-syms)
                             :export-users dependent-pkgs
                             :export-references export-refs)))))
             (slot-value tracker 'package-metrics))
    
    ;; Phase 4: Record insights about package organization
    (maphash (lambda (source-pkg relationships)
               (let ((total-refs 0)
                     (total-users 0))
                 ;; Analyze usage patterns
                 (dolist (rel relationships)
                   (destructuring-bind (nil symbols) rel
                     (incf total-users)
                     (incf total-refs (length symbols)))
                   
                   ;; Record noteworthy patterns
                   (when (> total-refs (* 3 total-users))  ; Avg > 3 refs per user
                     (record-anomaly tracker
                       :type :package-cohesion
                       :severity :info
                       :package source-pkg
                       :description 
                       (format nil "Package ~A provides core functionality to ~D other packages (~D refs)"
                               source-pkg total-users total-refs))))))
             pkg-relationships)))


(defun analyze-type-relationships (tracker)
  "Analyze runtime type dependencies that could impact maintainability.
   Examines only successfully compiled and loaded class relationships.
   Focused on structural patterns rather than validity checking.
   
   Analysis covers:
   1. Slot type dependency cycles
   2. Inheritance hierarchy cycles  
   3. Complex specializer relationships
   4. Cross-package type coupling"
  
  ;; Data structures for tracking relationships
  (let ((type-metrics (make-hash-table :test 'equal))      ; For reporting metrics
        (visited (make-hash-table :test 'eq))              ; For DFS traversal
        (path nil))                                        ; Current DFS path

    ;; Clear any existing type graph in tracker
    (setf (slot-value tracker 'type-graph) (make-hash-table :test 'equal))

    ;; Step 1: Build type dependency graph from runtime class relationships
    (maphash (lambda (key def-list)
               (declare (ignore key))
               (dolist (def def-list)
                 (when (eq (definition.type def) :structure/class/condition)
                   (let* ((type-name (definition.name def))
                          (class (find-class type-name nil)))
                     (when (and class (typep class 'standard-class))
                       ;; Get dependencies through slots and inheritance
                       (let ((deps (get-type-dependencies class)))
                         ;; Store in tracker slot instead of local variable
                         (setf (gethash type-name (slot-value tracker 'type-graph)) deps)
                         ;; Record metrics about the relationships
                         (setf (gethash type-name type-metrics)
                               (analyze-type-usage class deps))))))))
             (slot-value tracker 'definitions))

    ;; Step 2: Detect cycles using tracker's type graph
    (labels ((visit (type-name)
               (let ((status (gethash type-name visited)))
                 (cond
                   ((eq status :completed) nil)
                   ((eq status :in-progress)
                    (let* ((cycle-start (position type-name path :test #'equal))
                           (cycle (reverse (cons type-name 
                                               (subseq path 0 cycle-start)))))
                      ;; Record cycle but only if it crosses package boundaries
                      (when (complex-type-cycle-p cycle)
                        (record-anomaly tracker
                          :type :complex-type-dependency
                          :severity :warning
                          :package (symbol-package (car cycle))
                          :context cycle
                          :description
                          (format nil "Complex type dependency cycle detected: ~{~A~^ -> ~}"
                                  cycle)))))
                   (t 
                    (setf (gethash type-name visited) :in-progress)
                    (push type-name path)
                    ;; Use tracker's type graph instead of local variable
                    (dolist (dep (gethash type-name (slot-value tracker 'type-graph)))
                      (visit dep))
                    (pop path)
                    (setf (gethash type-name visited) :completed))))))

      ;; Start DFS from each unvisited type using tracker's type graph
      (maphash (lambda (type-name deps)
                 (declare (ignore deps))
                 (unless (gethash type-name visited)
                   (visit type-name)))
               (slot-value tracker 'type-graph)))

    ;; Update tracker with type analysis metrics for reporting
    (setf (slot-value tracker 'type-metrics) type-metrics)))


(defun get-type-dependencies (class)
  "Get all direct type dependencies for a class through slots and inheritance.
   Returns list of dependent type names that are user-defined classes."
  (let ((deps nil))
    ;; Get slot type dependencies
    (dolist (slot (c2mop:class-direct-slots class))
      (let ((slot-type (c2mop:slot-definition-type slot)))
        (when (and slot-type (find-class slot-type nil))
          (pushnew slot-type deps))))
    
    ;; Get superclass dependencies
    (dolist (super (c2mop:class-direct-superclasses class))
      (pushnew (class-name super) deps))
    
    ;; Return only user-defined classes (exclude CL types)
    (remove-if (lambda (type-name)
                 (eq (symbol-package type-name)
                     (find-package :common-lisp)))
               deps)))


(defun analyze-type-usage (class deps)
  "Analyze how a class is used within the system.
   Returns metrics alist with usage patterns."
  (let* ((class-name (class-name class))
         (pkg (symbol-package class-name))
         (n-slots (length (c2mop:class-direct-slots class)))
         (n-methods (length (c2mop:specializer-direct-methods class)))
         (n-subclasses (length (c2mop:class-direct-subclasses class)))
         (foreign-deps (remove-if (lambda (dep)
                                   (eq (symbol-package dep) pkg))
                                 deps)))
    `((:total-slots . ,n-slots)
      (:total-methods . ,n-methods) 
      (:total-subclasses . ,n-subclasses)
      (:cross-package-deps . ,(length foreign-deps)))))


(defun complex-type-cycle-p (cycle)
  "Returns true if cycle exhibits complex dependencies:
   - Crosses package boundaries
   - Involves 3 or more types
   - Contains bidirectional relationships"
  (and (> (length cycle) 2)
       (> (length (remove-duplicates cycle 
                                   :key #'symbol-package))
          1)))


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
