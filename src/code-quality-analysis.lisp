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
                       (analyze-package-usage tracker pkg))
               
                 ;; NEW: Perform advanced package analysis for cohesion and coupling
                 (analyze-internal-dependencies tracker pkg)
                 (analyze-symbol-ownership tracker pkg)
               
                 ;; For client usage analysis, only process if there are clients
                 (let ((client-packages (package-used-by-list pkg)))
                   (when client-packages
                     (analyze-client-usage-patterns tracker pkg client-packages)))))
             all-project-packages)
    
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun analyze-package-exports (tracker)
  "Analyze runtime export patterns between packages.
   Examines actual package relationships in the running system rather than
   static source references. Identifies packages that provide core functionality
   to multiple other packages."
  (let ((export-usage (make-hash-table :test 'equal))    ; Maps packages to their export usage
        (pkg-relationships (make-hash-table :test 'equal))) ; Maps packages to dependent packages
    
    ;; Phase 1: Collect all packages in the system
    (let ((all-packages nil))
      ;; Get all packages defined in our project and their dependents
      (maphash (lambda (pkg-name _)
                 (declare (ignore _))
                 (when-let ((pkg (find-package pkg-name)))
                   (pushnew pkg all-packages)
                   ;; Include packages that use this one
                   (dolist (user (package-used-by-list pkg))
                     (pushnew user all-packages))))
               (slot-value tracker 'defined-packages))
      
      ;; Phase 2: Analyze export usage for each package
      (dolist (provider-pkg all-packages)
        (let ((provider-name (package-name provider-pkg))
              (export-count 0))
          ;; Count exported symbols
          (do-external-symbols (sym provider-pkg)
            (incf export-count)
            ;; For each package that might use this symbol
            (dolist (client-pkg all-packages)
              (unless (eq client-pkg provider-pkg) ; Skip self-references
                (let ((client-name (package-name client-pkg)))
                  ;; Check if client can access this symbol
                  (multiple-value-bind (found status) 
                      (find-symbol (symbol-name sym) client-pkg)
                    (when (and found 
                               (member status '(:inherited :external))
                               (eq (symbol-package found) provider-pkg))
                      ;; Record this usage
                      (push (symbol-name sym)
                            (gethash (list provider-name client-name) 
                                     export-usage nil))))))))
          
          ;; Update package exports count in metrics
          (when-let ((metrics (gethash provider-name (slot-value tracker 'package-metrics))))
            (setf (getf (gethash provider-name (slot-value tracker 'package-metrics)) 
                        :exported-symbols)
                  export-count))))
      
      ;; Phase 3: Analyze significant relationships
      (maphash (lambda (key symbols)
                 (destructuring-bind (provider-name client-name) key
                   ;; Only record significant relationships (more than 2 symbols)
                   (when (> (length symbols) 2)
                     (push (list client-name symbols)
                           (gethash provider-name pkg-relationships)))))
               export-usage)
      
      ;; Phase 4: Update metrics and record cohesion anomalies
      (maphash (lambda (provider-name relationships)
                 (let ((dependent-pkgs (length relationships))
                       (total-refs 0))
                   ;; Calculate total references
                   (dolist (rel relationships)
                     (incf total-refs (length (second rel))))
                   
                   ;; Update metrics
                   (when-let ((metrics (gethash provider-name 
                                                (slot-value tracker 'package-metrics))))
                     (setf (getf metrics :export-users) dependent-pkgs
                           (getf metrics :export-references) total-refs))
                   
                   ;; Record noteworthy patterns - packages with significant influence
                   (when (and (> dependent-pkgs 0)
                              (> total-refs (* 3 dependent-pkgs))) ; Avg > 3 refs per client
                     (record-anomaly tracker
                       :type :package-core-provider
                       :severity :info
                       :file (find-package-definition-file tracker provider-name)
                       :package provider-name
                       :description 
                       (format nil "Package ~A provides core functionality to ~D other packages (~D refs)"
                               provider-name dependent-pkgs total-refs)))))
               pkg-relationships))))


#+ignore (defun analyze-internal-dependencies (tracker package)  ;perhaps enhance later
  "Measure how functions within a package refer to each other."
  (let ((symbol-graph (make-hash-table))
        (isolated-symbols nil)
        (connected-groups nil))
    ;; Analyze call patterns between internal symbols
    (do-symbols (sym package)
      (when (eq (symbol-package sym) package)
        ;; Find all references from this symbol's function to other package symbols
        (let ((references (collect-function-references sym package)))
          (if references
              (setf (gethash sym symbol-graph) references)
              (push sym isolated-symbols)))))
    
    ;; Identify connected components in the dependency graph
    (setf connected-groups (find-connected-components symbol-graph))
    
    ;; Cohesion issue: Multiple isolated groups of functionality
    (when (> (length connected-groups) 1)
      (record-anomaly tracker
        :type :package-fragmentation
        :severity :info
        :file (find-package-definition-file tracker (package-name package))
        :package package
        :context connected-groups
        :description 
        (format nil "Package ~A contains ~D disconnected functional groups" 
                (package-name package) (length connected-groups))))))


#+ignore (defun analyze-client-usage-patterns (tracker package client-packages)  ;perhaps enhance later
  "Analyze whether different clients use distinct subsets of a package's exports."
  (let ((client-usage-patterns (make-hash-table :test 'eq))
        (export-clients (make-hash-table)))
    
    ;; Record which exports are used by which clients
    (do-external-symbols (sym package)
      (dolist (client client-packages)
        (when (symbol-used-by-package-p sym client)
          (push client (gethash sym export-clients))
          (push sym (gethash client client-usage-patterns)))))
    
    ;; Calculate interface segregation metrics
    (let* (;(all-exports (collect-all-external-symbols package))
           ;(total-exports (length all-exports))
           (client-count (length client-packages))
           (usage-overlap-matrix (compute-client-overlap client-usage-patterns))
           (disjoint-client-groups (identify-disjoint-clients usage-overlap-matrix 0.2)))
      
      ;; Cohesion issue: Different clients use completely different parts of the interface
      (when (and (> client-count 1) (> (length disjoint-client-groups) 1))
        (record-anomaly tracker
          :type :interface-segregation
          :severity :info
          :file (find-package-definition-file tracker (package-name package))
          :package package
          :context disjoint-client-groups
          :description 
          (format nil "Package ~A has ~D disjoint client groups using separate parts of its interface" 
                  (package-name package) (length disjoint-client-groups)))))))


(defun analyze-symbol-ownership (tracker package)
  "Examine what percentage of symbols used by a package are defined elsewhere."
  (let ((own-symbol-count 0)
        (imported-symbol-count 0)
        (references-by-external-package (make-hash-table :test 'eq)))
    
    ;; Count symbols and their origins
    (do-symbols (sym package)
      (let ((home-package (symbol-package sym)))
        (if (eq home-package package)
            (incf own-symbol-count)
            (progn 
              (incf imported-symbol-count)
              (incf (gethash home-package references-by-external-package 0))))))
    
    ;; Calculate dependency dispersion
    (let* ((total-symbols (+ own-symbol-count imported-symbol-count))
           (external-packages (hash-table-count references-by-external-package))
           (ownership-ratio (/ own-symbol-count total-symbols)))
      
      ;; Potential cohesion issue: Heavy reliance on external symbols
      (when (< ownership-ratio 0.5)
        (record-anomaly tracker
          :type :external-dependency
          :severity :info
          :file (find-package-definition-file tracker (package-name package))
          :package package
          :context (loop for pkg being the hash-keys of references-by-external-package
                         collect (package-name pkg))
          :description 
          (format nil "Package ~A owns only ~D% of its symbols, relying on ~D external packages" 
                  (package-name package) 
                  (round (* 100 ownership-ratio))
                  external-packages))))))


(defun collect-function-references (sym package)
  "Find functions in PACKAGE that are referenced by the function SYM.
   Returns a list of symbols called by this function."
  (let ((refs nil))
    ;; Get definition key for this function
    (let ((def-key (make-tracking-key sym package :function))
          (context-refs (make-hash-table :test 'equal)))
      
      ;; Find the definition's context
      (when-let ((defs (gethash def-key (slot-value *current-tracker* 'definitions))))
        (let ((def-context (definition.context (first defs)))
              (def-file (definition.file (first defs))))
          
          ;; Collect all references from the same file and same context
          (maphash 
            (lambda (key ref-list)
              (declare (ignore key))
              (dolist (ref ref-list)
                (when (and (equal (reference.file ref) def-file)
                           (eq (reference.type ref) :function)
                           (eq (symbol-package (reference.name ref)) package)
                           (not (eq (reference.name ref) sym)))
                  ;; Store reference by its context
                  (push ref (gethash (reference.context ref) context-refs)))))
            (slot-value *current-tracker* 'references))
          
          ;; Add references whose context is a subform of our definition
          (maphash
            (lambda (context refs-in-context)
              (when (subform-p context def-context)
                (dolist (ref refs-in-context)
                  (pushnew (reference.name ref) refs))))
            context-refs))))
    refs))


(defun subform-p (possible-subform form)
  "Check if POSSIBLE-SUBFORM appears within FORM."
  (or (equal possible-subform form)
      (and (consp form)
           (or (some (lambda (x) (subform-p possible-subform x)) form)))))


(defun find-connected-components (graph)
  "Find connected components in a GRAPH.
   GRAPH is a hash table mapping nodes to their adjacent nodes.
   Returns a list of lists, each containing nodes in a connected component."
  (let ((visited (make-hash-table :test 'eq))
        (components nil))
    
    ;; DFS to build a component from a starting node
    (labels ((explore (node component)
               (unless (gethash node visited)
                 (setf (gethash node visited) t)
                 (push node component)
                 ;; Visit each adjacent node
                 (dolist (adj (gethash node graph nil))
                   (setf component (explore adj component))))
               component))
      
      ;; Try to start DFS from each unvisited node
      (maphash (lambda (node _)
                 (declare (ignore _))
                 (unless (gethash node visited)
                   (push (explore node nil) components)))
               graph))
    
    ;; Return the identified components
    components))


(defun symbol-used-by-package-p (sym client-package)
  "Determine if CLIENT-PACKAGE actually uses the symbol SYM.
   This checks both for symbol accessibility and actual references."
  (let ((found-usage nil))
    ;; First check if the symbol is visible in the client package
    (multiple-value-bind (found status) 
        (find-symbol (symbol-name sym) client-package)
      (when (and found (member status '(:external :inherited)))
        ;; Then check for actual usage in references
        (maphash (lambda (key ref-list)
                   (declare (ignore key))
                   (unless found-usage
                     (setf found-usage 
                           (some (lambda (ref)
                                  (and (eq (reference.package ref) client-package)
                                       (eq (reference.name ref) sym)))
                                ref-list))))
                 (slot-value *current-tracker* 'references))))
    found-usage))


(defun collect-all-external-symbols (package)
  "Return a list of all symbols exported by PACKAGE."
  (let ((exports nil))
    (do-external-symbols (sym package)
      (push sym exports))
    exports))


(defun compute-client-overlap (client-usage-patterns)
  "Compute a matrix of overlap coefficients between clients.
   CLIENT-USAGE-PATTERNS maps clients to lists of used symbols.
   Returns a hash table mapping client pairs to their overlap coefficient."
  (let ((overlap-matrix (make-hash-table :test 'equal)))
    ;; For each pair of clients
    (maphash (lambda (client1 symbols1)
               (maphash (lambda (client2 symbols2)
                          ;; Skip comparing a client with itself
                          (unless (eq client1 client2)
                            ;; Calculate Szymkiewicz–Simpson overlap coefficient
                            (let* ((intersection-size 
                                    (length (intersection symbols1 symbols2)))
                                   (min-size (min (length symbols1) 
                                                 (length symbols2)))
                                   (overlap (if (zerop min-size)
                                              0.0
                                              (/ intersection-size min-size))))
                              (setf (gethash (cons client1 client2) overlap-matrix)
                                    overlap))))
                        client-usage-patterns))
             client-usage-patterns)
    overlap-matrix))


(defun identify-disjoint-clients (overlap-matrix threshold)
  "Find groups of clients with minimal overlap in their usage patterns.
   OVERLAP-MATRIX maps client pairs to their overlap coefficients.
   THRESHOLD is the maximum overlap to consider clients as disjoint.
   Returns a list of client groups where each group has minimal inter-group overlap."
  (let ((clients nil)
        (client-graph (make-hash-table :test 'eq)))
    
    ;; First collect all unique clients
    (loop for pair being the hash-keys of overlap-matrix
          do (pushnew (car pair) clients)
             (pushnew (cdr pair) clients))
    
    ;; Build a graph where edges exist between clients with significant overlap
    (loop for pair being the hash-keys of overlap-matrix
          for overlap = (gethash pair overlap-matrix)
          when (> overlap threshold)
          do (let ((client1 (car pair))
                   (client2 (cdr pair)))
               (push client2 (gethash client1 client-graph nil))
               (push client1 (gethash client2 client-graph nil))))
    
    ;; Find connected components in this graph
    (find-connected-components client-graph)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun analyze-clos-relationships (tracker)
  "Analyze runtime type dependencies that could impact maintainability.
   Examines only successfully compiled and loaded class relationships.
   Focused on structural patterns rather than validity checking."
  
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
                   ;; Case 1: Already fully explored - no cycles here
                   ((eq status :completed) nil)
                   
                   ;; Case 2: Found a cycle - node already in current path
                   ((eq status :in-progress)
                    (let* ((cycle-start (position type-name path :test #'equal))
                           (detected-cycle (reverse (cons type-name 
                                                        (subseq path 0 cycle-start))))
                           ;; Find primary element - first alphabetically by fully-qualified name
                           (primary-element 
                            (first (sort (copy-list detected-cycle) #'string< 
                                        :key (lambda (elt)
                                               (format nil "~A:~A"
                                                      (package-name (symbol-package elt))
                                                      (symbol-name elt))))))
                           ;; Determine primary package for reporting
                           (primary-package (symbol-package primary-element))
                           ;; Now reorganize the cycle to start with the primary element
                           (primary-pos (position primary-element detected-cycle :test #'equal))
                           (normalized-cycle (append (subseq detected-cycle primary-pos)
                                                   (subseq detected-cycle 0 primary-pos))))
                      
                      ;; Record cycle but only if it's complex
                      (when (complex-type-cycle-p detected-cycle)
                        (record-anomaly tracker
                          :type :complex-type-cycle
                          :severity :warning
                          :package primary-package
                          :context normalized-cycle ; Use normalized cycle for context
                          :description
                          (format nil "Complex type dependency cycle detected: ~{~A~^ -> ~} -> ~A"
                                  normalized-cycle (first normalized-cycle))))))
                   
                   ;; Case 3: New node - explore it (using ELSE clause rather than T)
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


(defun complex-type-cycle-p (cycle)
  "Returns true if cycle exhibits complex dependencies:
   - Crosses package boundaries
   - Involves 3 or more types
   - Contains bidirectional relationships"
  (and (> (length cycle) 2)
       (> (length (remove-duplicates cycle 
                                   :key #'symbol-package))
          1)))


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


(defun check-type-cycles (type-name path type-graph types-seen cycles)
  "Recursive depth-first search to detect cycles in type definitions.
   TYPE-NAME - The type being examined
   PATH - List of types in the current dependency path
   TYPE-GRAPH - Hash table mapping type names to their dependencies
   TYPES-SEEN - Hash table tracking visited types and their status
   CYCLES - List to collect detected cycles
   
   Returns the updated CYCLES list."
  
  ;; Get type status from the cache
  (let ((status (gethash type-name types-seen)))
    (cond
      ;; Already fully explored - no cycles here
      ((eq status :completed) cycles)
      
      ;; Found type in current path - cycle detected
      ((eq status :in-progress)
       (let* ((cycle-start (position type-name path :test #'equal))
              (cycle-types (reverse (cons type-name (subseq path 0 cycle-start)))))
         ;; Store as list of symbols instead of formatted string
         (pushnew cycle-types cycles :test #'equal)))
      
      ;; New type - explore it
      (t
       (setf (gethash type-name types-seen) :in-progress)
       (let ((updated-path (cons type-name path)))
         ;; Recursively check each referenced type
         (dolist (ref-type (gethash type-name type-graph))
           (when (gethash ref-type type-graph) ; Only check user-defined types
             (setq cycles (check-type-cycles ref-type updated-path type-graph types-seen cycles)))))
       
       ;; Mark as fully explored after checking all dependencies
       (setf (gethash type-name types-seen) :completed)
       cycles))))


(defun analyze-unused-imports (tracker)
  "Detect packages that import or use symbols which are never referenced.
   Analyzes both :USE and :IMPORT-FROM relationships to identify
   potentially unnecessary package dependencies."
  
  ;; First, build a mapping of which packages import which other packages
  (let ((package-imports (make-hash-table :test 'equal))  ; Maps packages to their imports
        (import-usage (make-hash-table :test 'equal)))    ; Maps (package . import) to usage count
    
    ;; Phase 1: Record all package imports
    (maphash (lambda (pkg-name def-form)
               (declare (ignore def-form))
               (when-let ((pkg (find-package pkg-name)))
                 ;; Record all packages used via :USE
                 (dolist (used-pkg (package-use-list pkg))
                   (when used-pkg  ; Ensure package exists
                     (pushnew (package-name used-pkg)
                             (gethash pkg-name package-imports))))
                 
                 ;; Also record imports via :IMPORT-FROM by analyzing imported symbols
                 (do-symbols (sym pkg)
                   (let ((sym-pkg (symbol-package sym)))
                     (when (and sym-pkg                      ; Symbol has a home package
                               (not (eq sym-pkg pkg))        ; Not defined in this package
                               (member sym (package-shadowing-symbols pkg)) ; Explicitly imported
                               (not (member sym-pkg (package-use-list pkg)))) ; Not from :USE
                       (pushnew (package-name sym-pkg)
                               (gethash pkg-name package-imports)))))))
             (slot-value tracker 'defined-packages))
    
    ;; Phase 2: Analyze all references to track usage of imported packages
    (maphash (lambda (key refs)
               (declare (ignore key))
               (dolist (ref refs)
                 (let* ((sym (reference.name ref))
                        (ref-pkg (reference.package ref))
                        (sym-pkg (when (symbolp sym) (symbol-package sym))))
                   ;; Only proceed if we have valid packages
                   (when (and ref-pkg sym-pkg 
                             (not (eq ref-pkg sym-pkg)))  ; Different packages
                     ;; Record usage of this imported package
                     (let ((import-key (cons (package-name ref-pkg) 
                                          (package-name sym-pkg))))
                       (incf (gethash import-key import-usage 0)))))))
             (slot-value tracker 'references))
    
    ;; Phase 3: Detect unused imports
    (maphash (lambda (pkg-name imports)
               (dolist (import-pkg imports)
                 (let ((import-key (cons pkg-name import-pkg)))
                   (when (and (zerop (gethash import-key import-usage 0))
                              (not (string= import-pkg "COMMON-LISP")))
                     ;; Found an unused import
                     (when-let ((pkg (find-package pkg-name)))
                       (record-anomaly tracker
                         :type :unused-import
                         :severity :info
                         :file (find-package-definition-file tracker pkg-name)
                         :package pkg
                         :description 
                         (format nil "Package ~A imports ~A but doesn't use any of its symbols"
                                 pkg-name import-pkg)))))))
             package-imports)))