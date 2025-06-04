;; -*- scheme -*-
;; Example usage of the modernized GIRepository interface

(use-modules (gi repository)
             (oop goops)
             (ice-9 format)
             (ice-9 pretty-print))

;; ============================================================================
;; Basic Repository Operations
;; ============================================================================

(define (demo-basic-operations)
  "Demonstrate basic repository operations."
  (format #t "=== Basic Repository Operations ===~%")
  
  ;; Load GLib namespace
  (format #t "Loading GLib namespace...~%")
  (require-namespace "GLib" "2.0")
  
  ;; Show loaded namespaces
  (format #t "Loaded namespaces: ~a~%" (get-loaded-namespaces))
  
  ;; Get dependencies
  (format #t "GLib dependencies: ~a~%" (get-dependencies "GLib"))
  
  ;; Show repository stats
  (format #t "Repository stats: ~a~%" (repository-stats))
  
  (newline))

;; ============================================================================
;; Info Exploration
;; ============================================================================

(define (demo-info-exploration)
  "Demonstrate info exploration capabilities."
  (format #t "=== Info Exploration ===~%")
  
  ;; Ensure GLib is loaded
  (require-namespace "GLib" "2.0")
  
  ;; Find a specific function
  (let ((main-loop-info (find-by-name "GLib" "MainLoop")))
    (if main-loop-info
        (begin
          (format #t "Found MainLoop info:~%")
          (display-info main-loop-info)
          (newline)
          
          ;; Show info summary
          (format #t "MainLoop summary:~%")
          (pretty-print (info-summary main-loop-info)))
        (format #t "MainLoop not found~%")))
  
  ;; List all functions in GLib
  (format #t "~%GLib functions (first 5):~%")
  (let ((functions (take (get-infos-by-type "GLib" GI-INFO-TYPE-FUNCTION) 5)))
    (for-each (lambda (func)
                (format #t "  - ~a~%" (gi-name func)))
              functions))
  
  (newline))

;; ============================================================================
;; Namespace Analysis
;; ============================================================================

(define (demo-namespace-analysis)
  "Demonstrate namespace analysis features."
  (format #t "=== Namespace Analysis ===~%")
  
  ;; Load GObject namespace
  (require-namespace "GObject" "2.0")
  
  ;; Show namespace summary
  (display-namespace-summary "GObject")
  
  ;; Search for specific patterns
  (format #t "~%Searching for 'Object' pattern in GObject:~%")
  (let ((object-infos (search-by-pattern ".*Object.*" '("GObject"))))
    (for-each (lambda (info)
                (format #t "  - ~a (~a)~%" 
                        (gi-name info) 
                        (info-type->string (gi-type info))))
              (take object-infos 5)))
  
  (newline))

;; ============================================================================
;; Performance Features
;; ============================================================================

(define (demo-performance-features)
  "Demonstrate performance optimization features."
  (format #t "=== Performance Features ===~%")
  
  ;; Show cache status before preloading
  (format #t "Cache stats before preload: ~a~%" (repository-stats))
  
  ;; Preload a namespace for better performance
  (format #t "Preloading GLib namespace...~%")
  (let ((preloaded-count (preload-namespace "GLib")))
    (format #t "Preloaded ~a infos~%" preloaded-count))
  
  ;; Show cache status after preloading
  (format #t "Cache stats after preload: ~a~%" (repository-stats))
  
  ;; Demonstrate fast lookups
  (format #t "~%Fast lookup test:~%")
  (time (begin
          (find-by-name "GLib" "MainLoop")
          (find-by-name "GLib" "String")
          (find-by-name "GLib" "List")
          (find-by-name "GLib" "HashTable")
          (find-by-name "GLib" "Error")))
  
  (newline))

;; ============================================================================
;; GOOPS Integration Demo
;; ============================================================================

(define (demo-goops-integration)
  "Demonstrate GOOPS integration features."
  (format #t "=== GOOPS Integration ===~%")
  
  ;; Load Gio for more complex types
  (require-namespace "Gio" "2.0")
  
  ;; Find an object info
  (let ((file-info (find-by-name "Gio" "File")))
    (when file-info
      (format #t "File info class: ~a~%" (class-of file-info))
      (format #t "Is object info? ~a~%" (gi-object-info? file-info))
      (format #t "Is function info? ~a~%" (gi-function-info? file-info))
      
      ;; Show class hierarchy
      (format #t "Class precedence list:~%")
      (for-each (lambda (class)
                  (format #t "  - ~a~%" (class-name class)))
                (class-precedence-list (class-of file-info)))))
  
  (newline))

;; ============================================================================
;; Error Handling Demo
;; ============================================================================

(define (demo-error-handling)
  "Demonstrate error handling capabilities."
  (format #t "=== Error Handling ===~%")
  
  ;; Try to load a non-existent namespace
  (format #t "Trying to load non-existent namespace...~%")
  (with-gi-error-handling
    (require-namespace "NonExistentNamespace" "1.0"))
  
  ;; Try to find a non-existent info
  (format #t "Trying to find non-existent info...~%")
  (let ((result (find-by-name "GLib" "NonExistentFunction")))
    (format #t "Result: ~a~%" result))
  
  (newline))

;; ============================================================================
;; Utility Functions Demo
;; ============================================================================

(define (demo-utility-functions)
  "Demonstrate various utility functions."
  (format #t "=== Utility Functions ===~%")
  
  ;; Check if namespace is loaded
  (format #t "Is GLib loaded? ~a~%" (namespace-loaded? "GLib"))
  (format #t "Is Gtk loaded? ~a~%" (namespace-loaded? "Gtk"))
  
  ;; Find all infos with same name across namespaces
  (format #t "~%All 'Object' infos across namespaces:~%")
  (let ((object-infos (find-all-by-name "Object")))
    (for-each (lambda (info)
                (format #t "  - ~a:~a~%" (gi-namespace info) (gi-name info)))
              object-infos))
  
  ;; Demonstrate with-namespace
  (format #t "~%Using with-namespace for Gio:~%")
  (with-namespace "Gio" 
    (lambda ()
      (format #t "Gio namespace is available!~%")
      (let ((app-info (find-by-name "Gio" "Application")))
        (when app-info
          (format #t "Found Application: ~a~%" (gi-name app-info))))))
  
  (newline))

;; ============================================================================
;; Main Demo Function
;; ============================================================================

(define (run-demo)
  "Run all demonstration functions."
  (format #t "GIRepository Modernized Interface Demo~%")
  (format #t "=====================================~%~%")
  
  ;; Enable debug output for this demo
  (auto-load-dependencies! #t)
  
  ;; Run all demos
  (demo-basic-operations)
  (demo-info-exploration)
  (demo-namespace-analysis)
  (demo-performance-features)
  (demo-goops-integration)
  (demo-error-handling)
  (demo-utility-functions)
  
  (format #t "Demo completed!~%")
  (format #t "Final repository stats: ~a~%" (repository-stats)))

;; ============================================================================
;; Interactive Exploration Functions
;; ============================================================================

(define (explore-namespace namespace)
  "Interactive function to explore a namespace."
  (format #t "Exploring namespace: ~a~%~%" namespace)
  
  ;; Ensure namespace is loaded
  (unless (namespace-loaded? namespace)
    (format #t "Loading namespace...~%")
    (require-namespace namespace))
  
  ;; Show summary
  (display-namespace-summary namespace)
  
  ;; List some contents
  (format #t "~%Sample contents:~%")
  (let ((infos (take (get-all-infos namespace) 10)))
    (for-each display-info infos)))

(define (find-and-describe namespace name)
  "Find an info and display detailed information about it."
  (let ((info (find-by-name namespace name)))
    (if info
        (begin
          (format #t "Found ~a:~a~%~%" namespace name)
          (display-info info)
          (format #t "~%Detailed summary:~%")
          (pretty-print (info-summary info)))
        (format #t "~a:~a not found~%" namespace name))))

;; ============================================================================
;; Example Usage in REPL
;; ============================================================================

#|
To use this in a REPL:

  ;; Load the module
  (use-modules (gi repository))
  (load "gi-example.scm")
  
  ;; Run the full demo
  (run-demo)
  
  ;; Or explore interactively
  (explore-namespace "GLib")
  (find-and-describe "GLib" "MainLoop")
  
  ;; Check performance
  (preload-namespace "GObject")
  (repository-stats)
  
  ;; Search functionality
  (search-by-pattern ".*Error.*" '("GLib" "GObject"))
|#

;; Run demo if this file is executed directly
(when (and (defined? 'command-line) (not (null? (command-line))))
  (run-demo))

  