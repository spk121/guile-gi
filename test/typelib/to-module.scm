(use-modules (gi) (gi repository)
             (test automake-test-lib))

(unless (false-if-exception (require "Gio" "2.0"))
  (exit EXIT_SKIPPED))

(automake-test
 (let* ((module (typelib->module '(test typelib->module) "Gio" "2.0"))
        (iface (module-public-interface module)))
   (and (module-defined? iface '<GApplication>)
        (not (module-defined? (current-module) '<GApplication>)))))
