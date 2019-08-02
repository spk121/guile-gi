(use-modules (gi) (gi repository)
             (oop goops)
             (test automake-test-lib))

(unless (false-if-exception (require "Gio" "2.0"))
  (exit EXIT_SKIPPED))

(load-by-name "Gio" "ActionMap")
(load-by-name "Gio" "Application")

(automake-test
 (memq <GActionMap> (class-precedence-list <GApplication>)))
