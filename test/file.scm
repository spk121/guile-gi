(use-modules (gi) (gi repository)
             (srfi srfi-64))

(test-begin "file")

(if (false-if-exception (require "Gio" "2.0"))
    (test-assert "load File"
      ((negate null?)
       (load-by-name "Gio" "File")))
    (begin
      (test-expect-fail "load File")
      (test-assert "load File" #f)))

(unless (test-passed?)
  (test-skip most-positive-fixnum))

(test-assert "new-for-path"
  (file:new-for-path "foo.txt"))

(test-assert "has-parent?"
  (file:has-parent? (file:new-for-path "/tmp")))

(test-assert "get-parent"
  (let ((parent-path
	 (file:get-path
	  (file:get-parent (file:new-for-path "/tmp")))))
    (or (string=? "/" parent-path)
	(string=? "\\" parent-path))))

(test-assert "get-child"
  (let ((child-path
	 (file:get-path
	  (file:get-child (file:new-for-path "/") "tmp"))))
    (or (string=? "/tmp" child-path)
	(string=? "\\tmp" child-path))))

(test-end "file")
