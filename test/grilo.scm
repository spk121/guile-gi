(use-modules (gi)
             (srfi srfi-64))

(test-begin "grilo")

(unless (false-if-exception (use-typelibs (("Grl" "0.3") #:prefix grl::)))
  (test-skip most-positive-fixnum))

(test-equal "init"
  #("grilo.scm" "--no-grilo-option" "value")
  (grl::init #("grilo.scm" "--no-grilo-option" "value")))

(define data #f)

(test-assert "allocate data"
  (set! data (grl::media:audio-new)))

(let ((title "START:DASH!!"))
  (test-equal "set title"
    title
    (begin (grl::set-title data (string-copy title))
           (grl::get-title data))))

(let ((artists '("Honoka Kosaka" "Kotori Minami" "Umi Sonada" #| ... |#)))
  (test-equal "set multiple artists"
    artists
    (begin
      (for-each
       (lambda (artist)
         (grl::add-artist data (string-copy artist)))
       artists)
      (grl::get-single-values-for-key-string data grl::METADATA_KEY_ARTIST))))

(test-end "grilo")
