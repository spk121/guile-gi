(use-modules (test grilo base))

(grilo-test
 (let ((data (media:audio-new))
       (title "START:DASH!!")
       (artists '("Honoka Kosaka" "Kotori Minami" "Umi Sonada" #| ... |#)))
  (set-title data ;; make sure references aren't shared
             (string-copy title))
  (for-each
   (lambda (artist)
     (add-artist data (string-copy artist)))
   artists)
  (format #t "Data: ~S~%" data)
  (format #t "Title: ~S~%" (get-title data))
  (format #t "Artists: ~S~%" (get-single-values-for-key-string data METADATA_KEY_ARTIST))
  (and (equal? (get-title data) title)
       (equal? (get-single-values-for-key-string data METADATA_KEY_ARTIST)
               artists))))
