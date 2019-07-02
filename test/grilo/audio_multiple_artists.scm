(use-modules (test grilo base))

(grilo-test
 (let ((data (media:audio-new))
       (title "START:DASH!!")
       (artists '("Honoka Kosaka" "Kotori Minami" "Umi Sonada" #| ... |#)))
  (with-object data (set-title
                     ;; make sure references aren't shared
                     (string-copy title)))
  (for-each
   (lambda (artist)
     (with-object data (add-artist
                        (string-copy artist))))
   artists)
  (and (equal? (with-object data (get-title)) title)
       ;; list output is not yet implemented
       (equal? (with-object data (get-single-values-for-key-string METADATA_KEY_ARTIST))
               artists))))
