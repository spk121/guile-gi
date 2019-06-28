(use-modules (test grilo base))

(automake-test
 (let ((data (media:audio-new))
       (title "START:DASH!!")
       (artists '("Honoka Kosaka" "Kotori Minami" "Umi Sonada" #| ... |#)))
  (send data (set-title
              ;; make sure references aren't shared
              (string-copy title)))
  (for-each
   (lambda (artist)
     (send data (add-artist
                 (string-copy artist))))
   artists)
  (and (equal? (send data (get-title)) title)
       ;; list output is not yet implemented
       (equal? (send data (get-single-values-for-key-string METADATA_KEY_ARTIST))
               artists))))
