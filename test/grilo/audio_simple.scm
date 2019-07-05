(use-modules (test grilo base))

(grilo-test
 (let ((data (media:audio-new))
       (title "START:DASH!!"))
   (with-object data (set-title
                      ;; make sure references aren't shared
                      (string-copy title)))
   (equal? (with-object data (get-title))
           title)))
