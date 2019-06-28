(use-modules (test grilo base))

(automake-test
 (let ((data (media:audio-new))
       (title "START:DASH!!"))
   (send data (set-title
               ;; make sure references aren't shared
               (string-copy title)))
   (equal? (send data (get-title))
           title)))
