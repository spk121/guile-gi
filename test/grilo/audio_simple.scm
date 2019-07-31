(use-modules (test grilo base))

(grilo-test
 (let ((data (media:audio-new))
       (title "START:DASH!!"))
   (set-title data (string-copy title))
   (equal? (get-title data) title)))
