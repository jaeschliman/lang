(print '(hello world))
(fork (sleep-ms 1000)
      (print "so long!")) ;; expect this

(let ()
  (save-snapshot "./0.amb")
  (print '(resumed from here))) ;; expect this

;; this won't print in the new image b/c it has not been read into the thread.
;; it will work once we are using the meta runner 
(print '(hello again world!))
