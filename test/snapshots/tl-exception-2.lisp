(fork
 (sleep-ms 5)
 (print 'background-0)
 (sleep-ms 5)
 (print 'background-1))

(print `(before error))
(kill-thread (current-thread))
(print `(should not print))
