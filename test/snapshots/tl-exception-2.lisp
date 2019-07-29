(define print-lock (make-semaphore #t))
(define (p thing)
    (semaphore-wait print-lock)
  (print thing)
  (signal-semaphore print-lock))

(let ((pkg *package*))
  (fork
   (binding ((*package* pkg))
     (sleep-ms 15)
     (p 'background-0)
     (sleep-ms 15)
     (p 'background-1))))

(p `(before error))
(kill-thread (current-thread))
(p `(should not print))
