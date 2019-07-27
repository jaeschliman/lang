(let ((pkg *package*))
  (fork
   (binding ((*package* pkg))
     (sleep-ms 5)
     (print 'background-0)
     (sleep-ms 5)
     (print 'background-1))))

(print `(before error))
(+f 1.0 2)
(print `(should not print))
