(let ((pkg *package*))
  (fork
   (binding ((*package* pkg))
     (sleep-ms 5)
     (print 'background-0)
     (sleep-ms 5)
     (print 'background-1))))

(print `(before error))
(throw 'goodbye!)
(print `(should not print))
