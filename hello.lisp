(print 42)
(print '(hello world))
(print 42)
(print '(hello))

(print ((lambda (x y) y) 10 11))

(lambda (x)
  (lambda (y)
    (print x)))
