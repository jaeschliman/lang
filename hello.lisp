(print '(hello world))
(print 42)

(print ((lambda (x y) x) 10 11))
(print ((lambda (x y) y) 10 11))

(print '(* < > >= @))

(
(

(lambda (x)
  (print 'before-closure)
  (lambda ()
    (print x)
    (print 'after-closure)
    ))
  'closure)

)


(
(
(lambda (x y)
  (lambda (z)
    (print x)
    (print y)
    (print z)))
'x-value 'y-value
)
'z-value
)
