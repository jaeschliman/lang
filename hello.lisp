(print '(hello world))
(print 42)

(print ((lambda (x y) x) 10 11))
(print ((lambda (x y) y) 10 11))

(
(

(lambda (x)
  (print 'beforeclosure)
  (lambda ()
    (print x)
    (print 'afterclosure)
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
'xvalue 'yvalue
)
'zvalue
)
