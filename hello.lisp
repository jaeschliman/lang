(print '(hello world))
(print 42)

;; argument order
(print ((lambda (x y) x) 10 11))
(print ((lambda (x y) y) 10 11))

;; fancy characters
(print '(* < > >= @))

(print ;comment inside expr
 'hello
 )

;; things that would be easier to read with definitions
((

(lambda (x) ; since we don't have `let`
  (print 'before-closure) 
  (lambda ()
    (print x)
    (print 'after-closure)
    ))

  'closure))


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
