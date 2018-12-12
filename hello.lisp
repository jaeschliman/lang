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

(set-symbol-value 'x 10)
(print x)

(set-symbol-value 'make-adder (lambda (amount) (lambda (x) (add x amount))))
(set-symbol-value 'add1 (make-adder 1))
(set-symbol-value 'add2 (make-adder 2))
(print (add1 10))
(print (add2 20))
(print (add1 11))
(print (add2 21))

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
