(print '(hello world))
(print 42)

(if #t
    (print 'it-was-true-1)
    (print 'it-was-false-1))

(if #f
    (print 'it-was-true-2)
    (print 'it-was-false-2))

(print '(expecting tt tf ft ff))
(print
 (if #t
     (if #t 'tt 'tf)
     (if #t 'ft 'ff)))
(print
 (if #t
     (if #f 'tt 'tf)
     (if #t 'ft 'ff)))
(print
 (if #f
     (if #t 'tt 'tf)
     (if #t 'ft 'ff)))
(print
 (if #f
     (if #t 'tt 'tf)
     (if #f 'ft 'ff)))

(print '(#\h #\e #\l #\l #\o))
(print '(#\whoops))
(print #\) )

(print '(if #t #f #t))

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


(set-symbol-value 'print-arg-2 (lambda (x y) (print-stacktrace) (print y) ))
(set-symbol-value 'call-2 (lambda (f x y) (f x y)))

(print '(should print 22))
(call-2 print-arg-2 10 22)

(let ((x 10)
      (y 11))
  (print '(hello again world))
  (print x)
  (print y))

(let ((x 10)
      (y 11))
  (print '(hello again world))
  (let ((z (add1 y)))
    (print x)
    (print y)
    (print z)))

(let ((x 10)
      (y 11))
  (let ((y x)
        (x y))
    (print y)
    (print x)))


(let ((inc3 (lambda (x) (add 3 x))))
  (print (inc3 39)))


(let ((amount 20))
  (set-symbol-value 'add-amount (lambda (x) (add x amount))))

(print (add-amount 22))

(set-symbol-value 'make-adder-2 (lambda (arg)
                                  (let ((amount arg))
                                    (lambda (x) (add x amount)))))

(set-symbol-value 'add1-2 (make-adder-2 1))
(set-symbol-value 'add2-2 (make-adder-2 2))
(print (add1-2 10))
(print (add2-2 20))
(print (add1-2 11))
(print (add2-2 21))

(let ((shared 1))
  (set-symbol-value 'shared-1 (lambda (x) (add x shared)))
  (set-symbol-value 'shared-2 (lambda (x) (add x shared))))

(print (shared-1 1))
(print (shared-2 1))
