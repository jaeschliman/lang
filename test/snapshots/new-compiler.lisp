(set '*package* (symbol-package 'define))

(define hello-world (bytecode->closure
                     (with-output-to-bytecode ()
                       (emit-pair PUSHLIT (emit-lit "hello, world"))
                       (emit-pair PUSHLIT (emit-lit 'print))
                       (emit-u16 LOAD_GLOBAL)
                       (emit-pair CALL 1))))

(hello-world)

(define goodbye-world (bytecode->closure
                       (with-output-to-bytecode ()
                         (jump 'goodbye)
                         (emit-pair PUSHLIT (emit-lit "hello, world"))
                         (emit-pair PUSHLIT (emit-lit 'print))
                         (emit-u16 LOAD_GLOBAL)
                         (emit-pair CALL 1)
                         (label 'goodbye)
                         (emit-pair PUSHLIT (emit-lit "goodbye, world"))
                         (emit-pair PUSHLIT (emit-lit 'print))
                         (emit-u16 LOAD_GLOBAL)
                         (emit-pair CALL 1))))

(goodbye-world)


(define (dbg expr)
    (try-catch (lambda ()
                 (let* ((expanded (macroexpand (quasiquote-expand expr)))
                        (r
                         (binding ((*context-table* (make-ht)))
                            (analyse-forms expanded)
                            (bytecode->closure (with-output-to-bytecode ()
                                                  (with-expression-context (expanded)
                                                    (emit-expr expanded '())))))))
                   (r)))
               (lambda (ex)
                 (print `(exception in dbg: ,ex)))))

(dbg '(print "hello again, world!"))

(dbg `(if (= 3 3) (print "3 equals 3") (print "3 does not equal 3")))
(dbg `(if (= 4 3) (print "4 equals 3") (print "4 does not equal 3")))

(dbg `(let ((x 10)) (print "compiled a let without body refs")))
(dbg `(let ((message "compiled a let with a body ref"))
        (print message)))

(dbg `(print (closure? (lambda () "does nothing"))))
(dbg `(print ((lambda () "returned a value"))))
(dbg `(print (closure? (lambda (arg) arg))))
(dbg `(print ((lambda (arg) arg) "returned an argument")))
(dbg `(print ((lambda (arg) (list arg)) "used an arg")))
(dbg `(print (list (list 'list '1 '2) '= ((lambda (x y) (list x y)) 1 2))))
(dbg `(print (closure? (let ((x 10)) (lambda () x)))))
(dbg `(print ((let ((x 10)) (lambda () (let ((x 20)) x))))))
(dbg `(print ((let ((x 10)) (lambda () (let ((y 20)) x))))))
(dbg `(print ((let ((x 10)) (lambda () (let ((y 20)) y))))))
(print '(expecting 20 and 10))
(dbg `(print ((let ((x 10) (y 20))
                (print y)
                (lambda () x)))))
(print '(expecting 20))
(dbg `(let ((x 10) (y 20))
        (lambda () x)
        (print y)))
(print '(expecting 5))
(dbg `(let ((make-adder (lambda (n) (lambda (x) (+ n x)))))
        (print ((make-adder 2) 3))))

(dbg `(set 'iota (lambda (n) (if (= n 0) 'iota-done (iota (- n 1))))))

(print (iota 10))
(print (iota 10000))

(defparameter *test-var* 10)
(print `(expecting 20))
(dbg `(print (with-special-binding *test-var* 20 *test-var*)))

(print `(expecting 20))
(dbg `(let ((x 8)) (set! x 20) (print x)))
(print `(expecting 20))
(dbg `(let* ((shared 0)
             (store (lambda (x) (set! shared x))))
        (store 20)
        (print shared)))

(print `(expecting (got 1 2 3)))
(dbg `(let ((vararg (lambda xs (cons 'got xs))))
        (print (vararg 1 2 3))))
(print *varargs*)

(dbg `(let ((init 0)
            (myloop #f))
        (set! myloop (lambda (it)
                       (unless (nil? it)
                         (myloop (cdr it)))))
        (myloop '(1 2 3 4))
        (print "ran myloop")))

(dbg `(set 'mapcar (#/lang/%nlambda () (f lst)
                        (if (nil? lst) lst
                            (cons (f (car lst)) (mapcar f (cdr lst)))))))

(print 'done)

