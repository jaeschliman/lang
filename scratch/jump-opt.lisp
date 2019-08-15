(set '*package* (symbol-package 'define))

(defmacro trc (x)
  `(let ((xxx ,x))
     (print xxx)
     xxx))

(set '*trace-eval* #t)

(binding ((*enable-jump-opts* #t))
         (eval `(define (my-reverse-list lst)
                    (let ((helper #f))
                      (set! helper (lambda (rem acc)
                                     (if (nil? rem) acc
                                         (helper (cdr rem) (trc (cons (car rem) acc))))))
                      (helper lst '())))))

(print (my-reverse-list '(1 2 3 4 5 6 7 8 9 10)))

(binding ((*enable-jump-opts* #t))
         (eval `(define (iota n)
                    (if (< n 0) 'done
                        (let ()
                          (print n)
                          (iota (- n 1)))))))

(print (iota 10))

(binding ((*enable-jump-opts* #t) (*trace-eval* #t))
         (eval `(define (iota2 n)
                    (let -iota ((n n))
                         (if (< n 0) 'done
                             (let ()
                               (print n)
                               (-iota (- n 1))))))))

(print (iota2 10))

(define (sub1 n) (- n 1))
(define (zero? n) (eq n 0))
(print (%letrec ((is-even? (lambda (n)
                             (or (zero? n)
                                 (is-odd? (sub1 n)))))
                 (is-odd? (lambda (n)
                            (and (not (zero? n))
                                 (is-even? (sub1 n))))))
                (is-odd? 11)))


(try-catch (lambda ()
             (binding ((*enable-inline-let-bound-lambdas* #t) (*trace-eval* #f))
                      (eval '(let ((should-inline (lambda () (print "this ran!") 42)))
                              (print 'before-inline)
                              (should-inline)
                              (print 'after-inline)))))
           (lambda (ex)
             (print ex)))

(when #f
  (try-catch (lambda ()
               (binding ((*enable-inline-let-bound-lambdas* #t))
                        (eval '(let ((should-inline (lambda (x) (+ x x))))
                                (print 'before-inline)
                                (print `(two plus two is ,(should-inline 2)))
                                (print 'after-inline)))))
             (lambda (ex)
               (print ex))))

(print 'done)
