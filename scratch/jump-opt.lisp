(set '*package* (symbol-package 'define))

(%load "./scratch/compiler.lisp")

(defmacro trc (x)
  `(let ((xxx ,x))
     (print xxx)
     xxx))

;; (set '*trace-eval* #t)

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
                              (print 'before-before-inline)
                              (print 'before-inline)
                              (print `(the answer is ,(should-inline)))
                              (print 'after-inline)))))
           (lambda (ex)
             (print ex)))

(try-catch (lambda ()
             (binding ((*enable-inline-let-bound-lambdas* #t) (*trace-eval* #f))
                      (eval '(let ((should-inline (lambda (x) (+ x x))))
                              (print 'before-inline)
                              (print `(two plus two is ,(should-inline 2)))
                              (print 'after-inline)))))
           (lambda (ex)
             (print ex)))

(try-catch (lambda ()
             (binding ((*enable-inline-let-bound-lambdas* #t) (*trace-eval* #f))
                      (eval '(let ((should-inline (lambda (x) (+ x x))))
                              (print 'before-inline)
                              (print `(two plus two is ,(should-inline 2)))
                              (let ((x 3))
                                (print `(three plus three is ,(should-inline x))))
                              (print 'after-inline)))))
           (lambda (ex)
             (print ex)))

(when #f
  (try-catch (lambda ()
               (binding ((*enable-inline-let-bound-lambdas* #t) (*trace-eval* #f))
                        (eval '(let ((forty-two 42))
                                (let ((closure (lambda ()
                                                 (let ((outer-inline
                                                        (lambda (x)
                                                          (let ((should-inline (lambda (x) (+ x forty-two))))
                                                            (print 'before-inline)
                                                            (print `(two plus forty-two is ,(should-inline 2)))
                                                            (let ((x 3))
                                                              (print `(three plus forty-two is ,(should-inline x))))
                                                            (print `(x = ,x))
                                                            (print 'after-inline)))))
                                                   (let ((y 20))
                                                     (outer-inline 311))))))
                                  closure
                                  (closure))))))
             (lambda (ex)
               (print ex))))

(when #f
  (try-catch
   (lambda ()
     (binding ((*enable-inline-let-bound-lambdas* #t) (*trace-eval* #f))
              (eval '(let* ((inner 41)
                            (do-it (lambda ()
                                     (let ((should-inline (lambda (x) (print '(here comes trouble))
                                                                  (+ 1 inner))))
                                       (print (should-inline))))))
                      do-it ;; to force closure
                      (do-it)))))
   (lambda (ex)
     (print ex))))

(print 'done)

(when #t
  (try-catch (lambda ()
               (binding ((*enable-inline-let-bound-lambdas* #t) (*trace-eval* #f))
                        (eval '(let* ((unused (print "first thing"))
                                      (outer-inline
                                       (lambda (x)
                                         (let ((should-inline
                                                (lambda (x)
                                                  (print 'before-inner-inline)
                                                  (let ((r (+ x x)))
                                                    (print 'after-inner-inline)
                                                    r))))
                                           (print 'before-inline)
                                           (print `(two plus two is ,(should-inline 2)))
                                           ;; (let ((x 3))
                                           ;;   (print `(three plus three is ,(should-inline x))))
                                           (print `(x = ,x))
                                           (print 'after-inline)))))
                                (let ((y 20))
                                  (print "starting up")
                                  (outer-inline 311)
                                  (print "ending it"))))))
             (lambda (ex)
               (print ex))))


(print " 0 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
(when #t
  (try-catch (lambda ()
               (binding ((*enable-inline-let-bound-lambdas* #t) (*trace-eval* #f))
                        (eval '(let* ((force-closure 23)
                                      (outer-inline (lambda (x)
                                                      force-closure
                                                      x)))
                                (print "starting up")
                                (outer-inline 311)
                                (print "ending it")
                                (lambda () force-closure)))))
             (lambda (ex)
               (print ex))))
(print " 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
