(defmacro unless (test & body)
  `(when (not ,test) ,@body))

(define run #f)
(define (run fn)
    (set-stack-mark 'run)
  (let ((it (fn)))
    (if (continuation? it)
        (if (nil? (continuation-value it)) ;; TODO: pass an exception instead
            #f ;; false indicates there was an exception
            (let* ((val (continuation-value it))
                   (tryfn (car val))
                   (catchfn (cdr val)))
              (let ((ok (run tryfn)))
                (unless ok (run catchfn)) ;; TODO: what if we throw in catch clause?
                (resume-stack-snapshot it '())
                ok)))
        #t)))) ;; true indicates there was no exception

(define (try-catch tryfn catchfn)
    (snapshot-to-stack-mark 'run (cons tryfn catchfn)))

(define (except)
   (snapshot-to-stack-mark 'run '()))

(define (test1)
    (try-catch (lambda ()
                 (print 'hello)
                 (print 'world))
               (lambda ()
                 (print 'whoops)))
  (print 'done!))

(define (test2)
    (try-catch (lambda ()
                 (print 'hello)
                 (except)
                 (print 'world))
               (lambda ()
                 (print 'whoops)))
  (print 'done!))



(define (test3)
    (try-catch (lambda ()
                 (print 'hello)
                 (try-catch (lambda ()
                              (print 'nested)
                              (except))
                            (lambda ()
                              (print 'inner-whoops)))
                 (except)
                 (print 'world))
               (lambda ()
                 (print 'whoops)))
  (print 'done!))

(print '(running test 1))
(run test1)
(print '(running test 2))
(run test2)
(print '(running test 3))
(run test3)

'done
