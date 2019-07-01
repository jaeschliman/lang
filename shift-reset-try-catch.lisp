(define run-reset #f)
(define (run-reset tag fn)
    (set-stack-mark tag)
  (let ((it (fn)))
    (if (continuation? it)
        (let* ((resume (lambda (val)
                         (let* ((thunk (lambda () (resume-stack-snapshot it val)))) 
                           (run-reset tag thunk))))
               (handler (continuation-value it)))
          (handler resume))
        it)))

(define (run-shift tag fn)
    (snapshot-to-stack-mark tag fn))

(defmacro reset-tag (tag & body)
  `(run-reset ,tag (lambda () ,@body)))

(defmacro shift-tag (tag var & body)
  `(run-shift ,tag (lambda (,var) ,@body)))

(define (escape-to-tag tag value)
    (shift-tag tag _ value))

(define (try-catch tryfn catchfn)
    (reset-tag
     'success
     (let ((exception (reset-tag
                       'exception
                       (let ((result (tryfn)))
                         (escape-to-tag 'success result))))) 
       (catchfn exception))))

(define (throw ex)
    (escape-to-tag 'exception ex))

(try-catch (lambda ()
             (print 'hello)
             (print 'world))
           (lambda (ex)
             (print `(caught ,ex))))

(try-catch (lambda ()
             (print 'hello)
             (throw 'world)
             (print 'whoops))
           (lambda (ex)
             (print ex)))

(try-catch (lambda ()
             (print 'hello)
             (try-catch (lambda ()
                          (throw 'world)
                          (print 'whoops))
                        (lambda (ex)
                          (print `(rethrowing ,ex))
                          (throw ex)
                          (print 'whoops)))
             (print 'whoops))
           (lambda (ex)
             (print ex)))

(try-catch (lambda ()
             (print 'hello)
             (*f 1.0 1) ;; catch a type error
             (print 'whoops))
           (lambda (ex)
             (print `(caught ,ex))
             (print 'world)))

'done
