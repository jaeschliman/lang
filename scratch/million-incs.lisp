(defmacro sync (semaphore & body)
  (let ((sem (gensym))
        (res (gensym)))
    `(let ((,sem ,semaphore)
           (,res '()))
       (semaphore-wait ,sem)
       (set! ,res (let () ,@body))
       (signal-semaphore ,sem)
       ,res)))

(define (now) (#/lang/current-time-ms))

(defmacro timing ( & body)
  `(let ((start (now)))
     ,@body
     (print (list 'took: (-i (now) start)))))

(define (atom val)
  (let ((r (make-array 2)))
    (aset r 0 (make-semaphore #t))
    (aset r 1 val)
    r))

(define (deref atom)
  (sync (aget atom 0)
        (aget atom 1)))

(define (swap! atom fn)
  (sync (aget atom 0)
        (aset atom 1 (fn (aget atom 1)))))

(define (inc n) (+i n 1))

(defmacro spin-wait (condition)
  `(let loop ()
        (unless ,condition
          (loop))))

(let ((it (atom 0)))
  (timing
   (dotimes (_ 1000000)
     (#/lang/fork-thunk 0 (lambda () (swap! it inc))))
   (spin-wait (eq 1000000 (deref it))))
  (print (deref it)))


