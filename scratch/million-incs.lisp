(define now #/lang/current-time-ms)

(defmacro timing ( & body)
  `(let ((start (now)))
     ,@body
     (print (list 'took: (-i (now) start)))))

(define (atom val) (vector val))

(define (deref atom) (aget atom 0))

(define (swap! atom fn)
  (let loop ((expect (deref atom)))
       (unless (#/lang/cas-vector atom 0 expect (fn expect))
         (loop (deref atom)))))

(define (inc n) (+i n 1))

(defmacro spin-wait (condition)
  `(let loop ()
        (unless ,condition
          (loop))))

;; (set '#/lang/compiler/*trace-eval* #t)
(forever
 (print '--------------------------------------------------)
 (let* ((it (atom 0))
        (thunk (lambda () (swap! it inc))))
   (timing
    (dotimes (_ 1000000) (#/lang/fork-thunk 0 thunk))
    (spin-wait (eq 1000000 (deref it))))
   (print (deref it))
   (swap! it (lambda (_) 0))
   (timing (dotimes (_ 1000000) (thunk)))
   (print (deref it)))
 (sleep-ms 100))


