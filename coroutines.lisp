(define (call-with-tag tag body handler)
    (set-stack-mark tag)
  (let* ((snapshot (body)))
    (if (continuation? snapshot)
        (let* ((resume (lambda (v)
                         ;; FIXME: appears to be necc. to avoid TCO when resuming
                         (let ((r (resume-stack-snapshot snapshot v)))
                           r)))
               (result (handler resume (continuation-value snapshot))))
          result)
        snapshot)))

(define co-stack-mark 'co)
(define (yield value) (snapshot-to-stack-mark 'co value))

(define (make-coroutine fn)
  (lambda args
    (call-with-tag 'co
                   (lambda () (apply fn args))
                   (lambda (k v) (list k v)))))

(define (step-coroutine fn-and-val)
    (let ((fn (first fn-and-val)))
      (call-with-tag 'co (lambda () (fn '()))
                     (lambda (k v) (list k v)))))


(define (my-counter-body start)
    (let ((loop #f))
      (set! loop (lambda (i)
                   (yield i)
                   (loop (+i i 1))))
      (loop start)))

(define count-up (make-coroutine my-counter-body))

(print "running....")
(define co1 (count-up 0))
(define co2 (step-coroutine co1))
(define co3 (step-coroutine co2))
(define co4 (step-coroutine co3))
(print "ran....")
(print (second co1))
(print (second co2))
(print (second co3))
(print (second co4))


(define running (list (count-up 0)
                      (count-up 2)
                      (count-up 7)))

(define (step!)
    (set-symbol-value 'running (mapcar step-coroutine running))
  (print (mapcar second running)))

(define (repeat n fn)
    (let ((loop #f))
      (set! loop (lambda (n)
                   (when (not (eq 0 n))
                     (fn)
                     (loop (-i n 1)))))
      (loop n)))


(repeat 10 step!)
