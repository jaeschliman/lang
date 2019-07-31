;;  interaction support -----------------------------------------------------------

(define (ignore1 _))
(define onshow ignore1)
(define onmousedown ignore1)
(define onmousedrag ignore1)
(define onmousemove ignore1)
(define onkey ignore1)
(define onframe ignore1)

(defmacro forever (& forms)
  `(let ((loop #f))
     (set! loop (lambda () ,@forms (loop)))
     (loop)))

(define wants-display #f)

(define event-ready-semaphore (make-semaphore #f))
(define pending-events '())

(define (handle-event e)
    (let ((name (car e))
          (data (cdr e)))
      (case name
        (onmousemove (onmousemove data))
        (onmousedown (onmousedown data))
        (onmousedrag (onmousedrag data))
        (onkey       (onkey       data))
        (onshow      (onshow      data)))))

(define (poll-for-pending-events)
    (semaphore-wait event-ready-semaphore)
  (let ((found pending-events)) ;; clearly not thread safe ; )
    (unless (nil? found)
      (set 'pending-events '())
      (mapcar handle-event (reverse-list found)))))

(let ((started-event-loop #f))
  (define (start-event-loop)
      (unless started-event-loop
        (fork (forever (poll-for-pending-events))))
    (set! started-event-loop #t)))

(define (start-additional-event-loop)
    (fork (forever (poll-for-pending-events))))

(define (request-display w h)
    (set 'wants-display (make-point w h))
  (start-event-loop))

'done
