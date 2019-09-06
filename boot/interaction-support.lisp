;;  interaction support -----------------------------------------------------------

(define (ignore1 _))
(define onshow ignore1)
(define onmousedown ignore1)
(define onmouseup ignore1)
(define onmousedrag ignore1)
(define onmousemove ignore1)
(define onkey ignore1)
(define onframe ignore1)

(defmacro forever (& forms)
  `(let loop () ,@forms (loop)))

(at-boot (define wants-display #f))

(at-boot (define event-ready-semaphore (make-semaphore #f)))
(at-boot (define pending-events '()))

(define (handle-event e)
    (let ((name (car e))
          (data (cdr e)))
      (case name
        (onmousemove (onmousemove data))
        (onmousedown (onmousedown data))
        (onmouseup   (onmouseup   data))
        (onmousedrag (onmousedrag data))
        (onkey       (onkey       data))
        (onshow      (onshow      data)))))

(define (poll-for-pending-events)
    (semaphore-wait event-ready-semaphore)
  (let ((found pending-events)) ;; clearly not thread safe ; )
    (unless (nil? found)
      (set 'pending-events '())
      (dolist (e (reverse-list found))
        (handle-event e)))))

(at-boot
 (let ((started-event-loop #f))
   (define (start-event-loop)
       (unless started-event-loop
         (fork-with-priority 30000 (forever (poll-for-pending-events))))
     (set! started-event-loop #t))))

;; TODO this should no longer be needed
(define (start-additional-event-loop)
    (fork-with-priority 20000 (forever (poll-for-pending-events))))

(define (request-display w h)
    (set 'wants-display (make-point w h))
  (start-event-loop))

'done
