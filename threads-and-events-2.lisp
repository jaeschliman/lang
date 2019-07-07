(defmacro forever (& forms)
  `(let ((loop #f))
     (set! loop (lambda () ,@forms (loop)))
     (loop)))

(defmacro repeat (count & forms)
  `(let ((loop #f))
     (set! loop (lambda (c) ,@forms
                        (when (>i c 0)
                          (loop (-i c 1)))))
     (loop ,count)))

(define pending-events '())

(define (handle-event e)
    ;; (print e)
  (when (eq (car e) 'mousedrag)
    (set-pixel (cadr e))))

(define got-event (make-semaphore 0))

(define (poll-for-pending-events)
    (semaphore-wait got-event)
  (print `(got event!))
  (let ((found pending-events)) ;; clearly not thread safe ; )
    (when (not (nil? found))
      (set 'pending-events '()) 
      (mapcar handle-event (reverse-list found)))))

(let ((started #f))
  (define (maybe-start-polling)
      (when (not started)
        (set! started #t)
        (forever (poll-for-pending-events)))))

(define (onshow w h))
(define (onframe dt) (maybe-start-polling))

(define (onmousedrag pt)
    (set 'pending-events (cons `(mousedrag ,pt) pending-events))
  (signal-semaphore got-event))
