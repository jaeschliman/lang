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
    (print e)
  (when (eq (car e) 'mousemove)
    (set-pixel (cadr e))))

(define (poll-for-pending-events) ;; clearly not thread safe ; )
    (sleep-ms 500)
    (let ((found pending-events))
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

(define (onmousemove pt)
    (set 'pending-events (cons `(mousemove ,pt) pending-events)))
