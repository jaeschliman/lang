(define (call-with-tag tag body handler)
    (set-stack-mark tag)
  (let* ((invoke body)
         (snapshot (invoke)))
    (set! invoke #f)
    (if (continuation? snapshot)
        (let* ((resume (lambda (v)
                         ;; not sure about this... but needed...
                         (set-stack-mark tag)
                         ;; FIXME: appears to be necc. to avoid TCO when resuming
                         (let ((r (resume-stack-snapshot snapshot v)))
                           (set! snapshot #f)
                           r)))
               (result (handler resume (continuation-value snapshot))))
          ;; (set! body #f)
          ;; 
          ;; (set! resume #f)
          ;; (set! handler #f)
          result)
        snapshot)))

(define (yield) (snapshot-to-stack-mark 'co '()))
(define (continue k v) k)

(define (make-coroutine fn)
  (lambda args
    (call-with-tag 'co
                   (lambda ()
                     ;; (set-stack-mark 'co)
                     (let ((r (apply fn args)))
                       r))
                   continue)))

(define (step-coroutine next)
    (let ((fn next))
      (call-with-tag 'co (lambda ()
                           ;; (set-stack-mark 'co)
                           (let ((r (fn '())))
                             (set! fn #f)
                             r))
                     continue)))

(defmacro forever (& body)
  (let ((loop (gensym)))
    `(let ((,loop #f))
       (set! ,loop (lambda ()
                     ,@body
                     (yield)
                     (,loop)))
       (,loop))))


(define (my-counter-body start)
    (let ((curr start))
      (forever
       (set! curr (+i 1 curr))
       (print curr))))

(define count-up (make-coroutine my-counter-body))

(define (my-bouncer sx sy)
    (let ((x sx)
          (y sy)
          (dx -1)
          (dy 1)
          (mx 900)
          (my 500)
          (w 10)
          (h 10))
      (forever
       (set! x (+i x dx))
       (set! y (+i y dy))
       (when (<i x 0)
         (set! x 0)
         (set! dx (*i dx -1)))
       (when (<i y 0)
         (set! y 0)
         (set! dy (*i dy -1)))
       (when (>i x mx)
         (set! x mx)
         (set! dx (*i dx -1)))
       (when (>i y my)
         (set! y my)
         (set! dy (*i dy -1)))
       (let ((ul (make-point x y)))
         (screen-fill-rect ul
                           (point+ ul (make-point w h))
                           0xff0000)))))

(define bounce-at (make-coroutine my-bouncer))

(define running (list
                 (bounce-at 10 10)
                 (bounce-at 200 70)
                 (bounce-at 100 100)
                 (bounce-at 150 33)
                 (bounce-at 300 122)

                      ))

(define (step!)
    (set-symbol-value 'running (mapcar step-coroutine running)))

(define screen-size #f)
(define (clear-screen)
    (screen-fill-rect 0@0 screen-size 0xffffff))

(define (mouse-handler p)
    (clear-screen)
    (step!))

(define (add-point p)
    (set-symbol-value 'running
                      (cons (bounce-at (point-x p) (point-y p))
                            running)))

(define (ignore1 _) '())
(define (ignore2 _ __) '())

(define (onshow w h) (set-symbol-value 'screen-size (make-point w h)))
(define onkey ignore1)
(define onmousemove mouse-handler)
(define onmousedown add-point)
(define onmousedrag mouse-handler)
