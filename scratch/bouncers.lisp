(set '#/lang/*enable-inline-let-bound-lambdas* #t)
(set '#/lang/*enable-inline-letrec-bound-lambdas* #t)

;; TODO: this would be a good one to stress-test shift/reset

(define (call-with-tag tag body handler)
    (set-stack-mark tag)
  (let* ((invoke body)
         (snapshot (invoke)))
    (set! invoke #f)
    (if (continuation? snapshot)
        (let* ((resume (lambda (v)
                         (set-stack-mark tag)
                         ;; FIXME: appears to be necc. to avoid TCO when resuming
                         (let ((r (resume-stack-snapshot snapshot v)))
                           (set! snapshot #f)
                           r)))
               (result (handler resume (continuation-value snapshot))))
          result)
        snapshot)))

(define (yield) (snapshot-to-stack-mark 'co '()))
(define (continue k v) k)

(define (make-coroutine fn)
  (lambda args
    (call-with-tag 'co
                   (lambda ()
                     (let ((r (apply fn args)))
                       r))
                   continue)))

(define (step-coroutine next)
    (let ((fn next))
      (call-with-tag 'co (lambda ()
                           (let ((r (fn '())))
                             (set! fn #f)
                             r))
                     continue)))

(defmacro coop-forever (& body)
  (let ((loop (gensym)))
    `(let ((running #t)
           (,loop #f))
       (set! ,loop (lambda ()
                     ,@body
                     (if running
                         (let ()
                           (yield)
                           (,loop))
                         '())))
       (,loop))))

(define (my-counter-body start)
    (let ((curr start))
      (coop-forever
       (set! curr (+i 1 curr))
       (print curr))))

(define count-up (make-coroutine my-counter-body))

(define screen-width 1280)
(define screen-height 720)
(define screen-size (make-point screen-width screen-height))

(define spawn #f)
(define pending '())
(define bounce-at #f)
(define (spawn-next sx sy sdx sdy max-age)
   (set-symbol-value 'pending (cons (bounce-at sx sy sdx sdy max-age) pending)))

(define (my-bouncer sx sy sdx sdy max-age)
    (let ((x sx)
          (y sy)
          (dx sdx)
          (dy sdy)
          (dage 1)
          (mx (point-x screen-size))
          (my (point-y screen-size))
          (w 15.0)
          (h 15.0)
          (age 0)
          (alpha 128)
          (spawn-age (f->i (*f (i->f max-age) 0.35))) 
          (child-age (f->i (*f (i->f max-age) 0.75))))
      (coop-forever
       (when (>i alpha 0)
         (let* ((W (f->i w)) (H (f->i h))
                (center (make-point x y))
                (ul (point- center (make-point (/i W 2) (/i H 2)))))
           (screen-fill-rect ul
                             (point+ ul (make-point W H))
                             (+i (*i alpha 0x01000000)
                                 (+i age
                                     0x00000fff)))))
       (set! x (+i x dx))
       (set! y (+i y dy))
       (set! w (+f 1.0 (*f 0.065 (i->f age))))
       (set! h (+f 1.0 (*f 0.065 (i->f age))))
       (set! age (+i age dage))
       (when (and (eq age spawn-age)
                  (>i age 32))
         (let* ((p (make-point dx dy))
                (pa (point-rotate p 45.0))
                (pb (point-rotate p 135.0)))
           (spawn-next x y (point-x pa) (point-y pa) child-age)
           (spawn-next x y (point-x pb) (point-y pb) child-age)))
       (when (>i age max-age)
         (set! age max-age)
         (set! dage (*i dage -1))

         (set! running #f))

       (when (<i age 0)
         (set! age 0)
         (set! dage (*i dage -1)))

       (set! alpha (-i max-age age))

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
       (when (>i alpha 0)
         (let* ((W (f->i w)) (H (f->i h))
                (center (make-point x y))
                (ul (point- center (make-point (/i W 2) (/i H 2)))))
           (screen-fill-rect ul
                             (point+ ul (make-point W H))
                             (+i (*i alpha 0x01000000)
                                 (+i age
                                     0x00ff0000))))))))

(define bounce-at (make-coroutine my-bouncer))

(define running '())

(define (mapcar? fn lst)
    (let loop ((lst lst) (acc '()))
         (if (nil? lst) (reverse-list acc)
             (let ((n (fn (car lst))))
               (if (nil? n)
                   (loop (cdr lst) acc)
                   (loop (cdr lst) (cons n acc)))))))

(define (new-append as bs)
    (if (nil? bs) as
        (let ((r bs))
          (dolist (a (reverse-list as))
            (set! r (cons a r)))
          r)))

(define (step!)
  (set-symbol-value 'running (mapcar? step-coroutine running))
  (set-symbol-value 'running (append running pending))
  (set-symbol-value 'pending '()))

(define (clear-screen)
    (screen-fill-rect 0@0 screen-size 0xffffffff))

(define (update)
    (clear-screen)
    (step!))

(define last-point 0@0)
(define last-last-point 0@0)

(define (set-last-point p)
    (set-symbol-value 'last-last-point last-point)
    (set-symbol-value 'last-point p))

(define (spawn sx sy sdx sdy max-age)
      (set-symbol-value 'running
                        (cons (bounce-at sx sy
                                         sdx sdy
                                         max-age)
                              running)))

(define (add-point p)
    (let ((delta
           (point-rotate
            (point- last-point last-last-point)
            90.0)))
      (spawn (point-x p) (point-y p)
             (point-x delta) (point-y delta) 255)))

(define (go-bananas p)
    (set-last-point p)
  (add-point p))

(define onmousemove set-last-point)
(define onmousedown add-point)
(define onmousedrag go-bananas)
(define (onkey data)
    (print "saving snapshot")
  (save-snapshot "./bouncers.amb"))

(fork-with-priority 100 (forever (update) (sleep-ms 10)))

(request-display screen-width screen-height)
