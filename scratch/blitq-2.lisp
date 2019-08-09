
(define (min a b) (if (<= a b) a b))

(define mouse-position 0@0)
(define screen-width (f->i (* 1920 0.75)))
(define screen-height (f->i (* 1080 0.75)))
(define (somewhere-on-screen) (make-point (random screen-width) (random screen-height)))
(define screen-size (make-point screen-width screen-height))
(define back-buffer (make-image screen-width screen-height))

(define cow (load-image "./res/cow.png"))
(define sky (load-image "./res/sky_500.png"))

(define (clear-screen)
    (fill-rect back-buffer 0@0 screen-size 0xffffffff))

(define (flip-buffer)
    (blit-to-screen back-buffer 0@0 100 0))

(define (dp a)
    (let ((b (point+ a 5@5))
          (color 0xffff00ff))
      (fill-rect back-buffer (point+ a -5@-5) b color)))

(define (bq sr ds a b c d e f g h)
    (blitq sr ds a b c d e f g h)
  (dp a)
  (dp b)
  (dp c)
  (dp d)
  (dp e)
  (dp f)
  (dp g)
  (dp h))

(define (draw-it b)
    (let ((p (aget b 1)))
      (blitq (aget b 0) back-buffer
          0@0 500@0 0@500 500@500
          (point+ p 0@0) (point+ p 500@0) (point+ p 0@500) (point+ p 500@500)))
    (let ((p (aget b 2)))
      (bq (aget b 0) back-buffer
          (aget b 3) (aget b 4) (aget b 5) (aget b 6)
          (aget b 7) (aget b 8) (aget b 9) (aget b 10))))

(define (make-one img src-origin dest-origin e f g h)
    (vector img src-origin dest-origin
            e f g h
            (point+ dest-origin e)
            (point+ dest-origin f)
            (point+ dest-origin g)
            (point+ dest-origin h)))

(define items (list (make-one cow 0@0 700@0 0@0 500@0 0@500 500@500)))

(define (drawq)
  (dolist (it items)
    (draw-it it)))

(define (update-screen!)
    (forever
     (clear-screen)
     (drawq)
     (flip-buffer)
     (sleep-ms 30)))

(define (dsq pa pb)
    (let ((a (- (point-x pa) (point-x pb)))
          (b (- (point-y pa) (point-y pb))))
      (+ (* a a) (* b b))))

(define update-current-point #f)
(define (maybe-move a b update)
    (if (< (dsq a b) 100)
        (let () (set 'update-current-point update) #t)
        #f))

(define (maybe-move-item item p)
    (or
     (maybe-move p (aget item  3) (lambda (p) (aset item  3 p)))
     (maybe-move p (aget item  4) (lambda (p) (aset item  4 p)))
     (maybe-move p (aget item  5) (lambda (p) (aset item  5 p)))
     (maybe-move p (aget item  6) (lambda (p) (aset item  6 p)))
     (maybe-move p (aget item  7) (lambda (p) (aset item  7 p)))
     (maybe-move p (aget item  8) (lambda (p) (aset item  8 p)))
     (maybe-move p (aget item  9) (lambda (p) (aset item  9 p)))
     (maybe-move p (aget item 10) (lambda (p) (aset item 10 p)))))

(define (onmousedown p)
    (let ((loop #f))
      (set! loop (lambda (its)
                   (if (nil? its)
                       (set 'update-current-point #f)
                       (or (maybe-move-item (car its) p)
                           (loop (cdr its))))))
      (loop items)))

(define (onmousedrag p)
    (and update-current-point (update-current-point p)))

(request-display screen-width screen-height)

(fork-with-priority 1 (update-screen!))
