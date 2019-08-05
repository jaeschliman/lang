(set '*package* (symbol-package 'define))

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
  (dp e)
  (dp f)
  (dp g)
  (dp h))

(define (draw-it b)
    (bq (aget b 0) back-buffer
        0@0 500@0 0@500 500@500
        (aget b 1) (aget b 2) (aget b 3) (aget b 4)))

(define (make-one img e f g h)
    (vector img e f g h))



(define items (list
               (make-one cow 0@0 300@0 0@300 300@300)
               (make-one sky 500@100 1000@0 500@300 1050@500)
               (make-one cow 500@100 1000@0 500@300 1000@500)
               (make-one cow 0@500 500@500 0@1000 500@800)))

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

(define (maybe-move a b update)
    (if (< (dsq a b) 100)
        (let () (update a) #t)
        #f))

(define (maybe-move-item item p)
    (or
     (maybe-move p (aget item 1) (lambda (p) (aset item 1 p)))
     (maybe-move p (aget item 2) (lambda (p) (aset item 2 p)))
     (maybe-move p (aget item 3) (lambda (p) (aset item 3 p)))
     (maybe-move p (aget item 4) (lambda (p) (aset item 4 p)))))

(define (onmousedrag p)
    (let ((loop #f))
      (set! loop (lambda (its)
                   (unless (nil? its)
                     (or (maybe-move-item (car its) p)
                         (loop (cdr its))))))
      (loop items)))


(request-display screen-width screen-height)

(fork-with-priority 1 (update-screen!))
