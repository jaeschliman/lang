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
    (let ((b (point+ a 15@15))
          (color 0xffff00ff))
      (fill-rect back-buffer a b color)))

(define (bq sr ds a b c d e f g h)
  ;(blitq sr ds a b c d e f g h)
  (dp e)
  (dp f)
  (dp g)
  (dp h)
  )

(define (drawq)
    ;; (bq cow back-buffer
    ;;     0@0 500@0 0@500 500@500
    ;;     0@0 300@0 0@300 300@300)
  (bq sky back-buffer
      0@0 500@0 0@500 500@500
      500@100 1000@0 500@300 1000@500)
  (bq cow back-buffer
      0@0 500@0 0@500 500@500
      500@100 1000@0 500@300 1000@500)
  ;; (bq cow back-buffer
  ;;     0@0 500@0 0@500 500@500
  ;;     0@500 500@500 0@1000 500@1000)

  )

(define (update-screen!)
    (forever
     (clear-screen)
     (drawq)
     (flip-buffer)
     (sleep-ms 300)))

(fork-with-priority 100000 (update-screen!))


(request-display screen-width screen-height)
