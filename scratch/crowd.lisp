(define screen-width 800)
(define screen-height 800)
(define (somewhere-on-screen) (make-point (random screen-width) (random screen-height)))
(define screen-size (make-point screen-width screen-height))
(define back-buffer (make-image screen-width screen-height))

(define (clear-screen) (fill-rect back-buffer 0@0 screen-size 0xffffffff))

(define (flip-buffer) (blit-to-screen back-buffer 0@0 100 0))

(define crowd (load-image "./res/crowd.png"))
(define (rp m x y) (make-point (* 2 (- (random x) m))
                               (* 2 (- (random y) m))))

(define items '())
(dotimes (y 10)
  (dotimes (x 10)
    (let ((sul (make-point (* x 25) (* y 25)))
          (dul (make-point (f->i (* screen-width (/ x 10.0)))
                           (f->i (* screen-height (/ y 10.0))))))
      (set 'items (cons (cons sul dul) items)))))

(define (draw!)
    (blit crowd back-buffer 0@0 0@0 500@500 (* 2 (/ 800.0 500.0)) 0.0 0xffffffff)
    (dolist (pair items)
      (let ((sul (car pair))
            (dul (cdr pair)))
        (blitq crowd back-buffer
               (point+ sul (rp 2 4 4))
               (point+ sul (point+ 25@0 (rp 2 4 4)))
               (point+ sul (point+ 0@25 (rp 2 4 4)))
               (point+ sul (point+ 25@25 (rp 2 4 4)))
               (point+ dul (rp 10 20 20))
               (point+ 80@0 (point+ dul (rp 10 20 20)))
               (point+ 0@80 (point+ dul (rp 10 20 20)))
               (point+ 80@80 (point+ dul (rp 10 20 20)))))))

(define (update-screen!)
    (forever
     (clear-screen)
     (draw!)
     (flip-buffer)
     (sleep-ms 30)))

(request-display screen-width screen-height)
(fork-with-priority 1 (update-screen!))
