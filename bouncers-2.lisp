(define mouse-position 0@0)
(define boxes '())
(define screen-width 500)
(define screen-height 500)
(define screen-size (make-point screen-width screen-height))
(define back-buffer (make-image screen-width screen-height))

(define (ordered? a b c)
    (and (<i a b) (<i b c)))

(define (move-box box)
    (let* ((x (aget box 0))
           (y (aget box 1))
           (dx (aget box 2))
           (dy (aget box 3))
           (nx (+i x dx))
           (ny (+i y dy))
           (sw (point-x screen-size))
           (sh (point-y screen-size))
           (mx (point-x mouse-position))
           (my (point-y mouse-position)))
      (when (<i nx 0)
        (set! nx 0)
        (set! dx (*i -1 dx)))
      (when (<i ny 0)
        (set! ny 0)
        (set! dy (*i -1 dy)))
      (when (>i nx sw)
        (set! nx sw)
        (set! dx (*i -1 dx)))
      (when (>i ny sh)
        (set! ny sh)
        (set! dy (*i -1 dy)))
      (let ((mouseover (and (ordered? nx mx (+i 15 nx))
                            (ordered? ny my (+i 15 ny)))))
        (when mouseover
          (kill-thread (current-thread)))
        (if mouseover
            (aset box 4 0xff0000ff)
            (aset box 4 0xff00ffff))
        (unless mouseover
          (aset box 0 nx)
          (aset box 1 ny)
          (aset box 2 dx)
          (aset box 3 dy)))))

(define (add-box p)
    (let ((box (make-array 5)))
      (aset box 0 (point-x p))
      (aset box 1 (point-y p))
      (aset box 2 1)
      (aset box 3 1)
      (aset box 4  0xff00ffff)
      (set 'boxes (cons box boxes))
      (fork-with-priority 0 (forever
                             (sleep-ms 16)
                             (move-box box)))))

(define (clear-screen)
    (fill-rect back-buffer 0@0 screen-size 0xffffffff))

(define (flip-buffer)
    (blit-to-screen back-buffer 0@0 100 0))

(define (draw-one-box box)
    (let* ((x (aget box 0))
           (y (aget box 1))
           (color (aget box 4))
           (a (make-point x y))
           (b (point+ a 10@10)))
      (fill-rect back-buffer a b color)))

(define (draw-boxes)
    (let ((loop #f))
      (set! loop (lambda (remaining)
                   (unless (nil? remaining)
                     (draw-one-box (car remaining))
                     (loop (cdr remaining)))))
      (loop boxes)))

(define (draw-frame)
    (clear-screen)
  (draw-boxes)
  (flip-buffer))

(define (onmousedown p) (add-box p))
(define (onmousedrag p) (add-box p) (set 'mouse-position p))
(define (onmousemove p) (set 'mouse-position p))

(fork-with-priority 100 (forever (draw-frame) (sleep-ms 10)))

(request-display screen-width screen-height)
