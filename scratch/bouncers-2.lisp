(defmacro sync (sem & body)
  `(let ()
     (semaphore-wait ,sem)
     ,@body
     (signal-semaphore ,sem)))

(define add-box-lock (make-semaphore #t))

(define mouse-position 0@0)
(define boxes '())
(define screen-width (f->i (* 1920 0.75)))
(define screen-height (f->i (* 1080 0.75)))
(define screen-size (make-point screen-width screen-height))
(define back-buffer (make-image screen-width screen-height))

(define (ordered? a b c)
  (and (<i a b) (<i b c)))

(define rainbow-colors
  '(0xffff0000
    0xffff8800
    0xffffaa00
    0xffffdd00
    0xffffff00
    0xff88ff00
    0xffaaff00
    0xffddff00
    0xff00ff00
    0xff00ff88
    0xff00ffaa
    0xff00ffdd
    0xff00ffff
    0xff0088ff
    0xff00aaff
    0xff00ddff
    0xff0000ff
    0xff8800ff
    0xffaa00ff
    0xffdd00ff
    0xffff00ff
    0xffff88ff
    0xffffaaff
    0xffffddff
    0xffffffff
    0xff888888
    0xffaaaaaa
    0xffdddddd
    0xff000000
    0xff880000
    0xffaa0000
    0xffdd0000))

(define (mappend f as)
  (let loop ((as as) (f f) (acc '()))
       (if (nil? as) (reverse-list acc)
           (let ((r acc))
             (dolist (b (reverse-list (f (car as))))
               (set! r (cons b r)))
             (loop (cdr as) f r)))))

(define (list-to-pairs l)
  (let ((final (first l)))
    (let loop ((as l) (bs (cdr l)) (acc '()))
         (if (nil? bs) (reverse-list (cons (list (car as) final) acc))
             (loop (cdr as) (cdr bs) (cons (list (car as) (car bs)) acc))))))

;; not really a good blend, but ah well
(define (blend-colors a b)
  (+ (/ a 2) (/ b 2)))

(define (blend-components n a b)
  (let* ((shiftl (*i n 8))
         (shiftr (*i n -8))
         (ca (bit-and 0xff (ash a shiftr)))
         (cb (bit-and 0xff (ash b shiftr))))
    (ash (+i (/i ca 2) (/i cb 2)) shiftl)))

(define (proper-blend-colors a b)
  (bit-or 0xff000000
          (bit-or (blend-components 2 a b)
                  (bit-or (blend-components 1 a b)
                          (blend-components 0 a b)))))


(define (extend-rainbow!)
  (set 'rainbow-colors
       (mappend (lambda (colors)
                  (list (first colors)
                        (blend-colors (first colors) (second colors))
                        (second colors)))
                (list-to-pairs rainbow-colors))))

(extend-rainbow!)
(extend-rainbow!)
(extend-rainbow!)

(extend-rainbow!)
(extend-rainbow!)
(extend-rainbow!)

(define alternate-colors
    '(0xff0000ff
      0xff00ffff))

(define colors rainbow-colors)

(define (move-box box)
  (let* ((dx (aget box 2))
         (dy (aget box 3))
         (nx (+i (aget box 0) dx))
         (ny (+i (aget box 1) dy))
         (sw (point-x screen-size))
         (sh (point-y screen-size))
         (mx (point-x mouse-position))
         (my (point-y mouse-position))
         (nc (cdr (aget box 4)))
         (sc (+i 1 (aget box 5))))
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
    (when (nil? nc)
      (set! nc colors))
    (when (eq 0 (%i sc 233)) (set! dx (*i -1 dx)))
    (when (eq 0 (%i sc 172)) (set! dy (*i -1 dy)))
    (when (eq 0 (%i sc 389)) (let ((tmp dy))
                               (set! dy dx)
                               (set! dx tmp)))
    (aset box 0 nx)
    (aset box 1 ny)
    (aset box 2 dx)
    (aset box 3 dy)
    (aset box 4 nc)
    (aset box 5 sc)))

(define (add-box p &opt (dx 1) (dy 1) (priority 0))
  (let ((box (make-array 6)))
    (aset box 0 (point-x p))
    (aset box 1 (point-y p))
    (aset box 2 dx)
    (aset box 3 dy)
    (aset box 4 colors)
    (aset box 5 0)
    (sync add-box-lock (set 'boxes (cons box boxes)))
    (fork-with-priority priority
                        (let ((box box))
                          (forever
                           (sleep-ms 16)
                           (move-box box))))))

(define (clear-screen)
  (fill-rect back-buffer 0@0 screen-size 0xffffffff))

(define (flip-buffer)
  (blit-to-screen back-buffer 0@0 100 0))

(define (draw-one-box box)
  (let* ((x (aget box 0))
         (y (aget box 1))
         (sc (/i (aget box 5) 275))
         (d (if (<i sc 2) 2 (if (<i sc 6) sc 6)))
         (color (car (aget box 4)))
         (a (make-point x y))
         (b (point+ a (make-point d d))))
    (fill-rect back-buffer a b color)))

(define stack-max 0)
(define (check-stack)
    (when (>i (#/lang/%stack-depth-in-bytes) stack-max)
      (set 'stack-max (#/lang/%stack-depth-in-bytes))
      (print `(--> ,stack-max))))

(forward draw-curr-boxes)
(define debug-colors colors)
(define (draw-curr-boxes)
    (sleep-ms 10)
  ;; (clear-screen)
  (dolist (box boxes)
    (draw-one-box box))
  (when (nil? debug-colors) (set 'debug-colors colors))
  (fill-rect back-buffer 0@0 20@20 (car debug-colors))
  (set 'debug-colors (cdr debug-colors))
  (flip-buffer)
  (update-display)
  (check-stack)
  (draw-curr-boxes))

(define (onmousedown p) (add-box p))
(define (add-some-boxes p)
  (add-box p)
  (add-box (point+ p 0@20) -1 2)
  (add-box (point+ p 0@10) 1 -2)
  (add-box (point+ p -20@10) 3 1)
  (add-box (point+ p -10@10))
  (add-box (point+ p -20@20) -1 1)
  (add-box (point+ p 10@0) -1 -1)
  (add-box (point+ p 20@0) -2 1)
  (add-box (point+ p -10@0) 1 2)
  (add-box (point+ p -20@0))
  (add-box (point+ p 10@10) -3 2)
  (add-box (point+ p 20@20) 1 1))
(define (onmousedrag p)
    (add-some-boxes p)
  ;; (add-some-boxes (point+ p -15@15))
  ;; (add-some-boxes (point+ p 25@25))
  (set 'mouse-position p))
(define (onmousemove p) (set 'mouse-position p))

(fork-with-priority 100000 (draw-curr-boxes))

(define (make-source priority p &opt (dx 1) (dy 1))
  (let ((box (make-array 6)))
    (aset box 0 (point-x p))
    (aset box 1 (point-y p))
    (aset box 2 dx)
    (aset box 3 dy)
    (aset box 4 colors)
    (aset box 5 0)
    (fork-with-priority
     priority
     (forever (move-box box)
              (add-some-boxes (make-point (aget box 0) (aget box 1)))
              (sleep-ms 200)))))

(when #f
  (define (onshow)
    (make-source 50 (make-point (/ screen-width 2) (/ screen-height 5)) 4 5)
    (make-source 50 (make-point (/ screen-width 3) (/ screen-height 2)) 2 -1)
    (make-source 50 (make-point (/ screen-width 4) (/ screen-height 2)))
    (make-source 50 (make-point (/ screen-width 6) (/ screen-height 3)) 1 -3)
    (make-source 50 (make-point (/ screen-width 5) (/ screen-height 2)))
    (make-source 250 (make-point (/ screen-width 6) (/ screen-height 3)) -1 -1)
    (make-source 250 (make-point (/ screen-width 5) (/ screen-height 2)) -2 -3)
    (make-source 250 (make-point (/ screen-width 6) (/ screen-height 3)) -2 1)
    (make-source 250 (make-point (/ screen-width 5) (/ screen-height 2)) -3 2)))



(define (onkey k)
  (print "----------------------------------------")
  (#/lang/%print-stats)
  (#/lang/%clear-stats)
  (print "cleared stats"))

(fork-with-priority 50 (forever (sleep-ms 2000)
                                (print (thread-count))))

(request-display screen-width screen-height)
