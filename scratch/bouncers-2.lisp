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
      (when (eq 0 (% sc 233)) (set! dx (*i -1 dx)))
      (when (eq 0 (% sc 172)) (set! dy (*i -1 dy)))
      (when (eq 0 (% sc 389)) (let ((tmp dy))
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
      (fork-with-priority priority (forever
                                    (sleep-ms 128)
                                    (move-box box)))))

(define (clear-screen)
    (fill-rect back-buffer 0@0 screen-size 0xffffffff))

(define (flip-buffer)
    (blit-to-screen back-buffer 0@0 100 0))

(define (draw-one-box box)
    (let* ((x (aget box 0))
           (y (aget box 1))
           (sc (/i (aget box 5) 35))
           (d (if (<i sc 2) 2 sc))
           (color (car (aget box 4)))
           (a (make-point x y))
           (b (point+ a (make-point d d))))
      (fill-rect back-buffer a b color)))

(define (draw-curr-boxes)
    (let ((color colors))
      (forever
       (clear-screen)
       (dolist (box boxes)
         (draw-one-box box))
       (when (nil? color) (set! color colors))
       (fill-rect back-buffer 0@0 20@20 (car color))
       (set! color (cdr color))
       (flip-buffer)
       (update-display)
       (sleep-ms 5))))

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

(make-source 50 (make-point (/ screen-width 2) (/ screen-height 5)) 4 5)
(make-source 50 (make-point (/ screen-width 3) (/ screen-height 2)) 2 -1)
(make-source 50 (make-point (/ screen-width 4) (/ screen-height 2)))
(make-source 50 (make-point (/ screen-width 6) (/ screen-height 3)) 1 -3)
(make-source 50 (make-point (/ screen-width 5) (/ screen-height 2)))
(make-source 250 (make-point (/ screen-width 6) (/ screen-height 3)) -1 -1)
(make-source 250 (make-point (/ screen-width 5) (/ screen-height 2)) -2 -3)
(make-source 250 (make-point (/ screen-width 6) (/ screen-height 3)) -2 1)
(make-source 250 (make-point (/ screen-width 5) (/ screen-height 2)) -3 2)



(let ((pkg *package*))
  (fork-with-priority 50
   (binding ((*package* pkg))
            (forever (sleep-ms 2000)
                     (print (thread-count))))))

(request-display screen-width screen-height)
