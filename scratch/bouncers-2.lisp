(defmacro sync (sem & body)
  `(let ()
     (semaphore-wait ,sem)
     ,@body
     (signal-semaphore ,sem)))

(define add-box-lock (make-semaphore #t))

(define mouse-position 0@0)
(define boxes '())
(define screen-width 800)
(define screen-height 800)
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
      ;; let (
      ;;      (mouseover (and (ordered? nx mx (+i 15 nx))
      ;;                      (ordered? ny my (+i 15 ny))))
      ;;      )
      ;; (when mouseover
      ;;   (kill-thread (current-thread)))
      ;; (if mouseover
      ;;     (aset box 4 0xff0000ff)
      ;;     (aset box 4 0xff00ffff))
      (if (eq (aget box 4) 0xff00ffff)
          (aset box 4 0xff0000ff)
          (aset box 4 0xff00ffff))
      (let () ;;unless mouseover
        (aset box 0 nx)
        (aset box 1 ny)
        (aset box 2 dx)
        (aset box 3 dy))))

(define (add-box p &opt (dx 1) (dy 1))
    (let ((box (make-array 5)))
      (aset box 0 (point-x p))
      (aset box 1 (point-y p))
      (aset box 2 dx)
      (aset box 3 dy)
      (aset box 4  0xff00ffff)
      (sync add-box-lock (set 'boxes (cons box boxes)))
      (fork-with-priority 0 (forever
                             (sleep-ms 64)
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
           (b (point+ a 4@4)))
      (fill-rect back-buffer a b color)))


(define (draw-curr-boxes)
    (forever
     (clear-screen)
     (dolist (box boxes)
       (draw-one-box box))
     (flip-buffer)
     (update-display)
     (sleep-ms 10)))

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
  (add-box (point+ p 20@20)))
(define (onmousedrag p)
    (add-some-boxes p)
  (add-some-boxes (point+ p -15@15))
  (add-some-boxes (point+ p 25@25))
  (set 'mouse-position p))
(define (onmousemove p) (set 'mouse-position p))

(fork-with-priority 10000 (draw-curr-boxes))

(fork-with-priority 50 (forever (add-some-boxes (make-point (/ screen-width 2) (/ screen-height 5)))
                               (sleep-ms 200)))

(fork (sleep-ms 500)
      (fork-with-priority 50 (forever (add-some-boxes (make-point (/ screen-width 3) (/ screen-height 2)))
                                     (sleep-ms 200))))

(fork (sleep-ms 1000)
      (fork-with-priority 50 (forever (add-some-boxes (make-point (/ screen-width 4) (/ screen-height 2)))
                                     (sleep-ms 200))))

(fork (sleep-ms 2000)
      (fork-with-priority 50 (forever (add-some-boxes (make-point (/ screen-width 6) (/ screen-height 3)))
                                     (sleep-ms 200))))
(fork (sleep-ms 2000)
      (fork-with-priority 50 (forever (add-some-boxes (make-point (/ screen-width 5) (/ screen-height 2)))
                                     (sleep-ms 200))))

(fork-with-priority 50 (forever (add-some-boxes (make-point (/ screen-width 20) (/ screen-height 5)))
                               (sleep-ms 200)))

(fork (sleep-ms 500)
      (fork-with-priority 250 (forever (add-some-boxes (make-point (/ screen-width 3) (/ screen-height 20)))
                                     (sleep-ms 200))))

(fork (sleep-ms 1000)
      (fork-with-priority 250 (forever (add-some-boxes (make-point (/ screen-width 40) (/ screen-height 2)))
                                     (sleep-ms 200))))

(fork (sleep-ms 2000)
      (fork-with-priority 250 (forever (add-some-boxes (make-point (/ screen-width 6) (/ screen-height 30)))
                                     (sleep-ms 200))))
(fork (sleep-ms 2000)
      (fork-with-priority 250 (forever (add-some-boxes (make-point (/ screen-width 50) (/ screen-height 3)))
                                     (sleep-ms 200))))



(let ((pkg *package*))
  (fork-with-priority 50
   (binding ((*package* pkg))
            (forever (sleep-ms 2000) (print `(thread count = ,(length (list-all-threads))))))))

(request-display screen-width screen-height)
