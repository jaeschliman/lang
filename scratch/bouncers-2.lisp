(defmacro sync (sem & body)
  `(let ()
     (semaphore-wait ,sem)
     ,@body
     (signal-semaphore ,sem)))

(define add-box-lock (make-semaphore #t))
(define draw-lock (make-semaphore #t))
(define curr-box-lock (make-semaphore #t))

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

(define (start-drawing-thread) #f)

(define (add-box p)
    (let ((box (make-array 5)))
      (aset box 0 (point-x p))
      (aset box 1 (point-y p))
      (aset box 2 1)
      (aset box 3 1)
      (aset box 4  0xff00ffff)
      (sync add-box-lock (set 'boxes (cons box boxes)))
      (if (= 0 (% (length boxes) 125)) (start-drawing-thread))
      (fork-with-priority 0 (forever
                             (sleep-ms 32)
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
           (b (point+ a 5@5)))
      (fill-rect back-buffer a b color)))

(define curr-boxes '())

(define (draw-boxes)
    (let ((loop #f))
      (set! loop (lambda (remaining)
                   (unless (nil? remaining)
                     (draw-one-box (car remaining))
                     ;; only draw every other box, becuase drawing them all is slow
                     (loop (cddr remaining)))))
      (loop boxes)))

(define (draw-curr-boxes)
    (forever
     (let ((these-boxes curr-boxes))
       (if (nil? these-boxes)
           (let ()
             ;; sync curr-box-lock
             ;; sync draw-lock
             (flip-buffer)
             (update-display)
             (sleep-ms 32)
             (sync curr-box-lock
                   (if (nil? curr-boxes)
                       (let ()
                         (clear-screen)
                         (set 'curr-boxes boxes)))))
           (let ()
             (draw-one-box (car these-boxes))
             (sync curr-box-lock (set 'curr-boxes (cdr curr-boxes))))))))


(define (draw-frame)
    (clear-screen)
  (draw-boxes)
  (flip-buffer))

(define (onmousedown p) (add-box p))
(define (add-some-boxes p)
    (add-box p)
  (add-box (point+ p 0@20))
  (add-box (point+ p 0@10))
  (add-box (point+ p -20@10))
  (add-box (point+ p -10@10))
  (add-box (point+ p -20@20))
  (add-box (point+ p 10@0))
  (add-box (point+ p 20@0))
  (add-box (point+ p -10@0))
  (add-box (point+ p -20@0))
  (add-box (point+ p 10@10))
  (add-box (point+ p 20@20)))
(define (onmousedrag p)
    (add-some-boxes p)
  (add-some-boxes (point+ p -15@15))
  (add-some-boxes (point+ p 25@25))
  (set 'mouse-position p))
(define (onmousemove p) (set 'mouse-position p))

(define (start-drawing-thread)
    (print "starting additional threads to simulate thread priority")

  (fork (forever (add-some-boxes (make-point (/ screen-width 2) (/ screen-height 5)))
                 (sleep-ms 200)))
  (fork (forever (add-some-boxes (make-point (/ screen-width 3) (/ screen-height 2)))
                 (sleep-ms 200)))
  (fork (forever (add-some-boxes (make-point (/ screen-width 4) (/ screen-height 2)))
                 (sleep-ms 200)))
  (start-additional-event-loop)
  (fork (draw-curr-boxes)))


(fork (draw-curr-boxes))

(fork (forever (add-some-boxes (make-point (/ screen-width 2) (/ screen-height 5)))
               (sleep-ms 200)))

(fork (sleep-ms 500)
      (fork (forever (add-some-boxes (make-point (/ screen-width 3) (/ screen-height 2)))
                     (sleep-ms 200))))

(fork (sleep-ms 1000)
      (fork (forever (add-some-boxes (make-point (/ screen-width 4) (/ screen-height 2)))
                     (sleep-ms 200))))

(let ((pkg *package*))
  (fork
   (binding ((*package* pkg))
            (forever (sleep-ms 2000) (print `(thread count = ,(length (list-all-threads))))))))

(request-display screen-width screen-height)
