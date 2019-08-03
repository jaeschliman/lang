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

(define ufo (load-image "./res/ufo.png"))
(define city (load-image "./res/buildings.png"))

(define (point-angle p)
    (atan2f (i->f (point-y p))
            (i->f (point-x p))))

(define (point-length p)
    (let ((a (point-x p))
          (b (point-y p)))
      (sqrtf (i->f (+ (* a a) (* b b))))))

(define (move-box box)
    (let* ((x (aget box 0))
           (y (aget box 1))
           (angle (aget box 2))
           (speed (aget box 3))
           (accel 4)
           (to-m (point- mouse-position (make-point x y)))
           (sc (+i 1 (aget box 5)))
           (p  (point-rotate (make-point (f->i speed) 0) (* 57.2958 angle)))
           (dp (point-rotate (make-point accel 0) (* 57.2958 (point-angle to-m))))
           (np (point+ p dp))
           (dx (point-x np))
           (dy (point-y np))
           (nx (+i x dx))
           (ny (+i y dy)))
      (aset box 0 nx)
      (aset box 1 ny)
      (aset box 2 (point-angle  np))
      (aset box 3 (point-length np))
      (aset box 5 sc)))

(define (add-box p &opt (angle 0.0) (speed 5.0) (priority 0))
    (let ((box (make-array 6)))
      (aset box 0 (point-x p))
      (aset box 1 (point-y p))
      (aset box 2 angle)
      (aset box 3 speed)
      (aset box 5 0)
      (sync add-box-lock (set 'boxes (cons box boxes)))
      (fork-with-priority priority (forever
                                    (sleep-ms 32)
                                    (move-box box)))))

(define (clear-screen)
    (fill-rect back-buffer 0@0 screen-size 0xffffffff))

(define (flip-buffer)
    (blit-to-screen back-buffer 0@0 100 0))

(define (draw-image-with-scale-and-rotation img at sc rot)
    (blit img back-buffer at 0@0 (make-point (image-width img) (image-height img)) sc rot))


(define (draw-one-box box)
    (let* ((x (aget box 0))
           (y (aget box 1))
           (sc (aget box 5)))
      (draw-image-with-scale-and-rotation ufo (make-point x y)
                                          (+ 0.15 (* 0.02 (cos (* 0.5 sc))))
                                          (* 12.0 (cos sc)))))

(define (draw-curr-boxes)
    (let ((step 0))
      (forever
       (clear-screen)
       (let* ((scale (/ screen-width 2.0 (image-width city)))
              (w (f->i (* (image-width city) scale)))
              (offs (% step w))
              (bottom (f->i (- screen-height (* scale (image-height city)))))
              (image-size (make-point (image-width city) (image-height city))))
         (blit city back-buffer (make-point (+ (* -1 w) offs) bottom) 0@0 image-size scale 0.0)
         (blit city back-buffer (make-point (+ 0 offs) bottom) 0@0 image-size scale 0.0)
         (blit city back-buffer (make-point (+ w offs) bottom) 0@0 image-size scale 0.0))
       (dolist (box boxes)
         (draw-one-box box))
       (flip-buffer)
       (update-display)
       (set! step (+ 1 step))
       (sleep-ms 5))))

(define (onmousedown p) (add-box p))
(define (onmousedrag p) (add-box p) (set 'mouse-position p))
(define (onmousemove p) (set 'mouse-position p))

(fork-with-priority 100000 (draw-curr-boxes))

(define (somewhere-on-screen)
    (make-point (random screen-width) (random screen-height)))

(fork-with-priority 200 (forever (add-box (somewhere-on-screen))
                                 (sleep-ms 1000)))

(let ((pkg *package*))
  (fork-with-priority 50
   (binding ((*package* pkg))
            (forever (sleep-ms 2000)
                     (print (thread-count))))))

(request-display screen-width screen-height)
