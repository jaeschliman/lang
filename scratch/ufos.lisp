(defmacro sync (sem & body)
  `(let ()
     (semaphore-wait ,sem)
     ,@body
     (signal-semaphore ,sem)))

(define add-box-lock (make-semaphore #t))

(define (min a b) (if (<= a b) a b))

(define mouse-position 0@0)
(define boxes '())
(define screen-width (f->i (* 1920 0.75)))
(define screen-height (f->i (* 1080 0.75)))
(define (somewhere-on-screen) (make-point (random screen-width) (random screen-height)))
(define screen-size (make-point screen-width screen-height))
(define back-buffer (make-image screen-width screen-height))

(define (scale-image s img)
    (let* ((w (f->i (* s (image-width img))))
           (h (f->i (* s (image-height img))))
           (r (make-image w h)))
      (blit img r 0@0 0@0 (make-point (image-width img) (image-height img)) s 0.0 0xffffffff)
      r))

(define ufo (scale-image 0.25 (load-image "./res/ufo.png")))
(define cow (scale-image 0.25 (load-image "./res/cow.png")))
(define sky (load-image "./res/sky.png"))
(define city (load-image "./res/buildings.png"))
(define beam (load-image "./res/beam.png"))


(define (point-angle p)
    (atan2f (i->f (point-y p))
            (i->f (point-x p))))

(define (point-length p)
    (let ((a (point-x p))
          (b (point-y p)))
      (sqrtf (i->f (+ (* a a) (* b b))))))

(define (chance n)
    (= 1 (random n)))

(define (draw-beam a b)
    (let* ((angle (point-angle (point- b a)))
           (right (point-rotate (make-point -5 0) (+ angle 45)))
           (left (point-rotate (make-point 5 0) (+ angle 45)))
           (bottom-right (point-rotate (make-point -15 0) (+ angle 45)))
           (bottom-left (point-rotate (make-point 15 0) (+ angle 45))))
      (blitq beam back-buffer
             0@0 (make-point (image-width beam) 0)
             (make-point 0 (image-height beam)) (make-point (image-width beam) (image-height beam))
             (point+ a left)
             (point+ a right)
             (point+ b bottom-left)
             (point+ b bottom-right))))

(define (move-box box)
    (let* ((x (aget box 0))
           (y (aget box 1))
           (angle (aget box 2))
           (speed (aget box 3))
           (a-target (aget box 4))
           (target (if (point? a-target) a-target
                       (make-point (aget a-target 0) (aget a-target 1))))
           (accel (aget box 6))
           (to-m (point- target (make-point x y)))
           (sc (+i 1 (aget box 5)))
           (p  (point-rotate (make-point (f->i speed) 0) (* 57.2958 angle)))
           (dp (point-rotate (make-point accel 0) (* 57.2958 (point-angle to-m))))
           (np (point+ p dp))
           (dx (point-x np))
           (dy (point-y np))
           (nx (+i x dx))
           (ny (+i y dy)))
      (if (< (point-length (point- (make-point nx ny) target)) 8)
          (aset box 4 (if (chance 2) (somewhere-on-screen) (car boxes)))
          (if (not (point? a-target))
              (aset a-target 4 box)))
      (aset box 0 nx)
      (aset box 1 ny)
      (aset box 2 (point-angle np))
      (aset box 3 (min 20.0 (point-length np)))
      (aset box 5 sc)))

(define (random-angle) (* 0.01 (random 3141)))
(define (random-speed) (+ 1.0 (random 10)))
(define (random-accel) (+ 4 (random 10)))

(define (add-box p &opt
                 (angle (random-angle))
                 (speed (random-speed))
                 (accel (random-accel))
                 (priority 0))
    (let ((box (make-array 7))
          (other (car boxes)))
      (aset box 0 (point-x p)) ;; x
      (aset box 1 (point-y p)) ;; y
      (aset box 2 angle)
      (aset box 3 speed)
      (aset box 4 (if (or (chance 5) (nil? other)) (somewhere-on-screen) other)) ;; target
      (aset box 5 0) ;; step count
      (aset box 6 accel)
      (sync add-box-lock (set 'boxes (cons box boxes)))
      (fork-with-priority priority (forever
                                    (sleep-ms 32)
                                    (move-box box)))))

(define (clear-screen)
    (fill-rect back-buffer 0@0 screen-size 0xffffffff))

(define (flip-buffer)
    (blit-to-screen back-buffer 0@0 100 0))

(define (draw-image-with-scale-and-rotation img at sc rot)
    (let* ((w (image-width img))
           (h (image-height img))
           (sw (* w sc))
           (sh (* h sc))
           (ofs (make-point (f->i (* sw -0.5)) (f->i (* sh -0.5)))))
      (blit img back-buffer (point+ at ofs)
            0@0 (make-point w h) sc rot 0xffffffff)))


(define (draw-one-box box)
    (let* ((x (aget box 0))
           (y (aget box 1))
           (a-target (aget box 4))
           (target (if (point? a-target) #f
                       (make-point (aget a-target 0) (aget a-target 1))))
           (spd (/ (min 40 (aget box 3)) 40.0))
           (sc (aget box 5)))
      (when target
        (unless (or (= x (point-x target))
                    (= y (point-y target))
                    (= 0 (% sc 2)))
          (draw-beam (make-point x y) target)))
      (draw-image-with-scale-and-rotation ufo (make-point x y)
                                          (* 4.0 (+ 0.05
                                                    (* 0.15 spd)
                                                    (* 0.02 (cos (* 0.5 sc)))))
                                          (* 12.0 (cos sc)))))

(define (draw-curr-boxes)
  (let ((step 0))
    (forever
     (clear-screen)
     (let ((s (/ screen-width 1.0 (image-width sky))))
       (draw-image-with-scale-and-rotation sky (make-point (/ screen-width 2) (/ screen-height 2))
                                           (+ s (* 0.22 (abs (sin (* step 0.01)))))
                                           0.0))
     (let* ((scale (/ screen-width 2.0 (image-width city)))
            (w (f->i (* (image-width city) scale)))
            (offs (% step w))
            (bottom (f->i (- screen-height (* scale (image-height city)))))
            (image-size (make-point (image-width city) (image-height city)))
            (draw (lambda (p)
                    (blit city back-buffer p 0@0 image-size scale 0.0 0xffffffff))))
       (draw (make-point (+ (* -1 w) offs) bottom))
       (draw (make-point (+ 0 offs) bottom))
       (draw (make-point (+ w offs) bottom)))
     (dolist (box boxes)
       (draw-one-box box))
     (dotimes (i 16)
       (let* ((x (- (* i 100) (% step 100)))
              (y (- screen-height 75 (* (sin x) 10))))
         (blit cow back-buffer (make-point x (f->i y))
               0@0 (make-point (image-width cow) (image-height cow))
               0.9 (* (cos (* x 0.105 1.5)) 13.0) 0xffffffff)))
     (flip-buffer)
     (update-display)
     (set! step (+ 1 step))
     (sleep-ms 5))))

(define (onmousedown p) (add-box p))
(define (onmousedrag p) (add-box p) (set 'mouse-position p))
(define (onmousemove p) (set 'mouse-position p))

(fork-with-priority 100000 (draw-curr-boxes))


(fork-with-priority 200 (forever (add-box (somewhere-on-screen))
                                 (sleep-ms 1000)))

(let ((pkg *package*))
  (fork-with-priority 50
   (binding ((*package* pkg))
            (forever (sleep-ms 2000)
                     (print (thread-count))))))

(request-display screen-width screen-height)
