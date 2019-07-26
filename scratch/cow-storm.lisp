
(define screen-width 1280)
(define screen-height 720)
(define screen-size (make-point screen-width screen-height))
(define curr-point (make-point (/i screen-width 2)
                               (/i screen-height 2)))

(set 'cow (load-image "./res/cow.png"))

(set 'scale (lambda (s v) (/i (*i s v) 100)))

(set 'center-image-at (lambda (p s img)
                        (let ((half-img-width (/i (image-width img) 2)))
                          (let ((amt (scale s half-img-width)))
                            (point- p (make-point amt amt))))))

(set 'ctx-idx 0)
(set 'ctx (make-array 255))
(aset ctx 0 curr-point)
(aset ctx 1 0.0)

(set 'push-t-r (lambda (p r)
                 (let ((prev-p (aget ctx ctx-idx))
                       (prev-r (aget ctx (+i ctx-idx 1))))
                   (let ((new-p (point+ prev-p (point-rotate p prev-r)))
                         (new-r (*f -1.0 (+f prev-r r))))
                     (set 'ctx-idx (+i ctx-idx 2))
                     (aset ctx ctx-idx new-p)
                     (aset ctx (+i 1 ctx-idx) new-r)))))

(set 'pop-t-r (lambda () (set 'ctx-idx (-i ctx-idx 2))))

(set 'draw-cows #f)
(set 'draw-cows
     (lambda (s)
       (let ((p (aget ctx ctx-idx))
             (r (aget ctx (+i ctx-idx 1))))
         (blit-to-screen cow (center-image-at p s cow) s (f->i r))
         (if (>i s 20)
             (let ()
               (push-t-r -170@-150 -33.0)
               (draw-cows (scale s 75))
               (pop-t-r)
               (push-t-r -100@-300 45.0)
               (draw-cows (scale s 40))
               (pop-t-r)
               (push-t-r 100@-70 122.0)
               (draw-cows (scale s 50))
               (pop-t-r)
               (push-t-r  250@90 33.0)
               (draw-cows (scale s 40))
               (pop-t-r))))))

(set 'cow-mania (lambda (s)
                  (draw-cows s)))

(define (draw-frame dt)
    (screen-fill-rect 0@0 screen-size 0xffffffff)
  (let ((p curr-point))
    (let ((s (+i 80 (/i (point-y p) 24)))
          (r (%f (+f (*f 100.0 dt) (aget ctx 1)) 360.0)))
      (aset ctx 1 r)
      (cow-mania s)))
  (when #t
    (let ((p curr-point))
      (let* ((s (+i 80 (/i (point-y p) 24)))
             (old-r (aget ctx 1))
             (r (%f (+f 33.0 (aget ctx 1)) 360.0)))
        (aset ctx 1 r)
        (cow-mania s)
        (aset ctx 1 old-r)))))

(set 'mouse-handler
     (lambda (p)
       (set 'curr-point p)
       (aset ctx 0 p)))

(set 'onmousemove mouse-handler)
(set 'onmousedown mouse-handler)
(set 'onmousedrag mouse-handler)

(fork-with-priority 100 (forever (draw-frame (/f 1.0 60.0)) (sleep-ms 10)))

(request-display screen-width screen-height)
