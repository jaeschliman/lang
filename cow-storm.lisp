(set-symbol-value 'set set-symbol-value)
(set 'ignore1 (lambda (_)))

(set 'cow (load-image "./res/cow.png"))

(set 'scale (lambda (s v) (/i (*i s v) 100)))

(set 'center-image-at (lambda (p s img)
                        (let ((half-img-width (/i (image-width img) 2)))
                          (let ((amt (scale s half-img-width)))
                            (point- p (make-point amt amt))))))

;; TODO: replace with an extendable array
;; (set 'ctx (list (cons 0@0 0)))
(set 'ctx-idx 0)
(set 'ctx (make-array 255))
(aset ctx 0 0@0)
(aset ctx 1 0)

(set 'push-t-r (lambda (p r)
                 (let ((prev-p (aget ctx ctx-idx))
                       (prev-r (aget ctx (+i ctx-idx 1))))
                   (let ((new-p (point+ prev-p (point-rotate p (i->f prev-r))))
                         (new-r (*i -1  (+i prev-r r))))
                     (set 'ctx-idx (+i ctx-idx 2))
                     (aset ctx ctx-idx new-p)
                     (aset ctx (+i 1 ctx-idx) new-r)))))

(set 'pop-t-r (lambda () (set 'ctx-idx (-i ctx-idx 2))))

(set 'draw-cows #f)
(set 'draw-cows
     (lambda (s)
       (let ((p (aget ctx ctx-idx))
             (r (aget ctx (+i ctx-idx 1))))
         (blit-to-screen cow (center-image-at p s cow) s r)
         (if (>i s 30)
             (let ()
               (push-t-r -170@-150 -33)
               (draw-cows (scale s 70))
               (pop-t-r)
               (push-t-r -100@-300 173)
               (draw-cows (scale s 50))
               (pop-t-r)
               (push-t-r 100@-70 80)
               (draw-cows (scale s 60))
               (pop-t-r)
               (push-t-r  250@90 33)
               (draw-cows (scale s 80))
               (pop-t-r))))))

(set 'screen-size #f)

(set 'cow-mania (lambda (s)
                  (screen-fill-rect 0@0 screen-size 0xffffff)
                  (draw-cows s)))

(define curr-point #f)

(define (draw-frame dt)
    (let ((p curr-point))
      (let ((s (+i 80 (/i (point-y p) 24)))
            (r (%i (+i 2 (aget ctx 1)) 360)))
        (aset ctx 1 r)
        (cow-mania s))))

(set 'mouse-handler
     (lambda (p)
       (set 'curr-point p)
       (aset ctx 0 p)))

;;;;;;; register event handlers

(set 'onshow (lambda (w h)
               (set 'screen-size (make-point w h))
               (let ((mid (make-point (/i w 2) (/i h 2))))
                 (set 'curr-point mid)
                 (aset ctx 0 mid))))

(set 'onmousemove mouse-handler)
(set 'onmousedown mouse-handler)
(set 'onmousedrag mouse-handler)
(set 'onkey ignore1)
(set 'onframe draw-frame)
