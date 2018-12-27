(set-symbol-value 'set set-symbol-value)
(set 'ignore1 (lambda (_)))

(set 'cow (load-image "./res/cow.png"))

(set 'scale (lambda (s v) (/i (*i s v) 100)))

(set 'center-image-at (lambda (p s img)
                        (let ((half-img-width (/i (image-width img) 2)))
                          (let ((amt (scale s half-img-width)))
                            (point- p (make-point amt amt))))))

(set 'ctx (list))
(set 'push-t-r (lambda (p r)
                 (set 'ctx (cons (cons p r) ctx))))

(set 'ctx (cons (cons 0@0 0) ctx))

(set 'push-t-r (lambda (p r)
                (let ((prev-p (car (car ctx)))
                      (prev-r (cdr (car ctx))))
                  (set 'ctx
                       (cons (cons (point+ prev-p (point-rotate p (i->f prev-r)))
                                   (+i prev-r r))
                             ctx)))))


(set 'pop-t-r (lambda () (set 'ctx (cdr ctx))))

(set 'draw-cows #f)
(set 'draw-cows
     (lambda (s)
       (let ((p (car (car ctx)))
             (r (cdr (car ctx))))
         (blit-to-screen cow (center-image-at p s cow)
                s (point-x p))
         (if (>i s 30)
             (let ()
               (push-t-r -170@-150 -33)
               (draw-cows (scale s 70))
               (pop-t-r)
               (push-t-r 100@-70 80)
               (draw-cows (scale s 60))
               (pop-t-r)
               (push-t-r  250@90 33)
               (draw-cows (scale s 80))
               (pop-t-r))))))

(set 'screen-size #f)

(set 'cow-mania (lambda (p s)
                  (screen-fill-rect 0@0 screen-size 0xffffff)
                  (push-t-r p (point-x p))
                  (draw-cows s)
                  (pop-t-r)))

(set 'mouse-handler
     (lambda (p)
       (let ((s (+i 80 (/i (point-y p) 24))))
         (cow-mania p s))))

;;;;;;; register event handlers

(set 'onshow (lambda (w h) (set 'screen-size (make-point w h))))

(set 'onmousemove mouse-handler)
(set 'onmousedown mouse-handler)
(set 'onmousedrag mouse-handler)
(set 'onkey ignore1)
