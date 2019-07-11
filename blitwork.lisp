(define screen-width 750)
(define screen-height 500)

(define screen-size (make-point screen-width screen-height))

(define cow (load-image "./res/cow.png"))
(define magnifier (make-image 128 128))
(define scale-factor (i->f (/i 128 32)))

(define (magnify at-point)
    (let ((p (point- at-point 16@16)))
      (blit-from-screen
       magnifier
       0@0 p (point+ p 32@32)
       scale-factor 
       0.0)
      (blit-to-screen
       magnifier
       (point- screen-size 256@256)
       200 0)))


(define (onshow size)
     (screen-fill-rect 0@0
                       (make-point (image-width cow) (image-height cow))
                       0xffffffff)
     (blit-to-screen cow 0@0 100 0))

(define onmousemove magnify)

(request-display screen-width screen-height)
