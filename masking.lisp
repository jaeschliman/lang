(set-symbol-value 'set set-symbol-value)
(set 'ignore1 (lambda (_)))
(set 'cow (load-image "./res/cow.png"))
(set 'texture (make-image 500 500))
(fill-rect texture 0@0 500@500 0xffff0000)
(blit cow texture -100@-100 0@0 500@500 1.0 0.0)
(set 'buffer (make-image 500 500))
(set 'font (load-image "./res/charmap-futuristic_black.png"))
(set 'font-extent (make-point (image-width font) (image-height font)))
(set 'mask (make-image (image-width font) (image-width font)))
(set 'mask-extent (make-point (image-width mask) (image-height mask)))
(fill-rect mask 0@0 mask-extent 0xffffffff)
(blit font mask 0@0 0@0 font-extent 1.0 0.0)

(set 'screen-size 0@0)
(set 'mouse-pos 0@0)

(define (display)
               (screen-fill-rect 0@0 screen-size 0xff00cccc)
               (fill-rect buffer 0@0 500@500 0xff00ff00)
               (blit-with-mask
                texture buffer font 0@0
                0@0 500@500 1.0 0.0
                0@0 font-extent 3.3 0.0)
               (blit-to-screen mask 500@0 100 0)
               (blit-to-screen font 800@0 100 0)
               (blit-to-screen buffer 0@0 100 0))

(define (update-cow)
    (fill-rect texture 0@0 500@500 0xffff0000)
  (blit cow texture mouse-pos 0@0 500@500 1.0 0.0))

(define (update-mousepos p)
    (let ((x (%i (-i (point-x p) 250) 500))
          (y (%i (-i (point-y p) 250) 500)))
      (set 'mouse-pos (make-point x y))))

(set 'onshow (lambda (w h)
               (set 'screen-size (make-point w h)))) 

(set 'onmousemove update-mousepos)
(set 'onmousedown ignore1)
(set 'onmousedrag ignore1)
(set 'onkey ignore1)
(set 'onframe (lambda (dt)
                (update-cow)
                (display)))
