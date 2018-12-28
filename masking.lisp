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

(set 'onshow (lambda (w h)
               (screen-fill-rect 0@0 (make-point w h) 0xff00cccc)
               (fill-rect buffer 0@0 500@500 0xff00ff00)
               (blit-with-mask
                texture buffer font 0@0
                0@0 500@500 1.0 0.0
                0@0 font-extent 3.3 0.0)
               (blit-to-screen mask 500@0 100 0)
               (blit-to-screen font 800@0 100 0)
               (blit-to-screen buffer 0@0 100 0)))

(set 'onmousemove ignore1)
(set 'onmousedown ignore1)
(set 'onmousedrag ignore1)
(set 'onkey ignore1)
