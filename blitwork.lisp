(set-symbol-value 'set set-symbol-value)
(set 'ignore1 (lambda (_)))

(set 'colors
     '(0xffff0000
       0xffffff00
       0xff00ff00
       0xff00ffff
       0xff0000ff
       0xffff00ff
       0xffffffff
       0xff000000))

(set 'selected-color (car colors))
(set 'screen-size #f)

(set 'cow (load-image "./res/cow.png"))
(set 'magnifier (make-image 128 128))
(set 'scale-factor (i->f (/i 128 32)))

(set 'magnify (lambda (at-point)
                (let ((p (point- at-point 16@16)))
                  (blit-from-screen
                   magnifier
                   0@0 p (point+ p 32@32)
                   scale-factor 
                   0.0)
                  (blit-to-screen
                   magnifier
                   (point- screen-size 256@256)
                   200 0))))


;;;;;;; register event handlers

(set 'onshow (lambda (w h)
               (set 'screen-size (make-point w h))
               (screen-fill-rect 0@0
                                 (make-point (image-width cow) (image-height cow))
                                 selected-color)
               (blit-to-screen cow 0@0 100 0)))

(set 'onmousemove magnify)
(set 'onmousedown ignore1)
(set 'onmousedrag ignore1)
(set 'onkey ignore1)
