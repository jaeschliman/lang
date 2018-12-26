(set-symbol-value 'ignore1 (lambda (_)))

;; this is awkward, but don't have self-referencing definitions yet
;; also don't have loops yet!
(set-symbol-value 'draw-boxes #f)
(set-symbol-value 'draw-boxes
                  (lambda (top size colors)
                    (if (not (nil? colors))
                        (let ((color (car colors))
                              (ul (make-point 0 top))
                              (lr (make-point size (+i top size))))
                          (fill-rect ul lr color)
                          (draw-boxes (+i top size) size (cdr colors))))))

(set-symbol-value 'cow (load-image "/Users/jsn/Downloads/cow.png"))
;; (set-symbol-value 'cow (load-image "/Users/jsn/Downloads/test_pattern.png"))

(set-symbol-value 'colors
                  '(0xff0000
                    0xffff00
                    0x00ff00
                    0x00ffff
                    0x0000ff
                    0xff00ff
                    0xffffff
                    0x000000))

(set-symbol-value 'color-well-size #f)

(set-symbol-value 'selected-color (car colors))

(set-symbol-value 'paint
                  (lambda (p) (fill-rect p (point+ p 15@15) selected-color)))

(set-symbol-value 'choose-color
                  (lambda (p)
                    (let ((idx (/i (point-y p) color-well-size)))
                      (let ((color (nth colors idx)))
                        (if (not (nil? color))
                            (set-symbol-value 'selected-color color))))))

(set-symbol-value 'scale (lambda (s v) (/i (*i s v) 100)))

(set-symbol-value 'center-image-at (lambda (p s half-img-width)
                                  (let ((amt (+i half-img-width
                                                (scale s half-img-width))))
                                    (point- p (make-point amt amt)))))

(set-symbol-value 'draw-cows #f)
(set-symbol-value 'draw-cows
                  (lambda (p s r)
                    (blit-at cow (center-image-at p s 250)
                             s (point-x p))
                    (if (>i s 30)
                        (let ()
                          (draw-cows (point+ p -70@-50)
                                     (scale s 70)
                                     (-i r 33))
                          (draw-cows (point+ p -70@50)
                                     (scale s 60)
                                     (+i r 80))
                          (draw-cows (point+ p 50@50)
                                     (scale s 80)
                                     (+i r 33))))))

(set-symbol-value 'mouse-handler
                  (lambda (p)
                    (if (<i (point-x p) color-well-size)
                        (choose-color p)
                        (let ((s (+i 50 (/i (point-y p) 24))))
                          (fill-rect 0@0 640@480 0xffffff)
                          (draw-cows p s (point-x p))
                          (paint p)))))

;;;;;;; register event handlers

(set-symbol-value 'onshow (lambda (w h)
                            (set-symbol-value 'color-well-size (/i h 8))
                            (draw-boxes 0 color-well-size colors)))

(set-symbol-value 'onmousemove ignore1)
;; (set-symbol-value 'onmousedrag set-pixel)
(set-symbol-value 'onmousedown mouse-handler)
(set-symbol-value 'onmousedrag mouse-handler)
(set-symbol-value 'onkey (lambda (code)
                           (print '(you pressed a key))
                           (print code)))
