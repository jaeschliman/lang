(set-symbol-value 'ignore1 (lambda (_)))

;; this is awkward, but don't have self-referencing definitions yet
;; also don't have loops yet!
(set-symbol-value 'draw-boxes #f)
(set-symbol-value 'draw-boxes
                  (lambda (top size colors)
                    (if (not (nil? colors))
                        (let ((color (car colors))
                              (ul (make-point 0 top))
                              (lr (make-point size (+ top size))))
                          (fill-rect ul lr color)
                          (draw-boxes (+ top size) size (cdr colors))))))

(set-symbol-value 'cow (load-image "/Users/jsn/Downloads/cow.png"))

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
                    (let ((idx (/ (point-y p) color-well-size)))
                      (let ((color (nth colors idx)))
                        (if (not (nil? color))
                            (set-symbol-value 'selected-color color))))))

(set-symbol-value 'mouse-handler
                  (lambda (p)
                    (if (< (point-x p) color-well-size)
                        (choose-color p)
                        (let ()
                          ;; (fill-rect 0@0 640@480 0xffffff)
                          (blit-image cow (point- p 250@250))
                          (paint p)))))

;;;;;;; register event handlers

(set-symbol-value 'onshow (lambda (w h)
                            (set-symbol-value 'color-well-size (/ h 8))
                            (draw-boxes 0 color-well-size colors)))

(set-symbol-value 'onmousemove ignore1)
;; (set-symbol-value 'onmousedrag set-pixel)
(set-symbol-value 'onmousedown mouse-handler)
(set-symbol-value 'onmousedrag mouse-handler)
(set-symbol-value 'onkey (lambda (code)
                           (blit-image cow 0@0)
                           (print '(you pressed a key))
                           (print code)))
