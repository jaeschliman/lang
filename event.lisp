(set-symbol-value 'ignore1 (lambda (_)))
(set-symbol-value 'onkey (lambda (code)
                           (print '(you pressed a key))
                           (print code)))

(set-symbol-value 'draw-boxes #f)
(set-symbol-value 'draw-next-box
                  (lambda (top size colors)
                    (let ((top top) ;; BUG!!
                          (colors colors) ;;BUG!!
                          (size size) ;; BUG!!
                          (color (car colors))
                          (ul (make-point 0 top))
                          (lr (make-point size (+ top size))))
                      (fill-rect ul lr color)
                      (draw-boxes (+ top size) size (cdr colors)))))
(set-symbol-value 'draw-boxes
                  (lambda (top size boxes)
                    (if (not (nil? boxes))
                        (draw-next-box top size boxes))))

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
(set-symbol-value 'onshow (lambda (w h)
                            (set-symbol-value 'color-well-size (/ h 8))
                            (draw-boxes 0 color-well-size colors)))

(set-symbol-value 'selected-color (car colors))

;; noticed a bug here where attempting to use p in a let and in body resulted
;; in triggering an assertion. time to write some tests.
(set-symbol-value 'paint
                  (lambda (p)
                    (fill-rect p (point+ p 15@15) selected-color)))

(set-symbol-value 'choose-color
                  (lambda (p)
                    (let ((p p) ;;BUG!
                          (idx (/ (point-y p) color-well-size)))
                      (let ((color (nth colors idx)))
                        (if (not (nil? color))
                            (set-symbol-value 'selected-color color))))))

(set-symbol-value 'mouse-handler
                  (lambda (p)
                    (if (< (point-x p) color-well-size)
                        (choose-color p)
                        (paint p))))

;; TODO: onmousedown
(set-symbol-value 'onmousemove ignore1)
;; (set-symbol-value 'onmousedrag set-pixel)
(set-symbol-value 'onmousedrag mouse-handler)
