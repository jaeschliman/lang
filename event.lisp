(set-symbol-value 'ignore1 (lambda (_)))
(set-symbol-value 'onkey (lambda (code)
                           (print '(you pressed a key))
                           (print code)))

(set-symbol-value 'draw-boxes #f)
(set-symbol-value 'draw-next-box
                  (lambda (top colors)
                    (let ((top top) ;; BUG!!
                          (colors colors) ;;BUG!!
                          (color (car colors))
                          (ul (make-point 0 top))
                          (lr (make-point 30 (+ top 30))))
                      (fill-rect ul lr color)
                      (draw-boxes (+ top 30) (cdr colors)))))
(set-symbol-value 'draw-boxes
                  (lambda (top boxes)
                    (if (not (nil? boxes))
                        (draw-next-box top boxes))))

(set-symbol-value 'colors
                  '(0xff0000
                    0xffff00
                    0x00ff00
                    0x00ffff
                    0x0000ff
                    0xff00ff
                    0xffffff
                    0x000000))

(set-symbol-value 'onshow (lambda (w h)
                            (draw-boxes 0 colors)))

(set-symbol-value 'selected-color (car colors))

;; noticed a bug here where attempting to use p in a let and in body resulted
;; in triggering an assertion. time to write some tests.
(set-symbol-value 'paint
                  (lambda (p)
                    (fill-rect p (point+ p 15@15) selected-color)))

(set-symbol-value 'choose-color
                  (lambda (p)
                    (let ((p p)
                          (idx (/ (point-y p) 30)))
                      (let ((color (nth colors idx)))
                        (if (not (nil? color))
                            (set-symbol-value 'selected-color color))))))

(set-symbol-value 'mouse-handler
                  (lambda (p)
                    (if (< (point-x p) 30)
                        (choose-color p)
                        (paint p))))

(set-symbol-value 'onmousemove ignore1)
;; (set-symbol-value 'onmousedrag set-pixel)
(set-symbol-value 'onmousedrag mouse-handler)
