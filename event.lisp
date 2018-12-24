(set-symbol-value 'ignore1 (lambda (_)))
(set-symbol-value 'onkey (lambda (code)
                           (print '(you pressed a key))
                           (print code)))

;; noticed a bug here where attempting to use p in a let and in body resulted
;; in triggering an assertion. time to write some tests.
(set-symbol-value 'draw-rectangles
                  (lambda (p)
                    (fill-rect (point- p 15@15) p                0xff)
                    (fill-rect p                (point+ p 15@15) 0xff00)
                    (fill-rect (point- p 0@15)  (point+ p 15@0)  0xffff00)
                    (fill-rect (point- p 15@0)  (point+ p 0@15)  0xff0000)))

(set-symbol-value 'onmousemove ignore1)
;; (set-symbol-value 'onmousedrag set-pixel)
(set-symbol-value 'onmousedrag draw-rectangles)
