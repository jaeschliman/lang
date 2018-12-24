(set-symbol-value 'ignore1 (lambda (_)))
(set-symbol-value 'onkey (lambda (code)
                           (print '(you pressed a key))
                           (print code)))

(set-symbol-value 'onmousemove ignore1)
(set-symbol-value 'onmousedrag set-pixel)
