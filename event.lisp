(set-symbol-value 'onkey (lambda (code)
                           (print '(you pressed a key))
                           (print code)))

(set-symbol-value 'onmousemove (lambda (pt)
                                 (print 'mousemove)
                                 (print pt)))
