(set-symbol-value 'onkey (lambda (code)
                           (print '(you pressed a key))
                           (print code)))

(set-symbol-value 'onmousemove (lambda (x y)
                                 (print 'mousemove)
                                 (print x)
                                 (print y)))
