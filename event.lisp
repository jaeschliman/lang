(set-symbol-value 'onkey (lambda (code)
                           (print '(you pressed a key))
                           (print code)))
