(defmacro repeat (count & forms)
  `(let ((loop #f))
     (set! loop (lambda (c) ,@forms
                        (when (>i c 0)
                          (loop (-i c 1)))))
     (loop ,count)))

(defmacro repeat-slowly (delay count & forms)
  `(repeat ,count ,@forms (sleep-ms ,delay)))

(print '(one moment please))
(let ()
  (sleep-ms 500)
  (print "zzzz"))
(define (snooze) (sleep-ms 500))
;; (sleep-ms 1000)
(snooze)
(print '(after sleeping))

(define (say-bye)
    (repeat-slowly 100 10 (print '(world)))
  (print 'the-end))
(fork (repeat-slowly 100 10 (print '(!!!!!))))
(fork (say-bye))
(fork (repeat-slowly 100 10 (print '(hello))))
(print 'goodbye)
'done
