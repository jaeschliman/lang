(defmacro forever (& forms)
  `(let ((loop #f))
     (set! loop (lambda () ,@forms (loop)))
     (loop)))

(defmacro repeat (count & forms)
  `(let ((loop #f))
     (set! loop (lambda (c) ,@forms
                        (when (>i c 0)
                          (loop (-i c 1)))))
     (loop ,count)))

(defmacro repeat-slowly (delay count & forms)
  `(repeat ,count ,@forms (sleep-ms ,delay)))


(print '(one moment please))
(sleep-ms 1000)

(define (say-bye)
    (repeat-slowly 500 200 (print '(world)))
  (print 'the-end))
(fork (repeat-slowly 300 200 (print '(!!!!!))))
(fork (say-bye))
(fork (repeat-slowly 200 200 (print '(hello))))
(print 'goodbye)
'done
