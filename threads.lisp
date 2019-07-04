
(define (lambda->continuation fn)
    (set-stack-mark 'helper)
  (let ((r ((lambda ()
              (snapshot-to-stack-mark 'helper '())
              (fn)))))
    r))

(define cont (lambda->continuation (lambda () (print '(hello world!)))))

(defmacro fork (& forms)
  `(fork-continuation (lambda->continuation (lambda () ,@forms))))

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


;; still haven't got termination worked out yet.
(fork (forever (print 'hello)))
;; (fork (repeat 1000 (print '!!!!!))) -- not working yet
(fork (forever (print '!!!!!)))
(forever (print 'world)) ;; required

'done
