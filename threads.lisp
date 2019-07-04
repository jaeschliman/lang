
(define (lambda->continuation fn)
    (set-stack-mark 'helper)
  (let ((r ((lambda ()
              (snapshot-to-stack-mark 'helper '())
              (fn)))))
    r))

(define cont (lambda->continuation (lambda () (print '(hello world!)))))

(defmacro fork (& forms)
  `(fork-continuation (lambda->continuation (lambda () (let ((r (let () ,@forms))) r)))))

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

;; this has to be run via the metacompiler,
;; becuase the cpp compiler re-runs the interp loop for each form,
;; and the 'preemption' doesn't work correctly that way.
;; going to take a good bit of thought to figure out how to do this with
;; with the events based system...
(define (say-bye)
    (repeat 200 (print 'world))
  (print 'the-end))
(fork (repeat 200 (print '!!!!!)))
(fork (repeat 200 (print 'hello))) ;; changing this 200 to a 800 'removes' the crash
(fork (say-bye))
(print 'goodbye)
'done
