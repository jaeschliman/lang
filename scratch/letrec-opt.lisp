(set '*package* (symbol-package 'define))

(binding (;;(*trace-eval* #t)
          (*recompiling* #t))
         (%load "./scratch/compiler.lisp"))
(set '*enable-inline-letrec-bound-lambdas* #t)
(set '*note-closed-over-vars* #t)

(print 'hello-world)

(dolist (x '(1 2 3 a b c))
  (print x))

(let ((force-closure #t))
  (forever (dolist (x '(1 2 3 a b c))
             (sleep-ms 15)
             (lambda () force-closure)
             (print (%stack-depth-in-bytes)))))


(define (mappend f as)
    (let loop ((as as) (f f) (acc '()))
         (if (nil? as) (reverse-list acc)
             (let ((r acc))
               (dolist (b (reverse-list (f (car as))))
                 (set! r (cons b r)))
               (loop (cdr as) f r)))))

(define (list-to-pairs l)
    (let ((final (first l)))
      (let loop ((as l) (bs (cdr l)) (acc '()))
           (if (nil? bs) (reverse-list (cons (list (car as) final) acc))
               (loop (cdr as) (cdr bs) (cons (list (car as) (car bs)) acc))))))

(print (list-to-pairs '(a b c)))
(print (list-to-pairs '(a b c d)))
