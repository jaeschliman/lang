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


