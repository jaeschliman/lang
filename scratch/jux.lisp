(define (run fn) (reset-tag 'jux (fn)))

(define jux (lambda vals (shift-tag 'jux app (dolist (v vals) (app v)))))

(define (show x) (print `(showing: ,x)))

(define (frob x) (print `(frobbing: ,x)))

(run (lambda ()
       (if (jux #t #f)
           ((jux show frob) (jux 'a 'b 'c))
           ((jux frob show) (jux 'the 'false 'branch)))))
