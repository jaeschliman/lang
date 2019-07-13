(define (cons-up) (list 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9))
(define (cons-more) (cons-up) (cons-up) (cons-up) (cons-up) (cons-up))
(define (cons-alot) (cons-more) (cons-more) (cons-more) (cons-more) (cons-more) (cons-more) (cons-more))
(fork (forever (cons-alot)))
(fork (forever (cons-alot)))
(fork (forever
       (sleep-ms 500)
       (print `(hello there))))
