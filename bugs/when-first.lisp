;; change the first `when` to an `if` and it goes away

(define (bug a b)
    (let () (print b))
  (let ((a '()))
    (lambda () b)))

(bug 'a 'b)

'done
