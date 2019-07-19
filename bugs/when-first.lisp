;; change the first `when` to an `if` and it goes away

(define (bug a b)
    (let () (print b))
  (let ((a '()))
    (print b)
    (let* ((a '())
           (a '())
           (a '()))
      (dotimes (_ 3) b)
      (print b))))

(bug a b)

'done
