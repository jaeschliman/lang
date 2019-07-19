;; should be fixed

(define (bug bugme)
    (let () (print bugme))
  (lambda () bugme))

(define (nobug bugme)
    ((lambda () bugme))
  (let () bugme))

(print 'no-bug)
(nobug 'bug)
(print 'bug)
(print ((bug 'bug)))

'done
