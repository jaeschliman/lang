;; change the first `when` to an `if` and it goes away

(define (bug bugme)
    (let () bugme)
  (lambda () bugme))

(define (nobug bugme)
    ((lambda () bugme))
  (let () bugme))

(print 'no-bug)
(nobug 'bug)
(print 'bug)
(bug 'bug)

'done
