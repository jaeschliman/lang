
(define (thing-a a b c)
    (print (thread-get-debug-info (current-thread)))
  '())

(define (thing-b a b c)
    (thing-a c b a)
  '())

(define (thing-c a b c d)
    (thing-b a b c) 
  '())

(let ()
  (thing-c 0 1 2 3)
  '())

'done
