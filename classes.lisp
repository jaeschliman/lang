(define Pair (create-class 'Pair 2))
(print Pair)

(define my-pair (instantiate-class Pair))
(print my-pair)
(print (class-of my-pair))

(instance-set-ivar my-pair 0 'a)
(instance-set-ivar my-pair 1 'b)
(print (instance-get-ivar my-pair 0))
(print (instance-get-ivar my-pair 1))

'done
