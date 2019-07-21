(define (get-slot obj slot)
    ((class-get-metadata (class-of obj) 'slot-reader) obj slot))
(define (set-slot obj slot value)
    ((class-get-metadata (class-of obj) 'slot-writer) obj slot value))

(define Pair (create-class 'Pair 2))

(class-set-metadata Pair 'slot-reader (lambda (obj slot)
                                        (case slot
                                          (x (instance-get-ivar obj 0))
                                          (y (instance-get-ivar obj 1)))))

(class-set-metadata Pair 'slot-writer (lambda (obj slot value)
                                        (case slot
                                          (x (instance-set-ivar obj 0 value))
                                          (y (instance-set-ivar obj 1 value)))))
(print Pair)

(define my-pair (instantiate-class Pair))
(print my-pair)
(print (class-of my-pair))

(instance-set-ivar my-pair 0 'a)
(instance-set-ivar my-pair 1 'b)
(print (instance-get-ivar my-pair 0))
(print (instance-get-ivar my-pair 1))
(set-slot my-pair 'x 'A)
(set-slot my-pair 'y 'B)
(print (get-slot my-pair 'x))
(print (get-slot my-pair 'y))

'done
