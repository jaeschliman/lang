;;  built in classes --------------------------------------------------------------

(define (set-class-name class name)
    (if (class? class)
        (instance-set-ivar class 0 name)
        (throw "in set-class-name, class is not a class")))

(define         Continuation (class-of (lambda->continuation (lambda ()))))
(set-class-name Continuation 'Continuation)
(define         Fixnum       (class-of 0))
(set-class-name Fixnum       'Fixnum)
(define         Float        (class-of 0.0))
(set-class-name Float        'Float)
(define         String       (class-of ""))
(set-class-name String       'String)
(define         Symbol       (class-of 'symbol))
(set-class-name Symbol       'Symbol)
(define         Closure      (class-of (lambda ())))
(set-class-name Closure      'Closure)
(define         PrimOp       (class-of class-of))
(set-class-name PrimOp       'PrimOp)
(define         Image        (class-of (make-image 0 0)))
(set-class-name Image        'Image)
(define         Point        (class-of 0@0))
(set-class-name Point        'Point)
(define         Array        (class-of (make-array 0)))
(set-class-name Array        'Array)
(define         HashTable    (class-of (make-ht)))
(set-class-name HashTable    'HashTable)
(define         Null         (class-of '()))
(set-class-name Null         'Null)
(define         Cons         (class-of (cons 0 0)))
(set-class-name Cons         'Cons)
(define         Boolean      (class-of #t))
(set-class-name Boolean      'Boolean)
(define         Character    (class-of #\0))
(set-class-name Character    'Character)
(define         Semaphore    (class-of (make-semaphore 0)))
(set-class-name Semaphore    'Semaphore)
(define         Thread       (class-of (current-thread)))
(set-class-name Thread       'Thread)

(define         FileOutputStream (class-of *standard-output*))
(set-class-name FileOutputStream 'FileOutputStream)

(define         Class        (class-of (class-of 0)))
(set-class-name Class        'Class)

(define (isa? obj class) ;; since we don't support class inheritance yet, this will do
    (eq (class-of obj) class))

'done
