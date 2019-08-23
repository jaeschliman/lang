(define Pair (create-class 'Pair '(fst rst)))
(define (fst p) (instance-get-ivar p 0))
(define (rst p) (instance-get-ivar p 1))

(define (make-pair a b)
  (let ((r (instantiate-class Pair)))
    (instance-set-ivar r 0 a)
    (instance-set-ivar r 1 b)
    r))

(define Queue (create-class 'Queue '(front back)))

(define (make)
  (let ((r (instantiate-class Queue)))
    (instance-set-ivar r 0 '())
    (instance-set-ivar r 1 '())
    r))

(define (empty? q)
  (nil? (instance-get-ivar q 0)))

(define (add q item)
  (let ((p (make-pair item '())))
    (cond ((empty? q)
           (instance-set-ivar q 0 p)
           (instance-set-ivar q 1 p))
          (#t
           (let ((last (instance-get-ivar q 1)))
             (instance-set-ivar last 1 p)
             (instance-set-ivar q 1 p))))))

(defmacro doq (binds & body)
  (let ((sym (car binds))
        (form (cadr binds))
        (cur (gensym))
        (loop (gensym)))
    `(let ,loop ((,cur (instance-get-ivar ,form 0)))
          (unless (nil? ,cur)
            (let ((,sym (fst ,cur)))
              ,@body
              (,loop (rst ,cur)))))))
