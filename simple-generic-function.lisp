(define GenericFunction1 (create-class 'GenericFunction1 1))

(define (make-generic-function1)
    (let ((result (instantiate-class GenericFunction1)))
      (instance-set-ivar result 0 (make-ht))
      result))

(define (generic-function-1-add-method fn class method)
    (let ((ht (instance-get-ivar fn 0)))
      (ht-at-put ht class method))
  '())

(define (generic-function-1-invoke fn arg)
    (let* ((ht (instance-get-ivar fn 0))
           (method (ht-at ht (class-of arg))))
      (method arg)))

(class-set-applicator GenericFunction1 generic-function-1-invoke)

(define length (make-generic-function1))

(generic-function-1-add-method length String string-length)
(generic-function-1-add-method length Cons list-length)
(generic-function-1-add-method length Null (lambda (_) 0))

(print (length '(1 2 3)))
(print (length "hello there"))
(print (length '()))

(define GenericFunction2 (create-class 'GenericFunction2 1))

(define (make-generic-function2)
    (let ((result (instantiate-class GenericFunction2)))
      (instance-set-ivar result 0 (make-ht))
      result))

(define (generic-function-2-add-method fn class-list method)
    (let ((ht (instance-get-ivar fn 0))
          (class-a (first class-list)))
      (when (nil? (ht-at ht class-a))
        (ht-at-put ht class-a (make-ht)))
      (set! ht (ht-at ht class-a))
      (ht-at-put ht (second class-list) method))
  '())

(define (generic-function-2-invoke fn arg1 arg2)
    (let* ((ht (instance-get-ivar fn 0))
           (ht2 (ht-at ht (class-of arg1)))
           (method (ht-at ht2 (class-of arg2))))
      (method arg1 arg2)))

(class-set-applicator GenericFunction2 generic-function-2-invoke)

(define ArithmeticOp (create-class 'ArithmeticOp 1))

(define arithmetic-op-invoke
    (lambda args
      (let ((combine (instance-get-ivar (car args) 0)))
        (reduce-list combine (cadr args) (cddr args)))))

(class-set-applicator ArithmeticOp arithmetic-op-invoke)

(defmacro define-arithmetic-op (name & clauses)
  (let ((combine (gensym))
        (it (gensym)))
    `(let ((,it (instantiate-class ArithmeticOp))
           (,combine (make-generic-function2)))
       (instance-set-ivar ,it 0 ,combine)
       ,@(mapcar
          (lambda (clause)
            `(generic-function-2-add-method ,combine (list ,@(first clause))
                                            ,(second clause)))
          clauses)
       (define ,name ,it))))

(define-arithmetic-op *
  ((Fixnum Fixnum) *i)
  ((Float Float) *f)
  ((Fixnum Float) (lambda (a b) (*f (i->f a) b)))
  ((Float Fixnum) (lambda (a b) (*f a (i->f b)))))

(define-arithmetic-op +
  ((Fixnum Fixnum) +i)
  ((Float Float) +f)
  ((Fixnum Float) (lambda (a b) (+f (i->f a) b)))
  ((Float Fixnum) (lambda (a b) (+f a (i->f b)))))

(define-arithmetic-op -
  ((Fixnum Fixnum) -i)
  ((Float Float) -f)
  ((Fixnum Float) (lambda (a b) (-f (i->f a) b)))
  ((Float Fixnum) (lambda (a b) (-f a (i->f b)))))

(define-arithmetic-op /
  ((Fixnum Fixnum) /i)
  ((Float Float) /f)
  ((Fixnum Float) (lambda (a b) (/f (i->f a) b)))
  ((Float Fixnum) (lambda (a b) (/f a (i->f b)))))

(print (* 1 2 3))
(print (* 1.0 2.0 3.0))
(print (* 1 2.0 3))

(print (+ 1 2 3))
(print (+ 1.0 2.0 3.0))
(print (+ 1 2.0 3))

(print (- 1 2 3))
(print (- 1.0 2.0 3.0))
(print (- 1 2.0 3))

(print (/ 1 2 3))
(print (/ 1.0 2.0 3.0))
(print (/ 1 2.0 3))

'done
