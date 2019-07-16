(define GenericFunction1 (create-class 'GenericFunction1 2))
(define GenericFunction2 (create-class 'GenericFunction2 2))

(define (gf-lookup-1 ht class)
  (let ((r (ht-at ht class)))
    (if (nil? r) (ht-at ht #t)
        r)))

(define gf-store #f)
(define (gf-store ht classes method)
    (if (nil? (cdr classes))
        (ht-at-put ht (car classes) method)
        (let ((nxt (ht-at ht (car classes))))
          (when (nil? nxt)
            (ht-at-put ht (car classes) (make-ht))
            (set! nxt (ht-at ht (car classes))))
          (gf-store nxt (cdr classes) method))))

(define (make-generic-function arity)
    (let ((class (case arity
                   (1 GenericFunction1)
                   (2 GenericFunction2))))
      (when (nil? class) (throw `(unsupported gf arity ,arity)))
      (let ((result (instantiate-class class)))
        (instance-set-ivar result 0 (make-ht))
        (instance-set-ivar result 1 arity)
        result)))

(define (generic-function-add-method fn classlist method)
    (when (not (eq (instance-get-ivar fn 1) (list-length classlist)))
      (throw `(generic-function-add-method wrong arity)))
    (let ((ht (instance-get-ivar fn 0)))
      (gf-store ht classlist method)
      '()))

(define (generic-function-1-invoke fn arg)
    (let* ((ht (instance-get-ivar fn 0))
           (method (gf-lookup-1 ht (class-of arg))))
      (method arg)))

(class-set-applicator GenericFunction1 generic-function-1-invoke)

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
           (,combine (make-generic-function 2)))
       (instance-set-ivar ,it 0 ,combine)
       ,@(mapcar
          (lambda (clause)
            `(generic-function-add-method ,combine (list ,@(first clause))
                                          ,(second clause)))
          clauses)
       (define ,name ,it))))

(define-arithmetic-op *
  ((Fixnum Fixnum) *i)
  ((Float Float)   *f)
  ((Fixnum Float)  (lambda (a b) (*f (i->f a) b)))
  ((Float Fixnum)  (lambda (a b) (*f a (i->f b)))))

(define-arithmetic-op +
  ((Fixnum Fixnum) +i)
  ((Float Float)   +f)
  ((Fixnum Float)  (lambda (a b) (+f (i->f a) b)))
  ((Float Fixnum)  (lambda (a b) (+f a (i->f b)))))

(define-arithmetic-op -
  ((Fixnum Fixnum) -i)
  ((Float Float)   -f)
  ((Fixnum Float)  (lambda (a b) (-f (i->f a) b)))
  ((Float Fixnum)  (lambda (a b) (-f a (i->f b)))))

(define-arithmetic-op /
  ((Fixnum Fixnum) /i)
  ((Float Float)   /f)
  ((Fixnum Float)  (lambda (a b) (/f (i->f a) b)))
  ((Float Fixnum)  (lambda (a b) (/f a (i->f b)))))

(define ComparisonOp (create-class 'ComparisonOp 1))

(define (pairwise-compare compare items)
    (let ((loop #f))
      (set! loop (lambda (cmp lst)
                   (if (nil? (cdr lst)) #t
                       (and (cmp (car lst) (cadr lst))
                            (loop cmp (cdr lst))))))
      (loop compare items)))

(define comparison-op-invoke
    (lambda args
      (let ((compare (instance-get-ivar (car args) 0)))
        (pairwise-compare compare (cdr args)))))

(class-set-applicator ComparisonOp comparison-op-invoke)

(defmacro define-comparison-op (name & clauses)
  (let ((compare (gensym))
        (it (gensym)))
    `(let ((,it (instantiate-class ComparisonOp))
           (,compare (make-generic-function 2)))
       (instance-set-ivar ,it 0 ,compare)
       ,@(mapcar
          (lambda (clause)
            `(generic-function-add-method ,compare (list ,@(first clause))
                                          ,(second clause)))
          clauses)
       (define ,name ,it))))

(define-comparison-op <
  ((Fixnum Fixnum) <i)
  ((Float Float)   <f)
  ((Fixnum Float)  (lambda (a b) (<f (i->f a) b)))
  ((Float Fixnum)  (lambda (a b) (<f a (i->f b)))))

(define-comparison-op >
  ((Fixnum Fixnum) >i)
  ((Float Float)   >f)
  ((Fixnum Float)  (lambda (a b) (>f (i->f a) b)))
  ((Float Fixnum)  (lambda (a b) (>f a (i->f b)))))

(define-comparison-op =
  ((Fixnum Fixnum) eq)
  ((Float Float)   eq)
  ((Fixnum Float)  (lambda (a b) (eq (i->f a) b)))
  ((Float Fixnum)  (lambda (a b) (eq a (i->f b)))))

(define (%<= a b) (or (< a b) (= a b)))
(define (%>= a b) (or (> a b) (= a b)))

(define-comparison-op <=
  ((Fixnum Fixnum) %<=)
  ((Float Float)   %<=)
  ((Fixnum Float)  %<=)
  ((Float Fixnum)  %<=))

(define-comparison-op >=
  ((Fixnum Fixnum) %>=)
  ((Float Float)   %>=)
  ((Fixnum Float)  %>=)
  ((Float Fixnum)  %>=))

(define length (make-generic-function 1))
(generic-function-add-method length (list String) string-length)
(generic-function-add-method length (list Cons)   list-length)
(generic-function-add-method length (list Null)   (lambda (_) 0))

'done
