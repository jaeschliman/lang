(defmacro define-numeric-op (name & clauses)
  (let ((it (gensym))
        (count (list-length (first (first clauses)))))
    `(let ((,it (make-generic-function ,count)))
       ,@(mapcar
          (lambda (clause) `(generic-function-add-method ,it (list ,@(first clause)) ,(second clause)))
          clauses)
       (define ,name ,it))))

(define (absi n) (if (<i n 0) (*i -1 n) n))
(define (absf n) (if (<f n 0.0) (*f -1.0 n) n))

(define-numeric-op abs ((Fixnum) absi) ((Float) absf))

(define-numeric-op log
  ((Fixnum Fixnum) (lambda (a b) (logf (i->f a) (i->f b))))
  ((Fixnum Float ) (lambda (a b) (logf (i->f a) b)))
  ((Float  Fixnum) (lambda (a b) (logf a (i->f b))))
  ((Float  Float ) logf))

(define-numeric-op pow
  ((Fixnum Fixnum) (lambda (a b) (f->i (powf (i->f a) (i->f b)))))
  ((Fixnum Float ) (lambda (a b) (powf (i->f a) b)))
  ((Float  Fixnum) (lambda (a b) (powf a (i->f b))))
  ((Float  Float ) powf))

(define-numeric-op floor ((Fixnum) (lambda (x) x))               ((Float) floorf))
(define-numeric-op ceil  ((Fixnum) (lambda (x) x))               ((Float) ceilf))
(define-numeric-op rem   ((Fixnum) (lambda (_) 0))               ((Float) remf))
(define-numeric-op cos   ((Fixnum) (lambda (a) (cosf (i->f a)))) ((Float) cosf))
(define-numeric-op sin   ((Fixnum) (lambda (a) (sinf (i->f a)))) ((Float) sinf))
(define-numeric-op tan   ((Fixnum) (lambda (a) (tanf (i->f a)))) ((Float) tanf))

(define ArithmeticOp (create-class 'ArithmeticOp '(combine)))

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
            `(generic-function-add-method ,combine (list ,@(first clause)) ,(second clause)))
          clauses)
       (define ,name ,it))))

(define-arithmetic-op *
  ;; ((Bignum Bignum) *b)
  ;; ((Fixnum Bignum) (lambda (a b) (*b (i->b a) b)))
  ;; ((Bignum Fixnum) (lambda (a b) (*b a (i->b b))))
  ((Fixnum Fixnum) *i)
  ((Float Float)   *f)
  ((Fixnum Float)  (lambda (a b) (*f (i->f a) b)))
  ((Float Fixnum)  (lambda (a b) (*f a (i->f b)))))

(define-arithmetic-op +
  ((Bignum Bignum) +b)
  ((Fixnum Bignum) (lambda (a b) (+b (i->b a) b)))
  ((Bignum Fixnum) (lambda (a b) (+b a (i->b b))))
  ((Fixnum Fixnum) +i)
  ((Float Float)   +f)
  ((Fixnum Float)  (lambda (a b) (+f (i->f a) b)))
  ((Float Fixnum)  (lambda (a b) (+f a (i->f b)))))

(define-arithmetic-op -
  ((Bignum Bignum) -b)
  ((Fixnum Bignum) (lambda (a b) (-b (i->b a) b)))
  ((Bignum Fixnum) (lambda (a b) (-b a (i->b b))))
  ((Fixnum Fixnum) -i)
  ((Float Float)   -f)
  ((Fixnum Float)  (lambda (a b) (-f (i->f a) b)))
  ((Float Fixnum)  (lambda (a b) (-f a (i->f b)))))

(define-arithmetic-op /
  ((Fixnum Fixnum) /i)
  ((Float Float)   /f)
  ((Fixnum Float)  (lambda (a b) (/f (i->f a) b)))
  ((Float Fixnum)  (lambda (a b) (/f a (i->f b)))))

(define-arithmetic-op %
  ((Fixnum Fixnum) %i)
  ((Float Float)   %f)
  ((Fixnum Float)  (lambda (a b) (%f (i->f a) b)))
  ((Float Fixnum)  (lambda (a b) (%f a (i->f b)))))

(define ComparisonOp (create-class 'ComparisonOp '(compare)))

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

'done
