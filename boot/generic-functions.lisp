(at-boot (define GenericFunction1 (create-class 'GenericFunction1 '(table arity))))
(at-boot (define GenericFunction2 (create-class 'GenericFunction2 '(table arity))))

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
           (ht2 (gf-lookup-1 ht (class-of arg1)))
           (method (gf-lookup-1 ht2 (class-of arg2))))
      (method arg1 arg2)))

(class-set-applicator GenericFunction2 generic-function-2-invoke)

(defmacro defgeneric (name arglist)
  `(define ,name (make-generic-function ,(list-length arglist))))

(define length (make-generic-function 1))
(generic-function-add-method length (list String) string-byte-length) ;; hmmm not sure about this
(generic-function-add-method length (list Cons)   list-length)
(generic-function-add-method length (list Null)   (lambda (_) 0))

'done
