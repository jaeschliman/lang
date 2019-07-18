;; first things first -------------------------------------------------------------

(set-symbol-value 'lambda->continuation (lambda (fn)
                                          (set-stack-mark 'helper)
                                          (let ((r ((lambda ()
                                                      (snapshot-to-stack-mark 'helper '())
                                                      (fn)))))
                                            r)))

;;  built in classes --------------------------------------------------------------

(set-symbol-value 'set-class-name (lambda (class name)
                                  (if (class? class) (instance-set-ivar class 0 name))))

(set-symbol-value 'init-builtin-class
                  (lambda (name class)
                    (set-symbol-value name class)
                    (set-class-name class name)))

(init-builtin-class 'Continuation     (class-of (lambda->continuation (lambda ()))))
(init-builtin-class 'Fixnum           (class-of 0))
(init-builtin-class 'Float            (class-of 0.0))
(init-builtin-class 'String           (class-of ""))
(init-builtin-class 'Symbol           (class-of 'symbol))
(init-builtin-class 'Closure          (class-of (lambda ())))
(init-builtin-class 'PrimOp           (class-of class-of))
(init-builtin-class 'Image            (class-of (make-image 0 0)))
(init-builtin-class 'Point            (class-of 0@0))
(init-builtin-class 'Array            (class-of (make-array 0)))
(init-builtin-class 'HashTable        (class-of (make-ht)))
(init-builtin-class 'Null             (class-of '()))
(init-builtin-class 'Cons             (class-of (cons 0 0)))
(init-builtin-class 'Boolean          (class-of #t))
(init-builtin-class 'Character        (class-of #\0))
(init-builtin-class 'Semaphore        (class-of (make-semaphore 0)))
(init-builtin-class 'Thread           (class-of (current-thread)))
(init-builtin-class 'FileOutputStream (class-of *standard-output*))
(init-builtin-class 'Package          (class-of *package*))
(init-builtin-class 'Class            (class-of (class-of 0)))


;; since we don't support class inheritance yet, this will do
(set-symbol-value 'isa? (lambda (obj class) (eq (class-of obj) class)))

;;  type tests for built in classes -----------------------------------------------

(set-symbol-value 'class?        (lambda (x) (isa? x Class)))
(set-symbol-value 'bool?         (lambda (x) (isa? x Boolean)))
(set-symbol-value 'closure?      (lambda (x) (isa? x Closure)))
(set-symbol-value 'primop?       (lambda (x) (isa? x PrimOp)))
(set-symbol-value 'float?        (lambda (x) (isa? x Float)))
(set-symbol-value 'fixnum?       (lambda (x) (isa? x Fixnum)))
(set-symbol-value 'pair?         (lambda (x) (isa? x Cons)))
(set-symbol-value 'nil?          (lambda (x) (isa? x Null)))
(set-symbol-value 'ht?           (lambda (x) (isa? x HashTable)))
(set-symbol-value 'array?        (lambda (x) (isa? x Array)))
(set-symbol-value 'symbol?       (lambda (x) (isa? x Symbol)))
(set-symbol-value 'char?         (lambda (x) (isa? x Character)))
(set-symbol-value 'image?        (lambda (x) (isa? x Image)))
(set-symbol-value 'point?        (lambda (x) (isa? x Point)))
(set-symbol-value 'continuation? (lambda (x) (isa? x Continuation)))
(set-symbol-value 'string?       (lambda (x) (isa? x String)))
(set-symbol-value 'semaphore?    (lambda (x) (isa? x Semaphore)))
(set-symbol-value 'thread?       (lambda (x) (isa? x Thread)))
(set-symbol-value 'package?      (lambda (x) (isa? x Package)))


'done
