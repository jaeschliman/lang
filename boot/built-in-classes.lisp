;; first things first -------------------------------------------------------------

(set-symbol-value 'lambda->continuation (%nlambda lambda->continuation (fn)
                                          (set-stack-mark 'helper)
                                          (let ((r ((%nlambda () ()
                                                      (snapshot-to-stack-mark 'helper '())
                                                      (fn)))))
                                            r)))

;;  built in classes --------------------------------------------------------------

(set-symbol-value 'set-class-name (%nlambda set-class-name (class name)
                                  (if (class? class) (instance-set-ivar class 0 name))))

(set-symbol-value 'class-name (%nlambda class-name (class) (instance-get-ivar class 0)))

(set-symbol-value 'init-builtin-class
                  (%nlambda init-builtin-class (name class)
                    (set-symbol-value name class)
                    (set-class-name class name)))

(init-builtin-class 'Continuation     (class-of (lambda->continuation (%nlambda () ()))))
(init-builtin-class 'Fixnum           (class-of 0))
(init-builtin-class 'Float            (class-of 0.0))
(init-builtin-class 'String           (class-of ""))
(init-builtin-class 'Symbol           (class-of 'symbol))
(init-builtin-class 'Closure          (class-of (%nlambda () ())))
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
(set-symbol-value 'isa? (%nlambda isa? (obj class) (eq (class-of obj) class)))

;;  type tests for built in classes -----------------------------------------------

(set-symbol-value 'class?        (%nlambda class?        (x) (isa? x Class)))
(set-symbol-value 'bool?         (%nlambda bool?         (x) (isa? x Boolean)))
(set-symbol-value 'closure?      (%nlambda closure?      (x) (isa? x Closure)))
(set-symbol-value 'primop?       (%nlambda primop?       (x) (isa? x PrimOp)))
(set-symbol-value 'float?        (%nlambda float?        (x) (isa? x Float)))
(set-symbol-value 'fixnum?       (%nlambda fixnum?       (x) (isa? x Fixnum)))
(set-symbol-value 'pair?         (%nlambda pair?         (x) (isa? x Cons)))
(set-symbol-value 'nil?          (%nlambda nil?          (x) (isa? x Null)))
(set-symbol-value 'ht?           (%nlambda ht?           (x) (isa? x HashTable)))
(set-symbol-value 'array?        (%nlambda array?        (x) (isa? x Array)))
(set-symbol-value 'symbol?       (%nlambda symbol?       (x) (isa? x Symbol)))
(set-symbol-value 'char?         (%nlambda char?         (x) (isa? x Character)))
(set-symbol-value 'image?        (%nlambda image?        (x) (isa? x Image)))
(set-symbol-value 'point?        (%nlambda point?        (x) (isa? x Point)))
(set-symbol-value 'continuation? (%nlambda continuation? (x) (isa? x Continuation)))
(set-symbol-value 'string?       (%nlambda string?       (x) (isa? x String)))
(set-symbol-value 'semaphore?    (%nlambda semaphore?    (x) (isa? x Semaphore)))
(set-symbol-value 'thread?       (%nlambda thread?       (x) (isa? x Thread)))
(set-symbol-value 'package?      (%nlambda package?      (x) (isa? x Package)))


'done
