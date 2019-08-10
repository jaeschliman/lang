(defmacro define-bytecodes (& codes)
  (let ((counter -1))
    `(let ()
       ,@(mapcar (lambda (code)
                  (set! counter (+ 1 counter))
                  `(define ,code ,counter))
                codes))))

(define-bytecodes
  END
  RET
  PUSHLIT
  POP
  BR_IF_ZERO
  BR_IF_NOT_ZERO
  DUP
  CALL
  TAIL_CALL
  LOAD_ARG
  LOAD_GLOBAL
  LOAD_SPECIAL
  LOAD_CLOSURE
  STORE_CLOSURE
  BUILD_CLOSURE
  PUSH_CLOSURE_ENV
  BR_IF_False
  JUMP
  STACK_RESERVE
  LOAD_FRAME_RELATIVE
  STORE_FRAME_RELATIVE
  POP_CLOSURE_ENV
  PUSH_SPECIAL_BINDING
  POP_SPECIAL_BINDING)

(defparameter *code* #f)
(defparameter *lits* #f)

(define Aggregator (create-class 'Aggregator '(count list)))

(define (make-agg)
    (let ((r (instantiate-class Aggregator)))
      (instance-set-ivar r 0 0)
      (instance-set-ivar r 1 '())
      r))

(define (agg-push agg it)
    (instance-set-ivar agg 0 (+ 1 (instance-get-ivar agg 0)))
  (instance-set-ivar agg 1 (cons it (instance-get-ivar agg 1))))

(define (agg-count agg) (instance-get-ivar agg 0))
(define (agg-items agg) (reverse-list (instance-get-ivar agg 1)))

(define (emit-u16 it) (agg-push *code* it) (- (agg-count *code*) 1))
(define (emit-lit it) (agg-push *lits* it) (- (agg-count *lits*) 1)) 
(define (emit-pair a b) (emit-u16 (bit-or (ash b 8) a)))

(define (finalize-bytecode name varargs)
    (let ((code (make-array-u16 (agg-count *code*)))
          (lits (make-array (agg-count *lits*))))
      (let ((i -1))
        (dolist (it (agg-items *code*))
          (set! i (+ 1 i))
          (aset-u16 code i it)))
      (let ((i -1))
        (dolist (it (agg-items *lits*))
          (set! i (+ 1 i))
          (aset lits i it)))
      (make-bytecode varargs name code lits)))

(defmacro with-output-to-bytecode (_ & body)
  `(binding ((*code* (make-agg)) (*lits* (make-agg)))
            ,@body
            (finalize-bytecode 'anon #f)))

(define hello-world (bytecode->closure
                     (with-output-to-bytecode ()
                       (emit-pair PUSHLIT (emit-lit "hello, world"))
                       (emit-pair PUSHLIT (emit-lit 'print))
                       (emit-u16 LOAD_GLOBAL)
                       (emit-pair CALL 1)
                       (emit-u16 RET)
                       (emit-u16 END))))

(hello-world)

(print 'done)
