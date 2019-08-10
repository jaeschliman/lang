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
(defparameter *labels* #f)
(defparameter *jump-locations* #f)
(defparameter *tmp-count* #f)
(defparameter *expr-context* '())
(defparameter *context-table* #f)

(define %expr-meta #f)
(define (%expr-meta ctx e k)
    (if (nil? ctx) '()
        (let ((ht (ht-at (cdr ctx) e)))
          (if (nil? ht)
              (%expr-meta (car ctx) e k)
              (ht-at ht k)))))

(define (expr-meta e k)
    (%expr-meta *expr-context* e k))

(define (declare-local-binding symbol)
    (when (nil? (ht-at (cdr *expr-context*) symbol))
      (ht-at-put (cdr *expr-context*) symbol (make-ht))))

(define %expr-set-meta #f)
(define (%expr-set-meta ctx e k v)
    (if (nil? ctx)
        (print `(bad call to %expr-set-meta ,e ,k ,v))
        (let ((ht (ht-at (cdr ctx) e)))
          (if (nil? ht)
              (%expr-set-meta (car ctx) e k v)
              (ht-at-put ht k v)))))

(define (expr-set-meta e k v)
    (%expr-set-meta *expr-context* e k v))

(define (%ensure-expression-context e)
    (when (nil? (ht-at *context-table* e))
      (ht-at-put *context-table* e (cons *expr-context* (make-ht))))
  (ht-at *context-table* e))

(defmacro with-expression-context (opts & body)
  (let ((e (car opts)))
    `(binding ((*expr-context* (%ensure-expression-context ,e)))
              ,@body)))

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

(define (label name)
    (ht-at-put *labels* name (agg-count *code*)))

(define (save-jump-location label)
  (emit-u16 0) ;; to be filled in by fixup-jump-locations
  (set '*jump-locations* (cons (cons label (- (agg-count *code*) 1)) *jump-locations*)))

(define (jump label)
    (emit-u16 JUMP)
  (save-jump-location label))

(define (jump-if-false label)
    (emit-u16 BR_IF_False)
  (save-jump-location label))

(define (reserve-tmps count)
    (let ((r *tmp-count*))
      (set '*tmp-count* (+ count *tmp-count*))
      r))

(define (fixup-jump-locations code)
    (dolist (pair *jump-locations*)
      (let* ((label (car pair))
             (code-location (cdr pair))
             (jump-target (ht-at *labels* label)))
        (aset-u16 code code-location jump-target))))

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
      (fixup-jump-locations code)
      (aset-u16 code 1 *tmp-count*)
      (make-bytecode varargs name code lits)))

(defmacro with-output-to-bytecode (_ & body)
  `(binding ((*code* (make-agg))
             (*lits* (make-agg))
             (*labels* (make-ht))
             (*jump-locations* '())
             (*tmp-count* 0))
            (emit-u16 STACK_RESERVE)
            (emit-u16 0)
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

(define goodbye-world (bytecode->closure
                       (with-output-to-bytecode ()
                         (jump 'goodbye)
                         (emit-pair PUSHLIT (emit-lit "hello, world"))
                         (emit-pair PUSHLIT (emit-lit 'print))
                         (emit-u16 LOAD_GLOBAL)
                         (emit-pair CALL 1)
                         (label 'goodbye)
                         (emit-pair PUSHLIT (emit-lit "goodbye, world"))
                         (emit-pair PUSHLIT (emit-lit 'print))
                         (emit-u16 LOAD_GLOBAL)
                         (emit-pair CALL 1)
                         (emit-u16 RET)
                         (emit-u16 END))))

(goodbye-world)

(define emit-expr #f)

(define (emit-if it env)
    (let ((done (gensym))
          (false (gensym)))
      (emit-expr (second it))
      (jump-if-false false)
      (emit-expr (third it))
      (jump done)
      (label false)
      (emit-expr (fourth it))
      (label done)))

(define (emit-call it env)
    (let ((argc 0))
      (dolist (e (cdr it))
        (set! argc (+ 1 argc))
        (emit-expr e env))
      (emit-expr (car it) env)
      (emit-pair CALL argc)))

(define (emit-expr it env)
    (cond
      ((symbol? it)
       (let ((type (expr-meta it)))
         (case type
           (()
            (emit-pair PUSHLIT (emit-lit it))
            (emit-u16 LOAD_GLOBAL))
           (#t (throw `(bad type for symbol ,it ,type))))))
      ((pair? it)
       (let ((head (car it)))
         (case head
           (if (emit-if it env))
           (#t (emit-call it env)))))
      (#t
       (emit-pair PUSHLIT (emit-lit it)))))

(define (dbg expr)
    (try-catch (lambda ()
                 (let ((r
                        (binding ((*context-table* (make-ht)))
                                 (bytecode->closure (with-output-to-bytecode ()
                                                      (with-expression-context (expr)
                                                        (emit-expr expr '()))
                                                      (emit-u16 RET)
                                                      (emit-u16 END))))))
                   (r)))
               (lambda (ex)
                 (print `(exception in dbg: ,ex)))))

(dbg '(print "hello again, world!"))

(dbg `(if (= 3 3) (print "3 equals 3") (print "3 does not equal 3")))
(dbg `(if (= 4 3) (print "4 equals 3") (print "4 does not equal 3")))


(print 'done)
