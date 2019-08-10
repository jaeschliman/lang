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

(define (declare-local-binding thing)
    (when (nil? (ht-at (cdr *expr-context*) thing))
      (ht-at-put (cdr *expr-context*) thing (make-ht))))

(define %expr-set-meta #f)
(define (%expr-set-meta ctx e k v)
    (if (nil? ctx)
        (throw `(bad call to %expr-set-meta ,e ,k ,v))
        (let ((ht (ht-at (cdr ctx) e)))
          (if (nil? ht)
              (%expr-set-meta (car ctx) e k v)
              (ht-at-put ht k v)))))

(define (expr-set-meta e k v)
    (%expr-set-meta *expr-context* e k v))

(define (ctx-annot-put k v)
    (declare-local-binding '())
  (expr-set-meta '() k v))

(define (ctx-annot-read ctx k)
    (ht-at (ht-at (cdr ctx) '()) k))

(define (%ensure-expression-context e)
    (when (nil? (ht-at *context-table* e))
      (ht-at-put *context-table* e (cons *expr-context* (make-ht))))
  (ht-at *context-table* e))

(defmacro with-expression-context (opts & body)
  (let ((e (car opts)))
    `(binding ((*expr-context* (%ensure-expression-context ,e)))
              ,@body)))

(define %binding-depth #f)
(define (%binding-depth ctx e acc)
    (if (nil? ctx) -1
        (let ((ht (ht-at (cdr ctx) e)))
          (if (nil? ht)
              (%binding-depth (car ctx) e (+ 1 acc))
              acc))))

(define (binding-depth sym)
    (%binding-depth *expr-context* sym 0))

(define %binding-context #f)
(define (%binding-context ctx e)
    (if (nil? ctx) '()
        (let ((ht (ht-at (cdr ctx) e)))
          (if (nil? ht)
              (%binding-context (car ctx) e)
              ctx))))

(define (binding-context sym)
    (%binding-context *expr-context* sym))

(define (binding-context-annot sym k v)
    (let ((ctx (binding-context sym)))
      (ht-at-put (ht-at (cdr ctx) '()) k v)))

(define binding-context-is-closed-over #f)
(define (binding-context-is-closed-over ctx)
    (if (nil? ctx) #f
        (or (eq #t (ctx-annot-read ctx 'closed-over))
            (binding-context-is-closed-over (car ctx)))))

(define (expression-context-is-closed-over e)
    (with-expression-context (e)
      (binding-context-is-closed-over *expr-context*)))

(define %binding-crosses-lambda #f)
(define (%binding-crosses-lambda ctx e saw-lambda)
    (if (nil? ctx) #f
        (let* ((type (ctx-annot-read ctx 'type))
               (ht (ht-at (cdr ctx) e))
               (not-here (nil? ht))
               (crossed-lambda (and (eq type 'lambda) not-here)))
          (if not-here
              (%binding-crosses-lambda (car ctx) e (or saw-lambda crossed-lambda))
              saw-lambda))))

(define (binding-crosses-lambda symbol)
    (%binding-crosses-lambda *expr-context* symbol #f))

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

(define (store-tmp idx) (emit-pair STORE_FRAME_RELATIVE idx))
(define (load-tmp idx)  (emit-pair LOAD_FRAME_RELATIVE idx))
(define (load-arg idx)  (emit-pair LOAD_ARG idx))

(define (push-closure slots) (emit-u16 PUSH_CLOSURE_ENV) (emit-u16 slots))
(define (pop-closure)        (emit-u16 POP_CLOSURE_ENV))

(define (store-closure slot depth)
    (emit-u16 STORE_CLOSURE)
    (emit-u16 slot)
    (emit-u16 depth))

(define (load-closure slot depth)
    (emit-u16 LOAD_CLOSURE)
    (emit-u16 slot)
    (emit-u16 depth))

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
            (emit-u16 RET)
            (emit-u16 END)
            (emit-lit '())
            (finalize-bytecode 'anon #f)))

(define hello-world (bytecode->closure
                     (with-output-to-bytecode ()
                       (emit-pair PUSHLIT (emit-lit "hello, world"))
                       (emit-pair PUSHLIT (emit-lit 'print))
                       (emit-u16 LOAD_GLOBAL)
                       (emit-pair CALL 1))))

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
                         (emit-pair CALL 1))))

(goodbye-world)

(define mark-variables #f)

(define (mark-let e)
    (let ((binds (cadr e))
          (body (cddr e)))
      (dolist (pair binds) (mark-variables (cadr pair)))
      (with-expression-context (body)
        (ctx-annot-put 'type 'let)
        (dolist (pair binds)
          (let ((sym (car pair)))
            (declare-local-binding sym)
            (expr-set-meta sym 'type 'local)))
        (dolist (e body) (mark-variables e)))))

(define (mark-lambda e)
    (let* ((args (cadr e))
           (body (cddr e)))
      (with-expression-context (body)
        (ctx-annot-put 'type 'lambda)
        (let ((idx 0))
          (dolist (arg args)
            (declare-local-binding arg)
            (expr-set-meta arg 'type 'argument)
            (expr-set-meta arg 'index idx)
            (set! idx (+ 1 idx)))
          (dolist (e body) (mark-variables e))))))

(define (mark-variables e)
    (cond
      ((symbol? e)
       (when (binding-crosses-lambda e)
         (binding-context-annot e 'closed-over #t)
         (expr-set-meta e 'type 'closure)))
      ((pair? e)
       (case (car e)
         (quote)
         (if (dolist (e (cdr e)) (mark-variables e)))
         (let (mark-let e))
         (lambda (mark-lambda e))
         (#t (dolist (e e) (mark-variables e)))))))

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

(define (emit-let it env)
    (let* ((binds (cadr it))
           (body (cddr it))
           (count (length binds)))
      (if (expression-context-is-closed-over body)
          (let* ((idx 0)
                 (closure-idx 0)
                 ;; this is wasteful -- we are reserving stack space for items
                 ;; which wind up stored in the closure
                 (start (reserve-tmps count))) 
            (dolist (pair binds)
              (let ((sym (car pair)))
                (emit-expr (second pair) env)
                (with-expression-context (body)
                  (case (expr-meta sym 'type)
                    (local
                     (store-tmp (+ idx start))
                     (expr-set-meta sym 'index (+ idx start)))
                    (closure
                     ;; leave on stack to be picked up by push-closure
                     (expr-set-meta sym 'closure-index closure-idx)
                     (set! closure-idx (+ 1 closure-idx))))
                  (expr-set-meta sym 'closure-index idx))
                (set! idx (+ 1 idx))))
            (with-expression-context (body)
              (push-closure closure-idx)
              (dolist (e body) (emit-expr e env))
              (pop-closure)))
          (let* ((start (reserve-tmps count))
                 (idx start))
            (dolist (pair binds)
              (emit-expr (second pair) env)
              (store-tmp idx)
              (set! idx (+ 1 idx)))
            (with-expression-context (body)
              (set! idx start)
              (dolist (pair binds)
                (let ((sym (car pair)))
                  (expr-set-meta sym 'index idx)
                  (set! idx (+ 1 idx))))
              (dolist (e body)
                (emit-expr e env)))))))

(define (emit-flat-lambda it env)
    (let* ((body (cddr it))
           (bc (with-output-to-bytecode ()
                 (with-expression-context (body)
                   (dolist (e body)
                     (emit-expr e env))))))
      (emit-pair PUSHLIT (emit-lit (bytecode->closure bc)))))

(define (emit-lambda it env)
    (if (expression-context-is-closed-over (cddr it))
        ;; TODO: only close over those args that require it
        (let* ((args (cadr it))
               (body (cddr it))
               (closure-idx 0)
               (arg-idx 0)
               (bc (with-output-to-bytecode ()
                     (with-expression-context (body)
                       (dolist (a args)
                         (when (eq 'closure (expr-meta a 'type))
                           (expr-set-meta a 'closure-index closure-idx)
                           (set! closure-idx (+ 1 closure-idx))
                           (load-arg arg-idx))
                         (set! arg-idx (+ 1 arg-idx)))
                       (push-closure closure-idx)
                       (dolist (e body)
                         (emit-expr e env))
                       (pop-closure)))))
          (emit-pair PUSHLIT (emit-lit bc))
          (emit-u16 BUILD_CLOSURE))   
        (emit-flat-lambda it env)))

(define (emit-expr it env)
    (cond
      ((symbol? it)
       (let ((type (expr-meta it 'type)))
         (case type
           (()
            (emit-pair PUSHLIT (emit-lit it))
            (emit-u16 LOAD_GLOBAL))
           (local
            (load-tmp (expr-meta it 'index)))
           (argument
            (load-arg (expr-meta it 'index)))
           (closure
            (let ((depth (binding-depth it))
                  (slot  (expr-meta it 'closure-index)))
              (load-closure slot depth)))
           (#t (throw `(bad type for symbol ,it ,type))))))
      ((pair? it)
       (let ((head (car it)))
         (case head
           (quote  (emit-pair PUSHLIT (emit-lit (cadr it))))
           (if     (emit-if it env))
           (let    (emit-let it env))
           (lambda (emit-lambda it env))
           (#t     (emit-call it env)))))
      (#t
       (emit-pair PUSHLIT (emit-lit it)))))

(define (dbg expr)
    (try-catch (lambda ()
                 (let ((r
                        (binding ((*context-table* (make-ht)))
                                 (mark-variables expr)
                                 (bytecode->closure (with-output-to-bytecode ()
                                                      (with-expression-context (expr)
                                                        (emit-expr expr '())))))))
                   (r)))
               (lambda (ex)
                 (print `(exception in dbg: ,ex)))))

(dbg '(print "hello again, world!"))

(dbg `(if (= 3 3) (print "3 equals 3") (print "3 does not equal 3")))
(dbg `(if (= 4 3) (print "4 equals 3") (print "4 does not equal 3")))

(dbg `(let ((x 10)) (print "compiled a let without body refs")))
(dbg `(let ((message "compiled a let with a body ref"))
        (print message)))

(dbg `(print (closure? (lambda () "does nothing"))))
(dbg `(print ((lambda () "returned a value"))))
(dbg `(print (closure? (lambda (arg) arg))))
(dbg `(print ((lambda (arg) arg) "returned an argument")))
(dbg `(print ((lambda (arg) (list arg)) "used an arg")))
(dbg `(print (list (list 'list '1 '2) '= ((lambda (x y) (list x y)) 1 2))))
(dbg `(print (closure? (let ((x 10)) (lambda () x)))))
(dbg `(print ((let ((x 10)) (lambda () (let ((x 20)) x))))))
(dbg `(print ((let ((x 10)) (lambda () (let ((y 20)) x))))))
(dbg `(print ((let ((x 10)) (lambda () (let ((y 20)) y))))))
(print '(expecting 20 and 10))
(dbg `(print ((let ((x 10) (y 20))
                (print y)
                (lambda () x)))))
(print '(expecting 20))
(dbg `(let ((x 10) (y 20))
        (lambda () x)
        (print y)))
(print '(expecting 5))
(dbg `(let ((make-adder (lambda (n) (lambda (x) (+ n x)))))
        (print ((make-adder 2) 3))))

(print 'done)
