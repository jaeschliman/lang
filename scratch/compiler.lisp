(define (ensure-list x) (if (or (nil? x) (pair? x)) x (list x)))

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
  SWAP
  CALL
  TAIL_CALL
  LOAD_ARG
  STORE_ARG
  LOAD_GLOBAL
  LOAD_SPECIAL
  LOAD_CLOSURE
  STORE_CLOSURE
  BUILD_CLOSURE
  SAVE_CLOSURE_ENV
  RESTORE_CLOSURE_ENV
  PUSH_CLOSURE_ENV
  BR_IF_False
  JUMP
  PUSH_JUMP
  POP_JUMP
  STACK_RESERVE
  LOAD_FRAME_RELATIVE
  STORE_FRAME_RELATIVE
  POP_CLOSURE_ENV
  PUSH_SPECIAL_BINDING
  POP_SPECIAL_BINDING)

(#/lang/at-boot (defparameter *trace-eval* #f))
(#/lang/at-boot (defparameter *enable-jump-opts* #t))
(#/lang/at-boot (defparameter *enable-inline-let-bound-lambdas* #t))
(#/lang/at-boot (defparameter *enable-inline-letrec-bound-lambdas* #t))

(defparameter *note-closed-over-vars* #f)

(defparameter *code* #f)
(defparameter *lits* #f)
(defparameter *labels* #f)
(defparameter *jump-locations* #f)
(defparameter *tmp-count* #f)
(defparameter *expr-context* '())
(defparameter *context-table* #f)
(defparameter *tail-position* #t)
(defparameter *inlining-lambda* #f)
(defparameter *varargs* #f)
(defparameter *closure-depth* 0)

(define (expr-meta e k &opt (default '()))
  (let loop ((ctx *expr-context*) (e e) (k k) (d default))
       (if (nil? ctx) '()
           (let ((ht (ht-at (cdr ctx) e)))
             (if (nil? ht)
                 (loop (car ctx) e k d)
                 (let ((found (ht-at ht k)))
                   (if (nil? found) d found)))))))

(define (declare-local-binding thing)
  (when (nil? (ht-at (cdr *expr-context*) thing))
    (ht-at-put (cdr *expr-context*) thing (make-ht))))

(define (expr-set-meta e k v)
  (let loop ((ctx *expr-context*) (e e) (k k) (v v))
       (if (nil? ctx)
           (throw `(bad call to expr-set-meta ,e ,k ,v))
           (let ((ht (ht-at (cdr ctx) e)))
             (if (nil? ht)
                 (loop (car ctx) e k v)
                 (ht-at-put ht k v))))))

(define (ctx-annot-put k v)
  (declare-local-binding '())
  (expr-set-meta '() k v))

(define (ctx-annot-read ctx k) (ht-at (ht-at (cdr ctx) '()) k))
(define (ctx-annot k)
  (declare-local-binding '())
  (ctx-annot-read *expr-context* k))

(define (%ensure-expression-context e)
  (when (nil? (ht-at *context-table* e))
    (ht-at-put *context-table* e (cons *expr-context* (make-ht))))
  (ht-at *context-table* e))

(defmacro with-expression-context (opts & body)
  (let ((e (car opts)))
    `(binding ((*expr-context* (%ensure-expression-context ,e)))
       ,@body)))

(define (context-read form k) (with-expression-context (form) (ctx-annot-read *expr-context* k)))
(define (context-write form k v) (with-expression-context (form) (ctx-annot-put k v)))

(define (%ctx-will-be-fully-inlined? ctx)
  (let ((ht (ht-at (cdr ctx) '())))
    (if (nil? ht) #f
        (eq #t (ht-at ht 'fully-inlined)))))

(define (binding-depth sym)
  (let loop ((ctx *expr-context* ) (e sym) (acc 0))
       (if (nil? ctx) -1
           (let ((ht (ht-at (cdr ctx) e)))
             (if (nil? ht)
                 (loop (car ctx) e (+ acc (if (%ctx-will-be-fully-inlined? ctx) 0 1)))
                 acc)))))

(define (binding-context sym)
  (let loop ((ctx *expr-context*) (e sym))
       (if (nil? ctx) '()
           (let ((ht (ht-at (cdr ctx) e)))
             (if (nil? ht)
                 (loop (car ctx) e)
                 ctx)))))

(define (binding-context-annot sym k v)
  (let ((ctx (binding-context sym)))
    (ht-at-put (ht-at (cdr ctx) '()) k v)))

(define (containing-contexts-annot sym k v)
  (let ((end (binding-context sym)))
    (let loop ((ctx *expr-context*))
         (unless (nil? ctx)
           (ht-at-put (ht-at (cdr ctx) '()) k v)
           (unless (eq ctx end)
             (loop (car ctx)))))))

(define (binding-context-is-closed-over ctx)
  (if (nil? ctx) #f
      (eq #t (ctx-annot-read ctx 'closed-over))))

(define (%bound-at? sym ctx)
  (not (nil? (ht-at (cdr ctx) sym))))

(define (%ctx-counts-in-closure-depth ctx)
  (and (binding-context-is-closed-over ctx)
       (not (%ctx-will-be-fully-inlined? ctx))))

(define (closure-depth sym)
  (let loop ((ctx *expr-context*) (sym sym) (acc 0))
       (cond
         ((nil? ctx) -1)
         ((%bound-at? sym ctx) acc)
         (#t (loop (car ctx) sym (+ acc (if (%ctx-counts-in-closure-depth ctx) 1 0)))))))

(define (expression-context-is-closed-over e)
  (with-expression-context (e)
    (binding-context-is-closed-over *expr-context*)))

(forward %binding-crosses-lambda)
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

(forward %enclosing-lambda-count)
(define (enclosing-lambda-count symbol) (%enclosing-lambda-count *expr-context* symbol 0))
(define (%enclosing-lambda-count ctx e acc)
  (if (nil? ctx) acc
      (let* ((type (ctx-annot-read ctx 'type))
             (ht (ht-at (cdr ctx) e))
             (not-here (nil? ht))
             (crossed-lambda (and (eq type 'lambda) not-here)))
        (if not-here
            (%enclosing-lambda-count (car ctx) e (if crossed-lambda (+ 1 acc) acc))
            acc))))

(define Aggregator (create-class 'Aggregator '(count list)))

(define (make-agg)
  (let ((r (instantiate-class Aggregator)))
    (iset r 'count 0)
    (iset r 'list '())
    r))

(define (agg-count agg) (iget agg 'count))
(define (agg-items agg) (reverse-list (iget agg 'list)))

(define (agg-push agg it)
  (iset agg 'count (+ 1 (iget agg 'count)))
  (iset agg 'list (cons it (iget agg 'list))))

(define (agg-push-uniq agg it)
  (let ((items (agg-items agg)))
    (let loop ((idx 0) (rem items))
         (if (nil? rem)
             (let () (agg-push agg it) (- (agg-count agg) 1))
             (if (eq it (car rem)) idx
                 (loop (+ 1 idx) (cdr rem)))))))

(define (emit-u16 it) (agg-push *code* it) (- (agg-count *code*) 1))
(define (emit-lit it) (agg-push-uniq *lits* it))
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
      (when (nil? jump-target) (throw `(undefined label: ,label)))
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
    (when *trace-eval*
      (print `(bytecode length: ,(array-length-u16 code)))
      (print `(literal count: ,(array-length lits)))
      (print `(temporary count: ,*tmp-count*)))
    (make-bytecode varargs name code lits)))

(defparameter *lambda-name* '())

(defmacro with-output-to-bytecode (_ & body)
  `(binding ((*lambda-name* '())
             (*code* (make-agg))
             (*lits* (make-agg))
             (*labels* (make-ht))
             (*jump-locations* '())
             (*varargs* #f)
             (*tmp-count* 0))
     (emit-u16 STACK_RESERVE)
     (emit-u16 0)
     ,@body
     (emit-u16 RET)
     (emit-u16 END)
     (emit-lit '())
     (finalize-bytecode *lambda-name* *varargs*)))

(defparameter *in-call-position* #f)
(defparameter *being-set* #f)
(defparameter *binding-name* '())
(defparameter *toplevel-form* '())

(forward walk-form)

(define (walk-body es walk)
  (let ((last (- (length es) 1))
        (idx 0))
    (dolist (e es)
      (binding ((*tail-position* (and *tail-position* (= idx last))))
        (walk-form e walk))
      (set! idx (+ 1 idx)))))

(define (walk-letrec e walk)
  (let ((binds (cadr e))
        (body (cddr e)))
    (with-expression-context (body)
      ((walk 'scope) 'letrec binds body)
      (dolist (pair binds)
        (binding ((*binding-name* (car pair)) (*tail-position* #f))
          (walk-form (cadr pair) walk)))
      (walk-body body walk))))

(define (walk-let e walk)
  (let ((binds (cadr e))
        (body (cddr e)))
    (cond ((nil? binds) (walk-body body walk))
          (#t (dolist (pair binds)
                (binding ((*tail-position* #f))
                  (walk-form (cadr pair) walk)))
              (with-expression-context (body)
                ((walk 'scope) 'let binds body)
                (walk-body body walk))))))

(define (walk-lambda e walk in-call-position)
  (let* ((args (caddr e))
         (body (cdddr e))
         (inline? '()))
    (with-expression-context (body)
      (set! inline? (or in-call-position (eq #t (ctx-annot 'inline)))))
    (cond
      ((and inline? (nil? args)) (walk-body body walk))
      (inline? (with-expression-context (body)
                 ((walk 'scope) 'inline-lambda args body)
                 (walk-body body walk)))
      (#t (with-expression-context (body)
            ((walk 'scope) 'lambda args body)
            (walk-body body walk))))))

(define (walk-if e walk)
  (binding ((*tail-position* #f)) (walk-form (second e) walk))
  (walk-form (third e) walk)
  (walk-form (fourth e) walk))

(define (walk-form e walk)
  (cond ((symbol? e) ((walk 'var) e))
        ((pair? e)
         (let ((was-in-call-position *in-call-position*))
           (binding ((*in-call-position* #f))
             (case (car e)
               (quote) ;; do nothing
               (if (walk-if e walk))
               (set! (binding ((*being-set* #t)) (walk-form (second e) walk))
                     (walk-form (third e) walk))
               (#/lang/%letrec (walk-letrec e walk))
               (#/lang/%let (walk-let e walk))
               (#/lang/%nlambda (walk-lambda e walk was-in-call-position))
               (with-special-binding (binding ((*tail-position* #f)) (walk-body (cddr e) walk)))
               (#t (binding ((*in-call-position* #t)) (walk-form (car e) walk))
                   (dolist (e (cdr e)) (walk-form e walk)))))))))


(define (walk-variables e fn)
  (let ((walk (lambda (what)
                (case what
                  (scope (lambda (a b c)))
                  (var fn)))))
    (walk-form e walk)))
(define (walk-scopes e fn)
  (let ((walk (lambda (what)
                (case what
                  (scope fn)
                  (var (lambda (a)))))))
    (walk-form e walk)))

(define (%mark-scopes e)
  (walk-scopes
   e (lambda (name binds body) (ctx-annot-put 'type name))))

(define (%mark-bindings e)
  (walk-scopes
   e (lambda (name binds body)
       (let ((idx 0))
         (dolist (b (ensure-list binds))
           (let* ((b (ensure-list b))
                  (sym (car b))
                  (form (cadr b)))
             (declare-local-binding sym)
             (expr-set-meta sym 'type (if (eq name 'lambda) 'argument 'local))
             (expr-set-meta sym 'index idx)
             (expr-set-meta sym 'bound-form form)
             (set! idx (+ 1 idx))))))))

(define (%mark-reference-counts e)
  (walk-variables
   e (lambda (e)
       (unless (nil? (expr-meta e 'type))
         (expr-set-meta e 'used #t)
         (expr-set-meta e 'reference-count (+ 1 (expr-meta e 'reference-count 0)))
         (expr-set-meta e (cond (*in-call-position* 'called)
                                (*being-set* 'mutated)
                                (#t 'value-taken))
                        #t)))))

(define (%symbol-is-in-jump-position sym)
  (and *enable-jump-opts*
       ;; *tail-position* ;; XXX support for this in walk-form is not ready yet
       *in-call-position*
       (eq sym *binding-name*)
       (eq 1 (enclosing-lambda-count sym))))

(define (%binding-is-inlineable-lambda? b)
  (let ((sym (first b)) (form (second b)))
    (and (pair? form)
         (eq '#/lang/%nlambda (car form))
         ;; not varargs?
         (not (symbol? (car (cddr form))))
         ;; XXX hack not optional args?
         (not (eq (car (cdddr form)) '%%has-optionals))
         (expr-meta sym 'called #f)
         (not (expr-meta sym 'crosses-lambda-in-initial-pass #f))
         (not (expr-meta sym 'value-taken #f))
         (not (expr-meta sym 'mutated #f)))))

(define (%binding-prepare-for-lambda-inlining b)
  (let* ((sym (car b))
         (form (cadr b))
         (lambda-body (cdddr form))
         (lambda-args (caddr form)))
    (expr-set-meta sym 'type 'inline)
    (expr-set-meta sym 'body lambda-body)
    (with-expression-context (lambda-body)
      (ctx-annot-put 'type 'inline-lambda)
      (ctx-annot-put 'inline #t)
      ;; FIXME: right idea, but the test is not quite right?
      (ctx-annot-put 'fully-inlined (nil? lambda-args))
      (ctx-annot-put 'entry-label (list 'entry-label)))))

(define (%note-inlineable-let-bound-lambdas e)
  (walk-variables
   e (lambda (e)
       (unless (or (nil? (expr-meta e 'type))
                   (%symbol-is-in-jump-position e))
         (when (binding-crosses-lambda e) (expr-set-meta e 'crosses-lambda-in-initial-pass #t)))))
  (walk-scopes
   e (lambda (name binds body)
       (cond
         ((and *enable-inline-letrec-bound-lambdas* (eq name 'letrec) (eq 1 (length binds)))
          (when (%binding-is-inlineable-lambda? (first binds))
            ;; (print `(could inline letrec-bound lambda: ,(first binds)))
            (%binding-prepare-for-lambda-inlining (first binds))))
         ((and *enable-inline-let-bound-lambdas* (eq name 'let) (eq 1 (length binds)))
          (dolist (b binds)
            (when (%binding-is-inlineable-lambda? b)
              ;; (print `(could inline let-bound lambda: (,(expr-meta sym 'reference-count)) ,b))
              (%binding-prepare-for-lambda-inlining b))))))))

(define (%mark-closed-over-bindings e)
  (walk-variables
   e (lambda (e)
       (when (binding-crosses-lambda e)
         (when *note-closed-over-vars* (print `(closing over var: ,e)))
         (binding-context-annot e 'closed-over #t)
         (expr-set-meta e 'type 'closure))))
  (walk-variables
   e (lambda (e)
       (when (eq (expr-meta e 'type) 'closure)
         (containing-contexts-annot e 'contains-closure #t)))))

(define (analyse-forms e)
  (%mark-scopes e)
  (%mark-bindings e)
  (%mark-reference-counts e)
  (when (or *enable-inline-let-bound-lambdas* *enable-inline-letrec-bound-lambdas*)
    (%note-inlineable-let-bound-lambdas e))
  (%mark-closed-over-bindings e))

(forward emit-expr)

(define (emit-if it env)
  (let ((done (gensym))
        (false (gensym)))
    (binding ((*tail-position* #f)) (emit-expr (second it) env))
    (jump-if-false false)
    (emit-expr (third it) env)
    (jump done)
    (label false)
    (emit-expr (fourth it) env)
    (label done)))


(define (%call-get-binding-name it)
  (if (and (pair? it)
           (eq (car it) 'set-symbol-value)
           (pair? (cadr it))
           (eq (caadr it) 'quote))
      (second (cadr it))
      *binding-name*))

(define (emit-body it env)
  (let ((last (- (length it) 1))
        (idx 0))
    (emit-pair PUSHLIT (emit-lit '()))
    (dolist (e it)
      (emit-u16 POP)
      (binding ((*tail-position* (and *tail-position* (= idx last))))
        (emit-expr e env))
      (set! idx (+ 1 idx)))))

(define (emit-let it env &opt (binds (cadr it)) (body (cddr it)))
  (cond
    ((nil? binds) (emit-body body env))
    (#t (let* ((count (length binds))
               (closed? (expression-context-is-closed-over body)))
          (let* ((idx 0)
                 (closure-idx 0)
                 ;; this is wasteful -- we are reserving stack space for items
                 ;; which wind up stored in the closure or inlined
                 (start (reserve-tmps count)))
            (dolist (pair binds)
              (let ((sym (car pair)))
                (binding ((*tail-position* #f)) (emit-expr (second pair) env))
                (with-expression-context (body)
                  (case (expr-meta sym 'type)
                    ;; do nothing
                    (inline)
                    (local
                     (store-tmp (+ idx start))
                     (expr-set-meta sym 'index (+ idx start)))
                    (closure
                     ;; leave on stack to be picked up by push-closure
                     (expr-set-meta sym 'closure-index closure-idx)
                     (set! closure-idx (+ 1 closure-idx)))))
                (set! idx (+ 1 idx))))
            (with-expression-context (body)
              (binding ((*closure-depth* (+ (if closed? 1 0) *closure-depth*)))
                (when closed? (push-closure closure-idx))
                (emit-body body env)
                (when closed? (pop-closure)))))))))

(define (zip a b)
  (let loop ((as a) (bs b) (r '()))
       (if (or (nil? as) (nil? bs))
           (reverse-list r)
           (loop (cdr as) (cdr bs) (cons (list (car as) (car bs)) r)))))

(define (emit-inline-lambda-call it env)
  (let* ((lambda (car it))
         (args (caddr lambda))
         (body (cdddr lambda))
         (vals (cdr it))
         (binds (zip args vals)))
    ;; (print `(emitting inline lambda call: ,lambda))
    (emit-let '() env binds body)))

(define (emit-inline-lambda-body env args body)
  (let ((closed? (expression-context-is-closed-over body)))
    (context-write body 'closure-depth *closure-depth*)
    (context-write body 'closed closed?)
    (cond
      ((nil? args) (emit-body body env))
      (#t (let* ((start (context-read body 'initial-arg-index))
                 (idx 0))
            ;; (if closed?
            ;;     (print `(emitting closed over inline body: ,body))
            ;;     (print `(emitting inline body: ,body)))
            (with-expression-context (body)
              (dolist (arg args)
                (cond (closed?
                       (load-tmp (+ idx start))
                       (expr-set-meta arg 'type 'closure)
                       (expr-set-meta arg 'closure-index idx))
                      (#t
                       (expr-set-meta arg 'type 'local)
                       (expr-set-meta arg 'index (+ idx start))))
                (set! idx (+ 1 idx)))
              (binding ((*closure-depth* (+ (if closed? 1 0) *closure-depth*)))
                (when closed? (push-closure idx))
                (emit-body body env)
                (when closed? (pop-closure)))))))))

(define (emit-inline-lambda it env)
  (binding ((*inlining-lambda* #t))
    ;; (print `(inlining lambda: ,it))
    (let* ((args (caddr it))
           (body (cdddr it))
           (hop (gensym)))
      ;; save initial arg index
      (context-write body 'initial-arg-index (reserve-tmps (length args)))
      ;; jump over inlined code
      (jump hop)
      ;; write entry label
      (label (context-read body 'entry-label))
      ;; emit body
      (binding ((*tail-position* #t)) (emit-inline-lambda-body env args body))
      ;; jump to exit label
      (emit-u16 SWAP)
      (emit-u16 POP_JUMP)
      ;; end of inline code
      (label hop))))

(defmacro trc (f)
  `(let ((xxx ,f))
     (print (list ',f '= xxx))
     xxx))

(define (emit-call-to-inlined-lambda it env)
  (let* ((sym (car it))
         (args (cdr it))
         (body (expr-meta sym 'body))
         (closure-diff (- *closure-depth* (context-read body 'closure-depth)))
         (closed? (context-read body 'closed))
         (idx (context-read body 'initial-arg-index))
         (self-call? (and *tail-position* (eq (car it) *binding-name*))))
    ;; (print `(emitting inlined call: ,it for: ,(expr-meta sym 'body)))
    ;; write args to temp slots
    (dolist (arg args)
      (set! idx (+ 1 idx))
      (binding ((*tail-position* #f)) (emit-expr arg env)))
    (dolist (arg args)
      (set! idx (- idx 1))
      (store-tmp idx))
    ;; prepare closure depth for inlined env
    (when closed?
      (when (and (not self-call?) (> closure-diff 0))
        (emit-u16 SAVE_CLOSURE_ENV)
        (emit-u16 closure-diff))
      (when self-call?
        (dotimes (_ closure-diff)
          (pop-closure))))
    ;; push pc of return label
    (let ((return-label (list 'return-label it)))
      (unless self-call?
        (emit-u16 PUSH_JUMP)
        (save-jump-location return-label))
      ;; jump to entry label
      (jump (context-read body 'entry-label))
      ;; write return label
      (label return-label))
    ;; restore closure (now beneath call's return value)
    (when (and closed? (not self-call?) (> closure-diff 0))
      (emit-u16 SWAP)
      (emit-u16 RESTORE_CLOSURE_ENV))))

(define (emit-letrec it env)
  (let* ((binds (cadr it))
         (body (cddr it))
         (count (length binds))
         (closed? (expression-context-is-closed-over body)))
    (let* ((idx 0)
           (closure-idx 0)
           ;; this is wasteful -- we are reserving stack space for items
           ;; which wind up stored in the closure
           (start (reserve-tmps count)))
      (with-expression-context (body)
        (dolist (pair binds)
          (let ((sym (car pair)))
            (case (expr-meta sym 'type)
              (local (expr-set-meta sym 'index (+ idx start)))
              (closure (expr-set-meta sym 'closure-index closure-idx)
                       (set! closure-idx (+ 1 closure-idx))
                       ;; set initial closure value as nil (picked up by push-closure)
                       (emit-pair PUSHLIT (emit-lit '())))))
          (set! idx (+ 1 idx))))
      (with-expression-context (body)
        (binding ((*closure-depth* (+ (if closed? 1 0) *closure-depth*)))
          (when closed? (push-closure closure-idx))
          (dolist (pair binds)
            (let ((sym (car pair)))
              (binding ((*tail-position* #f) (*binding-name* sym))
                (emit-expr (second pair) env))
              (case (expr-meta sym 'type)
                (local (store-tmp (expr-meta sym 'index)))
                (closure (store-closure (expr-meta sym 'closure-index)
                                        (closure-depth sym))))))
          (emit-body body env)
          (when closed? (pop-closure)))))))

(define (emit-flat-lambda it env)
  (let* ((args (caddr it))
         (body (cdddr it))
         (bc (with-output-to-bytecode ()
               (set '*varargs* (symbol? args))
               (set '*lambda-name* (cadr it))
               (label 'start)
               (label 're-entry)
               (with-expression-context (body)
                 (binding ((*tail-position* #t) (*closure-depth* 0)) (emit-body body env))))))
    (if (eq #t (context-read body 'contains-closure))
        (let ()
          (emit-pair PUSHLIT (emit-lit bc))
          (emit-u16 BUILD_CLOSURE))
        (emit-pair PUSHLIT (emit-lit (bytecode->closure bc))))))

(define (emit-lambda it env)
  (binding ((*inlining-lambda* #f))
    (let ((body (cdddr it)))
      (cond ((eq #t (context-read body 'inline))
             (emit-inline-lambda it env))
            ((expression-context-is-closed-over body)
             (let* ((args (caddr it))
                    (closure-idx 0)
                    (arg-idx 0)
                    (bc (with-output-to-bytecode ()
                          (set '*varargs* (symbol? args))
                          (set '*lambda-name* (cadr it))
                          (label 'start)
                          (with-expression-context (body)
                            (dolist (a (ensure-list args))
                              (when (eq 'closure (expr-meta a 'type))
                                (expr-set-meta a 'closure-index closure-idx)
                                (set! closure-idx (+ 1 closure-idx))
                                (load-arg arg-idx))
                              (set! arg-idx (+ 1 arg-idx)))
                            (push-closure closure-idx)
                            (label 're-entry)
                            (binding ((*tail-position* #t)
                                      (*closure-depth* 1))
                              (emit-body body))
                            (pop-closure)))))
               (emit-pair PUSHLIT (emit-lit bc))
               (emit-u16 BUILD_CLOSURE)))
            (#t (emit-flat-lambda it env))))))

(define (emit-with-special-binding it env)
  (let ((sym (second it))
        (val (third it))
        (exp (fourth it)))
    (emit-pair PUSHLIT (emit-lit sym))
    (binding ((*tail-position* #f)) (emit-expr val env))
    (emit-u16 PUSH_SPECIAL_BINDING)
    ;; XXX is this restriction on tail position really needed?
    (binding ((*tail-position* #f)) (emit-expr exp env))
    (emit-u16 POP_SPECIAL_BINDING)))

(define (emit-set! it env)
  (let* ((sym (second it))
         (val (third it))
         (type (expr-meta sym 'type)))
    (binding ((*tail-position* #f)) (emit-expr val env))
    (emit-u16 DUP) ;; return the result
    (case type
      (() (throw `(cannot set! to global variable: ,it)))
      (argument (throw `(cannot set! to argument: ,it)))
      (local
       (emit-pair STORE_FRAME_RELATIVE (expr-meta sym 'index)))
      (closure
       (emit-u16 STORE_CLOSURE)
       (emit-u16 (expr-meta sym 'closure-index))
       (emit-u16 (closure-depth sym))))))

;; should be true iff it is a self tail-call
(define (call-can-be-jump-optimized? it env)
  (and *tail-position*
       (if (symbol? (car it))
           (let ((sym (car it)))
             (and (eq sym *binding-name*)
                  (eq 1 (enclosing-lambda-count sym))))
           #f)))

(define (call-is-inlineable-lambda? it env)
  (and (pair? (car it))
       (eq (caar it) '#/lang/%nlambda)))

(define (call-is-to-inlined-lambda? it env)
  (and (symbol? (car it))
       (eq 'inline (expr-meta (car it) 'type))))

(define (call-is-jump-to-inlined-lambda? it env)
  ;; XXX hack need to remove this binding
  ;;     doing it for now as all use sites of this are known
  ;;     for the moment, but need a way to
  ;;     determine if we are in tail position
  ;;     and avoid emitting tail calls (in inline lambda case)
  ;;     will likely just add another dynamic var
  (binding ((*tail-position* #t))
    (and (call-can-be-jump-optimized? it env) (call-is-to-inlined-lambda? it env))))

(define (%emit-call it env)
  (cond
    ((and *enable-inline-letrec-bound-lambdas* (call-is-jump-to-inlined-lambda? it env))
     (emit-call-to-inlined-lambda it env))
    ((call-is-to-inlined-lambda? it env)
     (emit-call-to-inlined-lambda it env))
    ((call-is-inlineable-lambda? it env)
     (emit-inline-lambda-call it env))
    ((and *enable-jump-opts* (call-can-be-jump-optimized? it env))
     ;; this is a self call reduced to a jump
     (binding ((*tail-position* #f))
       ;; (print `(jumping to start: ,it))
       (let* ((idx 0)
              (closed? (> *closure-depth* 1))
              (bound-form
               (if (nil? (expr-meta (car it) 'type))
                   ;; XXX hack this is nasty
                   ;; need a toplevel *expr-context* instead
                   (third *toplevel-form*)
                   (expr-meta (car it) 'bound-form)))
              (args (caddr bound-form)))
         (when (not (eq (length args) (length (cdr it))))
           (throw `(bad inlined call arg count does not match: ,args ,it)))
         (dolist (e (cdr it))
           (emit-expr e env)
           (set! idx (+ 1 idx)))
         (dolist (e (reverse-list args))
           (set! idx (- idx 1))
           ;; (print `(saving expr ,e as arg ,idx))
           (case (expr-meta e 'type)
             (argument (emit-pair STORE_ARG idx))
             (local (emit-pair STORE_ARG idx))
             (closure
              (store-closure (expr-meta e 'closure-index)
                             (closure-depth e)))))
         ;; (print `(exiting from ,*closure-depth* closures))
         (when closed?
           (dotimes (_ (- *closure-depth* 1))
             (pop-closure))))
       (jump 're-entry)))
    (#t (let ((argc 0))
          (binding ((*tail-position* #f))
            (dolist (e (cdr it))
              (set! argc (+ 1 argc))
              (emit-expr e env))
            (emit-expr (car it) env))
          (emit-pair (if (and *tail-position* (not *inlining-lambda*)) TAIL_CALL CALL) argc))) ))

;; XXX HACK: rather than special-casing set-symbol-value here, we
;; should have an internal %define form and a root *expr-context* that
;; represents the top level, and handle things in the analyse step.
(define (emit-call it env)
  (let ((binding-name (%call-get-binding-name it)))
    (if (eq binding-name *binding-name*)
        (%emit-call it env)
        (binding ((*binding-name* binding-name)
                  (*toplevel-form* it))
          (%emit-call it env)))))

(define (emit-expr it env)
  (cond
    ((symbol? it)
     (let ((type (expr-meta it 'type)))
       ;; (when *enable-inline-let-bound-lambdas*
       ;;   (print `(loading symbol ,it of type ,type)))
       (case type
         (()
          (emit-pair PUSHLIT (emit-lit it))
          (emit-u16 (if (special-symbol? it) LOAD_SPECIAL LOAD_GLOBAL)))
         (local
          (load-tmp (expr-meta it 'index)))
         (argument
          (load-arg (expr-meta it 'index)))
         (closure
          (let ((slot  (expr-meta it 'closure-index))
                (depth (closure-depth it)))
            ;; (when *enable-inline-let-bound-lambdas*
            ;;   (print `(loading closure ,it ,slot ,depth)))
            (load-closure slot depth)))
         (#t (throw `(bad type for symbol ,it ,type))))))
    ((pair? it)
     (let ((head (car it)))
       (case head
         (quote  (emit-pair PUSHLIT (emit-lit (cadr it))))
         (if     (emit-if it env))
         (set!   (emit-set! it env))
         (#/lang/%letrec (emit-letrec it env))
         (#/lang/%let (emit-let it env))
         (#/lang/%nlambda  (emit-lambda it env))
         (with-special-binding (emit-with-special-binding it env))
         (#t     (emit-call it env)))))
    (#t
     (emit-pair PUSHLIT (emit-lit it)))))

(defmacro XX (form)
  `(try-catch (lambda () ,form)
              (lambda (ex)
                (let ((f ',form))
                  (print (list 'EXCEPTION 'IN f ex))
                  (throw ex)))))

(define (eval expr)
  (when *trace-eval* (print `(expanding: ,expr)))
  (let ((expanded (macroexpand (quasiquote-expand expr))))
    (when *trace-eval* (print `(compiling: ,expanded)))
    (let ((thunk (binding ((*context-table* (make-ht)))
                   (when *trace-eval* (print `(analysing forms)))
                   (binding ((*toplevel-form* expanded))
                     (analyse-forms expanded)
                     (when *trace-eval* (print `(emitting bytecode)))
                     (XX
                      (bytecode->closure (with-output-to-bytecode ()
                                           (with-expression-context (expanded)
                                             (emit-expr expanded '())))))))))
      (when *trace-eval* (print `(evaluating: ,thunk)))
      (thunk))))

(defmacro let (bindings & body)
  (if (symbol? bindings)
      (let* ((symbol bindings)
             (bindings (car body))
             (body (cdr body))
             (args (mapcar car bindings))
             (inits (mapcar cadr bindings)))
        `(#/lang/%letrec ((,symbol (lambda ,args ,@body)))
                  (,symbol ,@inits)))
      `(#/lang/%let ,bindings ,@body)))
