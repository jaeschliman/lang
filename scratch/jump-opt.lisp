(set '*package* (symbol-package 'define))

(defmacro trc (x)
  `(let ((xxx ,x))
     (print xxx)
     xxx))

(binding ((*enable-jump-opts*))
         (eval `(define (my-reverse-list lst)
                    (let ((helper #f))
                      (set! helper (lambda (rem acc)
                                     (if (nil? rem) acc
                                         (helper (cdr rem) (trc (cons (car rem) acc))))))
                      (helper lst '())))))

(print (my-reverse-list '(1 2 3 4 5 6 7 8 9 10)))

(binding ((*enable-jump-opts*))
         (eval `(define (iota n)
                    (if (< n 0) 'done
                        (let ()
                          (print n)
                          (iota (- n 1)))))))

(print (iota 10))

;; FIXME: can only support one binding at a time for now.
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
                         (print `(setting initial nil))
                         (emit-pair PUSHLIT (emit-lit '())))))
            (set! idx (+ 1 idx))))
        (with-expression-context (body)
          (binding ((*closure-depth* (+ (if closed? 1 0) *closure-depth*)))
                   (when closed? (push-closure closure-idx))
                   (dolist (pair binds)
                     (let ((sym (car pair)))
                       (print `(emitting expr for binding: ,(second pair)))
                       (binding ((*tail-position* #f)) (emit-expr (second pair) env))
                       (case (expr-meta sym 'type)
                         (local (store-tmp (+ idx start)))
                         (closure
                          (print `(storing at: ,(expr-meta sym 'closure-index)))
                          (store-closure (expr-meta sym 'closure-index) *closure-depth*)))))
                   (emit-body body env)
                   (when closed? (pop-closure)))))))


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


(print 'here)
(binding ((*enable-jump-opts* #t) (*trace-eval* #t))
         (eval `(define (iota2 n)
                    (let -iota ((n n))
                         (if (< n 0) 'done
                             (let ()
                               (print n)
                               (-iota (- n 1))))))))

(print (iota2 10))


(print 'done)
