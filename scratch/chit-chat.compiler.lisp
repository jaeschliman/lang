
(defparameter *cc-in-block* #f)

(define cc-compile-expression #f)

(define (cc-remove-dead-code es)
  (let loop ((e es) (result '()))
       (if-nil? e (reverse-list result)
                (if (and (pair? (car e)) (eq 'return (caar e)))
                    (reverse-list (cons (car e) result))
                    (loop (cdr e) (cons (car e) result))))))

(define (cc-compile-block e)
    (let* ((info (cdr e))
           (args (plist-get :args info))
           (vars (plist-get :vars info))
           (body (plist-get :body info)))
      (binding ((*cc-in-block* #t))
        `(lambda ,args (let ,(mapcar (lambda (s) (list s '())) vars)
                         ,@(mapcar cc-compile-expression (cc-remove-dead-code body)))))))

(define (cc-compile-expression e)
    (if (pair? e)
        (let ()
          (case (car e)
            (quote e)
            (lisp (cadr e))
            (load (cadr e)) ;; TODO: ivar support
            (set! `(set! ,(cadr e) ,(cc-compile-expression (caddr e))))
            (block (cc-compile-block e))
            (return (let ((result (cc-compile-expression (cadr e))))
                      (if *cc-in-block* `(return-from-mark %invocation-tag ,result)
                          `(set! %result ,result))))
            (send `(@send ,(cadr e) ,@(mapcar cc-compile-expression (cddr e))))))
        e))


(define (cc-compile-method-body args vars body)
  `(lambda (self ,@args)
     (let ((%invocation-tag (cons '() '()))
           (%result self)
           ,@(mapcar (lambda (s) (list s '())) vars))
       (set-stack-mark %invocation-tag)
       ,@(mapcar cc-compile-expression (cc-remove-dead-code body))
       %result)))

(define (cc-compile-method m)
  (let* ((class? (eq (car m) 'class))
         (info (cdr m))
         (cls  (plist-get :class info))
         (name (plist-get :name  info))
         (args (plist-get :args  info))
         (vars (plist-get :vars  info))
         (body (plist-get :body  info))
         (class (if class? `(class-of ,cls) cls)))
    `(class-set-method ,class ',name ,(cc-compile-method-body args vars body))))

(define (cc-compile-script info)
  (let* ((vars (plist-get :vars  info))
         (body (plist-get :body  info)))
    (cc-compile-method-body '() vars body)))

(defmacro chitchat-methods (methodlist)
  `(let ()
     ,@(mapcar cc-compile-method methodlist)))


