
(defparameter *cc-in-block* #f)

(define cc-compile-expression #f)

(define (cc-compile-block e)
    '())

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
                      (if *cc-in-block* `(escape-to-tag %invocation-tag ,result)
                          `(set! %result ,result))))
            (send `(@send ,(cadr e) ,@(mapcar cc-compile-expression (cddr e))))))
        e))

(define (cc-remove-dead-code es)
    (let ((loop #f))
      (set! loop (lambda (e result)
                   (if-nil? e (reverse-list result)
                            (if (and (pair? (car e)) (eq 'return (caar e)))
                                (reverse-list (cons (car e) result))
                                (loop (cdr e) (cons (car e) result))))))
      (loop es '())))

(define (cc-compile-method-body args vars body)
    `(lambda (self ,@args)
       (let ((%invocation-tag (cons '() '()))
             (%result self)
             ,@(mapcar (lambda (s) (list s '())) vars))
         (set-stack-mark %invocation-tag)
         ,@(mapcar cc-compile-expression body)
         %result)))

(define (cc-compile-method m)
    (let* ((info (cdr m))
           (cls  (plist-get :class info))
           (name (plist-get :name  info))
           (args (plist-get :args  info))
           (vars (plist-get :vars  info))
           (body (plist-get :body  info)))
      (trace 
       `(class-set-method ,cls ',name ,(cc-compile-method-body args vars body)))))


(defmacro chitchat-methods (methodlist)
  `(let ()
     ,@(mapcar cc-compile-method methodlist)))


