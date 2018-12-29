(set-symbol-value 'set set-symbol-value)

;;; quasiquote support

(set 'has-unquote-splicing #f)
(set 'has-unquote-splicing
     (lambda (lst)
       (if (nil? lst) #f
           (if (pair? (car lst))
               (if (eq (car (car lst)) 'unquote-splicing) #t
                   (has-unquote-splicing (cdr lst)))
               (has-unquote-splicing (cdr lst))))))

(set 'mapcar #f)
(set 'mapcar (lambda (f lst)
               (if (nil? lst) lst
                   (cons (f (car lst)) (mapcar f (cdr lst))))))

(set 'qq-xform #f)

(set 'qq-unq-spl-form (lambda (form)
                        (if (pair? form)
                            (if (eq (car form) 'unquote)
                                (list 'list (car (cdr form)))
                                (if (eq (car form) 'unquote-splicing)
                                    (car (cdr form))
                                    (qq-xform form)))
                            (list 'list (list 'quote form)))))

(set 'qq-xform-for-unq-spl (lambda (lst) (cons 'append (mapcar qq-unq-spl-form lst))))

(set 'qq-unq-form (lambda (form)
                    (if (pair? form)
                        (if (eq (car form) 'unquote)
                            (car (cdr form))
                            (qq-xform form))
                        (list 'quote form))))

(set 'qq-xform-for-unq (lambda (lst) (cons 'list (mapcar qq-unq-form lst))))

(set 'qq-xform (lambda (x)
                 (if (pair? x)
                     (if (has-unquote-splicing x)
                         (qq-xform-for-unq-spl x)
                         (qq-xform-for-unq x))
                     (list 'quote x))))

(set 'append3 #f)
(set 'append3 (lambda (a b cs)
                (if (nil? a)
                    (if (nil? cs) b
                        (append3 b (car cs) (cdr cs)))
                    (cons (car a) (append3 (cdr a) b cs)))))

(set 'append (lambda args (append3 (car args) (car (cdr args)) (cdr (cdr args)))))

(set 'qq-process #f)

(set 'qq-process-let-binding
     (lambda (bind) (list (car bind) (qq-process (car (cdr bind))))))

(set 'qq-process-let
     (lambda (x)
       (append (list 'let
                (mapcar qq-process-let-binding (car (cdr x))))
               (mapcar qq-process (cdr (cdr x))))))

(set 'qq-process-lambda
     (lambda (x) (append (list 'lambda (car (cdr x))) (mapcar qq-process (cdr (cdr x))))))

(set 'qq-process
     (lambda (expr)
       (if (pair? expr)
           (let ((sym (car expr)))
             (if (eq sym 'let) (qq-process-let expr)
                 (if (eq sym 'lambda) (qq-process-lambda expr)
                     (if (eq sym 'quasiquote) (qq-xform (car (cdr expr)))
                         (mapcar qq-process expr)))))
           expr)))

(set 'compiler qq-process)
;;; macro support

(set 'macro-functions (make-ht))
(set 'set-macro-function (lambda (sym fn) (ht-at-put macro-functions sym fn)))

(set 'macroexpand #f)

(set 'mx-process-let-binding
     (lambda (bind)
       (list (car bind) (macroexpand (car (cdr bind))))))

(set 'mx-process-let
     (lambda (expr)
       `(let ,(mapcar mx-process-let-binding (car (cdr expr)))
          ,@(mapcar macroexpand (cdr (cdr expr))))))

(set 'mx-process-lambda
     (lambda (expr)
       `(lambda ,(car (cdr expr))
          ,@(mapcar macroexpand (cdr (cdr expr))) )))

(set 'macroexpand
     (lambda (expr)
       (if (pair? expr)
           (let ((sym (car expr)))
             (if (eq sym 'let) (mx-process-let expr)
                 (if (eq sym 'lambda) (mx-process-lambda expr)
                     (let ((expander (ht-at macro-functions sym)))
                       (if (not (nil? expander))
                           (let ((expansion (expander expr)))
                             (macroexpand expansion))
                           (mapcar macroexpand expr))))))
           expr)))

(set 'compiler (lambda (expr) (macroexpand (qq-process expr))))
;;; basic macros

(set-macro-function
 'and
 (lambda (expr)
   (let ((test (car (cdr expr)))
         (rest (cdr (cdr expr))))
     (if (nil? test)
         #t
         (if (not (nil? rest))
             (let ((name (gensym)))
               `(let ((,name ,test))
                  (if ,name (and ,@rest) #f)))
             test)))))

(set-macro-function
 'or
 (lambda (expr)
   (let ((test (car (cdr expr)))
         (rest (cdr (cdr expr))))
     (if (nil? test)
         #f
         (if (not (nil? rest))
             (let ((name (gensym)))
               `(let ((,name ,test))
                  (if ,name ,name (or ,@rest))))
             test)))))
;;; eval
(set 'eval (lambda (x) ((compile-to-closure x))))
