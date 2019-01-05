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
                                    (list 'list (qq-xform form))))
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
(set 'qq-process
     (lambda (expr)
       (if (pair? expr)
           (let ((sym (car expr)))
             (if (eq sym 'quote) expr
                 (if (eq sym 'quasiquote) (qq-xform (car (cdr expr)))
                     (mapcar qq-process expr))))
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
       ;; (print `(expanding: ,expr))
       (if (pair? expr)
           (let ((sym (car expr)))
             (if (eq sym 'quote) expr
                 (if (eq sym 'let) (mx-process-let expr)
                     (if (eq sym 'lambda) (mx-process-lambda expr)
                         (let ((expander (ht-at macro-functions sym)))
                           (if (not (nil? expander))
                               (let ((expansion (expander expr)))
                                 (macroexpand expansion))
                               (mapcar macroexpand expr)))))))
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

(set-macro-function
 'lambda-bind
 (lambda (expr)
   (let ((vars (car (cdr expr)))
         (form (car (cdr (cdr expr))))
         (body (cdr (cdr (cdr expr)))))
     (let ((name (gensym)))
       (if (nil? vars)
           `(let ((,name ,form)) ,@body)
           (if (symbol? vars)
               `(let ((,vars ,form)) ,@body)
               (if (eq (car vars) '&)
                   `(let ((,(car (cdr vars)) ,form)) ,@body)
                   `(let ((,(car vars) (car ,form)))
                      (let ((,name (cdr ,form)))
                        (lambda-bind ,(cdr vars) ,name ,@body))))))))))

(set-macro-function
 'defmacro
 (lambda (form)
   (lambda-bind
    (_ name params & body) form
    (let ((sym (gensym)))
      `(set-macro-function
        ',name
        (lambda (,sym)
          (lambda-bind ,params (cdr ,sym) ,@body)))))))

(defmacro define (binding & body)
  (if (pair? binding)
      (let ((name (car binding))
            (params (cdr binding)))
        `(let ()
           (set-symbol-value ',name #f)
           (set-symbol-value ',name (lambda ,params ,@body))))
      `(set-symbol-value ',binding ,(car body))))

(defmacro let* (bindings & body)
  (if (nil? (cdr bindings))
      `(let (,(car bindings))
         ,@body)
      `(let (,(car bindings))
         (let* ,(cdr bindings)
           ,@body))))

(defmacro cond (& clauses)
  (let ((clause (car clauses))
        (rest (cdr clauses)))
    (if (nil? rest)
        (cons 'if clause)
        `(if ,@clause (cond ,@rest)))))

(define (reverse-list lst)
    (let ((helper #f))
      (set! helper (lambda (rem acc)
                     (if (nil? rem) acc
                         (helper (cdr rem) (cons (car rem) acc)))))
      (helper lst '())))

(define (reduce-list fn seed list)
    (let ((helper #f)
          (acc seed))
      (set! helper (lambda (lst)
                     (if (nil? lst) acc
                         (let ()
                           (set! acc (fn acc (car lst)))
                           (helper (cdr lst))))))
      (helper list)
      acc))

(define (list-length lst)
    (reduce-list (lambda (acc _) (+i acc 1)) 0 lst))

(define (implode lst-of-chars)
    (let ((str (make-string (list-length lst-of-chars) #\Space)))
      (reduce-list (lambda (idx chr)
                     (char-at-put str idx chr)
                     (+i 1 idx))
                   0 lst-of-chars)
      (intern str)))

;; BUG!! could not properly nest backquotes...
(defmacro case (subj & tests)
  (let* ((name (gensym))
         (xform (lambda (it) `((eq ,name (quote ,(car it))) ,@(cdr it)))))
    `(let ((,name ,subj))
       (cond ,@(mapcar xform tests)))))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaar x) (car (car (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))

(define first car)
(define second cadr)
(define third caddr)



;;; eval
(set 'eval (lambda (x) ((compile-to-closure x))))
