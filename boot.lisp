(set-symbol-value 'set set-symbol-value)

;; helper functions

(set 'mapcar #f)
(set 'mapcar (lambda (f lst)
               (if (nil? lst) lst
                   (cons (f (car lst)) (mapcar f (cdr lst))))))

(set 'list-every #f)
(set 'list-every
     (lambda (pred lst)
       (if (nil? lst) #t
           (if (pred (car lst)) (list-every pred (cdr lst))
               #f))))

(set 'append3 #f)
(set 'append3 (lambda (a b cs)
                (if (nil? a)
                    (if (nil? cs) b
                        (append3 b (car cs) (cdr cs)))
                    (cons (car a) (append3 (cdr a) b cs)))))

(set 'append (lambda args (append3 (car args) (car (cdr args)) (cdr (cdr args)))))

;; ----------------------------------------
;;; nested quasiquote support
;;; nested support not yet well-tested

(set 'qq-process #f) ;; entry point for quasiquoting (toplevel function)
(set 'qq-xform #f)   ;; fn that begins qq-transforming a given form at lvl

(set 'qq-is-unq-sym (lambda (sym)
                      (if (eq sym 'unquote) #t
                          (eq sym 'unquote-splicing))))

(set 'qq-has-unq (lambda (f) (qq-is-unq-sym (car f))))
(set 'qq-has-unq-level #f)
(set 'qq-has-unq-level (lambda (form lvl)
                         (if (not (pair? form)) #f
                             (if (eq lvl 0) (qq-has-unq form)
                                 (if (not (qq-has-unq form)) #f
                                     (qq-has-unq-level (car (cdr form)) (-i lvl 1)))))))


(set 'qq-unq-form #f)
(set 'qq-unq-form-unq
     (lambda (form lvl sym)
       (if (eq lvl 0)
           (if (eq sym 'unquote)
               (list 'list (qq-process (car (cdr form))))
               (qq-process (car (cdr form))))
           (if (qq-has-unq-level form lvl)
               (qq-unq-form (car (cdr form)) (-i lvl 1))
               (list 'list
                     (list 'list (list 'quote sym)
                           (qq-xform (car (cdr form)) (-i lvl 1))))))))

(set 'qq-unq-form
     (lambda (form lvl)
       (if (pair? form)
           (let ((sym (car form)))
             (if (qq-is-unq-sym sym)
                 (qq-unq-form-unq form lvl sym)
                 (list 'list (qq-xform form lvl))))
           (list 'list (list 'quote form)))))


(set 'qq-simple-quote-result?
     (lambda (it)
       (if (not (pair? it)) #f
           (if (not (eq (car it) 'quote)) #f
               (nil? (cdr (cdr it)))))))

;; (list 'a 'b 'c) => '(a b c)
(set 'qq-quote-opt
     (lambda (lst)
       (if (list-every qq-simple-quote-result? lst)
           (list 'quote (mapcar (lambda (it) (car (cdr it))) lst))
           (cons 'list lst))))

(set 'qq-simple-list-result?
     (lambda (it)
       (if (not (pair? it)) #f
           (if (not (eq (car it) 'list)) #f
               (nil? (cdr (cdr it)))))))

;; we could do more here...
;; (append '('a) '('b) c) => (append '('a 'b) c)
;; and so on
;; (append '(a) '(b) '(c)) => (list a b c)
(set 'qq-append-opt
     (lambda (lst)
       (if (list-every qq-simple-list-result? lst)
           (qq-quote-opt (mapcar (lambda (it) (car (cdr it))) lst))
           (cons 'append lst))))

(set 'qq-xform-for-unq
     (lambda (lst lvl)
       (qq-append-opt
        (mapcar (lambda (f) (qq-unq-form f lvl)) lst))))

(set 'qq-xform (lambda (x lvl)
                 (if (pair? x)
                     (if (eq (car x) 'quasiquote)
                         (list 'list ''quasiquote (qq-xform (car (cdr x)) (+i 1 lvl)))
                         (qq-xform-for-unq x lvl))
                     (list 'quote x))))

(set 'qq-process
     (lambda (expr)
       (if (pair? expr)
           (let ((sym (car expr)))
             (if (eq sym 'quote) expr
                 (if (eq sym 'quasiquote) (qq-xform (car (cdr expr)) 0)
                     (mapcar qq-process expr))))
           expr)))

(set 'compiler qq-process)

;;; ----------------------------------------
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

;;; ----------------------------------------
;;; basic macros

(set-macro-function
 'and
 (lambda (expr)
   (let ((test (car (cdr expr)))
         (rest (cdr (cdr expr))))
     (if (nil? test) #t
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
     (if (nil? test) #f
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

(defmacro case (subj & tests)
  (let* ((name (gensym))
         (xform '()))
    `(let ((,name ,subj))
       (cond ,@(mapcar (lambda (it) `((eq ,name (quote ,(car it))) ,@(cdr it))) tests)))))

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
