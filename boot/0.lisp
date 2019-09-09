(set-symbol-value 'set set-symbol-value)

(set 'maybe-set (%nlambda () (s v) (if (not (symbol-bound? s)) (set s v))))

;; helper functions

(maybe-set 'list-every #f)
(set-symbol-value 'list-every
     (%nlambda () (pred lst)
       (if (nil? lst) #t
           (if (pred (car lst)) (list-every pred (cdr lst))
               #f))))

(maybe-set 'append3 #f)
(set-symbol-value 'append3 (%nlambda () (a b cs)
                (if (nil? a)
                    (if (nil? cs) b
                        (append3 b (car cs) (cdr cs)))
                    (cons (car a) (append3 (cdr a) b cs)))))

(maybe-set '%reverse-append #f)
(set-symbol-value '%reverse-append (%nlambda %reverse-append (rem acc)
                             (if (nil? rem) acc
                                 (%reverse-append
                                  (cdr rem) (cons (car rem) acc)))))

(set-symbol-value 'reverse-list (%nlambda reverse-list (lst) (%reverse-append lst '())))

(set-symbol-value '%append2 (%nlambda %append2 (a b) (if (nil? b) a (%reverse-append (reverse-list a) b))))

(maybe-set '%append3 #f)
(set-symbol-value '%append3 (%nlambda %append3 (a b cs)
                         (if (nil? cs) (%append2 a b)
                             (%append3 (%append2 a b) (car cs) (cdr cs)))))

(set-symbol-value 'append (%nlambda append args (%append3 (car args) (car (cdr args)) (cdr (cdr args)))))

(maybe-set '%mapcar-helper #f)
(set-symbol-value '%mapcar-helper (%nlambda %mapcar-helper (f lst acc)
                               (if (nil? lst) (reverse-list acc)
                                   (%mapcar-helper f (cdr lst) (cons (f (car lst)) acc)))))

(set-symbol-value 'mapcar (%nlambda mapcar (f lst) (%mapcar-helper f lst '())))


;; ----------------------------------------
;;; nested quasiquote support
;;; nested support not yet well-tested

(maybe-set 'qq-process #f) ;; entry point for quasiquoting (toplevel function)
(maybe-set 'qq-xform #f)   ;; fn that begins qq-transforming a given form at lvl

(set 'qq-is-unq-sym (%nlambda () (sym)
                      (if (eq sym 'unquote) #t
                          (eq sym 'unquote-splicing))))

(set 'qq-has-unq (%nlambda () (f) (qq-is-unq-sym (car f))))
(set 'qq-has-unq-level #f)
(set 'qq-has-unq-level (%nlambda () (form lvl)
                         (if (not (pair? form)) #f
                             (if (eq lvl 0) (qq-has-unq form)
                                 (if (not (qq-has-unq form)) #f
                                     (qq-has-unq-level (car (cdr form)) (-i lvl 1)))))))


(maybe-set 'qq-unq-form #f)
(set 'qq-unq-form-unq
     (%nlambda () (form lvl sym)
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
     (%nlambda () (form lvl)
       (if (pair? form)
           (%let ((sym (car form)))
             (if (qq-is-unq-sym sym)
                 (qq-unq-form-unq form lvl sym)
                 (list 'list (qq-xform form lvl))))
           (list 'list (list 'quote form)))))


(set 'qq-simple-quote-result?
     (%nlambda () (it)
       (if (not (pair? it)) #f
           (if (not (eq (car it) 'quote)) #f
               (nil? (cdr (cdr it)))))))

;; (list 'a 'b 'c) => '(a b c)
(set 'qq-quote-opt
     (%nlambda () (lst)
       (if (list-every qq-simple-quote-result? lst)
           (list 'quote (mapcar (%nlambda () (it) (car (cdr it))) lst))
           (cons 'list lst))))

(set 'qq-simple-list-result?
     (%nlambda () (it)
       (if (not (pair? it)) #f
           (if (not (eq (car it) 'list)) #f
               (nil? (cdr (cdr it)))))))

;; we could do more here...
;; (append '('a) '('b) c) => (append '('a 'b) c)
;; and so on
;; (append '(a) '(b) '(c)) => (list a b c)
(set 'qq-append-opt
     (%nlambda () (lst)
       (if (list-every qq-simple-list-result? lst)
           (qq-quote-opt (mapcar (%nlambda () (it) (car (cdr it))) lst))
           (cons 'append lst))))

(set 'qq-xform-for-unq
     (%nlambda () (lst lvl)
       (qq-append-opt
        (mapcar (%nlambda () (f) (qq-unq-form f lvl)) lst))))

(set 'qq-xform (%nlambda () (x lvl)
                 (if (pair? x)
                     (if (eq (car x) 'quasiquote)
                         (list 'list ''quasiquote (qq-xform (car (cdr x)) (+i 1 lvl)))
                         (qq-xform-for-unq x lvl))
                     (list 'quote x))))

(set 'qq-process
     (%nlambda () (expr)
       (if (pair? expr)
           (%let ((sym (car expr)))
             (if (eq sym 'quote) expr
                 (if (eq sym 'quasiquote) (qq-xform (car (cdr expr)) 0)
                     (mapcar qq-process expr))))
           expr)))

(set 'compiler qq-process)

;;; ----------------------------------------
;;; macro support

(if (not (symbol-bound? 'macro-functions)) (set 'macro-functions (make-ht)))

(set 'set-macro-function (%nlambda () (sym fn) (ht-at-put macro-functions sym fn)))

(maybe-set 'macroexpand #f)

(set 'mx-process-let-binding
     (%nlambda () (bind)
       (list (car bind) (macroexpand (car (cdr bind))))))

(set 'mx-process-let
     (%nlambda () (expr)
       `(%let ,(mapcar mx-process-let-binding (car (cdr expr)))
          ,@(mapcar macroexpand (cdr (cdr expr))))))

(set 'mx-process-letrec
     (%nlambda () (expr)
       `(%letrec ,(mapcar mx-process-let-binding (car (cdr expr)))
          ,@(mapcar macroexpand (cdr (cdr expr))))))

(set 'mx-process-lambda
     (%nlambda () (expr)
       `(%nlambda () ,(car (cdr expr))
          ,@(mapcar macroexpand (cdr (cdr expr))) )))

(if (not macroexpand)
    (set 'macroexpand
         (%nlambda () (expr)
                   ;; (print `(expanding: ,expr))
                   (if (pair? expr)
                       (%let ((sym (car expr)))
                         (if (eq sym 'quote) expr
                             (if (eq sym '%let) (mx-process-let expr)
                                 (if (eq sym '%letrec) (mx-process-letrec expr)
                                     (if (eq sym 'lambda) (mx-process-lambda expr)
                                         (%let ((expander (ht-at macro-functions sym)))
                                           (if (not (nil? expander))
                                               (%let ((expansion (expander expr)))
                                                 (macroexpand expansion))
                                               (mapcar macroexpand expr))))))))
                       expr))))

(set 'compiler (%nlambda () (expr) (macroexpand (qq-process expr))))
(set 'quasiquote-expand qq-process)

;;; ----------------------------------------
;;; basic macros

(if (nil? (ht-at macro-functions 'lambda))
    (set-macro-function
     'lambda
     (%nlambda () (expr)
               `(%nlambda () ,(car (cdr (cdr expr)))
                          ,@(cdr (cdr (cdr expr)))))))

;; simple rewrite of let for now
(if (nil? (ht-at macro-functions 'let))
    (set-macro-function 'let (lambda (expr) (cons '%let (cdr expr)))))

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
        (%nlambda ,name (,sym)
          (lambda-bind ,params (cdr ,sym) ,@body)))))))

(defmacro define (binding & body)
  (if (pair? binding)
      (let ((name (car binding))
            (params (cdr binding)))
        `(set-symbol-value ',name (%nlambda ,name ,params ,@body)))
      `(set-symbol-value ',binding ,(car body))))

(defmacro let* (bindings & body)
  (if (nil? (cdr bindings))
      `(let (,(car bindings))
         ,@body)
      `(let (,(car bindings))
         (let* ,(cdr bindings)
           ,@body))))

(defmacro cond (& clauses)
  (let* ((clause (car clauses))
         (test (car clause))
         (body `(let () ,@(cdr clause)))
         (rest (cdr clauses)))
    (if (nil? rest)
        (list 'if test body)
        `(if ,test ,body (cond ,@rest)))))

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

(defmacro case (subj & tests)
  (let* ((name (gensym))
         (xform '()))
    `(let ((,name ,subj))
       (cond ,@(mapcar (lambda (it)
                         (let ((thing (car it)))
                           (if (eq thing #t) ;; TODO: should only be allowed for final clause
                               `(#t ,@(cdr it))
                               `((eq ,name (quote ,thing)) ,@(cdr it)))))
                       tests)))))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))

(define first car)
(define second cadr)
(define third caddr)
(define (fourth x) (car (cdddr x)))

(define (identity x) x)

;;; more utils

(defmacro when (test & body)
  `(if ,test (let () ,@body)))

(defmacro unless (test & body)
  `(when (not ,test) ,@body))

(defmacro dotimes (binding & body)
  (let ((var (car binding))
        (max (gensym))
        (loop (gensym)))
    `(let ,loop ((,var 0) (,max ,(cadr binding)))
          (when (<i ,var ,max)
            ,@body
            (,loop (+i 1 ,var) ,max)))))

(defmacro dolist (binding & body)
  (let ((loop (gensym))
        (remaining (gensym))
        (var (car binding))
        (form (cadr binding)))
    `(let ,loop ((,remaining ,form))
          (unless (nil? ,remaining)
            (let ((,var (car ,remaining)))
              ,@body)
            (,loop (cdr ,remaining))))))

(defmacro if-nil? (test & rest) `(if (nil? ,test) ,@rest))

(defmacro forward (symbol) `(maybe-set ',symbol #f))

;;; test helpers

(forward deep-eq?)
(define (deep-eq? a b)
  (if (eq a b) #t
      (if (and (pair? a) (pair? b))
          (and (deep-eq? (car a) (car b))
               (deep-eq? (cdr a) (cdr b)))
          #f)))

;;; shift/reset exception support

(forward run-reset)
(define (run-reset tag fn)
    (set-stack-mark tag)
  (let ((it (fn)))
    (if (continuation? it)
        (let* ((resume (lambda (val)
                         (let* ((thunk (lambda () (resume-stack-snapshot it val)))) 
                           (run-reset tag thunk))))
               (handler (continuation-value it)))
          (handler resume))
        it)))

(define (run-shift tag fn)
    (snapshot-to-stack-mark tag fn))

(defmacro reset-tag (tag & body)
  `(run-reset ,tag (lambda () ,@body)))

(defmacro shift-tag (tag var & body)
  `(run-shift ,tag (lambda (,var) ,@body)))

(define (escape-to-tag tag value)
    (shift-tag tag _ value))

(define (try-catch tryfn catchfn)
    (reset-tag
     'success
     (let ((exception (reset-tag
                       'exception
                       (let ((result (tryfn)))
                         (escape-to-tag 'success result))))) 
       (catchfn exception))))

(define (throw ex)
    (escape-to-tag 'exception ex))

;;  special vaiable support -------------------------------------------------------

(defmacro binding (vars & body)
  (if (nil? vars)
      `(let () ,@body)
      (let ((it (car vars)))
        `(with-special-binding ,(car it) ,(cadr it) (binding ,(cdr vars) ,@body)))))

(defmacro defparameter (var value)
  `(let ()
     (set-symbol-value ',var ,value)
     (mark-symbol-as-special ',var)
     ',var))

(defparameter *command-line-args*)
(maybe-set '*recompiling* #f)
(mark-symbol-as-special '*recompiling*)

(defmacro at-boot (& forms)
  (if *recompiling* '() `(let () ,@forms)))

;;; eval

(at-boot (define (eval x) ((compile-to-closure (compiler x)))))

;;  primitive named-let support ---------------------------------------------------

(at-boot
 (defmacro let (bindings & body)
   (if (symbol? bindings)
       (let* ((symbol bindings)
              (bindings (car body))
              (body (cdr body))
              (args (mapcar car bindings))
              (inits (mapcar cadr bindings)))
         `(%let ((,symbol #f))
                (set! ,symbol (lambda ,args ,@body))
                (,symbol ,@inits)))
       `(%let ,bindings ,@body))))

;;  basic threading support -------------------------------------------------------

(defmacro fork-with-priority (priority & forms)
  `(fork-thunk ,priority (lambda () ,@forms)))


(defmacro fork (& forms)
  `(fork-thunk 1 (lambda () ,@forms)))

;;  utils for meta reader ---------------------------------------------------------

(define (safe-car it) (if (pair? it) (car it) it))
(define (safe-cdr it) (if (pair? it) (cdr it) it))

(define (ensure-list it) (if (or (pair? it) (nil? it)) it (list it)))


(defmacro string-do-chars (binding & body)
  (let ((loop (gensym))
        (str (gensym))
        (idx (gensym))
        (max (gensym))
        (init-str (gensym))
        (ch  (car binding)))
    `(let ((,init-str ,(cadr binding)))
       (let ,loop ((,str ,init-str) (,idx 0) (,max (string-byte-length ,init-str)))
            (when (<i ,idx ,max)
              (let ((,ch (char-at ,str ,idx)))
                ,@body
                (,loop ,str (+i (char-width ,ch) ,idx) ,max)))))))

(define (charlist-to-string lst-of-chars)
  (let* ((byte-len (reduce-list (lambda (acc ch) (+i acc (char-width ch))) 0 lst-of-chars))
         (str (make-string byte-len #\Space)))
    (reduce-list (lambda (idx chr)
                   (char-at-put str idx chr)
                   (+i (char-width chr) idx))
                 0 lst-of-chars)
    str))

(define (string-to-charlist str)
  (let ((result '()))
    (string-do-chars (ch str)
      (set! result (cons ch result)))
    (reverse-list result)))

(define (implode lst-of-chars) (intern (charlist-to-string lst-of-chars) *package*))

(define (char-between b a c)
  (or (eq a b) (eq b c)
      (and (char-< a b)
           (char-< b c))))

(define (whitespace-char? x)
  (or (eq x #\Space)
      (char-< x #\Space)) )

(define (alpha-char? x)
  (or (char-between x #\a #\z)
      (char-between x #\A #\Z)))

(define (character-name? str) (not (nil? (char-by-name str))))

(define (symbol-char x) ;; TODO: convert to lookup table
  (or (char-between x #\a #\z)
      (char-between x #\* #\-)
      (char-between x #\< #\Z)
      (char-between x #\/ #\9)
      (char-between x #\# #\&)
      (eq x #\!) (eq x #\^) (eq x #\_)
      (eq x #\|) (eq x #\~)
      (eq x #\:)))

(define (digit-char? ch)
    (char-between ch #\0 #\9))

(define (char-to-digit ch)
  (-i (char-code ch) (char-code #\0)))

(define (%slot-index object slot)
  (let ((slotnames (instance-get-ivar (class-of object) 5)))
    (let loop ((idx 0))
         (if (eq idx (array-length slotnames)) -1
             (if (eq slot (aget slotnames idx)) idx
                 (loop (+i idx 1)))))))

(define (iget object slotname)
  (instance-get-ivar object (%slot-index object slotname)))

(define (iset object slotname value)
  (instance-set-ivar object (%slot-index object slotname) value))

;;  the end -----------------------------------------------------------------------

'done
