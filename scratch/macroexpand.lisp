(define (ensure-list x) (if (or (nil? x) (pair? x)) x (list x)))

(define (assoc key alist)
  (cond ((nil? alist) '())
        ((eq (caar alist) key) (car alist))
        (#t (assoc key (cdr alist)))))

(defparameter *binds* '())

(define (local? sym) (not (nil? (assoc sym *binds*))))

(define (%to-locals syms)
  (mapcar (lambda (s) (cons (car (ensure-list s)) 'local)) (ensure-list syms)))

(defmacro with-bindings (binds & body)
  `(binding ((*binds* (append (%to-locals ,(car binds)) *binds*)))
     ,@body))

(define (expand-if form)
  (cons 'if (mapcar %macroexpand (cdr form))))

(define (expand-set! form)
  `(set! ,(second form) ,(%macroexpand (third form))))

(define (%expand-binding b)
  (list (car b) (%macroexpand (cadr b))))

(define (expand-letrec form)
  (let ((binds (cadr form))
        (body (cddr form)))
    (with-bindings (binds)
      `(#/lang/%letrec ,(mapcar %expand-binding binds) ,@(mapcar %macroexpand body)))))

(define (expand-let form)
  (let* ((binds (cadr form))
         (body (cddr form))
         (expanded-binds (mapcar %expand-binding binds)))
    (with-bindings (binds)
      `(#/lang/%let ,expanded-binds ,@(mapcar %macroexpand body)))))

(define (expand-lambda form)
  (let ((name (cadr form))
        (args (caddr form))
        (body (cdddr form)))
    `(#/lang/%nlambda ,name ,args ,@(with-bindings (args) (mapcar %macroexpand body)))))

(define (expand-special-binding form)
  `(with-special-binding ,(cadr form) ,@(mapcar %macroexpand (cddr form))))

(define (macro-function sym)
  (cond ((local? sym) '())
        (#t (ht-at #/lang/macro-functions sym))))

(define (expand-call form)
  (let ((it (car form)))
    (cond ((symbol? it)
           (let ((fn (macro-function it)))
             (if (nil? fn)
                 (cons it (mapcar %macroexpand (cdr form)))
                 (%macroexpand (fn form)))))
          (#t (mapcar %macroexpand form)))))

(define (%macroexpand form)
  (cond
    ((symbol? form) form)
    ((pair? form)
     (case (car form)
       (quote form)
       (if (expand-if form))
       (set! (expand-set! form))
       (#/lang/%letrec (expand-letrec form))
       (#/lang/%let (expand-let form))
       (#/lang/%nlambda (expand-lambda form))
       (with-special-binding (expand-special-binding form))
       (#t (expand-call form))))
    (#t form)))


(define macroexpand %macroexpand)
;; (define (macroexpand form)
;;   (let ((r (%macroexpand form)))
;;     (print `(+++> ,r))
;;     r))

;; was previously handled in mx internals
(defmacro lambda (args & body)
  `(#/lang/%nlambda () ,args ,@body))

;; (set '*package* (symbol-package 'define))
;; (binding ((*recompiling* #t))
;;   (load-as "compiler" "./scratch/compiler.lisp"))

;; (print `(two plus two is ,(+ 2 2)))

;; ;; please don't do this
;; (let ((lambda (lambda (x) (+ x x))))
;;  (print `(four plus four is ,(lambda 4))))
