(define (expect a b)
    (if (deep-eq? a b)
        (print `(PASS ,a = ,b))
        (print `(FAIL expected ,a = ,b))))

(expect '(2 3 4) (mapcar (lambda (x) (+i 1 x)) '(1 2 3)))

(set 'expect-t (lambda (r) (expect #t r)))
(set 'expect-f (lambda (r) (expect-t (not r))))
(define (expect-runs x) (expect #t #t))

(expect-t (#/lang/qq-simple-list-result? '(list 'a)))
(expect-f (#/lang/qq-simple-list-result? '(list 'a 'b)))

(expect-t (list-every #/lang/qq-simple-list-result?
                      '((list 'a) (list 'b) (list 'c))))

(expect-f (list-every #/lang/qq-simple-list-result?
                      '((list 'a) b (list 'c))))

(expect ''(a b c) (#/lang/qq-append-opt '((list 'a) (list 'b) (list 'c))))

(expect ''(a b c)
        (#/lang/qq-xform-for-unq '(a b c) 0))

(expect '(list 'a b 'c)
        (#/lang/qq-xform-for-unq '(a (unquote b) c) 0))

(expect ''(a b c (d e f))
        (#/lang/qq-xform '(a b c (d e f)) 0))

(expect '(list 'a 'b 'c (list 'd e 'f))
 (#/lang/qq-xform '(a b c (d (unquote e) f)) 0))

(expect '(append (list 'a) (list 'b) (list 'c) (list (list 'd e 'f)) g)
        (#/lang/qq-xform '(a b c (d (unquote e) f) (unquote-splicing g)) 0))

(expect
 '(list 'a 'b 'c (list 'd e 'f) (append g (list 'h)))
 (#/lang/qq-xform '(a b c (d (unquote e) f) ((unquote-splicing g) h)) 0))

;; basic varargs check
(set 'myfun (lambda args (cons 'myfun! args)))
(expect '(myfun! 1 2 3) (myfun 1 2 3))

;; append internals
(expect
 '(a b c 1 2 3 d e f 4 5 6 done)
 (#/lang/append3 '(a b c) '(1 2 3) '((d e f) (4 5 6) (done))))

(expect
 '(a b c 1 2 3)
 (#/lang/append3 '(a b c) '(1 2 3) (list)))

(expect
 '(a b c)
 (#/lang/append3 '(a b c) (list) (list)))

;; basic append checks
(expect '(a b)                 (append '(a b)))
(expect '(a b c d)             (append '(a b) '(c d)))
(expect '(a b c d e f)         (append '(a b) '(c d) '(e f)))
(expect '(a b c d e f g h)     (append '(a b) '(c d) '(e f) '(g h)))
(expect '(a b c d e f g h i j) (append '(a b) '(c d) '(e f) '(g h) '(i j)))

(expect ''x (#/lang/qq-process '(quasiquote x)))
(expect '(lambda (x) x) (#/lang/qq-process '(lambda (x) x)))

;; TODO: could optimize (list 'x) => '(x)
;; TODO: could optimize '1 => 1
(expect
 '(lambda (x) (append (list '1) (list '2) '(3 4) (list '5) (list '6)))
 (#/lang/qq-process '(lambda (x) (quasiquote (1 2 (unquote-splicing '(3 4)) 5 6)))))

(let ((fn (#/lang/eval (#/lang/qq-process '(lambda (x)
                              (quasiquote
                               (1 2
                                (unquote-splicing '(3 4))
                                5 6
                                (unquote x))))))))
  (expect '(1 2 3 4 5 6 7) (fn 7)))

(expect
 '(lambda (x) (append (list '1) (list '2) '(3 4) (list '5) (list '6) (list x)))
 (#/lang/qq-process '(lambda (x) `(1 2 ,@'(3 4) 5 6 ,x))))

(let ((fn (#/lang/eval (#/lang/qq-process '(lambda (x) `(1 2 ,@'(3 4) 5 6 ,x))))))
  (expect '(1 2 3 4 5 6 7) (fn 7)))

(set 'myfun (lambda (a b c) `(On a rainy ,a I saw ,b (and we ,@c))))

(expect
 '(On a rainy day I saw Julia (and we had a coffee))
 (myfun 'day 'Julia '(had a coffee)))

(expect
 '(are gensyms equal? #f)
 `(are gensyms equal? ,(eq (gensym) (gensym))))

(expect 'hello (macroexpand 'hello))

(expect-runs (ht-at #/lang/macro-functions 'and))

;; unclear how to test these.. other than asserting there is no error...
(expect-runs (macroexpand '(and)))
(expect-runs (macroexpand '(and a)))
(expect-runs (macroexpand '(and a b)))
(expect-runs (macroexpand '(and a b c)))
(expect 3 (eval (macroexpand '(and 1 2 3))))

(expect-runs (macroexpand '(or)))
(expect-runs (macroexpand '(or a)))
(expect-runs (macroexpand '(or a b)))
(expect-runs (macroexpand '(or a b c)))
(expect 1 (eval (macroexpand '(or 1 2 3))))

(expect 'a (or 'a 'b 'c))
(expect '(a b c) `(a b c))
(expect 'c (and 'a 'b 'c))

;; (print '`(let ((,name ,form)) ,@body))

(expect-runs (macroexpand '(lambda-bind (a b c) '(a b c) (list c b a))))
(expect '(c b a) (lambda-bind (a b c) '(a b c) (list c b a)))
(expect '((1 2 3) (1 2 3)) (lambda-bind x '(1 2 3) (list x x)))

;; TODO: dot-reader
;; (print (lambda-bind (x . y) '(1 2 3) (list x y)))
(expect '(1 (2 3)) (lambda-bind (x & y) '(1 2 3) (list x y)))

(expect-runs (macroexpand '(defmacro my-macro (a b c)
                            `(,c (list ,b ,a)))))

(expect-runs (macroexpand '(define x 10)))

(let* ((x 'hello)
       (y (list x 'world)))
  (expect '(hello world) y))

(define (show x)
    (cond ((#/lang/symbol? x) 'symbol)
          ((#/lang/nil? x) 'nil!)
          (#t 'other)))

(expect 'symbol (show 'x))
(expect 'nil! (show (list)))
(expect 'nil! (show '()))
(expect 'other (show (list (list))))
(expect 'other (show 5.0))

(define (set-test0)
    (let ((x 5))
      (list x x)
      (set! x 6)
      (list x x)
      (set! x (+i 1 x))
      (list x x)
      x))

(expect 7 (set-test0))

(define (iota n fn)
    (let ((loop #f))
      (set! loop
            (lambda (i)
              (when (<i i n)
                (fn i)
                (loop (+i 1 i)))))
      (loop 0)))

(let* ((r '())
       (collect (lambda (n) (set! r (cons n r)))))
  (iota 10 collect)
  (expect '(9 8 7 6 5 4 3 2 1 0) r))

;; TCO!
(expect-runs (iota 100000 (lambda (x) x)))

(let ((special #f))
  (define (store x) (set! special x))
  (define (load) special))

(store 'secret)
(expect 'secret (load))

(expect '(a b c) (reverse-list '(c b a)))

;; TODO: no string-equal? yet
(expect-runs (#/lang/make-string 3 #\x))

(define my-str (#/lang/make-string 3 #\Space))
(expect-runs (#/lang/char-at-put my-str 1 #\o))

(expect-runs (#/lang/implode '(#\h #\e #\l #\l #\o)))

(define (test-let-binding)
    (let ((x 'x-value))
      (let ((f (lambda () x)))
        x)))

(expect 'x-value (test-let-binding))

(define (test-case-expr x)
    (case x
      (hello 'hi)
      (hi 'hello)
      (* '+)
      (foo 'bar)))

(expect 'hello (test-case-expr 'hi))
(expect '+     (test-case-expr '*))
(expect '()    (test-case-expr '+))
(expect 'bar   (test-case-expr 'foo))
(expect '()    (test-case-expr 'unknown))


(expect '(a (quote (b c))) `(a '(b c)))
(expect '(a (quasiquote (b c))) `(a `(b c)))
(expect '(a (quasiquote (b (unquote c)))) `(a `(b ,c)))

(let ((c 3))
 (expect '(a (quasiquote (b 3))) `(a `(b ,,c))))

(let ((d 3))
  (expect
   '(a (quasiquote (b (unquote (c 3)))))
   `(a `(b ,(c ,d)))))

(let ((c '(3 4)))
  (expect
   '(a (quasiquote (b 3 4)))
   `(a `(b ,,@c))))

(let ((d '(3 4)))
  (expect
   '(a (quasiquote (b (unquote (c 3 4)))))
   `(a `(b ,(c ,@d)))))

(expect #t (deep-eq? '(a b c) '(a b c)))
(expect #f (deep-eq? '(a c b) '(a b c)))

;; maybe should support & in define as well?
;; (define (show-more a & more) (print more))
;; (show-more 0 1 2 3)
;; BUG! can't end file with a comment
'bye
