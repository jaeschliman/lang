
;; TODO: write more tests for qq and macroexpand

(set 'expect-t (lambda (r) (print (if r "test passed" "test failed"))))
(set 'expect-f (lambda (r) (expect-t (not r))))

(expect-t (has-unquote-splicing '(a b (unquote-splicing c) d e)))
(expect-t (has-unquote-splicing '(a (1 2) (unquote-splicing c) d e)))
(expect-f (has-unquote-splicing '(a b c)))
(expect-f (has-unquote-splicing '(a (1 2) (2 3) b)))

(print (mapcar (lambda (x) (+i 1 x)) '(1 2 3)))

(print (qq-xform-for-unq-spl '(a b c)))
(print (qq-xform-for-unq-spl '(a (unquote b) c)))
(print (qq-xform-for-unq-spl '(a (unquote-splicing b) c)))

(print (qq-xform-for-unq '(a b c)))
(print (qq-xform-for-unq '(a (unquote b) c)))

(print (qq-xform '(a b c (d e f))))
(print (qq-xform '(a b c (d (unquote e) f))))
(print (qq-xform '(a b c (d (unquote e) f) (unquote-splicing g))))
(print (qq-xform '(a b c (d (unquote e) f) ((unquote-splicing g) h))))

(set 'append2 #f)
(set 'append2 (lambda (a b)
                (if (nil? a) b
                    (cons (car a) (append2 (cdr a) b)))))

(print (append2 '(a b c) '(1 2 3)))

(print (append3 '(a b c) '(1 2 3) '((d e f) (4 5 6) (done))))
(print (append3 '(a b c) '(1 2 3) (list)))
(print (append3 '(a b c) (list) (list)))

(set 'myfun (lambda args (cons 'myfun! args)))
(print (list 'saw: (myfun 1 2 3)))

(print (append '(a b)))
(print (append '(a b) '(c d)))
(print (append '(a b) '(c d) '(e f)))
(print (append '(a b) '(c d) '(e f) '(g h)))
(print (append '(a b) '(c d) '(e f) '(g h) '(i j)))

(print ((compile-to-closure '(+i 2 2))))

(print (qq-process '(quasiquote x)))
(print (qq-process '(lambda (x) x)))
(print (qq-process '(lambda (x) (quasiquote (1 2 (unquote-splicing '(3 4)) 5 6)))))
(print (eval (qq-process '(lambda (x)
                           (quasiquote
                            (1 2
                             (unquote-splicing '(3 4))
                             5 6
                             (unquote x)))))))

(let ((fn (eval (qq-process '(lambda (x)
                              (quasiquote
                               (1 2
                                (unquote-splicing '(3 4))
                                5 6
                                (unquote x))))))))
  (print (fn 7)))

(print '(lambda (x) `(1 2 ,@'(3 4) 5 6 ,x)))
(print (qq-process '(lambda (x) `(1 2 ,@'(3 4) 5 6 ,x))))

(let ((fn (eval (qq-process '(lambda (x) `(1 2 ,@'(3 4) 5 6 ,x))))))
  (print (fn 7)))

;; (set 'compiler qq-process)

(set 'myfun (lambda (a b c)
              `(On a rainy ,a I saw ,b (and we ,@c))))

(print (myfun 'day 'Julia '(had a coffee)))

(print `(are gensyms equal? ,(eq (gensym) (gensym))))
(print (macroexpand 'hello))

(print (ht-at macro-functions 'and))

(print '(and expansions:))
(print (macroexpand '(and)))
(print (macroexpand '(and a)))
(print (macroexpand '(and a b)))
(print (macroexpand '(and a b c)))
(print (eval (macroexpand '(and 1 2 3))))

(print '(or expansions:))
(print (macroexpand '(or)))
(print (macroexpand '(or a)))
(print (macroexpand '(or a b)))
(print (macroexpand '(or a b c)))
(print (eval (macroexpand '(or 1 2 3))))

(print "hello macro world")
(print (or 'a 'b 'c))
(print `(a b c))
(print (and 'a 'b 'c))

;; (print '`(let ((,name ,form)) ,@body))

(print (macroexpand '(lambda-bind (a b c) '(a b c) (list c b a))))
(print (lambda-bind (a b c) '(a b c) (list c b a)))
(print (lambda-bind x '(1 2 3) (list x x)))
;; TODO: dot-reader
;; (print (lambda-bind (x . y) '(1 2 3) (list x y)))
(print (lambda-bind (x & y) '(1 2 3) (list x y)))

(print (macroexpand '(defmacro my-macro (a b c)
                      `(,c (list ,b ,a)))))

(print (macroexpand '(define x 10)))
(define x 10)
(print x)
(define (print-2 a b) (print b) (print a))
(print-2 'x 10)

(let* ((x 'hello)
       (y (list x 'world)))
  (print y))

(define (show x)
    (cond ((symbol? x) "a symbol")
          ((nil? x) "nil!")
          (#t "something else")))

(print (show 'x))
(print (show (list)))
(print (show 5.0))

;; maybe should support & in define as well?
;; (define (show-more a & more) (print more))
;; (show-more 0 1 2 3)
;; BUG! can't end file with a comment
'bye
