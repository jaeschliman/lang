(set-symbol-value 'set set-symbol-value)

(set 'has-unquote-splicing #f)
(set 'has-unquote-splicing
     (lambda (lst)
       (if (nil? lst) #f
           (if (pair? (car lst))
               (if (eq (car (car lst)) 'unquote-splicing) #t
                   (has-unquote-splicing (cdr lst)))
               (has-unquote-splicing (cdr lst))))))

(set 'expect-t (lambda (r) (print (if r "test passed" "test failed"))))
(set 'expect-f (lambda (r) (expect-t (not r))))

(expect-t (has-unquote-splicing '(a b (unquote-splicing c) d e)))
(expect-t (has-unquote-splicing '(a (1 2) (unquote-splicing c) d e)))
(expect-f (has-unquote-splicing '(a b c)))
(expect-f (has-unquote-splicing '(a (1 2) (2 3) b)))

(set 'mapcar #f)
(set 'mapcar (lambda (f lst)
               (if (nil? lst) lst
                   (cons (f (car lst)) (mapcar f (cdr lst))))))

(print (mapcar (lambda (x) (+i 1 x)) '(1 2 3)))

(set 'qq-xform (lambda (x) x))

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

(print (qq-xform-for-unq-spl '(a b c)))
(print (qq-xform-for-unq-spl '(a (unquote b) c)))
(print (qq-xform-for-unq-spl '(a (unquote-splicing b) c)))

(print (qq-xform-for-unq '(a b c)))
(print (qq-xform-for-unq '(a (unquote b) c)))

(set 'qq-xform (lambda (lst)
                 (if (has-unquote-splicing lst)
                     (qq-xform-for-unq-spl lst)
                     (qq-xform-for-unq lst))))

(print (qq-xform '(a b c (d e f))))
(print (qq-xform '(a b c (d (unquote e) f))))
(print (qq-xform '(a b c (d (unquote e) f) (unquote-splicing g))))
(print (qq-xform '(a b c (d (unquote e) f) ((unquote-splicing g) h))))

;; now need to implement varargs lambda so we can implement append...
