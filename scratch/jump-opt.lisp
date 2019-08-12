(set '*package* (symbol-package 'define))

(defmacro trc (x)
  `(let ((xxx ,x))
     (print xxx)
     xxx))

(binding ((*enable-jump-opts*))
         (eval `(define (my-reverse-list lst)
                    (let ((helper #f))
                      (set! helper (lambda (rem acc)
                                     (if (nil? rem) acc
                                         (helper (cdr rem) (trc (cons (car rem) acc))))))
                      (helper lst '())))))

(print (my-reverse-list '(1 2 3 4 5 6 7 8 9 10)))

(print 'done)
