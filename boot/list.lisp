(define (list->array lst)
  (let* ((len (list-length lst))
         (arry (make-array len))
         (idx  0))
    (dolist (it lst)
      (aset arry idx it)
      (set! idx (+i idx 1)))
    arry))

(define (array->list array)
    (let* ((len (array-length array))
           (loop #f)
           (result '()))
      (set! loop (lambda (i)
                   (unless (<i i 0)
                     (set! result (cons (aget array i) result))
                     (loop (-i i 1)))))
      (loop (-i len 1))
      result))

(define (safe-car it) (if (pair? it) (car it) it))
(define (safe-cdr it) (if (pair? it) (cdr it) it))

(define (ensure-list it) (if (or (pair? it) (nil? it)) it (list it)))

'done
