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

(define list-member? #f)
(define (list-member? item lst)
    (if-nil? lst #f
             (or (eq (car lst) item)
                 (list-member? item (cdr lst)))))

(define plist-get #f)
(define (plist-get item lst)
    (if-nil? lst lst
             (if (eq item (car lst)) (cadr lst)
                 (plist-get item (cddr lst)))))

'done
