(defmacro trace (form)
  `(let ((_res ,form))
     (print (list ',form '= _res))))

(trace "trace works")

(define rules (make-ht))
(define fail '(fail fail))
(define (failure? state) (eq state fail))

(define (state-stream state) (car state))
(define (state-result state) (nth state 1))

(define (state+result state res) (lambda-bind (stream _ & rest) state
                                              `(,stream ,res,@rest)))
(define (state+stream state stream) (cons stream (cdr state)))

(define (make-stream str) (cons 0 str))
(define (stream-read s) (char-at (cdr s) (car s)))
(define (stream-next s) (cons (+i 1 (car s)) (cdr s)))
(define (stream-end? s) (>i (+i 1 (car s)) (string-length (cdr s))))

(define (get-rule x) (ht-at rules x))
(define (set-rule x fn) (ht-at-put rules x fn))

;; stub for now
(define (char-between b a c) #t)

;;
;;
;;  alpha = any : x ? (or (char-between x #\a #\z) (char-between x #\A #\Z)) -> x
;;  symbol = alpha+ : chars -> (intern (to-string chars))
;;  -- would be nice to save input range, like:
;;  symbol = alpha+ : <start, end> -> (intern (stream-subseq start end))
;;

'(rule alpha
  (bind x any)
  (where (or
          (char-between x #\a #\z)
          (char-between x #\A #\Z)))
  (return x))

'(rule symbol
  (bind chars (+ alpha))
  (return (intern (to-string chars))))

'(lambda (st)
  (apply-rule st 'any
   (lambda (st)
     (let ((x (state-result st)))
       (if (or (char-between x #\a #\z)
               (char-between x #\A #\Z))
           x
           fail)))))

(define (apply-rule state sym next)
    (let* ((rule (get-rule sym))
           (res  (rule state)))
      (if (failure?) res) res (next res)))

(set-rule 'any (lambda (st)
                 (let ((s (state-stream st)))
                   (if (stream-end? s)
                       fail
                       (state+stream (state+result st (stream-read s))
                                     (stream-next s))))))
(set-rule 'alpha (lambda (st)
                   (apply-rule st 'any
                               (lambda (st)
                                 (let ((x (state-result st)))
                                   (if (or (char-between x #\a #\z)
                                           (char-between x #\A #\Z))
                                       x
                                       fail))))))
(define (id x) x)

(define (match-string string rule-name)
    (let* ((stream (make-stream string))
           (state  (list stream (list))))
      (state-result (apply-rule state rule-name id))))

(trace (match-string "" 'any))
(trace (match-string "x" 'any))
