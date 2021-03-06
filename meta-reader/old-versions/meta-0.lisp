(defmacro trace (form)
  `(let ((_res ,form))
     (print (list ',form '= _res))))

(trace "trace works")

(define (id x) x)

(define rules (make-ht))
(define fail '(fail fail))
(define (failure? state) (eq state fail))

(define (state-stream state) (car state))
(define (state-result state) (nth state 1))

(define (state+result state res) (lambda-bind (stream _ & rest) state
                                              `(,stream ,res,@rest)))
(define (state+stream state stream) (cons stream (cdr state)))
(define (state-cons a b) (state+result a (cons (state-result a) (state-result b))))
(define (state-cons-onto a b) (state+result b (cons (state-result a) (state-result b))))

(define (make-stream str) (cons 0 str))
(define (stream-read s) (char-at (cdr s) (car s)))
(define (stream-next s) (cons (+i 1 (car s)) (cdr s)))
(define (stream-end? s) (>i (+i 1 (car s)) (string-length (cdr s))))

(define (char-between b a c)
    (or (eq a b) (eq b c)
        (and (char-< a b)
             (char-< b c))))

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

(define apply-rule #f)
(define (rule-exactly it)
    (lambda (st)
      (apply-rule
       st 'any
       (lambda (st)
         (let ((x (state-result st)))
           (if (eq x it) st fail))))))

(define (get-rule x)
    (if (symbol? x)
        (ht-at rules x)
        (rule-exactly x)))

(define (set-rule x fn) (ht-at-put rules x fn))

(define (apply-rule state sym next)
    (let* ((rule (get-rule sym))
           (res  (rule state)))
      (if (failure? res) res (next res))))

(define (apply-rule? state sym next)
    (let ((result (apply-rule state sym id)))
      (if (failure? result) (next state) (next result))))

(define apply-rule*-aux #f) ;; annoying...
(define (apply-rule*-aux state sym next)
    (let ((apply-next #f)
          (last-state state))
      (set! apply-next
            (lambda (new-state)
              (if (eq new-state last-state) ;; no progress
                  (next (state+result
                         last-state
                         (reverse-list (state-result last-state))))
                  (let ((acc-state (state-cons new-state last-state)))
                    (set! last-state acc-state)
                    (apply-rule? last-state sym apply-next)))))
      (apply-rule? last-state sym apply-next)))

(define (apply-rule* state sym next)
    (let ((acc-state (state+result state '())))
      (apply-rule*-aux acc-state sym
                       (lambda (next-state)
                         (if (eq next-state acc-state)
                             (next state)
                             (next next-state))))))

(define (apply-rule+ state sym next)
    (apply-rule
     state sym
     (lambda (st)
       (let ((next-state (apply-rule* st sym id)))
         (if (eq next-state st)
             (next (state+result st (list (state-result st))))
             (next (state-cons-onto st next-state)))))))

(define (apply-or state syms next)
    (let ((helper #f))
      (set! helper (lambda (syms)
                     (if (nil? syms) fail
                         (let ((next-state (apply-rule state (car syms) id)))
                           (if (failure? next-state)
                               (helper (cdr syms))
                               (next next-state))))))
      (helper syms)))

(define (apply-seq state syms next)
    (let ((helper #f)
          (result '()))
      (set! helper (lambda (syms state)
                     (if (nil? syms) (next (state+result state (reverse-list result)))
                         (let ((next-state (apply-rule state (car syms) id)))
                           (if (failure? next-state) fail
                               (let ()
                                 (set! result (cons (state-result next-state) result))
                                 (helper (cdr syms) next-state)))))))
      (helper syms state)))

(set-rule 'any (lambda (st)
                 (let ((s (state-stream st)))
                   (if (stream-end? s)
                       fail
                       (state+stream (state+result st (stream-read s))
                                     (stream-next s))))))
(set-rule 'space
          (lambda (st)
            (apply-rule
             st 'any
             (lambda (st)
               (let ((x (state-result st)))
                 (if (or (eq x #\Space)
                         (char-< x #\Space))
                     st fail))))))

(set-rule 'ws (lambda (st) (apply-rule* st 'space id)))

(set-rule 'alpha (lambda (st)
                   (apply-rule
                    st 'any
                    (lambda (st)
                      (let ((x (state-result st)))
                        (if (or (char-between x #\a #\z)
                                (char-between x #\A #\Z))
                            (state+result st x)
                            fail))))))
(set-rule 'digit (lambda (st)
                   (apply-rule
                    st 'any
                    (lambda (st)
                      (let ((x (state-result st)))
                        (if (char-between x #\0 #\9)
                            (state+result st (-i (char-code x) (char-code #\0)))
                            fail))))))

(set-rule 'ident (lambda (st)
                   (apply-rule+
                    st 'alpha
                    (lambda (st)
                      (let ((chars (state-result st)))
                        (state+result st (implode chars)))))))

(set-rule 'integer
          (lambda (st)
            (apply-rule+
             st 'digit
             (lambda (st)
               (let ((nums (state-result st)))
                 (state+result
                  st
                  (reduce-list
                   (lambda (acc n)
                     (+i n (*i 10 acc)))
                   0 nums)))))))


(set-rule 'integer-or-ident (lambda (st) (apply-or st '(integer ident) id)))

(set-rule 'token
          (lambda (st)
            (apply-seq st '(ws integer-or-ident ws)
                       (lambda (st)
                         (let ((x (state-result st)))
                           (state+result st (nth x 1)))))))

;; expr = ( expr* ) | token

(set-rule 'wrapped-expression
          (lambda (st)
            (apply-seq st '(ws #\( ws maybe-exprs ws #\) ws)
                       (lambda (st)
                         (let ((x (state-result st)))
                           (state+result st (nth x 3)))))))

(set-rule 'maybe-exprs
          (lambda (st) (apply-rule* st 'expr id)))

(set-rule 'expr (lambda (st) (apply-or st '(wrapped-expression token) id)))

(define (match-string string rule-name)
    (let* ((stream (make-stream string))
           (state  (list stream 'initial-state)))
      (state-result (apply-rule state rule-name id))))

(define (match-string* string rule-name)
    (let* ((stream (make-stream string))
           (state  (list stream 'initial-state)))
      (state-result (apply-rule* state rule-name id))))

(trace (match-string "" 'any))
(trace (match-string "x" 'any))
(trace (match-string "x" 'alpha))
(trace (match-string "xyz" 'alpha))
(trace (match-string "" 'ident))
(trace (match-string "xyz" 'ident))
(trace (match-string* "" 'any))
(trace (match-string* "x" 'any))
(trace (match-string* "x" 'alpha))
(trace (match-string* "xyz" 'alpha))
(trace (match-string* "xyz" 'ident))
(trace (match-string* "" 'ident))
(trace (match-string "x+y" 'ident))
(trace (match-string* "0123456789" 'digit))
(trace (match-string* "0123456789x" 'digit))
(trace (match-string "0123456789" 'integer))
(trace (match-string "0123456789x" 'integer))
(trace (match-string "123xyz" 'integer-or-ident))
(trace (match-string "xyz123" 'integer-or-ident))
(trace (match-string* "123xyz" 'integer-or-ident))
(trace (match-string* "xyz123" 'integer-or-ident))
(trace (match-string "" 'ws))
(trace (match-string " " 'ws))
(trace (match-string "x" 'ws))
(trace (match-string "xyz" 'token))
(trace (match-string* " xyz 1 2 3 hello world" 'token))
(trace (match-string "xyz" 'expr))
(trace (match-string "123" 'expr))
(trace (match-string "xyz 123" 'expr))
(trace (match-string "123 xyz" 'expr))
(trace (match-string "()" 'expr))
(trace (match-string "(lambda (x) x)" 'expr))
(trace (match-string "
(define mapcar
    (lambda (f lst)
      (if (nilp lst) lst
          (cons (f (car lst)) (mapcar f (cdr lst))))))
" 'expr))
(trace (match-string "
   
 " 'ws))

'bye
