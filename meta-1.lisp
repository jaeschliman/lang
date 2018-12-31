(defmacro trace (form)
  `(let ((_res ,form))
     (print (list ',form '= _res))))

(trace "trace works")

(define (id x) x)

(define applicators (make-ht))
(defmacro define-apply (defn & body)
  (lambda-bind
   (name & params) defn
   `(ht-at-put applicators ',name (lambda ,params ,@body))))

(define (applicator form)
    (cond
      ((symbol? form) (ht-at applicators '%base))
      ((pair? form) (ht-at applicators (car form)))
      (#t (ht-at applicators '%exactly))))

(define (safe-cdr it) (if (pair? it) (cdr it) it))

(define (apply-rule rule state next)
    (let ((fn (applicator rule)))
      (if (not (nil? fn))
          (fn state (safe-cdr rule) next))))

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

(define (get-rule x) (ht-at rules x))
(define (set-rule x fn) (ht-at-put rules x fn))

(define-apply (%exactly state item next)
    (apply-rule 'any state (lambda (st)
                             (let ((x (state-result st)))
                               (if (eq x item) st fail)))))

(define-apply (%base state symbol next)
    (let* ((rule (get-rule symbol))
           (res  (rule state)))
      (if (failure? res) res (next res))))

(define-apply (? state args next)
    (let* ((rule (car args))
           (result (apply-rule rule state id)))
      (if (failure? result) (next state) (next result))))

(define (apply-rule*-aux rule state next)
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
                    (apply-rule rule last-state apply-next)))))
      (apply-rule rule last-state apply-next)))

(define-apply (* state args next)
    (let* ((rule (car args))
           (acc-state (state+result state '())))
      (apply-rule*-aux `(? ,rule) acc-state
                       (lambda (next-state)
                         (if (eq next-state acc-state)
                             (next state)
                             (next next-state))))))

(define-apply (+ state args next)
    (let* ((rule (car args)))
      (apply-rule
       rule state
       (lambda (st)
         (let ((next-state (apply-rule `(* ,rule) st id)))
           (if (eq next-state st)
               (next (state+result st (list (state-result st))))
               (next (state-cons-onto st next-state))))))))

(define-apply (or state rules next)
    (let ((helper #f))
      (set! helper (lambda (rules)
                     (if (nil? rules) fail
                         (let ((next-state (apply-rule (car rules) state id)))
                           (if (failure? next-state)
                               (helper (cdr rules))
                               (next next-state))))))
      (helper rules)))

(define-apply (seq state rules next)
    (let ((helper #f)
          (result '()))
      (set! helper (lambda (rules state)
                     (if (nil? rules) (next (state+result state (reverse-list result)))
                         (let ((next-state (apply-rule (car rules) state id)))
                           (if (failure? next-state) fail
                               (let ()
                                 (set! result (cons (state-result next-state) result))
                                 (helper (cdr rules) next-state)))))))
      (helper rules state)))

(set-rule 'any (lambda (st)
                 (let ((s (state-stream st)))
                   (if (stream-end? s)
                       fail
                       (state+stream (state+result st (stream-read s))
                                     (stream-next s))))))
(set-rule 'space
          (lambda (st)
            (apply-rule
             'any st
             (lambda (st)
               (let ((x (state-result st)))
                 (if (or (eq x #\Space)
                         (char-< x #\Space))
                     st fail))))))

(set-rule 'ws (lambda (st) (apply-rule '(* space) st id)))

(set-rule 'alpha (lambda (st)
                   (apply-rule
                    'any st
                    (lambda (st)
                      (let ((x (state-result st)))
                        (if (or (char-between x #\a #\z)
                                (char-between x #\A #\Z))
                            (state+result st x)
                            fail))))))
(set-rule 'digit (lambda (st)
                   (apply-rule
                    'any st
                    (lambda (st)
                      (let ((x (state-result st)))
                        (if (char-between x #\0 #\9)
                            (state+result st (-i (char-code x) (char-code #\0)))
                            fail))))))

(set-rule 'ident (lambda (st)
                   (apply-rule
                    '(+ alpha) st
                    (lambda (st)
                      (let ((chars (state-result st)))
                        (state+result st (implode chars)))))))

(set-rule 'integer
          (lambda (st)
            (apply-rule
             '(+ digit) st
             (lambda (st)
               (let ((nums (state-result st)))
                 (state+result
                  st
                  (reduce-list
                   (lambda (acc n)
                     (+i n (*i 10 acc)))
                   0 nums)))))))


(set-rule 'integer-or-ident (lambda (st) (apply-rule '(or integer ident) st id)))

(set-rule 'token
          (lambda (st)
            (apply-rule '(seq ws integer-or-ident ws) st
                        (lambda (st)
                          (let ((x (state-result st)))
                            (state+result st (nth x 1)))))))

;; expr = ( expr* ) | token

(set-rule 'wrapped-expression
          (lambda (st)
            (apply-rule '(seq ws #\( ws maybe-exprs ws #\) ws) st
                        (lambda (st)
                          (let ((x (state-result st)))
                            (state+result st (nth x 3)))))))

(set-rule 'maybe-exprs
          (lambda (st) (apply-rule '(* expr) st id)))

(set-rule 'expr (lambda (st) (apply-rule '(or wrapped-expression token) st id)))

(define (match-string string rule-name)
    (let* ((stream (make-stream string))
           (state  (list stream 'initial-state)))
      (state-result (apply-rule rule-name state id))))

(define (match-string* string rule-name)
    (let* ((stream (make-stream string))
           (state  (list stream 'initial-state)))
      (state-result (apply-rule `(* ,rule-name) state id))))
;;
(trace (match-string "" 'any))
(trace (match-string "x" 'any))
(trace (match-string "x" 'alpha))
(trace (match-string "xyz" 'alpha))
(trace (match-string "xyz" '(? alpha)))
(trace (match-string "xyz" '(* alpha)))
(trace (match-string "xyz" #\x))
(trace (match-string "xyz" '(seq #\x #\y #\z)))
(trace (match-string "xyzxyz" '(* (seq #\x #\y #\z))))
(trace (match-string "xyz" '(+ alpha)))
(trace (match-string "" 'ident))
(trace (match-string "xyz" 'ident))
(trace (match-string "x+y" 'ident))
(trace (match-string "0123456789" '(* digit)))
(trace (match-string "0123456789x" '(* digit)))
(trace (match-string "0123456789" 'integer))
(trace (match-string "0123456789x" 'integer))
(trace (match-string "123xyz" 'integer-or-ident))
(trace (match-string "xyz123" 'integer-or-ident))
(trace (match-string "123xyz" '(* integer-or-ident)))
(trace (match-string "xyz123" '(* integer-or-ident)))
(trace (match-string "123xyz" '(or integer ident)))
(trace (match-string "xyz123" '(or integer ident)))
(trace (match-string "123xyz" '(* (or integer ident))))
(trace (match-string "xyz123" '(* (or integer ident))))
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
