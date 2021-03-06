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
(define fail '(fail fail fail))
(define (failure? state) (eq state fail))
(define nothing '(nothing))
(define (nothing? result) (eq result nothing))

(define (make-initial-state stream) (list stream nothing (make-ht)))

(define (result-cons a b)
  (cond ((nothing? a) b)
        ((nothing? b) (list a))
        (#t (cons a b))))

(define (state-stream state) (car state))
(define (state-result state) (nth state 1))
(define (state-vars state) (nth state 2))
(define (state-vars-at state key) (ht-at (state-vars state) key))
(define (state-vars-at-put state key value)
    (ht-at-put (state-vars state) key value))

(define (state+result state res) (lambda-bind (stream _ & rest) state
                                              `(,stream ,res ,@rest)))
(define (state+stream state stream) (cons stream (cdr state)))
(define (state+vars state vars) (lambda-bind (a b c & rest) state
                                             `(,a ,b ,vars ,@rest)))
(define (state-cons a b) (state+result a (result-cons (state-result a) (state-result b))))
(define (state-cons-onto a b) (state+result b (result-cons (state-result a) (state-result b))))

(define (reverse-result result)
    (if (nothing? result) nothing (reverse-list result)))

(define (state-reverse-result state)
    (if (nothing? (state-result state)) state
        (state+result state (reverse-list (state-result state)))))

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
                               (if (eq x item) (next st) fail)))))

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
                  (next (state-reverse-result last-state))
                  (let ((acc-state (state-cons new-state last-state)))
                    (set! last-state acc-state)
                    (apply-rule rule last-state apply-next)))))
      (apply-rule rule last-state apply-next)))

(define-apply (* state args next)
    (let* ((rule (car args))
           (acc-state (state+result state '()))) ;; should always return a list
      (apply-rule*-aux `(? ,rule) acc-state next)))

(define-apply (+ state args next)
    (let* ((rule (car args)))
      (apply-rule
       rule state
       (lambda (st)
         (let ((next-state (apply-rule `(* ,rule) st id)))
           (if (eq next-state st)
               (next (state+result st (result-cons (state-result st) nothing)))
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
          (result nothing))
      (set! helper (lambda (rules state)
                     (if (nil? rules) (next (state+result state (reverse-result result)))
                         (let* ((reset (state+result state nothing))
                                (next-state (apply-rule (car rules) reset id)))
                           (if (failure? next-state) fail
                               (let ()
                                 (set! result (result-cons (state-result next-state)
                                                           result))
                                 (helper (cdr rules) next-state)))))))
      (helper rules state)))

(define (every-other lst item)
    (reverse-list
     (reduce-list (lambda (acc next) (cons item (cons next acc))) (list item) lst)))

(define-apply (tokens state rules next)
    (let ((new-rule (cons 'seq (every-other rules 'ws))))
      (apply-rule new-rule state next)))

(define-apply (token state rules next)
    (let ((new-rule (cons 'seq (every-other rules 'ws))))
      (apply-rule new-rule state (lambda (st)
                                   (let ((x (state-result st)))
                                     (next (state+result st (car x))))))))

(define-apply (ign state rules next)
    (let ((rule (car rules)))
      (apply-rule rule state (lambda (next-state)
                               (next (state+result next-state nothing))))))

;; this do/bind/return stuff is a bit much...
;; should really be compiling this stuff anyway, not interpreting.
(define-apply (do state rules next)
    (let ((helper #f)
          (vars (state-vars state)))
      (set! helper (lambda (rules state)
                     (if (nil? rules) (next (state+vars state vars))
                         (apply-rule (car rules) state
                                     (lambda (next-state)
                                       (helper (cdr rules) next-state))))))
      (helper rules (state+vars state (make-ht)))))

(define-apply (bind state form next)
    (let ((sym (nth form 0))
          (rule (nth form 1)))
      (apply-rule rule state
                  (lambda (st)
                    (state-vars-at-put st sym (state-result st))
                    (next st)))))

(define-apply (return state form next)
    (let* ((sym (nth form 0))
           (val (state-vars-at state sym)))
      (print `(returning: ,sym ,val))
      (next (state+result state val))))


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

(set-rule 'ws (lambda (st) (apply-rule '(ign (* (ign space))) st id)))

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
                            (state+result st (car x)))))))

;; expr = ( expr* ) | token

(set-rule 'wrapped-expression
          (lambda (st)
            (apply-rule '(seq ws #\( ws maybe-exprs ws #\) ws) st
                        (lambda (st)
                          (let ((x (state-result st)))
                            (state+result st (nth x 1)))))))

(set-rule 'maybe-exprs
          (lambda (st) (apply-rule '(* expr) st id)))

(set-rule 'expr (lambda (st) (apply-rule '(or wrapped-expression token) st id)))

;; defined as:
;; expr = '(' expr* : x ')' -> x
;;      |  ( integer | ident ) : x -> x

;; would be nice to define as:
;; expr = '(' expr* : x ')' -> x
;;      | integer
;;      | ident 

(set-rule 'meta-1
          (lambda (st)
            (apply-rule
             '(or
               (do
                (tokens #\( (bind x (* meta-1)) #\) )
                (return x))
               (or (token integer) (token ident)))
             st
             id)))

(define (match-string string rule-name)
    (let* ((stream (make-stream string))
           (state  (make-initial-state stream)))
      (state-result (apply-rule rule-name state id))))

(define (match-string* string rule-name)
    (let* ((stream (make-stream string))
           (state  (make-initial-state stream)))
      (state-result (apply-rule `(* ,rule-name) state id))))
;;
(trace (match-string "" 'any))
(trace (match-string "x" '(ign #\x)))
;; (trace (match-string "x" 'any))
;; (trace (match-string "x" 'alpha))
;; (trace (match-string "xyz" 'alpha))
(trace (match-string "xyz" '(? alpha)))
(trace (match-string "xyz" '(* alpha)))
;; (trace (match-string "xyz" #\x))
(trace (match-string "xyz" '(seq #\x #\y #\z)))
(trace (match-string "xyzxyz" '(* (seq #\x #\y #\z))))
;; (trace (match-string "xyz" '(+ alpha)))
;; (trace (match-string "" 'ident))
;; (trace (match-string "xyz" 'ident))
;; (trace (match-string "x+y" 'ident))
;; (trace (match-string "0123456789" '(* digit)))
;; (trace (match-string "0123456789x" '(* digit)))
;; (trace (match-string "0123456789" 'integer))
;; (trace (match-string "0123456789x" 'integer))
;; (trace (match-string "123xyz" 'integer-or-ident))
;; (trace (match-string "xyz123" 'integer-or-ident))
;; (trace (match-string "123xyz" '(* integer-or-ident)))
;; (trace (match-string "xyz123" '(* integer-or-ident)))
;; (trace (match-string "123xyz" '(or integer ident)))
;; (trace (match-string "xyz123" '(or integer ident)))
(trace (match-string "123xyz" '(* (or integer ident))))
(trace (match-string "xyz123" '(* (or integer ident))))
;; (trace (match-string "" 'ws))
(trace (match-string " " 'ws))
;; (trace (match-string "x" 'ws))
(trace (match-string "xyz" 'token))
(trace (match-string " xyz " 'token))
(trace (match-string " xyz " '(+ token)))
(trace (match-string " xyz " '(* token)))
(trace (match-string* " xyz 1 2 3 hello world" 'token))
(trace (match-string "xyz" 'expr))
(trace (match-string "123" 'expr))
;; (trace (match-string "xyz 123" 'expr))
;; (trace (match-string "123 xyz" 'expr))
(trace (match-string "()" 'expr))
(trace (match-string "(lambda (x) x)" 'expr))
(trace (match-string "
(define mapcar
    (lambda (f lst)
      (if (nilp lst) lst
          (cons (f (car lst)) (mapcar f (cdr lst))))))
" 'expr))
;; (trace (match-string "
  
;;  " 'ws))
;; (print "--------------------------------------------------------------------------------")
(trace (match-string "()" 'meta-1))
(trace (match-string "(lambda (x) x)" 'meta-1))
(trace (match-string "(hello meta 1 world)" 'meta-1))


'bye
