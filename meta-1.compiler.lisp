(defmacro trace (form)
  `(let ((_res ,form))
     (print (list ',form '= _res))))

(trace "trace works")

;; TODO: symbol hygiene for rule compiler internals

(define (id x) x)
(define (safe-cdr it) (if (pair? it) (cdr it) it))

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

(define (apply-rule rule state next)
    (let ((fn (applicator rule)))
      (if (not (nil? fn))
          (fn state (safe-cdr rule) next))))

(define compilers-table (make-ht))
(defmacro define-compile (defn & body)
  (lambda-bind
   (name & params) defn
   `(ht-at-put compilers-table ',name (lambda ,params ,@body))))

(define (compiler-for form)
    (cond
      ((symbol? form) (ht-at compilers-table '%base))
      ((pair? form) (ht-at compilers-table (car form)))
      (#t (ht-at compilers-table '%exactly))))

(define (compile-rule rule state next)
    (let ((fn (compiler-for rule)))
      (if (not (nil? fn))
          (fn state (safe-cdr rule) next))))

(defmacro dbg (rule string)
  (print (list rule string))
  ;; (print '------------>)
  ;; (print (compile-rule rule 'initial-state 'final-return))
  (let ((compiled (compile-rule rule 'state 'id)))
    (print '========>)
    `(let* ((stream (make-stream ,string))
            (state  (make-initial-state stream)))
       (print (state-result ,compiled)))))

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

(define-compile (%base state symbol next)
    `(apply-rule ',symbol ,state ,next))

(define-apply (%base state symbol next)
    (let* ((rule (get-rule symbol))
           (res  (rule state)))
      (if (failure? res) res (next res))))

(define-compile (%exactly state item next)
    (let ((x (gensym)))
      `(apply-rule 'any ,state (lambda (st)
                                 (let ((,x (state-result st)))
                                   (if (eq ,x ,item) (,next st) fail))))))

(define-compile (? state args next)
    (let* ((comp (compile-rule (car args) '_state_ 'id)))
      `(let* ((_state_ ,state)
              (result ,comp)
              (next   ,next))
         (if (failure? result) (next _state_) (next result)))))

(define-compile (* state args next)
    (let ((rule (compile-rule (cons '? args) 'last-state 'apply-next)))
      `(let* ((apply-next #f)
              (last-state (state+result ,state (list))) ;; should always return a list
              (_next_ ,next)
              (_run_1_ (lambda () ,rule)))
         (set! apply-next (lambda (new-state)
                            (if (eq new-state last-state)
                                (_next_ (state-reverse-result last-state))
                                (let ((acc-state (state-cons new-state last-state)))
                                  (set! last-state acc-state)
                                  (_run_1_)))))
         (_run_1_))))

(define-compile (+ state args next)
    (let* ((rule (car args))
           (gather
            `(lambda (st)
               (let ((next-state ,(compile-rule (list '* rule) 'st 'id)))
                 (if (eq next-state st)
                     (next (state+result st (result-cons (state-result st) nothing)))
                     (next (state-cons-onto st next-state))))))
           (run (compile-rule rule 'state gather)))
      `(let* ((next ,next)
              (state ,state))
         ,run)))

(define-compile (or state rules next)
    (let ((body
           (reduce-list
            (lambda (acc rule)
              `(let ((next-state ,(compile-rule rule 'state 'id)))
                 (if (failure? next-state)
                     ,acc
                     (next next-state))))
            'fail
            (reverse-list rules))))
      `(let ((state ,state)
             (next  ,next))
         ,body)))

;; changed to _not_ aggregate results as I'm not sure we actually need
;; that from seq.
(define-compile (seq state rules next)
    (let ((body
           (reduce-list (lambda (run-next rule)
                          `(lambda (st) ,(compile-rule rule 'st run-next)))
                        next
                        (reverse-list rules))))
      `(,body ,state)))

;; this is sort of a cheesy form of let, but it works for now
(define-compile (let state forms next)
    `(let ,(car forms)
       ,(compile-rule (cons 'seq (cdr forms))
                      state next)))

(define-compile (set! state forms next)
    (let ((var (car forms))
          (rule (car (cdr forms))))
      (compile-rule rule state `(lambda (_st_)
                                  (set! ,var (state-result _st_))
                                  (,next _st_)))))

(define-compile (where state forms next)
    (let ((test (car forms)))
      `(if ,test (,next ,state)
           fail)))

(define-compile (return state forms next)
    (let ((var (car forms)))
      `(,next (state+result ,state ,var))))

(set-rule 'any (lambda (st)
                 (let ((s (state-stream st)))
                   (if (stream-end? s)
                       fail
                       (state+stream (state+result st (stream-read s))
                                     (stream-next s))))))

(defmacro define-rule (name form)
  `(set-rule ',name (lambda (_state_) ,(compile-rule form '_state_ 'id))))

(define-rule space
    (let ((x '()))
      (set! x any)
      (where (or (eq x #\Space)
                 (char-< x #\Space)))
      (return x)))

(define-rule ws (* space))

(define-rule alpha
    (let ((x '()))
      (set! x any)
      (where (or (char-between x #\a #\z)
                 (char-between x #\A #\Z)))
      (return x)))

(define-rule digit
    (let ((x '()))
      (set! x any)
      (where (char-between x #\0 #\9))
      (return (-i (char-code x) (char-code #\0)))))

(define-rule ident
    (let ((x '()))
      (set! x (+ alpha))
      (return (implode x))))

(define-rule integer
    (let ((x '()))
      (set! x (+ digit))
      (return (reduce-list
               (lambda (acc n)
                 (+i n (*i 10 acc)))
               0 x))))

(define-rule token
    (let ((x '()))
      ws (set! x (or integer ident)) ws
      (return x)))

(define-rule expr
    (or (let ((x '()))
          ws #\( (set! x (* expr)) #\) ws
          (return x))
        token))


(dbg any "")
(dbg #\c  "c")
(dbg (? any) "")
(dbg (? any) "x")
(dbg (* any) "x")
(dbg (* any) "xyz123")
(dbg (+ any) "x")
(dbg (+ any) "xyz123")
(dbg (+ any) "")
(dbg (+ #\x) "xxx")
(dbg (+ #\x) "xyz")
(dbg (+ #\x) "zyx")
(dbg (or any any) "a")
(dbg (or #\a #\b) "a")
(dbg (or #\a #\b) "b")
(dbg (+ (or #\a #\b)) "b")
(dbg (+ (or #\a #\b)) "ababbaabab")
(dbg (seq any) "abc")
(dbg (seq any any any) "abc")
(dbg (let ((x 10)) (+ #\a)) "aaa")
(dbg (let ((x 10)) (+ #\a) (+ #\b)) "aaabbb")
(dbg (let ((x 10)) (set! x (+ #\a)) (+ #\b)) "aaabbb")
(dbg (let ((x 10))
       (set! x (+ #\a))
       (+ #\b)
       (return x)) "aaabbb")
(dbg (let ((x 10))
       (+ #\a)
       (set! x (+ #\b))
       (+ #\c)
       (return x)) "aabbbcc")

(dbg (let ((x '()))
       (set! x any)
       (where (char-between x #\0 #\9))
       (return (-i (char-code x) (char-code #\0))))
     "0")

(dbg (+ (let ((x '()))
          (set! x any)
          (where (char-between x #\0 #\9))
          (return (-i (char-code x) (char-code #\0)))))
     "012345abc")
(dbg (+ space) " 
    
")
(dbg ws " 
    
")
(dbg ident "hello")
(dbg integer "12345")
(dbg token " 234 ")
(dbg (* token) "hello from compiled meta 1 thing")
(dbg expr "hello")
(dbg expr "(lambda (x) x)")
(dbg expr "
(define mapcar
    (lambda (f lst)
      (if (nilp lst) lst
          (cons (f (car lst)) (mapcar f (cdr lst))))))
"
)

'bye
