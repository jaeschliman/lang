(defmacro trace (form)
  `(let ((_res ,form))
     (print (list ',form '= _res))))

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
      ((string? form) (ht-at compilers-table'%string))
      (#t (ht-at compilers-table '%exactly))))

(define xf-vars car)
(define xf-form cdr)
(define (xf+ a b)
    (cons (append (xf-vars a) (xf-vars b))
          (append (xf-form a) (xf-form b))))

(define xf-rule #f)
(define xf-acc (lambda (acc r) (xf+ acc (xf-rule r))))

(define (xf-un-vars xf)
    (mapcar (lambda (v) (list v '()))
            (xf-vars xf)))

(define (xf-un-xf xf)
    (if (nil? (xf-vars xf)) (car (xf-form xf))
        `(let ,(xf-un-vars xf) ,(car (xf-form xf)))))

(define (xf-rule-body b)
    (reduce-list xf-acc (cons '() '()) b))

(define (xf-tl toplevel-rule)
    (if (pair? toplevel-rule)
        (xf-un-xf (xf-rule toplevel-rule))
        toplevel-rule))

(define (xf-pass-thru r)
    (let ((xf (xf-rule-body (cdr r))))
      (cons (car xf)
            (list (cons (car r) (cdr xf))))))

(define (xf-ignore r)
    (cons '() (list r)))

(define (xf-rule r)
    (if (pair? r)
        (case (car r)
          (or     (cons '() (list (cons 'or (mapcar xf-tl (cdr r))))))
          (set!   (cons (list (second r)) (list r)))
          (seq    (xf-pass-thru r))
          (*      (xf-pass-thru r))
          (+      (xf-pass-thru r))
          (?      (xf-pass-thru r))
          (return (xf-ignore r))
          (where  (xf-ignore r))
          (extern (xf-ignore r))
          (let    (xf-ignore r))) ;; punting on let for now -- should be generated

        (if (symbol? r) (cons '() (list r))
            (cons '() (list r)))))

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

(define (make-meta parent) (list parent (make-ht)))
(define meta-by-name (make-ht))
(define (find-meta x) (if (symbol? x) (ht-at meta-by-name x) x))

(ht-at-put meta-by-name 'Base (make-meta '()))

;; it is a list so we can push/pop on it, but only the first element
;; is used to look things up.
(defparameter *meta-context* (list 'Base))

(define (push-meta-context meta)
    (set-symbol-value '*meta-context* (cons meta *meta-context*)))
(define (pop-meta-context)
    (set-symbol-value '*meta-context* (cdr *meta-context*)))

(define (meta-lookup rulename)
    (let ((lookup #f))
      (set! lookup (lambda (meta)
                     (if (nil? meta) '()
                         (let ((Meta (find-meta meta)))
                           (let ((exist (ht-at (second Meta) rulename)))
                             (if (nil? exist) (lookup (first Meta))
                                 exist))))))
      (lookup (first *meta-context*))))

(define (get-rule x) (meta-lookup x))
(define (set-rule x fn) (ht-at-put (second (find-meta (first *meta-context*))) x fn))

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

(define (state-position st) (car (car st)))

(define (match-1 rule string)
    (let* ((stream (make-stream string))
           (state  (make-initial-state stream))
           (fn (get-rule rule))
           (newstate (fn state)))
      (state-result newstate)))

(define (match-map xf rule string)
    (let* ((stream (make-stream string))
           (state  (make-initial-state stream))
           (fn (get-rule rule))
           (loop #f))
      (set! loop
            (lambda (state results)
              (let ((newstate (fn state)))
                (if (failure? newstate) (reverse-list results)
                    (let ((newresults (cons (state-result newstate) results)))
                      (if (stream-end? (state-stream newstate)) (reverse-list newresults)
                          (loop newstate newresults)))))))
      (mapcar xf (loop state '()))))

(define (match-all rule string)
    (match-map (lambda (x) x) rule string))

(set-rule 'any (lambda (st)
                 (let ((s (state-stream st)))
                   (if (stream-end? s)
                       fail
                       (state+stream (state+result st (stream-read s))
                                     (stream-next s))))))
(set-rule 'nothing (lambda (st)
                     (let ((s (state-stream st)))
                       (if (stream-end? s)
                           st
                           fail))))

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

(define-compile (%string state item next)
    (let ((chars (string-to-charlist item)))
      (compile-rule (cons 'seq chars)
                    state next)))

(define-compile (extern state args next)
    (let ((meta (first args))
          (rule (second args)))
      `(let ((_result_ '()))
         (binding ((*meta-context* (list ',meta)))
                  (set! _result_ ,(compile-rule rule state 'id)))
         (if (failure? _result_) fail (,next _result_)))))

(define-compile (? state args next)
    (let* ((comp (compile-rule (car args) '_state_ 'id)))
      `(let* ((_state_ ,state)
              (result ,comp)
              (next   ,next))
         (if (failure? result) (next (state+result _state_ '())) (next result)))))

(define-compile (* state args next)
    (let ((rule (compile-rule (cons '? args) 'last-state 'apply-next)))
      `(let* ((apply-next #f)
              (last-state (state+result ,state (list))) ;; should always return a list
              (_next_ ,next)
              (_run_1_ (lambda () ,rule)))
         (set! apply-next (lambda (new-state)
                            (if (eq (state-position new-state)
                                    (state-position last-state))
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
;; TODO: optimize the single-item case e.g. (seq x) => x
(define-compile (seq state rules next)
    (let ((body
           (reduce-list (lambda (run-next rule)
                          `(lambda (_st_) ,(compile-rule rule '_st_ run-next)))
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

(defmacro define-rule (name & forms)
  (let ((xform (xf-tl (cons 'seq forms))))
    `(set-rule ',name (lambda (_state_) ,(compile-rule xform '_state_ 'id)))))

;;;; ----------------------------------------

(define (char-between b a c)
    (or (eq a b) (eq b c)
        (and (char-< a b)
             (char-< b c))))

(define (whitespace-char? x)
    (or (eq x #\Space)
        (char-< x #\Space)) )

(define (alpha-char? x)
    (or (char-between x #\a #\z)
        (char-between x #\A #\Z)))

(define-rule space
  (set! x any)
  (where (whitespace-char? x))
  (return x)) 

(define-rule constituent
    (set! x any)
  (where (not (or
               (whitespace-char? x)
               (eq x #\))
               (eq x #\()
               (eq x #\[)
               (eq x #\])
               (eq x #\{)
               (eq x #\}))))
  (return x))

(define-rule non-space
  (set! x any)
  (where (not (whitespace-char? x)))
  (return x))

(define-rule ws (* space))

(define-rule alpha
  (set! x any)
  (where (or (char-between x #\a #\z)
             (char-between x #\A #\Z)))
  (return x))

(define-rule digit
  (set! x any)
  (where (char-between x #\0 #\9))
  (return (-i (char-code x) (char-code #\0))))

(define-rule ident
    (set! x (+ alpha))
  (return (implode x)))


(ht-at-put meta-by-name 'Lisp (make-meta 'Base))
(push-meta-context 'Lisp)

(define-rule eol
    (or #\Return #\Newline nothing))
(define-rule non-eol
    (set! x any)
  (where (not (or (eq x #\Return)
                  (eq x #\Newline))))
  (return x))

(define-rule comment
    #\; (* non-eol) eol)

(define-rule ws
    (* (or space comment)))

(define (character-name? str)
    (not (nil? (char-by-name str))))

(define-rule character
    #\# #\\ (set! -name (+ constituent))
    (where (character-name? (charlist-to-string -name)))
    (return (char-by-name (charlist-to-string -name))))

(define-rule non-quote
    (set! x any)
  (where (not (or (eq x #\")
                  (eq x #\\))))
  (return x))

(define (escaped-char-character ch)
    (case ch
         (#\n #\Newline)
         (#\r #\Return)
         (#\t #\Tab)
         (#\\ #\\)
         (#\" #\")))

(define-rule escaped-char
    #\\ (set! x any) (return (escaped-char-character x)))

(define-rule string-char
    (or non-quote escaped-char))

(define-rule string
    #\" (set! -chars (* string-char)) #\"
    (return (charlist-to-string -chars)))

(define (symbol-char x) ;; TODO: convert to lookup table
    (or (char-between x #\a #\z)
        (char-between x #\* #\-)
        (char-between x #\< #\Z)
        (char-between x #\/ #\9)
        (char-between x #\# #\&)
        (eq x #\!) (eq x #\^) (eq x #\_)
        (eq x #\|) (eq x #\~)))

(define-rule symbol-char
    (set! x any) (where (symbol-char x)) (return x))

(define-rule symbol
    (set! x symbol-char)
  (where (not (char-between x #\0 #\9)))
  (set! xs (* symbol-char))
  (return (implode (cons x xs))))

(define-rule integer
    (set! sign? (? #\-))  (set! x (+ digit))
    (return
      (let ((n (reduce-list
                (lambda (acc n) (+i n (*i 10 acc)))
                0 x))
            (s (if (nil? sign?) 1 -1)))
        (*i s n))))

(define-rule float
    (set! sign? (? #\-)) (set! x (+ digit)) #\. (set! y (+ digit))
    (return
      (let ((n (+f (i->f (reduce-list
                          (lambda (acc n) (+i n (*i 10 acc)))
                          0 x))
                   (reduce-list
                    (lambda (acc n) (*f 0.1 (+f (i->f n) acc)))
                    0.0 (reverse-list y))))
            (s (if (nil? sign?) 1.0 -1.0)))
        (*f s n))))

(define-rule point
    (set! x integer) #\@ (set! y integer)
    (return (make-point x y)))

(define-rule hex-char
    (set! x any) (where (or (char-between x #\0 #\9)
                            (char-between x #\a #\f)))
    (return x))

(define (hex-char-value ch)
    (if (char-between ch #\a #\f)
        (+i 10 (-i (char-code ch) (char-code #\a)))
        (-i (char-code ch) (char-code #\0))))

(define-rule hex-integer
    "0x" (set! x (+ hex-char))
    (return (reduce-list
             (lambda (acc hx) (+i (hex-char-value hx) (*i 16 acc)))
             0 x)))

(define-rule true "#t" (return #t))
(define-rule false "#f" (return #f))
(define-rule boolean (or true false))

(define-rule atom
    ws (set! x (or boolean character hex-integer float point integer symbol string)) ws
    (return x))

(define-rule quoted
    #\' ws (set! x expr) (return (list 'quote x)))
(define-rule quasiquoted
    #\` ws (set! x expr) (return (list 'quasiquote x)))
(define-rule unquoted-splicing
    #\, #\@ ws (set! x expr) (return (list 'unquote-splicing x)))
(define-rule unquoted
    #\, ws (set! x expr) (return (list 'unquote x)))

(define-rule quotation
    (or quoted quasiquoted unquoted-splicing unquoted))


(define-rule expr
    (or (seq ws #\( (set! x (* expr)) #\) ws
             (return x))
        quotation
        atom))

(pop-meta-context)
(ht-at-put meta-by-name 'Meta (make-meta 'Base))
(push-meta-context 'Meta)

(define-rule meta-mod
    (set! m (or #\* #\? #\+))
  (return (implode (list m))))


(define-rule string-lit
    (set! x (extern Lisp string)) (return (cons 'seq (string-to-charlist x))))

(define (bracket-escaped-char-character ch)
    (case ch
         (#\n #\Newline)
         (#\r #\Return)
         (#\t #\Tab)
         (#\\ #\\)
         (#\] #\])
         (#\- #\-)))

(define-rule bracket-body-char
    (set! x any) (where (not (or (eq x #\])
                                 (eq x #\\))))
    (return x))

(define-rule bracket-escaped-char
    #\\ (set! x any) (return (bracket-escaped-char-character x)))

(define-rule bracket-char
    (or bracket-body-char bracket-escaped-char))

(define-rule bracket-char-range
    (set! a bracket-char) #\- (set! b bracket-char)
    (return
      (let ((var (gensym)))
        `(seq (set! ,var any)
              (where (char-between ,var ,a ,b))
              (return ,var)))))

;; TODO: A-Za-z syntax
(define-rule bracket-lit
    "[" (set! chs (+ (or bracket-char-range bracket-char))) "]"
    (return (cons 'or chs)))

(define-rule meta-lit
    (or ident string-lit bracket-lit))

(define-rule meta-atom
    (or (seq (set! -id meta-lit) (set! -mm meta-mod) (return (list -mm -id)))
        meta-lit))

(define-rule meta-app-1
    (or (seq (set! -rule meta-atom) ws #\: (set! -as ident)
             (return `(set! ,-as ,-rule)))
        meta-atom
        meta-pred))

(define-rule meta-app
    (set! -app meta-app-1) (return -app))

(define-rule meta-pred
    #\? ws (set! -it (extern Lisp expr)) (return `(where ,-it)))

(define-rule meta-result
    "->" ws (set! -it (extern Lisp expr)) (return `(return ,-it)))

(define-rule rule-app
    (* #\Space) (set! -app meta-app-1) (return -app))

(define-rule rule-branch
    (set! -match (+ rule-app)) (* #\Space) (set! -result (? meta-result))
    (return (cons 'seq (append -match (if (nil? -result) '() (list -result))))))

(define-rule rule-body-list
    (set! -first rule-branch) ws (set! -rest (? more-rule-body-list))
    (return (cons -first -rest)))

(define-rule more-rule-body-list
    "|" (set! -rest rule-body-list) (return -rest))

(define-rule rule-body
    (set! -rules rule-body-list)
  (return (cons 'or -rules)))

(define-rule rule
    ws (set! -name ident) ws "=" (set! -body rule-body)
    (return (let ()
              ;; (print `(matched rule ,-name ,-body))
              `(define-rule ,-name ,-body))))

(define-rule meta-block
    ws "meta" ws (set! -n ident) ws #\{ ws
    (set! -rs (+ rule))
    ws #\}
    (return `(let ()
               (ht-at-put meta-by-name ',-n (make-meta 'Base))
               (binding ((*meta-context* (list ',-n)))
                        ,@-rs))))

(define-rule meta-main
    (set! -it (or meta-block (extern Lisp expr)))
  (return -it))

(define (whitespace-char? ch)
    (or (eq ch #\Space)
        (char-< ch #\Space)))
(define (digit-char? ch)
    (char-between ch #\0 #\9))
(define (char-to-digit ch)
    (-i (char-code ch) (char-code #\0)))
(define (make-integer numbers)
    (reduce-list
     (lambda (acc n) (+i n (*i 10 acc)))
     0 numbers))


(pop-meta-context)

(define (meta1-runfile path)
    (let ((input (slurp path)))
      (binding ((*meta-context* (list 'Meta)))
               (match-map eval 'meta-main input))))

(meta1-runfile "./meta-1.testfile0.lisp")

(binding ((*meta-context* (list 'testfile)))
         (match-map print 'main "
123 456 a 789 hello how are you foo with-dashes foofoo
"))

;; use as default reader for the repl
(define (run-string input)
    (push-meta-context 'Meta)
  (match-map eval 'meta-main input)
  (pop-meta-context))

;; (print "sleeping first")
;; (sleep-ms 500)
;; (print "slept")
;; (let ()
;;   (meta1-runfile "./threads.lisp")
;;   (print "ran the file in this expression"))
;; (print "ran the file")
'done
