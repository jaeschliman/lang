;; it is a list so we can push/pop on it, but only the first element
;; is used to look things up.
(at-boot (defparameter *meta-context* (list 'Base)))
(at-boot (defparameter *match-start*))
(at-boot (defparameter *match-end*))
(at-boot (defparameter *current-rule-name*))

(defmacro trace (form)
  `(let ((_res ,form))
     (print (list ',form '= _res))
     _res))

(at-boot (define MetaInputStream (create-class 'MetaInputStream '(pos str line memo next))))

(define (make-meta-input-stream pos str line)
  (let ((r (instantiate-class MetaInputStream)))
    (instance-set-ivar r 0 pos)
    (instance-set-ivar r 1 str)
    (instance-set-ivar r 2 line)
    (instance-set-ivar r 3 '())
    (instance-set-ivar r 4 '#f)
    r))

(defmacro meta-stream-pos  (it) `(instance-get-ivar ,it 0))
(defmacro meta-stream-str  (it) `(instance-get-ivar ,it 1))
(defmacro meta-stream-line (it) `(instance-get-ivar ,it 2))
(defmacro meta-stream-memo (it) `(instance-get-ivar ,it 3))
(defmacro meta-stream-next (it) `(instance-get-ivar ,it 4))

(at-boot (define fail '(fail fail fail)))
(defmacro failure? (state) `(,eq ,state ',fail))

(define (make-initial-state stream) (list stream '()))
(define state-stream car)
(define state-result cadr)

(define (make-stream str) (make-meta-input-stream 0 str (cons 0 0)))
(define (stream-line-position s) (car (meta-stream-line s)))
(define (stream-col-position s)  (cdr (meta-stream-line s)))
;; (define (stream-read s) (char-at (meta-stream-str s) (meta-stream-pos s)))

(defmacro stream-read (str)
  (let ((s (gensym)))
    `(let ((,s ,str))
       (char-at (meta-stream-str ,s) (meta-stream-pos ,s)))))

(define (stream-end? s)
  (eq (meta-stream-pos s) (string-byte-length (meta-stream-str s))))

(define (stream-advance s char)
  (or (meta-stream-next s)
      (let* ((nl? (eq char #\Newline))
             (col? (not (or nl? (eq char #\Return))))
             (prev-pos (meta-stream-line s))
             (pos (if (or col? nl?)
                      (cons (if nl? (+i 1 (car prev-pos))
                                (car prev-pos))
                            (cond (nl? 0)
                                  (col? (+i 1 (cdr prev-pos)))
                                  (#t (cdr prev-pos))))
                      prev-pos)))
        (instance-set-ivar s 4 (make-meta-input-stream
                                (+i (char-width char) (meta-stream-pos s))
                                (meta-stream-str s)
                                pos)))))


(define (stream-peek s)
  (if (stream-end? s) '()
      (char-at (meta-stream-str s) (meta-stream-pos s))))

(define (stream-at s key)
  (let ((ht (meta-stream-memo s)))
    (if-nil? ht '() (ht-at ht key))))

(define (stream-at-put s key val)
  (unless (eq key 'any)
    (if-nil? (meta-stream-memo s) (instance-set-ivar s 3 (make-ht)))
    (ht-at-put (meta-stream-memo s) key val)))

(define (state-position st) (meta-stream-pos (car st)))
(define (state-col-row st) (meta-stream-line (car st)))

(define result-cons cons)

(define state-result cadr)
(define (state+result state res) (list (car state) res))
(define (state+stream state stream) (cons stream (cdr state)))
(define (state+stream+result state stream result) (list stream result))
(define (state-cons a b) (state+result a (result-cons (state-result a) (state-result b))))
(define (state-cons-result a b) (state+result b (result-cons (state-result a) (state-result b))))

(define reverse-result reverse-list)

(define (state-reverse-result state)
  (if (nil? (state-result state)) state
      (state+result state (reverse-list (state-result state)))))

(at-boot (define sentinel (list 'sentinel)))

(at-boot (define applicators (make-ht)))

(defmacro define-apply (defn & body)
  (lambda-bind
   (name & params) defn
   `(ht-at-put applicators ',name (%nlambda (applicator for ,name) ,params ,@body))))

(define (applicator form)
  (cond
    ((symbol? form) (ht-at applicators '%base))
    ((pair? form) (ht-at applicators (car form)))
    (#t (ht-at applicators '%exactly))))

(defparameter *meta-trace* #f)
(defparameter *meta-trace-indent* 0)
(defparameter *meta-memo* #t)

;; to be defined later
(forward stream-write-string)
(define (%meta-print-indent)
  (stream-write-string *standard-output* (make-string *meta-trace-indent* #\Space)))

;; (define (apply-rule rule state next)
;;   (binding ((*meta-trace-indent* (+i 4 *meta-trace-indent*)))
;;     (let ((fn (applicator rule)))
;;       (when (nil? fn) (throw `(rule ,rule is undefined)))
;;       (when *meta-trace*
;;         (%meta-print-indent)
;;         (%print `(,rule ----- ,(state-position state))))
;;       (let* ((failed-from-recursion #f)
;;              (cache-hit #f)
;;              (exist (stream-at (state-stream state) rule))
;;              (applied (cond
;;                         ((eq exist sentinel) (let ()  (set! failed-from-recursion #t) fail))
;;                         ((nil? exist)
;;                          (let ()
;;                            (unless (eq rule 'any)
;;                              (stream-at-put (state-stream state) rule sentinel))
;;                            (let ((result (fn state (safe-cdr rule) identity)))
;;                              (unless (eq rule 'any)
;;                                (if *meta-memo*
;;                                    (stream-at-put (state-stream state) rule result)
;;                                    (stream-at-put (state-stream state) rule '())))
;;                              result)))
;;                         (#t (let () (set! cache-hit #t)
;;                                  exist)))))
;;         (when *meta-trace*
;;           (%meta-print-indent)
;;           (%print `(,rule => ,(state-result applied) ,failed-from-recursion ,cache-hit)))
;;         (next applied)))))

(define (--any st)
  (let ((s (state-stream st)))
    (if (stream-end? s) fail
        (let ((ch (stream-read s)))
          (state+stream+result st (stream-advance s ch) ch)))))

(define (--any-2 st next)
  (let ((s (state-stream st)))
    (if (stream-end? s) fail
        (let ((ch (stream-read s)))
          (next (state+stream+result st (stream-advance s ch) ch))))))

(forward get-rule)

;; (define (apply-rule rule state next)
;;   (let ((fn (get-rule rule))
;;         (stream (state-stream state)))
;;     (let* ((exist (stream-at stream rule)))
;;       (cond
;;         ((nil? exist)
;;          (stream-at-put stream rule sentinel)
;;          (let ((result (fn state)))
;;            (stream-at-put stream rule result)
;;            (if (failure? result) fail
;;                (next result))))
;;         ((eq exist sentinel) fail)
;;         ((failure? exist) fail)
;;         (#t (next exist))))))

;; (define (apply-rule rule state next)
;;   (let ((fn (get-rule rule))
;;         (stream (state-stream state)))
;;     (let* ((exist (stream-at stream rule)))
;;       (cond
;;         ((nil? exist)
;;          ;; (stream-at-put stream rule sentinel)
;;          (let ((result (fn state)))
;;            ;; (stream-at-put stream rule result)
;;            (if (failure? result) fail
;;                (next result))))
;;         ;; ((eq exist sentinel) fail)
;;         ((failure? exist) fail)
;;         (#t (next exist))))))

;; (define (apply-rule rule state next)
;;   (let ((fn (get-rule rule))
;;         (stream (state-stream state)))
;;     (let ((result (fn state)))
;;       (if (failure? result) result
;;           (next result)))))

(define (failcall next result) (if (failure? result) result (next result)))

;; (define (apply-rule rule state next) (failcall next ((get-rule rule) state)))

(define (apply-rule rule state next)
  (let ((r ((get-rule rule) state)))
    (if (failure? r) r (next r))))


(at-boot (define compilers-table (make-ht)))

(defmacro define-compile (defn & body)
  (lambda-bind
   (name & params) defn
   `(ht-at-put compilers-table ',name (lambda ,params ,@body))))

(define (compiler-for form)
  (cond
    ((symbol? form) (ht-at compilers-table '%base))
    ((pair? form) (ht-at compilers-table (car form)))
    ((string? form) (ht-at compilers-table '%string))
    (#t (ht-at compilers-table '%exactly))))

(define xf-vars car)
(define xf-form cdr)
(define (xf+ a b)
  (cons (append (xf-vars a) (xf-vars b))
        (append (xf-form a) (xf-form b))))

(define (xf-primary-form a) (car (xf-form a)))

(forward xf-rule)
(define xf-acc (lambda (acc r) (xf+ acc (xf-rule r))))

(define (xf-un-vars xf)
  (mapcar (lambda (v) (list v '()))
          (xf-vars xf)))

(define (xf-un-xf xf)
  (if (nil? (xf-vars xf)) (xf-primary-form xf)
      `(let ,(xf-un-vars xf) ,(xf-primary-form xf))))

(define (xf-rule-body b)
  (reduce-list xf-acc (cons '() '()) b))

(define (xf-tl toplevel-rule)
  (if (pair? toplevel-rule)
      (xf-un-xf (xf-rule toplevel-rule))
      toplevel-rule))

(define (xf-pass-thru r)
  (let* ((rule-head (car r))
         (rule-body (cdr r))
         (xf (xf-rule-body rule-body)))
    (cons (xf-vars xf)
          (list (cons rule-head (xf-form xf))))))

(define (xf-transform-set! r)
  (let* ((var (second r))
         (body (third r))
         (xf  (xf-rule body))
         (vars (cons var (xf-vars xf)))
         (new-body (xf-primary-form xf)))
    (cons vars (list `(set! ,var ,new-body)))))

(define (xf-ignore r)
  (cons '() (list r)))

(define (xf-rule r)
  (if (pair? r)
      (case (car r)
        (or     (xf-ignore (cons 'or (mapcar xf-tl (cdr r)))))
        (set!   (xf-transform-set! r))
        (seq    (xf-pass-thru r))
        (*      (xf-pass-thru r))
        (+      (xf-pass-thru r))
        (?      (xf-pass-thru r))
        (not    (xf-pass-thru r))
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

(defmacro define-rule (name & forms)
  (let ((xform (xf-tl (cons 'seq forms))))
    `(set-rule ',name (%nlambda (meta rule for ,name) (_state_) ,(compile-rule xform '_state_ 'identity)))))

(define (make-meta parent) (list parent (make-ht) (make-ht))) ; parent, rules, cache
(at-boot (define meta-by-name (make-ht)))
(define (find-meta x) (if (symbol? x) (ht-at meta-by-name x) x))

(at-boot (ht-at-put meta-by-name 'Base (make-meta '())))

(define (push-meta-context meta)
  (set-symbol-value '*meta-context* (cons meta *meta-context*)))
(define (pop-meta-context)
  (set-symbol-value '*meta-context* (cdr *meta-context*)))

(define meta-super first)

(define (meta-lookup rulename)
  (let* ((top (first *meta-context*)))
    (let lookup ((meta top))
         (if (nil? meta) '()
             (let ((Meta (find-meta meta)))
               (let ((exist (ht-at (second Meta) rulename)))
                 (if (nil? exist) (lookup (meta-super Meta))
                     exist)))))))

(define (get-rule x)
  (if (eq x 'any) --any
      (let ((r (meta-lookup x)))
        (when (nil? r) (throw `(rule ,x is undefined)))
        r)))

(define (set-rule x fn) (ht-at-put (second (find-meta (first *meta-context*))) x fn))

(define (current-match-string)
  (let ((str (second (state-stream *match-start*))))
    (string-substr-bytes str (state-position *match-start*) (state-position *match-end*))))

(set-rule 'any --any)

(set-rule 'nothing (lambda (st) (if (stream-end? (state-stream st)) st fail)))

;; (define-compile (%base state symbol next)
;;     (let ((s (gensym)))
;;       `(let ((,s ,state))
;;          (let ((result (binding ((*match-start* ,s)
;;                                  (*current-rule-name* ',symbol))
;;                          (apply-rule ',symbol ,s identity))))
;;            (if (failure? result) result (,next result))))))

;; (define (--base state symbol next)
;;   (let ((result (binding ((*match-start* state)
;;                           (*current-rule-name* symbol))
;;                   (apply-rule *current-rule-name* *match-start* identity))))
;;     (if (failure? result) result (next result))))

;; (define (--base state symbol next)
;;   (let ((result (apply-rule symbol state identity)))
;;     (if (failure? result) result (next result))))

;; (define (--base state symbol next)
;;   (apply-rule symbol state next))

(define-compile (%base state symbol next)
    (if (eq symbol 'any)
        (if (or (eq next identity) (eq next 'identity))
            `(--any ,state)
            `(--any-2 ,state ,next))
        `(apply-rule ',symbol ,state ,next)))

;; (define-apply (%base state symbol next)
;;     (let* ((rule (get-rule symbol))
;;            (res  (rule state)))
;;       (if (failure? res) res (next res))))

(define-apply (%base state symbol next) (failcall next ((get-rule symbol) state)))

(define (--exactly state item next)
  (let* ((s (--any state)))
    (if (failure? s) fail
        (if (eq (state-result s) item) (next s) fail))))

(define (--exactly-char s item next)
  (if (eq (stream-peek (state-stream s)) item) (next (--any s)) fail))

(define-compile (%exactly state item next)
    (if (char? item)
        `(--exactly-char ,state ,item ,next)
        `(--exactly ,state ,item ,next)))

(define-compile (%string state item next)
    (let ((chars (string-to-charlist item)))
      (compile-rule (cons 'seq chars)
                    state next)))

(define-compile (extern state args next)
    (let ((meta (first args))
          (rule (second args)))
      `(let ((_result_ '()))
         (binding ((*meta-context* (list ',meta)))
           (set! _result_ ,(compile-rule rule state 'identity)))
         (if (failure? _result_) fail (,next _result_)))))

(define-compile (? state args next)
    (let* ((comp (compile-rule (car args) '_state_ 'identity)))
      `(let* ((_state_ ,state)
              (result ,comp)
              (next   ,next))
         (if (failure? result) (next (state+result _state_ '())) (next result)))))

(define-compile (* state args next)
    `(let loop ((last-state (state+result ,state '())))
          (let ((r ,(compile-rule (car args) 'last-state 'identity)))
            (if (failure? r) (,next (state-reverse-result last-state))
                (loop (state-cons r last-state))))))

(define-compile (+ state args next)
    `(let loop ((last-state ,state)
                (first-run #t)
                (results '()))
          (let ((r ,(compile-rule (car args) 'last-state 'identity)))
            (if (failure? r)
                (if first-run fail
                    (,next (state+result last-state (reverse-list results))))
                (loop r #f (cons (state-result r) results))))))


(define-compile (or state rules next)
    (if (eq 1 (list-length rules))
        (compile-rule (first rules) state next)
        (let ((body
               (reduce-list
                (lambda (acc rule)
                  `(let ()
                     (set! next-state ,(compile-rule rule 'state 'identity))
                     (if (failure? next-state)
                         ,acc
                         (next next-state))))
                'fail
                (reverse-list rules))))
          `(let ((state ,state)
                 (next  ,next)
                 (next-state #f))
             ,body))))

(define-compile (seq state rules next)
    (if (eq 1 (list-length rules))
        (compile-rule (first rules) state next)
        (let* ((rev (reverse-list rules))
               (final (compile-rule (car rev) 'state next)))
          `(let ((state ,state))
             ,(reduce-list (lambda (nxt rule)
                             `(let ()
                                (set! state ,(compile-rule rule 'state 'identity))
                                (if (failure? state) fail
                                    ,nxt)))
                           final
                           (cdr rev))))))

;; this is sort of a cheesy form of let, but it works for now
(define-compile (let state forms next)
    `(let ,(car forms)
       ,(compile-rule (cons 'seq (cdr forms))
                      state next)))

(define-compile (set! state forms next)
    (let ((var (car forms))
          (rule (car (cdr forms))))
      `(let ((_r ,(compile-rule rule state 'identity)))
         (if (failure? _r) fail
             (let ()
               (set! ,var (state-result _r))
               (,next _r))))))


(define-compile (where state forms next)
    (let ((test (car forms)))
      `(if ,test (,next ,state)
           fail)))

(define-compile (not state forms next)
    (let ((rule (car forms))
          (_state (gensym)))
      `(let* ((,_state ,state))
         (if (failure? ,(compile-rule rule _state 'identity)) (,next ,_state)
             fail))))

(define-compile (return state forms next)
    (let ((form (car forms))
          (s (gensym)))
      `(let ((,s ,state))
         (,next (binding ((*match-end* ,s)) (state+result ,s ,form))))))

;;;;  base rules ----------------------------

(defmacro define-base-rule (name & body)
  `(binding ((*meta-context* (list 'Base)))
     (define-rule ,name ,@body)))

(define-base-rule space
  (set! x any)
  (where (whitespace-char? x))
  (return x)) 

(define-base-rule constituent
    (not (or space #\( #\) #\[ #\] #\{ #\})) any)

(define-base-rule non-space
    (not space) any)

(define-base-rule ws (* space))

(define-base-rule alpha
  (set! x any)
  (where (or (char-between x #\a #\z)
             (char-between x #\A #\Z)))
  (return x))

(define-base-rule digit
  (set! x any)
  (where (char-between x #\0 #\9))
  (return (-i (char-code x) (char-code #\0))))

(define-base-rule ident
  (set! x (+ alpha))
  (return (implode x)))

'done
