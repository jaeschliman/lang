;; it is a list so we can push/pop on it, but only the first element
;; is used to look things up.
(at-boot (defparameter *meta-context* (list 'Base)))
(at-boot (defparameter *match-start*))
(at-boot (defparameter *match-end*))
(at-boot (defparameter *current-rule-name*))
(at-boot (define %meta-package (%make-package "meta")))
(at-boot (package-add-subpackage %root-package %meta-package "meta"))
(package-import-symbol %meta-package 'any)
(package-import-symbol %meta-package 'nothing)

(defmacro trace (form)
  `(let ((_res ,form))
     (print (list ',form '= _res))
     _res))

(at-boot (define MetaInputStream (create-class 'MetaInputStream '(pos str line memo next head-rule))))

(define (make-meta-input-stream pos str line)
  (let ((r (instantiate-class MetaInputStream)))
    (instance-set-ivar r 0 pos)
    (instance-set-ivar r 1 str)
    (instance-set-ivar r 2 line)
    (instance-set-ivar r 3 '())
    (instance-set-ivar r 4 '#f)
    (instance-set-ivar r 5 '())
    r))

(define (%meta-string-stream-read pos table)
  (char-at (aget table 4) pos))
(define (%meta-string-stream-advance char pos table)
  (+i pos (char-width char)))
(define (%meta-string-stream-at-end? pos table)
  (eq pos (string-byte-length (aget table 4))))
(define (%meta-string-stream-initial-position table)
  0)

(define (make-meta-string-input string)
  (vector %meta-string-stream-read
          %meta-string-stream-advance
          %meta-string-stream-at-end?
          %meta-string-stream-initial-position
          string))

(defmacro meta-stream-pos  (it) `(instance-get-ivar ,it 0))
(defmacro meta-stream-input  (it) `(instance-get-ivar ,it 1))
(defmacro meta-stream-line (it) `(instance-get-ivar ,it 2))
(defmacro meta-stream-memo (it) `(instance-get-ivar ,it 3))
(defmacro meta-stream-next (it) `(instance-get-ivar ,it 4))
(defmacro meta-stream-head-rule (it) `(instance-get-ivar ,it 5))
(define (set-meta-stream-head-rule stream head) (instance-set-ivar stream 5 head))

(at-boot (define fail '(fail fail fail)))
(defmacro failure? (state) `(,eq ,state ',fail))

(define (make-initial-state stream) (list stream '()))
(define state-stream car)
(define state-result cadr)

(define (make-stream string)
  (let* ((input (make-meta-string-input string))
         (initial-position ((aget input 3) input)))
    (make-meta-input-stream
     initial-position
     input
     (cons 0 0))))

(define (stream-line-position s) (car (meta-stream-line s)))
(define (stream-col-position s)  (cdr (meta-stream-line s)))

(define (stream-read-pos s pos)
  (let ((tbl (meta-stream-input s)))
    ((aget tbl 0) pos tbl)))

(define (stream-read s)
  (stream-read-pos s (meta-stream-pos s)))

(define (stream-end? s)
  (let ((tbl (meta-stream-input s)))
    ((aget tbl 2) (meta-stream-pos s) tbl)))

(define (stream-next-position s char)
  (let ((tbl (meta-stream-input s)))
    ((aget tbl 1) char (meta-stream-pos s) tbl)))

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
                                (stream-next-position s char)
                                (meta-stream-input s)
                                pos)))))


(define (stream-peek s)
  (if (stream-end? s) '()
      (stream-read-pos s (meta-stream-pos s))))

(define (%vec-grow v)
  (let* ((len (array-length v))
         (nl (*i 2 len))
         (r  (make-array nl)))
    (let loop ((i 0))
         (unless (eq i len)
           (aset r i (aget v i))
           (loop (+i 1 i))))
    r))

(define (%vec-lookup v k)
  (let ((len (array-length v)))
    (let loop ((i 0))
         (unless (or (eq i len) (eq (aget v i) 0))
           (if (eq (aget v i) k) (aget v (+i 1 i))
               (loop (+i 2 i)))))))

(define (%vec-full? v)
  (not (eq 0 (aget v (+i -2 (array-length v))))))

(define (%vec-store v k val)
  (let ((r v))
    (when (%vec-full? v) (set! r (%vec-grow v)))
    (let loop ((i 0))
         (let ((found (aget r i)))
           (if (eq found k)
               (aset r (+i 1 i) val)
               (if (eq found 0)
                   (let ()
                     (aset r i k)
                     (aset r (+i 1 i) val))
                   (loop (+i 2 i))))))
    r))

;; (define (stream-at s key)
;;   (let ((v (meta-stream-memo s)))
;;     (if-nil? v '() (%vec-lookup v key))))

;; (define (stream-at-put s key val)
;;   (unless (eq key 'any)
;;     (if-nil? (meta-stream-memo s) (instance-set-ivar s 3 (make-array 32)))
;;     (instance-set-ivar s 3 (%vec-store (instance-get-ivar s 3)
;;                                    key val))
;;     val))


(define (stream-at s key)
  (let ((ht (meta-stream-memo s)))
    (if-nil? ht '() (ht-at ht key))))

(define (stream-at-put s key val)
  (unless (eq key 'any)
    (if-nil? (meta-stream-memo s) (instance-set-ivar s 3 (make-ht)))
    (ht-at-put (meta-stream-memo s) key val)))

;; (define (stream-contains? s key)
;;    (let ((ht (meta-stream-memo s)))
;;     (if-nil? ht #f (ht-contains? ht key))))

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
(forward get-super-rule)

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

;; (define (apply-rule rule state next)
;;   (let ((fn (get-rule rule))
;;         (stream (state-stream state)))
;;     (unless (stream-contains? stream rule)
;;       (stream-at-put stream rule (fn state)))
;;     (stream-at stream rule)))

(define (failcall next result) (if (failure? result) result (next result)))

;; (define (apply-rule rule state next) (failcall next ((get-rule rule) state)))

;; (define (apply-rule rule state next)
;;   (let ((r ((get-rule rule) state)))
;;     (if (failure? r) r (next r))))

(defmacro defdata (maker classname slots)
  (unless (symbol-bound? classname) (set classname #f))
  `(let ()
     (at-boot (define ,classname (create-class ',classname ',slots)))
     (define (,maker ,@slots)
       (let ((_r (instantiate-class ,classname)))
         ,@(mapcar (lambda (slot) `(iset _r ',slot ,slot)) slots)
         _r))))

(at-boot (defparameter *LR-stack* '()))

(forward list-member?)
(define (list-member? item lst)
  (if-nil? lst #f
           (or (eq (car lst) item)
               (list-member? item (cdr lst)))))

(defdata %make-failer Failer (used))
(define (make-failer) (%make-failer #f))
(define (is-Failer? x) (isa? x Failer))

(defdata make-memo Memo (ans))


(defdata make-LR LR (seed rule head next))
(defdata make-Head Head (rule involvedSet evalSet))

(define (state-leq? a b)
  (let ((pa (meta-stream-pos (state-stream a)))
        (pb (meta-stream-pos (state-stream b))))
    (or (eq pa pb) (<i pa pb))))

(define (Grow-LR rule state memo head)
  (let ((stream (state-stream state))
        (app (get-rule rule)))
    ;; Line A
    (set-meta-stream-head-rule stream head)
    (let loop ()
         ;; Pos <- P
         (iset head 'evalSet (iget head 'involvedSet))
         (let ((ans (app state)))
           (unless (or (failure? ans)
                       (state-leq? ans (iget memo 'ans)))
             (iset memo 'ans ans)
             (loop))))
    ;; Line C
    (set-meta-stream-head-rule stream '())
    ;; Pos <- M.Pos ?
    (iget memo 'ans)))

(defmacro while (condition & body)
  `(let loop ()
        (when ,condition
          ,@body
          (loop))))

(define (neq a b) (not (eq a b)))

(define (Setup-LR rule L)
  (when (nil? (iget L 'head))
    (iset L 'head (make-Head rule '() '())))
  (let ((s *LR-stack*))
    (while (neq (iget s 'head) (iget L 'head))
      (let ((newhead (iget L 'head)))
        (iset s 'head newhead)
        (iset newhead 'involvedSet
              (cons (iget s 'rule)
                    (iget newhead 'involvedSet)))
        (set! s (iget s 'next))))))

(define (list-set-remove set item)
  (let ((r '()))
    (dolist (i set) (unless (eq i item) (set! r (cons i r))))
    r))

(define (Recall rule state)
  (let* ((stream (state-stream state))
         (m (stream-at stream rule))
         (h (meta-stream-head-rule stream)))
    (cond
      ((nil? h) m)
      ((and (nil? m)
            (not (or (eq rule (iget h 'head))
                     (list-member? rule (iget h 'involvedSet)))))
       (make-memo fail))
      ((list-member? rule (iget h 'evalSet))
       (iset h 'evalSet (list-set-remove (iget h 'evalSet) rule))
       (iset m 'ans ((get-rule rule) state))
       m)
      (#t m))))

(define (LR-Answer rule state memo)
  (let* ((ans (iget memo 'ans))
         (head (iget ans 'head))
         (seed (iget ans 'seed)))
    (cond
      ((not (eq (iget head 'rule) rule))
       seed)
      (#t
       (iset memo 'ans seed)
       (if (failure? ans) fail
           (Grow-LR rule state memo head))))))

(define (Apply-Rule rule state)
  (let ((m (Recall rule state))
        (stream (state-stream state)))
    (cond ((nil? m)
           (let ((lr (make-LR fail rule '() *LR-stack*)))
             (set! m (make-memo lr))
             (stream-at-put stream rule m)
             (let* ((next-state (binding ((*LR-stack* lr))
                                  ((get-rule rule) state))))
               (cond
                 ((nil? (iget lr 'head))
                  (iset m 'ans next-state)
                  next-state)
                 (#t
                  (iset lr 'seed next-state)
                  (LR-Answer rule state m))))))
          ((isa? (iget m 'ans) LR)
           (Setup-LR rule (iget m 'ans))
           (iget (iget m 'ans) 'seed))
          (#t (iget m 'ans)))))

(define (apply-rule rule state next)
  (let ((result (Apply-Rule rule state)))
    (if (failure? result) fail (next result))))

(define (x-apply-rule rule state next)
  (let* ((stream (state-stream state))
         (memo (stream-at stream rule)))
    (let ((result
           (cond
             ((is-Failer? memo)
              (iset memo 'used #t)
              fail)
             ((nil? memo)
              (let ((app  (get-rule rule))
                    (orig state)
                    (failer (make-failer)))
                (stream-at-put stream rule failer)
                (let ((res (app orig)))
                  (cond ((failure? res) res)
                        (#t
                         (set! memo (make-memo res))
                         (stream-at-put stream rule memo)
                         (cond ((iget failer 'used)
                                (let ((sentinel (state-stream res)))
                                  (let loop ()
                                       (let ((ans (app orig)))
                                         (cond ((failure? ans)
                                                (iget memo 'ans))
                                               ((eq (state-stream ans) sentinel)
                                                (iget memo 'ans))
                                               (#t
                                                (iset memo 'ans ans)
                                                (loop)))))))
                               (#t (iget memo 'ans))))))))
             (#t (iget memo 'ans)))))
      (if (failure? result) result (next result)))))

(define (super-apply-rule here rule state next)
  (let ((r ((get-super-rule here rule) state)))
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
(define (xf-join-forms a b)
  (let ((fa (xf-form a))
        (fb (xf-form b)))
    (cond ((nil? fa) fb)
          ((nil? fb) fa)
          (#t (append fa fb)))))

(define (xf+ a b)
  (cons (append (xf-vars a) (xf-vars b))
        (xf-join-forms a b)))

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
        (super  (xf-ignore r))
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

(define (get-super-rule here rulename)
  (let* ((top (meta-super (find-meta here))))
    (let lookup ((meta top))
         (if (nil? meta) '()
             (let ((Meta (find-meta meta)))
               (let ((exist (ht-at (second Meta) rulename)))
                 (if (nil? exist) (lookup (meta-super Meta))
                     exist)))))))

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

(define-compile (super state args next)
    (let ((rule (first args)))
      `(super-apply-rule (first *meta-context*) ',rule ,state ,next)))

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
     (package-import-symbol %meta-package ',name)
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
