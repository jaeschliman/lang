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
    (#\[ #\[)
    (#\] #\])
    (#\- #\-)
    (#\" #\")))

(define-rule bracket-body-char
  (set! x any) (where (not (or (eq x #\]) (eq x #\\))))
  (return x))

(define-rule bracket-escaped-char
  #\\ (set! x any) (return (bracket-escaped-char-character x)))

(define-rule bracket-char
  (or bracket-body-char bracket-escaped-char))

(define (make-bracket-char-range a b)
  (let ((var (gensym)))
    `(seq (set! ,var any)
          (where (char-between ,var ,a ,b))
          (return ,var))))

(define-rule bracket-char-range
  (set! a bracket-char) #\- (set! b bracket-char)
  (return (make-bracket-char-range a b)))

(define-rule bracket-lit
  "[" (set! chs (+ (or bracket-char-range bracket-char))) "]"
  (return (cons 'or chs)))

(define-rule sym
  (not "->")
  (set! xs (+ (or alpha #\-))) (return (implode xs)))

(define-rule extern
  (set! ns sym) #\. (set! rule sym) (return `(extern ,ns ,rule)))

(define-rule grouped "(" (set! b rule-body) ")" (return b))

(define-rule meta-lit
  (or extern sym string-lit bracket-lit grouped))

(define-rule meta-lit-with-modifier
  (seq (set! -id meta-lit) (set! -mm meta-mod) (return (list -mm -id))))

(define-rule meta-atom
  (set! ?negated (? #\~))
  (set! item (or meta-lit-with-modifier meta-lit))
  (return (if (nil? ?negated) item `(not ,item))))

(define-rule meta-app
  (or (seq (set! -rule meta-atom) ws #\: (set! -as ident)
           (return `(set! ,-as ,-rule)))
      meta-atom
      meta-pred))

(define-rule meta-pred
  #\? ws (set! -it (extern Lisp expr)) (return `(where ,-it)))

(define-rule meta-result
  "->" ws (set! -it (extern Lisp expr)) (return `(return ,-it)))

(define-rule rule-app
  (* #\Space) (set! -app meta-app) (return -app))

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
  (set! -name sym) ws "=" (set! -body rule-body) ws
  (return (let ()
            ;; (print "----------------------------------------")
            ;; (print `(matched ,*current-rule-name* ,-name at ,(state-col-row *match-start*)
            ;;                  to ,(state-col-row *match-end*)))
            ;; (print (current-match-string))
            ;; (print "----------------------------------------")
            `(define-rule ,-name ,-body))))

(define (make-meta-definition name rules)
  `(let ()
     (ht-at-put meta-by-name ',name (make-meta 'Base))
     (binding ((*meta-context* (list ',name)))
       ,@rules)))

(define-rule meta-block
  ws "meta" ws (set! -n ident) ws #\{ ws
  (set! -rs (+ rule))
  ws #\} ws
  (return (make-meta-definition -n -rs)))

(define-rule meta-main
  (set! -it (or meta-block (extern Lisp expr)))
  (return -it))

(define (make-integer numbers)
  (reduce-list
   (lambda (acc n) (+i n (*i 10 acc)))
   0 numbers))


(pop-meta-context)

'done
