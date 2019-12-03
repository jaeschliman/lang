(ht-at-put meta-by-name 'Lisp (make-meta 'Base))
(push-meta-context 'Lisp)

(define-rule eol (or #\Return #\Newline nothing))
(define-rule non-eol (not eol) any)

(define-rule comment
  #\; (* non-eol) eol)

(define-rule ws
  (* (or space comment)))

(define-rule character
  #\# #\\ (set! -name (+ constituent))
  (where (character-name? (charlist-to-string -name)))
  (return (char-by-name (charlist-to-string -name))))

(define-rule non-quote (not (or #\" #\\)) any)

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

(define-rule symbol-char
  (set! x any) (where (symbol-char x)) (return x))

(define-rule symbol
  (set! x symbol-char)
  (where (not (char-between x #\0 #\9)))
  (set! xs (* symbol-char))
  (return (implode (cons x xs))))

(define (digits-to-integer xs)  (reduce-list (lambda (acc n) (+ n (* 10 acc))) 0 xs))

(define (digits-to-float xs ys)
  (+f (i->f (reduce-list
             (lambda (acc n) (+i n (*i 10 acc)))
             0 xs))
      (reduce-list
       (lambda (acc n) (*f 0.1 (+f (i->f n) acc)))
       0.0 (reverse-list ys))) )

(define (hex-char-value ch)
  (if (char-between ch #\a #\f)
      (+i 10 (-i (char-code ch) (char-code #\a)))
      (-i (char-code ch) (char-code #\0))))

(define (hex-chars-to-integer xs)
  (reduce-list (lambda (acc hx) (+i (hex-char-value hx) (*i 16 acc))) 0 xs))

(define-rule integer
  (set! sign? (? #\-))  (set! x (+ digit))
  (return
    (let ((n (digits-to-integer x))
          (s (if (nil? sign?) 1 -1)))
      (*i s n))))

(define-rule float
  (set! sign? (? #\-)) (set! x (+ digit)) #\. (set! y (+ digit))
  (return
    (let ((n (digits-to-float x y))
          (s (if (nil? sign?) 1.0 -1.0)))
      (*f s n))))

(define-rule point
  (set! x integer) #\@ (set! y integer)
  (return (make-point x y)))

(define-rule hex-char
  (set! x any) (where (or (char-between x #\0 #\9)
                          (char-between x #\a #\f)))
  (return x))

(define-rule hex-integer
  "0x" (set! x (+ hex-char))
  (return (hex-chars-to-integer x)))

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

'done
