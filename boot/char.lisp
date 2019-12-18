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

(define (character-name? str) (not (nil? (char-by-name str))))

(when #f
  (define (symbol-char x) ;; TODO: convert to lookup table
    (or (char-between x #\a #\z)
        (char-between x #\* #\-)
        (char-between x #\< #\Z)
        (char-between x #\/ #\9)
        (char-between x #\# #\&)
        (eq x #\!) (eq x #\^) (eq x #\_)
        (eq x #\|) (eq x #\~)
        (eq x #\:))))

(define (digit-char? ch)
    (char-between ch #\0 #\9))

(define (char-to-digit ch)
    (-i (char-code ch) (char-code #\0)))

'done
