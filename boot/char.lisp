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

(define (digit-char? ch)
    (char-between ch #\0 #\9))

(define (char-to-digit ch)
    (-i (char-code ch) (char-code #\0)))

'done
