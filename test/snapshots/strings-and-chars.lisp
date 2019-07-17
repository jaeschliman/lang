(define str " abcde😍😍😍fghijklmno😍")

(print str)

(print (char-code-at str 0))
(print (char-at str 0))
(print (char-at str 1))
(print (char-at str 2))
(print (char-at str 3))
(print (char-at str 4))
(print (char-at str 5))
(print (char-at str 6))

(string-do-chars (ch str)
 (print ch))


(print (string->char-array str))
(print (char-array->string (string->char-array str)))
