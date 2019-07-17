(define str " abcde😍😍😍fghijklmno😍")

(stream-write-string *standard-output* str)
(stream-write-string *standard-output* str)
(stream-write-string *standard-output* str)
(stream-write-char *standard-output* #\Newline)
(stream-write-string *standard-output*
                     (with-output-to-string (s)
                       (stream-write-string s str)
                       (stream-write-char s #\Newline)
                       (stream-write-string s str)
                       (stream-write-char s #\Newline)))

(dolist (ch (reverse-list (string->list str)))
  (stream-write-char *standard-output* ch))
