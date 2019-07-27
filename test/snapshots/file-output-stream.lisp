(define str " abcde😍😍😍fghijklmno😍")

(#/lang/%file-output-stream-write-string *standard-output* str)
(#/lang/%file-output-stream-write-string *standard-output* str)
(#/lang/%file-output-stream-write-string *standard-output* str)
(#/lang/%file-output-stream-write-char *standard-output* #\Newline)
(dolist (ch (reverse-list (string->list str)))
  (#/lang/%file-output-stream-write-char *standard-output* ch))
(#/lang/%file-output-stream-write-char *standard-output* #\Newline)
