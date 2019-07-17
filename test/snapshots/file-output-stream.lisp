(define str " abcde😍😍😍fghijklmno😍")

(%file-output-stream-write-string *standard-output* str)
(%file-output-stream-write-string *standard-output* str)
(%file-output-stream-write-string *standard-output* str)
(%file-output-stream-write-char *standard-output* #\Newline)
(dolist (ch (reverse-list (string->list str)))
  (%file-output-stream-write-char *standard-output* ch))
(%file-output-stream-write-char *standard-output* #\Newline)
