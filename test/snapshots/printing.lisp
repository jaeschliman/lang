(print (#/lang/integer-digit-length 10 10))
(print (#/lang/integer-nth-digit 120 0 10))

(print-integer 311)
(newline)
(print-integer -200)
(newline)
(print-float 1.3)
(newline)
(print-float 1.0)
(newline)
(print-float 2.0)
(newline)
(print-float 3.0)
(newline)
(print-float 12345.12345)
(newline)
(print-float 1.020304)
(newline)
(print-float -3.14)
(newline)

(binding ((*print-base* 2))
         (print-integer 0)
         (newline)
         (print-integer 1)
         (newline)
         (print-integer 2)
         (newline)
         (print-integer 3)
         (newline))

(binding ((*print-base* 16))
         (print-integer 255)
         (newline)
         (print-integer 128)
         (newline)
         (print-integer 64)
         (newline)
         (print-integer 16)
         (newline))

(print (with-output-to-string (s)
         (binding ((*standard-output* s))
                  (binding ((*print-base* 16))
                           (print-integer 255)
                           (newline)
                           (print-integer 128)
                           (newline)
                           (print-integer 64)
                           (newline)
                           (print-integer 16)
                           (newline))
                  (print-float 1.3)
                  (newline)
                  (print-float 1.0)
                  (newline)
                  (print-float 2.0)
                  (newline)
                  (print-float 3.0)
                  (newline)
                  (print-float 12345.12345)
                  (newline)
                  (print-float 1.020304)
                  (newline))))


(define (show x &opt (stream *standard-output*))
    (print-object x stream)
  (newline stream))

(show #\A)
(show 'x)
(show 10@-10)

(with-output-to-string (s)
  (show *package* s))
(with-output-to-string (s)
  (show (current-thread) s))

;; unexported (should print prefix)
(binding ((*package* (make-user-package "anon")))
  (show 'x))

(define root-package *package*)

;; exported (should not print prefix)
(binding ((*package* (make-user-package "anon")))
  (show 'define))

(show '#/lang/define)
(show '#/foo)
(define subpackage (make-user-package "anon"))
(package-add-subpackage *package* subpackage "sub")

(show 'sub/hello)

(show :ok)

(show "hello world")
