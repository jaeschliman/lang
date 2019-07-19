;; change the first `when` to an `if` and it goes away

(define (print-fl fl stream)
    (when (<f fl 0.0) (stream-write-string stream "-"))
  (let ((f (absf fl)))
    (%print-integer (f->i f) stream 10)
    (stream-write-char stream #\.)
    (let* ((precision 5)
           (rem (f->i (*f (powf 10.0 (i->f precision)) (remf f))))
           (len (integer-digit-length rem 10))
           (leading-zeroes (-i precision len)))
      (dotimes (_ leading-zeroes) (stream-write-char stream #\0))
      (%print-integer rem stream 10))))

(print-fl -3.0 *standard-output*)

'done
