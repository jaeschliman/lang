(define (integer-digit-length n base)
    (let ((len (f->i (floorf (logf (i->f base) (i->f n))))))
      (+i len 1)))

(define (integer-nth-digit n digit base)
    (let* ((len (integer-digit-length n base))
           (offs (-i len digit))
           (div (f->i (powf (i->f base) (i->f (-i offs 1))))))
      (%i (/i n div) base)))

(define *digit-table* (string->char-array "0123456789abcdefghijklmnopqrstuvwxyz"))

(define (digit-to-character n)
    (aget *digit-table* n))

(defparameter *print-base* 10)

(define (%print-integer n stream base)
    (dotimes (i (integer-digit-length n base))
      (stream-write-char stream (digit-to-character (integer-nth-digit n i base)))))

(define (print-integer n &opt (stream *standard-output*))
    (%print-integer n stream *print-base*))

(define (newline &opt (stream *standard-output*))
    (stream-write-char stream #\Newline))

(define (print-float f &opt (stream *standard-output*))
    (%print-integer (f->i f) stream 10)
  (stream-write-char stream #\.)
  (let* ((precision 5)
         (rem (f->i (*f (powf 10.0 (i->f precision)) (remf f))))
         (len (integer-digit-length rem 10))
         (leading-zeroes (-i precision len)))
    (dotimes (_ leading-zeroes) (stream-write-char stream #\0))
    (%print-integer rem stream 10)))

'done
