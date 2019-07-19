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

(define print-object (make-generic-function 2))

(generic-function-add-method
 print-object
 (list #t #t)
 (lambda (object stream)
   (stream-write-string stream "#<")
   (stream-write-string stream (symbol-name (class-name (class-of object))))
   (stream-write-char stream #\Space)
   (let ((hi (%obj-high-bits object))
         (lo (%obj-low-bits object)))
     (binding ((*print-base* 16))
        (dotimes (_ (-i 4 (integer-digit-length hi 16))) (stream-write-char stream #\0))
        (print-integer hi stream)
        (dotimes (_ (-i 4 (integer-digit-length lo 16))) (stream-write-char stream #\0))
        (print-integer lo stream)))
   (stream-write-char stream #\>)))

(generic-function-add-method
 print-object (list Fixnum #t) print-integer)

(generic-function-add-method
 print-object (list Float #t) print-float)

(define %print-list-loop #f)
(define (%print-list-loop first cons stream)
    (unless first (stream-write-char stream #\Space))
    (if (or (nil? (cdr cons)) (pair? (cdr cons)))
        (let ()
          (print-object (car cons) stream)
          (unless (nil? (cdr cons))
            (%print-list-loop #f (cdr cons) stream)))
        (let ()
          (print-object (car cons) stream)
          (stream-write-string stream " . ")
          (print-object (cdr cons) stream))))

(define (print-list lst &opt (stream *standard-output*))
    (stream-write-char stream #\()
  (%print-list-loop #t lst stream)
  (stream-write-char stream #\)))

(generic-function-add-method
 print-object (list Cons #t) print-list)

'done
