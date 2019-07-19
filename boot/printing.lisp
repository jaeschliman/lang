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

(define (absi n) (if (<i n 0) (*i -1 n) n))
(define (absf n) (if (<f n 0.0) (*f -1.0 n) n))

(defparameter *print-base* 10)

(define (%print-integer int stream base)
  (when (<i int 0) (stream-write-char stream #\-))
  (let ((n (absi int)))
    (dotimes (i (integer-digit-length n base))
      (stream-write-char stream (digit-to-character (integer-nth-digit n i base))))))

(define (print-integer n &opt (stream *standard-output*))
    (%print-integer n stream *print-base*))

(define (newline &opt (stream *standard-output*))
    (stream-write-char stream #\Newline))

(define (print-float fl &opt (stream *standard-output*))
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

(define print-object (make-generic-function 2))

(define (%print-object-address object stream)
    (let ((hi (%obj-high-bits object))
          (lo (%obj-low-bits object)))
      (binding ((*print-base* 16))
               (dotimes (_ (-i 8 (integer-digit-length hi 16))) (stream-write-char stream #\0))
               (print-integer hi stream)
               (dotimes (_ (-i 8 (integer-digit-length lo 16))) (stream-write-char stream #\0))
               (print-integer lo stream))) )

(generic-function-add-method
 print-object
 (list #t #t)
 (lambda (object stream)
   (stream-write-string stream "#<")
   (stream-write-string stream (symbol-name (class-name (class-of object))))
   (stream-write-char stream #\Space)
   (%print-object-address object stream)
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

(define (print-point p &opt (stream *standard-output*))
    (print-integer (point-x p) stream)
  (stream-write-char stream #\@)
  (print-integer (point-y p)))

(generic-function-add-method
 print-object (list Point #t) print-point)

;; TODO: decide on syntax for global package references, subpackages etc.
(define (%write-relative-package-name package relative-to stream)
    (unless (eq package relative-to)
      (stream-write-string stream (package-name package))
      (stream-write-char stream #\:)))

;; TODO: syntax for escaped symbols
(define (print-symbol it &opt (stream *standard-output*))
    (%write-relative-package-name (symbol-package it) *package* stream)
  (stream-write-string stream (symbol-name it)))

(generic-function-add-method
 print-object (list Symbol #t) print-symbol)

(generic-function-add-method
 print-object (list Null #t) (lambda (_ stream) (stream-write-string stream "()")))

(define (print-character ch &opt (stream *standard-output*))
    (stream-write-char stream #\#)
    (stream-write-char stream #\\)
  (stream-write-string stream (char-name ch)))

(generic-function-add-method
 print-object (list Character #t) print-character)

(define (print-class object &opt (stream *standard-output*))
  (stream-write-string stream "#<Class ")
  (stream-write-string stream (symbol-name (class-name object)))
  (stream-write-char stream #\Space)
  (%print-object-address object stream)
  (stream-write-char stream #\>))

(generic-function-add-method
 print-object (list Class #t) print-class)

(define (print-string str &opt (stream *standard-output*))
    (stream-write-char stream #\")
  ;; TODO: escape codes for unprintable characters
  (string-do-chars (ch str)
    (when (eq ch #\") (stream-write-char stream #\\))
    (stream-write-char stream ch))
    (stream-write-char stream #\"))

(generic-function-add-method
 print-object (list String #t) print-string)

(define (print-package object &opt (stream *standard-output*))
  (stream-write-string stream "#<Package ")
  (stream-write-string stream (package-name object))
  (stream-write-char stream #\Space)
  (%print-object-address object stream)
  (stream-write-char stream #\>))

(generic-function-add-method
 print-object (list Package #t) print-package)


'done
