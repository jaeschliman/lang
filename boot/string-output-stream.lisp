(at-boot (define StringOutputStream (create-class 'StringOutputStream '(list byte-count))))

(define (make-string-output-stream)
    (let ((result (instantiate-class StringOutputStream)))
      (instance-set-ivar result 0 '())
      (instance-set-ivar result 1 0)
      result))

(define (%string-output-stream-increment-byte-count s n)
    (instance-set-ivar s 1 (+i n (instance-get-ivar s 1))))

(define (%string-output-stream-push s thing)
    (instance-set-ivar s 0 (cons thing (instance-get-ivar s 0))))

(define (%string-output-stream-write-char s ch)
    (%string-output-stream-push s ch)
  (%string-output-stream-increment-byte-count s 1))

(define (%string-output-stream-write-string s string)
    (%string-output-stream-push s string)
  (%string-output-stream-increment-byte-count s (string-char-count string)))

(define (%array-write-char arr ch idx)
    (aset arr idx ch)
    (+i 1 idx))

(define (%array-write-string arr str idx)
    (let ((i idx))
      (string-do-chars (ch str)
        (aset arr i ch)
        (set! i (+i 1 i)))
      i))

(define (string-output-stream-get-string s)
    (let ((arr (make-array (instance-get-ivar s 1)))
          (idx 0))
      (dolist (it (reverse-list (instance-get-ivar s 0)))
        (set! idx (if (char? it) (%array-write-char arr it idx)
                      (%array-write-string arr it idx))))
      (char-array->string arr)))

(defmacro with-output-to-string (binding & body)
  (let ((var (car binding)))
    `(let ((,var (make-string-output-stream)))
       ,@body
       (string-output-stream-get-string ,var))))


(at-boot (define stream-write-char (make-generic-function 2)))

(generic-function-add-method stream-write-char (list StringOutputStream #t)
                             %string-output-stream-write-char)
(generic-function-add-method stream-write-char (list FileOutputStream #t)
                             %file-output-stream-write-char)

(at-boot (define stream-write-string (make-generic-function 2)))

(generic-function-add-method stream-write-string (list StringOutputStream #t)
                             %string-output-stream-write-string)
(generic-function-add-method stream-write-string (list FileOutputStream #t)
                             %file-output-stream-write-string)
