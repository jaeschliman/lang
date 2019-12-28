;; xvec as a writeable stream
(define %xvec-write-char xvec-push)
(define (%xvec-write-string v str) (string-do-chars (ch str) (xvec-push v ch)))
(generic-function-add-method stream-write-char (list XVec #t) %xvec-write-char)
(generic-function-add-method stream-write-string (list XVec #t) %xvec-write-string)

(define (%array-copy-forward a from-idx to-idx count)
  (let ((max (-i count 1)))
    (let loop ((i (+i from-idx max)) (j (+i to-idx max)) (rem count))
       (unless (eq 0 rem)
         (aset a j (aget a i))
         (loop (+i -1 i) (+i -1 j) (-i rem 1))))))

(define (%array-copy-backward a from-idx to-idx count)
  (let loop ((i from-idx) (j to-idx) (rem count))
     (unless (eq 0 rem)
       (aset a j (aget a i))
       (loop (+i 1 i) (+i 1 j) (-i rem 1)))))

(define (%array-copy-from-to a b from-idx to-idx count)
   (let loop ((i from-idx) (j to-idx) (rem count))
     (unless (eq 0 rem)
       (aset b j (aget a i))
       (loop (+i 1 i) (+i 1 j) (-i rem 1)))))

(define (array-copy-elements a b from-idx to-idx count)
  (if (eq a b)
      (if (<i from-idx to-idx) (%array-copy-forward a from-idx to-idx count)
          (%array-copy-backward a from-idx to-idx count))
      (%array-copy-from-to a b from-idx to-idx count)))

;; (let ((a (vector 0 0 1 2 3 4 0 0)))
;;   (array-copy-elements a a 3 2 3)
;;   (print a)
;;   (print `(expecting 2 3 4 4)))

;; (let ((a (vector 0 0 1 2 3 4 0 0)))
;;   (array-copy-elements a a 2 3 3)
;;   (print a)
;;   (print `(expecting  1 1 2 3)))

(define (xvec-to-array v)
  (let ((r (make-array (iget v 'count))))
    (each-with-index (it i v) (aset r i it))
    r))

(define xursor-bucket point-x)
(define xursor-item point-y)

(define (xursor-for-index v i)
  (let* ((buckets (iget v 'buckets)))
    (let loop ((bidx 0) (iidx i))
         (let ((used (aget (aget buckets bidx) 0)))
           (if (or (>i iidx used) (eq iidx used)) (loop (+i bidx 1) (-i iidx used))
               (make-point bidx (+i 1 iidx)))))))

(define (xvec-at-xursor v p)
  (aget (aget (iget v 'buckets) (xursor-bucket p)) (xursor-item p)))

(define (xvec-at v i)
  (xvec-at-xursor v (xursor-for-index v i)))

(define (%bucket-remove v xi count)
  (let* ((bucket (aget (iget v 'buckets) (xursor-bucket xi)))
         (end-idx (+i (xursor-item xi) count))
         (bucket-cnt (aget bucket 0))
         (bucket-rem (-i bucket-cnt (-i end-idx 1))))
    (array-copy-elements bucket bucket
                         end-idx (xursor-item xi) bucket-rem)
    (aset bucket 0 (-i bucket-cnt count))))

(define (xvec-delete-range v idx count)
  (let ((xa (xursor-for-index v idx))
        (xb (xursor-for-index v (+i idx count))))
    (if (eq (xursor-bucket xa) (xursor-bucket xb))
        (%bucket-remove v xa count)
        (print '(TODO)))))

;; (let* ((v (make-xvec)))
;;   (dotimes (i 200) (xvec-push v i))
;;   (dotimes (i 200) (print `(,i = ,(xvec-at v i)))))

(let ((v (make-xvec)))
  (stream-write-string v "hello, xxworld!")
  (xvec-delete-range v 7 2)
  (print (char-array->string (xvec-to-array v))))
