(define XVec (create-class 'XVec '(buckets count bucket-max)))

(define bucket-size 128)

(define (grow-array a)
  (let* ((len (array-length a))
         (res (make-array (*i 2 len))))
    (let loop ((idx 0))
         (unless (eq idx len)
           (aset res idx (aget a idx))
           (loop (+i idx 1))))
    res))

(define (alloc-bucket)
  (let ((a (make-array bucket-size)))
    (aset a 0 0)
    a))

(define (make-xvec)
  (let ((r (instantiate-class XVec))
         (buckets (make-array 4)))
    (aset buckets 0 (alloc-bucket))
    (iset r 'buckets buckets)
    (iset r 'count 0)
    (iset r 'bucket-max 0)
    r))

(forward xvec-add-bucket)
(define (xvec-add-bucket v)
  (cond
    ((eq (iget v 'bucket-max) (-i (array-length (iget v 'buckets)) 1))
     (iset v 'buckets (grow-array (iget v 'buckets)))
     (xvec-add-bucket v))
    (#t (iset v 'bucket-max (+i 1 (iget v 'bucket-max)))
        (aset (iget v 'buckets) (iget v 'bucket-max) (alloc-bucket)))))

(forward xvec-push)
(define (xvec-push v item)
  (let* ((bucket (aget (iget v 'buckets) (iget v 'bucket-max)))
         (used (aget bucket 0)))
    (cond ((eq used 127)
           (xvec-add-bucket v)
           (xvec-push v item))
          (#t
           (set! used (+i used 1))
           (aset bucket used item)
           (aset bucket 0 used)
           (iset v 'count (+i 1 (iget v 'count)))))))

(define (xvec-iter v fn)
  (let ((buckets (iget v 'buckets))
        (bucket-max (iget v 'bucket-max)))
    (let outer ((bi 0))
         (unless (>i bi bucket-max)
           (let* ((bucket (aget buckets bi))
                  (max (aget bucket 0)))
             (let inner ((ai 1))
                  (unless (>i ai max)
                    (fn (aget bucket ai))
                    (inner (+i 1 ai)))))
           (outer (+i 1 bi))))))

(define (xvec-push-new v item)
  (let ((buckets (iget v 'buckets))
        (bucket-max (iget v 'bucket-max))
        (stop #f)
        (idx -1))
    (let outer ((bi 0))
         (unless (or stop (>i bi bucket-max))
           (let* ((bucket (aget buckets bi))
                  (max (aget bucket 0)))
             (let inner ((ai 1))
                  (unless (>i ai max)
                    (set! idx (+i idx 1))
                    (if (eq (aget bucket ai) item)
                        (set! stop #t)
                        (inner (+i 1 ai))))))
           (outer (+i 1 bi))))
    (if stop idx
        (-i (xvec-push v item) 1))))

(define (xvec-count v) (iget v 'count))

;; (let ((x (make-xvec)))
;;   (print (xvec-count x))
;;   (xvec-push x 'hello)
;;   (print (xvec-count x))
;;   (xvec-iter x (lambda (x) (print `(saw: ,x)))))

;; (let ((x (make-xvec)))
;;   (dotimes (n 100)
;;     (xvec-push x n))
;;   (print (xvec-count x))
;;   (xvec-iter x (lambda (x) (print `(saw: ,x)))))
