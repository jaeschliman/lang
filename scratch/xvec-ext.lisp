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

(define (%cross-bucket-remove v xa xb count)
  (let* ((buckets (iget v 'buckets))
         (ba (aget buckets (xursor-bucket xa)))
         (bb (aget buckets (xursor-bucket xb)))
         (cnt count))
    (let ((taken (+i 1 (-i (aget ba 0) (xursor-item xa)))))
      (aset ba 0 (-i (xursor-item xa) 1))
      (set! cnt (-i cnt taken)))
    ;; TODO: handle ranges that span 3 or more buckets
    (let ((rem (-i (aget bb 0) cnt)))
      (array-copy-elements bb bb (+i cnt 1) 1 rem)
      (aset bb 0 rem))))

(define (xvec-delete-range v idx count)
  (let ((xa (xursor-for-index v idx))
        (xb (xursor-for-index v (+i idx (+i -1 count)))))
    (if (eq (xursor-bucket xa) (xursor-bucket xb))
        (%bucket-remove v xa count)
        (%cross-bucket-remove v xa xb count))
    (iset v 'count (-i (iget v 'count) count))))

(define (%insert-new-bucket-at v bidx)
  (xvec-add-bucket v)
  (let* ((bs (iget v 'buckets))
         (max (iget v 'bucket-max))
         (bnew (aget bs max)))
    (array-copy-elements bs bs bidx (+i bidx 1) (-i max bidx))
    (aset bs bidx bnew)))

(define (%get-bucket v idx) (aget (iget v 'buckets) idx))

(define (%bucket-insert-at-start-of-next v bidx item)
  (cond
    ((or (eq bidx (iget v 'bucket-max))
         (eq 127 (aget (%get-bucket v (+i 1 bidx)) 0)))
     (%insert-new-bucket-at v (+i 1 bidx))
     (let ((new-bucket (%get-bucket v (+i 1 bidx))))
       (aset new-bucket 1 item)
       (aset new-bucket 0 1)))
    (#t
     (let* ((next-bucket (%get-bucket v (+i 1 bidx)))
            (used (aget next-bucket 0))
            (cap (-i 127 used)))
       (array-copy-elements next-bucket next-bucket 1 2 used)
       (aset next-bucket 1 item)
       (aset next-bucket 0 (+i 1 used))))))

(define (%bucket-insert v xa item)
  (let* ((buckets (iget v 'buckets))
         (bidx (xursor-bucket xa))
         (b (aget buckets bidx))
         (used (aget b 0))
         (cap (-i 127 used))
         (idx (xursor-item xa)))
    (cond ((>i cap 0)
           (array-copy-elements b b idx (+i 1 idx) (-i used (-i idx 1)))
           (aset b idx item)
           (aset b 0 (+i 1 used)))
          (#t
           (let ((last (aget b 127)))
             (array-copy-elements b b idx (+i 1 idx) (-i 127 (+i 0 idx)))
             (aset b idx item)
             (%bucket-insert-at-start-of-next v bidx last))))))

(define (xvec-insert-at-index v item idx)
  (if (eq idx (iget v 'count))
      (xvec-push v item)
      (let ((xa (xursor-for-index v idx)))
        (%bucket-insert v xa item)
        (iset v 'count (+i 1 (iget v 'count))))))

(define (xvec-find-from-index v item idx)
  (if (or (>i idx (+i -1 (iget v 'count))) (<i idx 0)) (throw "index out of bounds")
      (let loop ((i idx) (end (xvec-count v)))
           (if (eq i end) -1
               (if (eq (xvec-at v i) item) i
                   (loop (+i 1 i) end))))))

(define (xvec-reverse-find-from-index v item idx)
  (if (or (>i idx (+i -1 (iget v 'count))) (<i idx 0)) (throw "index out of bounds")
      (let loop ((i idx) (end -1))
           (if (eq i end) -1
               (if (eq (xvec-at v i) item) i
                   (loop (+i -1 i) end))))))

(define (xursor-past-end? v p)
  (>i (xursor-bucket p) (iget v 'bucket-max)))

(define (xursor-advance v p)
  (if (xursor-past-end? v p) p
      (let* ((bidx (xursor-bucket p))
             (iidx (xursor-item p))
             (bucket (aget (iget v 'buckets) bidx))
             (maxidx (aget bucket 0)))
        (if (eq iidx maxidx)
            (make-point (+i 1 bidx) 1)
            (make-point bidx (+i 1 iidx))))))

(define (%meta-xvec-stream-read pos table)
  (xvec-at-xursor (aget table 4) pos))
(define (%meta-xvec-stream-advance char pos table)
  (xursor-advance (aget table 4) pos))
(define (%meta-xvec-stream-at-end? pos table)
  (xursor-past-end? (aget table 4) pos))
(define (%meta-xvec-stream-initial-position table character-index)
  (xursor-for-index (aget table 4) character-index))

(define (make-meta-xvec-input vec)
  (vector %meta-xvec-stream-read
          %meta-xvec-stream-advance
          %meta-xvec-stream-at-end?
          %meta-xvec-stream-initial-position
          vec))

(define (xvec-reset! v)
  (let ((bs (iget v 'buckets))
        (max (iget v 'bucket-max)))
    (let loop ((i 0))
         (aset (aget bs i) 0 0)
         (unless (eq i max) (loop (+i 1 i))))
    (iset v 'bucket-max 0)
    (iset v 'count 0)))

;; (let* ((v (make-xvec)))
;;   (dotimes (i 200) (xvec-push v i))
;;   (dotimes (i 200) (print `(,i = ,(xvec-at v i)))))

;; (let ((v (make-xvec)))
;;   (stream-write-string v "hello, xxworld!")
;;   (xvec-delete-range v 7 2)
;;   (print (char-array->string (xvec-to-array v))))

;; (let* ((v (make-xvec)))
;;   (dotimes (i 135) (xvec-push v i))
;;   (xvec-delete-range v 125 5)
;;   (each-with-index (item i v)
;;     (when (>i i 120)
;;       (print `(,i = ,item)))))
