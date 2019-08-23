
(define boxes '())

(define (min a b)
  (if (<i a b) a b))

(define (sign-f i)
  (if (<i i 0) -0.5 0.5))

(define (calc-delta pa pb)
  (let* ((d (point- pb pa))
         (x (point-x d))
         (y (point-y d))
         (m (* 3 (min (abs x) (abs y)))))
    (if (eq 0 m)
        (if (>i (abs x) (abs y)) (cons (sign-f x) 0.0) (cons 0.0 (sign-f y)))
        (cons (/f (i->f x) (i->f m))
              (/f (i->f y) (i->f m))))))

(define (make-box pa pb colors)
  (let ((d (calc-delta pa pb)))
    (vector pa pb
            (i->f (point-x pa)) (i->f (point-y pa))
            (car d) (cdr d)
            colors)))

(define colors '(0xffff0000 0xff00ff00 0xff0000ff))

(define (mappend f as)
  (let loop ((as as) (f f) (acc '()))
       (if (nil? as) (reverse-list acc)
           (let ((r acc))
             (dolist (b (reverse-list (f (car as))))
               (set! r (cons b r)))
             (loop (cdr as) f r)))))

(define (list-to-pairs l)
  (let ((final (first l)))
    (let loop ((as l) (bs (cdr l)) (acc '()))
         (if (nil? bs) (reverse-list (cons (list (car as) final) acc))
             (loop (cdr as) (cdr bs) (cons (list (car as) (car bs)) acc))))))

(define (blend-components n a b)
  (let* ((shiftl (*i n 8))
         (shiftr (*i n -8))
         (ca (bit-and 0xff (ash a shiftr)))
         (cb (bit-and 0xff (ash b shiftr))))
    (ash (+i (/i ca 2) (/i cb 2)) shiftl)))

(define (proper-blend-colors a b)
  (bit-or 0xff000000
          (bit-or (blend-components 2 a b)
                  (bit-or (blend-components 1 a b)
                          (blend-components 0 a b)))))

(define (extend-rainbow!)
  (set 'colors
       (mappend (lambda (colors)
                  (list (first colors)
                        (proper-blend-colors (first colors) (second colors))
                        (second colors)))
                (list-to-pairs colors))))

(extend-rainbow!)
(extend-rainbow!)
(extend-rainbow!)
(extend-rainbow!)
(extend-rainbow!)

(define (near? af ai)
  (<f (abs (-f af (i->f ai))) 3.0))

(define (reset-box b)
  (let ((origin (aget b 0)))
    (aset b 2 (i->f (point-x origin)))
    (aset b 3 (i->f (point-y origin)))))

(define (move-box b)
  (let ((dx (aget b 4))
        (dy (aget b 5))
        (x  (aget b 2))
        (y  (aget b 3))
        (c  (aget b 6))
        (dest (aget b 1)))
    (if (and (near? x (point-x dest))
             (near? y (point-y dest)))
        (reset-box b)
        (let ()
          (aset b 2 (+f x dx))
          (aset b 3 (+f y dy))
          (aset b 6 (if (nil? (cdr c)) colors (cdr c)))))))

(define (add-box pa pb)
  (let ((box (make-box pa pb colors)))
    (set 'boxes (cons box boxes))
    (fork (forever (sleep-ms 15) (move-box box)))))

(define last-point '())

(define (onmousedown p)
  (set 'last-point p))

(define (distance pa pb)
  (let* ((d (point- pa pb))
         (a (i->f (point-x d)))
         (b (i->f (point-y d))))
    (sqrtf (+f (*f a a) (*f b b)))))

(define (onmousedrag p)
  (when (>f (distance p last-point) 15.0)
    (add-box last-point p)
    (set 'last-point p)))

(define (draw-box b)
  (let ((p (make-point (f->i (aget b 2))
                       (f->i (aget b 3)))))
    (screen-fill-rect p (point+ p 10@10) (car (aget b 6)))))

(fork (forever
       (sleep-ms 5)
       (screen-fill-rect 0@0 900@900 0x11ffffff)
       (dolist (b boxes) (draw-box b))))

(request-display 900 900)
