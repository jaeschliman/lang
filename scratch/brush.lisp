(define colors
  '((0xffff0000 0xff00ff00 0xff0000ff)
    (0xffff0000 0xff00ff00)
    (0xff00ff00 0xff0000ff)
    (0xffff0000 0xff0000ff)
    (0xffffff00 0xff0000ff)
    (0xff00ff00 0xff00ffff)))

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

(define (extend-rainbow colors)
  (mappend (lambda (colors)
             (list (first colors)
                   (proper-blend-colors (first colors) (second colors))
                   (second colors)))
           (list-to-pairs colors)))

(define (stretch-colors colors)
  (extend-rainbow (extend-rainbow (extend-rainbow (extend-rainbow (extend-rainbow colors))))))

(set 'colors (mapcar stretch-colors colors))

(define brush-scale 10)
(define brush-spread 10)

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
            colors
            pa pb
            colors
            brush-scale)))

(define (near? af ai)
  (<f (abs (-f af (i->f ai))) 3.0))

(define (random-offset n)
  (+i (*i -1 (/i n 2)) (random n)))

(define (perturb-point p &opt (amt 6))
  (let ((x (+i (point-x p) (random-offset amt)))
        (y (+i (point-y p) (random-offset amt))))
    (make-point x y)))

(define (reset-box b)
  (let* ((pa (perturb-point (aget b 7) 22))
         (pb (perturb-point (aget b 8) 22))
         (d  (calc-delta pa pb)))
    (aset b 0 pa)
    (aset b 1 pb)
    (aset b 4 (car d))
    (aset b 5 (cdr d)))
  (aset b 7 (perturb-point (aget b 7) 3))
  (aset b 8 (perturb-point (aget b 8) 3))
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
          (aset b 6 (if (nil? (cdr c)) (aget b 9) (cdr c)))))))

(define (add-box pa pb)
  (let ((box (make-box pa pb (car colors))))
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

(define (onkey k)
  (print `(-> ,k))
  (case k
    (#\c (set 'colors (if (nil? (cdr colors)) colors (cdr colors))))
    (#\[ (set 'brush-scale (-i brush-scale 2)))
    (#\] (set 'brush-scale (+i brush-scale 2)))
    (#\- (set 'brush-spread (-i brush-spread 2)))
    (#\= (set 'brush-spread (+i brush-spread 2)))))

(define (onmousedrag p)
  (when (>f (distance p last-point) 15.0)
    (let* ((bs '())
           (add-p (lambda (dx dy)
                    (let ((d (make-point dx dy)))
                      (set! bs (cons
                                   (make-box (perturb-point (point+ last-point d))
                                             (perturb-point (point+ p d))
                                             (car colors))
                                   bs)))))
           (s brush-spread)
           (-s (- 0 s)))
      (add-p -s  0)
      (add-p  s  0)
      (add-p  0 -s)
      (add-p  0  s)
      (set 'boxes (cons bs boxes))
      (fork (forever (sleep-ms 15) (dolist (b bs) (move-box b))))
      (set 'last-point p))))

(define (draw-box b)
  (if (pair? b)
      (dolist (b b) (draw-box b))
      (let ((p (make-point (f->i (aget b 2))
                           (f->i (aget b 3)))))
        (screen-fill-rect p (point+ p (make-point (aget b 10) (aget b 10))) (car (aget b 6))))))


(fork-with-priority 10000 (forever
                           (sleep-ms 5)
                           (screen-fill-rect 0@0 900@900 0x11888888)
                           (dolist (b boxes) (draw-box b))))

(fork-with-priority 50 (forever (sleep-ms 2000)
                                (print (thread-count))))

(request-display 900 900)
