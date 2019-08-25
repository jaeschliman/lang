(load-as "queue" "./scratch/queue.lisp")
(load-as "w" "./scratch/sws.lisp")

(define screen-width 900)
(define screen-height 900)
(define screen-size (make-point screen-width screen-height))


(define buffer (make-image screen-width screen-height))
(fill-rect buffer 0@0 screen-size 0xff888888)

(define colors
  '((0xffff0000 0xff00ff00 0xff0000ff)
    (0xffff0000 0xff00ff00)
    (0xff00ff00 0xff0000ff)
    (0xffff0000 0xff0000ff)
    (0xffffff00 0xff0000ff)
    (0xff00ff00 0xff00ffff)))

(define picked-colors '())

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

(define brush-scale 4)
(define brush-spread 10)
(define brush-density 6)
(define brush-stroke-length 25.0)
(define travel-speed (*f 2.0 brush-stroke-length))

(define boxes (queue/make))

(define (distance pa pb)
  (let* ((d (point- pa pb))
         (a (i->f (point-x d)))
         (b (i->f (point-y d))))
    (sqrtf (+f (*f a a) (*f b b)))))

(define (min a b) (if (<i a b) a b))
(define (max a b) (if (<i a b) b a))
(define (minf a b) (if (<f a b) a b))
(define (maxf a b) (if (<f a b) b a))

(define (sign-f i)
  (if (<i i 0) -0.5 0.5))

(define (calc-delta pa pb)
  (let* ((d (point- pb pa))
         (x (point-x d))
         (y (point-y d))
         (m (* 3 (min (abs x) (abs y))))
         (dist (/f travel-speed (distance pa pb))))
    (if (eq 0 m)
        (if (>i (abs x) (abs y)) (cons (*f dist (sign-f x)) 0.0) (cons 0.0 (*f dist (sign-f y))))
        (cons (*f dist (/f (i->f x) (i->f m)))
              (*f dist (/f (i->f y) (i->f m)))))))

(define (make-box pa pb scale colors)
  (let ((d (calc-delta pa pb)))
    (vector pa pb
            (i->f (point-x pa)) (i->f (point-y pa))
            (car d) (cdr d)
            colors
            pa pb
            colors
            scale)))

(define (near? af ai slackf)
  (<f (abs (-f af (i->f ai))) (maxf (abs slackf) 1.0)))

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
    (if (and (near? x (point-x dest) (*f 3.0 dx))
             (near? y (point-y dest) (*f 3.0 dy)))
        (reset-box b)
        (let ()
          (aset b 2 (+f x dx))
          (aset b 3 (+f y dy))
          (aset b 6 (if (nil? (cdr c)) (aget b 9) (cdr c)))))))

(define (add-box pa pb)
  (let ((box (make-box pa pb brush-scale (car colors))))
    (set 'boxes (cons box boxes))
    (fork (forever (sleep-ms 15) (move-box box)))))

(define last-point '())
(define last-last-point '())
(define toggle #t)

(define root-widget (w/make-root 200 screen-height))
(define show-drawer #f)

(define (onmousemove p)
  (when (<i (point-x p) 20)
    (set 'show-drawer #t))
  (when (>i (point-x p) 200)
    (set 'show-drawer #f)))

(define (onmousedown p)
  (if (and show-drawer (<i (point-x p) 200))
      (w/accept-click root-widget p))
  (set 'last-point p)
  (set 'last-last-point p) ;; FIXME: would be better to start this at an offset
  (set 'toggle #t))

(define (onkey k)
  (print `(-> ,k))
  (case k
    (#\c (set 'picked-colors '()))
    (#\[ (set 'brush-scale (-i brush-scale 2)))
    (#\] (set 'brush-scale (+i brush-scale 2)))
    (#\- (set 'brush-spread (-i brush-spread 2)))
    (#\= (set 'brush-spread (+i brush-spread 2)))
    (#\o (set 'brush-density (-i brush-density 1)))
    (#\p (set 'brush-density (+i brush-density 1)))
    (#\k (set 'brush-stroke-length (-f brush-stroke-length 1.0)))
    (#\l (set 'brush-stroke-length (+f brush-stroke-length 1.0)))))


(define (maybe-add-point p from symbol toggle-value)
  (when (>f (distance p from) brush-stroke-length)
    (let* ((bs '())
           (add-p (lambda (dx dy scale)
                    (let ((d (make-point dx dy)))
                      (set! bs (cons
                                (make-box (perturb-point (point+ from d))
                                          (perturb-point (point+ p d))
                                          scale
                                          (car colors))
                                bs)))))
           (s (+i 1 (*i 2 brush-spread))))
      (dotimes (_ brush-density)
        (add-p (random-offset s) (random-offset s)
               (+i brush-scale (random-offset (-i brush-scale 1)))))
      (queue/add boxes bs)
      (fork (forever (sleep-ms 15) (dolist (b bs) (move-box b))))
      (set symbol p)
      (set 'toggle toggle-value))))

(define (onmousedrag p)
  (if toggle
      (maybe-add-point p last-point 'last-point #f)
      (maybe-add-point p last-last-point 'last-last-point #t)))

(define (draw-box b)
  (if (pair? b)
      (dolist (b b) (draw-box b))
      (let ((p (make-point (f->i (aget b 2))
                           (f->i (aget b 3)))))
        (fill-rect buffer p (point+ p (make-point (aget b 10) (aget b 10))) (car (aget b 6))))))

(define tray (w/make-rect 0 0 200 screen-height 0xffffffff))
(w/add-kid root-widget tray)

(define (make-color r g b a)
  (bit-or (ash a 24)
          (bit-or (ash r 16)
                  (bit-or (ash g 8) b))))
(define picker-colors '())

(let ((comps '(0xff 0xdd 0xaa 0x88 0x44 0x00)))
  (dolist (g comps)
    (dolist (b comps)
      (dolist (r comps)
        (set 'picker-colors (cons (make-color r g b 0xff) picker-colors))))))

(define (update-color-from-widget w p)
  (let ((color (w/wget w :color)))
    (binding ((#/lang/*print-base* 16))
      (print `(--> ,color)))
    (set 'picked-colors (cons color picked-colors))
    (set 'colors (list (stretch-colors picked-colors)))))

(let* ((colors picker-colors)
      (x 3) (y 50) (idx 0)
       (size 14)
       (margin 1)
       (with-margin (+ size (* 2 margin))))
  (dolist (c colors)
    (w/add-kid tray (w/make-rect (+ x margin) (+ y margin) size size c update-color-from-widget))
    (set! idx (+ 1 idx))
    (if (eq 0 (% idx 12))
        (let ()
          (set! y (+ y with-margin))
          (set! x 3))
        (set! x (+ x with-margin)))))

(fork-with-priority 10000 (forever
                           (sleep-ms 5)
                           (fill-rect buffer 0@0 screen-size 0x11888888)
                           (queue/doq (b boxes) (draw-box b))
                           (blit-to-screen buffer 0@0 100 0)
                           (when show-drawer (w/draw-root root-widget))))

(fork-with-priority 50 (forever (sleep-ms 2000)
                                (print (thread-count))))

(request-display screen-width screen-height)
