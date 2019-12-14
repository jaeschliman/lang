(defmacro sync (sem & body)
  `(let ()
     (semaphore-wait ,sem)
     ,@body
     (signal-semaphore ,sem)))

(define add-box-lock (make-semaphore #t))

(define mouse-position 0@0)
(define mx 0.0)
(define my 0.0)
(define boxes '())
(define screen-width (f->i (* 1920 0.75)))
(define screen-height (f->i (* 1080 0.75)))
(define screen-size (make-point screen-width screen-height))
(define back-buffer (make-image screen-width screen-height))


(define (minf a b) (if (<f a b) a b))
(define (maxf a b) (if (<f a b) b a))
(define absf #/lang/absf)
(define sinf #/lang/sinf)
(define cosf #/lang/cosf)
(define (signf x) (if (<f x 0.0) -1.0 1.0))
(define (abs-maxf a b) (if (<f (absf a) (absf b)) b (*f (signf b) a)))

(define (ordered? a b c) (and (<i a b) (<i b c)))

(define (lerpf n a b) (+f (*f n b) (*f (-f 1.0 n) a)))
(define (lerp3f n a b c)
  (if (<f n 0.0)
      (lerpf (*f -1.0 n) b a)
      (lerpf n b c)))

(define rainbow-colors
  '(0xffff0000
    0xffff8800
    0xffffaa00
    0xffffdd00
    0xffffff00
    0xff88ff00
    0xffaaff00
    0xffddff00
    0xff00ff00
    0xff00ff88
    0xff00ffaa
    0xff00ffdd
    0xff00ffff
    0xff0088ff
    0xff00aaff
    0xff00ddff
    0xff0000ff
    0xff8800ff
    0xffaa00ff
    0xffdd00ff
    0xffff00ff
    0xffff88ff
    0xffffaaff
    0xffffddff
    0xffffffff
    0xff888888
    0xffaaaaaa
    0xffdddddd
    0xff000000
    0xff880000
    0xffaa0000
    0xffdd0000))

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

;; not really a good blend, but ah well
(define (blend-colors a b)
  (+ (/ a 2) (/ b 2)))

(define (blend-components n a b)
  (let* ((shiftl (*i n 8))
         (shiftr (*i n -8))
         (ca (bit-and 0xff (ash a shiftr)))
         (cb (bit-and 0xff (ash b shiftr))))
    (ash (bit-and 0xff (+i (/i ca 2) (/i cb 2)))
         shiftl)))

(define (proper-blend-colors a b)
  (bit-or 0x99000000
          (bit-or (blend-components 2 a b)
                  (bit-or (blend-components 1 a b)
                          (blend-components 0 a b)))))

(define (mix-components n a b amt)
  (let* ((shiftl (*i n 8))
         (shiftr (*i n -8))
         (ca (bit-and 0xff (ash a shiftr)))
         (cb (bit-and 0xff (ash b shiftr))))
    (ash (bit-and 0xff (f->i (+f (*f (i->f cb) amt)
                                 (*f (i->f ca) (-f 1.0 amt)))))
         shiftl)))

(define (mix-colors a b amt)
  (bit-or 0x99000000
          (bit-or (mix-components 2 a b amt)
                  (bit-or (mix-components 1 a b amt)
                          (mix-components 0 a b amt)))))

(define (extend-rainbow!)
  (set 'rainbow-colors
       (mappend (lambda (colors)
                  (list (proper-blend-colors (first colors) (first colors))
                        (proper-blend-colors (first colors) (second colors))
                        (proper-blend-colors (second colors) (second colors))))
                (list-to-pairs rainbow-colors))))

(extend-rainbow!)
(extend-rainbow!)
(extend-rainbow!)

;; (extend-rainbow!)
;; (extend-rainbow!)
;; (extend-rainbow!)

(define alternate-colors
    '(0xff0000ff
      0xff00ffff))

(define colors rainbow-colors)

(define (inset by n)
  (+f by (*f (-f 1.0 (*f 2.0 by)) n)))

(define pi2 6.2839)

(define (clampf min n max) (maxf min (minf n max)))
(define (wrapf n)
  (if (<f n 0.0) (*f -1.0 n)
      (if (>f n 1.0) (-f 1.0 (-f n 1.0))
          n)))

(define (fuzz amt n)
  (let* ((rand (/f (i->f (random 1000)) 1000.0))
         (noise (-f (*f (*f amt 2.0) rand) (*f amt 0.5))))
    (wrapf (+f n noise))
    ;(clampf 0.0 (+f n noise) 1.0)
    ))

(define (move-box box)
  (let* ((dx (aget box 2))
         (dy (aget box 3))
         (nx (+f (aget box 0) dx))
         (ny (+f (aget box 1) dy))
         (sw (i->f (point-x screen-size)))
         (sh (i->f (point-y screen-size)))
         (nc (cdr (aget box 4)))
         (sc (+i 1 (aget box 5))))
    (when (<f nx 0.0) (set! nx sw))
    (when (<f ny 0.0) (set! ny sh))
    (when (>f nx sw) (set! nx 0.0))
    (when (>f ny sh) (set! ny 0.0))
    (when (nil? nc) (set! nc colors))
    (when (and (>i sc 128) ;(eq 0 (#/lang/%i (+i (f->i (aget box 6)) sc) 5))
               )
      (let* ((delx (abs-maxf 5.0 (-f mx nx)))
             (dely (abs-maxf 5.0 (-f my ny)))
             (mag
              (+f (aget box 6)
                  (sqrtf (+f (*f delx delx) (*f dely dely)))))
             (magmax (*f 0.7 (i->f screen-width)))
             (speedmax 5.0))
        (unless (or (eq 0.0 mag))
          (let* ((normalized-limited-distance (/f (minf mag (*f magmax 0.995)) magmax))
                 ;; (switch (+f 0.5 (*f 0.25 (cosf
                 ;;                          (*f (-f 0.1 (*f 0.05 normalized-limited-distance))
                 ;;                              (i->f sc))))))

                 ;; (switch3 (sinf
                 ;;           (*f (*f 0.005 (fuzz 0.22 normalized-limited-distance))
                 ;;               (i->f sc))))

                 ;; (X (*f 0.05 (*f normalized-limited-distance magmax)))
                 ;; (sinx (sinf X))
                 ;; (switch4-0 (+f sinx (sinf (*f (#/lang/%f X 10.0) sinx))))
                 ;; (switch4 (/f switch4-0 2.4))

                 ;; (fuzzed (fuzz 0.2 switch))
                 (scale speedmax)
                 (angle (#/lang/atan2f dely delx))
                 (nangle (#/lang/%f (+f angle (* 0.23 pi2)) pi2))
                 ;; (blangle (#/lang/%f (+f angle (* -0.23 pi2)) pi2))
                 (ndx0 (*f scale (cosf nangle)))
                 (ndy0 (*f scale (sinf nangle)))
                 ;; (ndx1 (*f scale (cosf blangle)))
                 ;; (ndy1 (*f scale (sinf blangle)))
                 ;; (ndx2 (*f scale (cosf angle)))
                 ;; (ndy2 (*f scale (sinf angle)))
                 (ndx3 (*f scale (cosf (aget box 7))))
                 (ndy3 (*f scale (sinf (aget box 7))))

                 (n (-f 1.0 normalized-limited-distance))
                 (mix (-f 1.0 (lerpf 0.7 n (*f n n))))
                 )

            ;; (set! dx (lerp3f switch4 ndx0 ndx1 ndx2))
            ;; (set! dy (lerp3f switch4 ndy0 ndy1 ndy2))
            (set! dx (lerpf mix ndx0 ndx3))
            (set! dy (lerpf mix ndy0 ndy3))
            ;; (aset box 8 normalized-limited-distance)
            (aset box 8 mix)
            ))))
    (aset box 0 nx)
    (aset box 1 ny)
    (aset box 2 dx)
    (aset box 3 dy)
    (aset box 4 nc)
    (aset box 5 sc)))

(define (add-box p &opt (dx 1.0) (dy 1.0) (priority 0))
  (let ((box (make-array 9)))
    (aset box 0 (i->f (point-x p)))
    (aset box 1 (i->f (point-y p)))
    (aset box 2 dx)
    (aset box 3 dy)
    (aset box 4 colors)
    (aset box 5 0)
    (aset box 6 (i->f (random 333)))
    (aset box 7 (#/lang/atan2f dy dx)) ; angle
    (aset box 8 1.0) ; normalized distance from mouse
    (sync add-box-lock (set 'boxes (cons box boxes)))
    (fork-with-priority 0 ;priority
                        ;let ((box box))
                        (forever
                         (let ((start (#/lang/current-time-ms)))
                           (move-box box)
                           (when (nil? (cdr (aget box 4)))
                             (kill-thread (current-thread)))
                           (draw-one-box box)
                           (let* ((delta (-i (#/lang/current-time-ms) start))
                                  (budget (-i 5 delta)))
                             (when (>i budget 0)
                                (sleep-ms budget))))))))

(define (clear-screen)
  (fill-rect back-buffer 0@0 screen-size 0xff1f1f1f))
(clear-screen)

(define (flip-buffer)
  (blit-to-screen back-buffer 0@0 100 0))

(define (draw-one-box box)
  (let* (
         ;; (x (aget box 0))
         ;; (y (aget box 1))
         ;(sc (/i (aget box 5) 275))
         (d 4; (if (<i sc 2) 2 (if (<i sc 6) sc 6))
           )
         (x (aget box 0))
         (y (aget box 1))
         (back-x (f->i (+f (aget box 0) (*f -1.0 (aget box 2)))))
         (back-y (f->i (+f (aget box 1) (*f -1.0 (aget box 3)))))
         (back (make-point back-x back-y))
         (original-color (car (aget box 4)))
         (dist (aget box 8)
           )
         (color (mix-colors 0xffff0000 original-color dist))
         ;; (a (make-point x y))
         (a (make-point (f->i (aget box 0))
                        (f->i (aget box 1))))
         ;;(b (point+ a (make-point d d)))
         )
    (#/lang/blit-from-screen back-buffer
                             (make-point (f->i x) (f->i y))
                             ;; back (point+ back 16@16)
                             back (point+ back 8@8)
                             1.0 33.3
                                        ;1.02 13.3
                             )
    ;; (fill-rect back-buffer (point+ a (make-point d d))  (point+ a 5@5) color)
    (fill-rect back-buffer a (point+ a (make-point d d)) color)
    ;; (fill-rect back-buffer a b color)
    ))

(define stack-max 0)
(define (check-stack)
    (when (>i (#/lang/%stack-depth-in-bytes) stack-max)
      (set 'stack-max (#/lang/%stack-depth-in-bytes))
      (print `(--> ,stack-max))))

(forward draw-curr-boxes)
(define debug-colors colors)
(define (x-draw-curr-boxes)
  (sleep-ms 60)
  ;; (clear-screen)
  (dolist (box boxes)
    (draw-one-box box))
  ;; (let loop ((cnt 0)
  ;;            (bx boxes))
  ;;      (unless (or (nil? bx)
  ;;                  (>i cnt 100000))
  ;;        (draw-one-box (car bx))
  ;;        (loop (+i cnt 1) (cdr bx))))
  (when (nil? debug-colors) (set 'debug-colors colors))
  (fill-rect back-buffer 0@0 20@20 (car debug-colors))
  (set 'debug-colors (cdr debug-colors))
  (flip-buffer)
  (update-display)
  ;(check-stack)
  (x-draw-curr-boxes))

(define (draw-curr-boxes)
  (sleep-ms 35)
  (flip-buffer)
  (update-display)
  ;(check-stack)
  (draw-curr-boxes))

(define (onmousedown p)
  (add-box p)
  (set 'mouse-position p) )

(define (add-some-boxes p)
  ;; (add-box p)
  (add-box (point+ p 0@20) -1.0 2.0)
  (add-box (point+ p 0@10) 1.2 -2.0)
  (add-box (point+ p -20@10) 3.0 1.0)
  (add-box (point+ p -10@10))
  (add-box (point+ p -20@20) -1.0 1.0)
  (add-box (point+ p 10@0) -1.0 -1.0)
  (add-box (point+ p 20@0) -2.0 1.0)
  (add-box (point+ p -10@0) 1.0 2.0)
  (add-box (point+ p -20@0))
  (add-box (point+ p 10@10) -3.0 2.0)
  (add-box (point+ p 20@20) 1.0 1.0)

  (add-box (point+ p 0@20) 1.0 -2.0)
  (add-box (point+ p 0@10) -1.0 2.0)
  (add-box (point+ p -20@10) -3.0 -1.0)
  (add-box (point+ p -10@10))
  (add-box (point+ p -20@20) 1.0 -1.0)
  (add-box (point+ p 10@0) 1.0 1.0)
  (add-box (point+ p 20@0) 2.0 -1.0)
  (add-box (point+ p -10@0) -1.0 -2.0)
  (add-box (point+ p -20@0) 3.0 3.0)
  (add-box (point+ p 10@10) 3.0 -2.0)
  (add-box (point+ p 20@20) -1.0 -1.0)
  )

(define (add-angled-box original-p other-p)
  (let ((p (point+ original-p other-p)))
    (let* ((dp (point- p mouse-position))
           (dx (*f 0.025 (i->f (point-x dp))))
           (dy (*f 0.025 (i->f (point-y dp))))
           (dp2 (point- original-p mouse-position))
           (dx2 (*f 0.05 (i->f (point-x dp2))))
           (dy2 (*f 0.05 (i->f (point-y dp2)))))
      (add-box p (+f dx dx2) (+f dy dy2)))))

(define (add-some-boxes-2 p)
  (add-angled-box p -20@10 )
  (add-angled-box p -10@10 )
  (add-angled-box p -20@20 )
  (add-angled-box p 10@10 )
  (add-angled-box p 20@20 )
  (add-angled-box p -20@10 )
  (add-angled-box p -10@10 )
  (add-angled-box p -20@20 )
  (add-angled-box p 10@10 )
  (add-angled-box p 20@20 ))


(define (onmousedrag p)
  (add-some-boxes p)
  (add-some-boxes-2 p)
  ;; (make-source 0 p)
  (set 'mouse-position p)
  (set 'mx (i->f (point-x p)))
  (set 'my (i->f (point-y p)))
  )

(define (onmousemove p)
  (set 'mouse-position p)
  (set 'mx (i->f (point-x p)))
  (set 'my (i->f (point-y p)))
  )

(fork-with-priority 20000 (draw-curr-boxes))
;; (fork-with-priority 100000 (draw-curr-boxes))
;; (fork-with-priority 0 (draw-curr-boxes))

(define (make-source priority p &opt (dx 1) (dy 1))
  (let ((box (make-array 7)))
    (aset box 0 (i->f (point-x p)))
    (aset box 1 (i->f (point-y p)))
    (aset box 2 (i->f dx))
    (aset box 3 (i->f dy))
    (aset box 4 colors)
    (aset box 5 0)
    (aset box 6 (i->f (random 100)))
    (aset box 7 (#/lang/atan2f (i->f dy) (i->f dx)))
    (let ((counter 0))
      (fork-with-priority
       priority
       (forever
        (set! counter (+i counter 1))
        (when (>i counter 10) (kill-thread (current-thread)))
        (move-box box)
        (add-some-boxes (make-point (f->i (aget box 0))
                                    (f->i (aget box 1))))
        (sleep-ms 1000))))))

(when #f
  (define (onshow)
    (make-source 50 (make-point (/ screen-width 2) (/ screen-height 5)) 4 5)
    (make-source 50 (make-point (/ screen-width 3) (/ screen-height 2)) 2 -1)
    (make-source 50 (make-point (/ screen-width 4) (/ screen-height 2)))
    (make-source 50 (make-point (/ screen-width 6) (/ screen-height 3)) 1 -3)
    (make-source 50 (make-point (/ screen-width 5) (/ screen-height 2)))
    (make-source 250 (make-point (/ screen-width 6) (/ screen-height 3)) -1 -1)
    (make-source 250 (make-point (/ screen-width 5) (/ screen-height 2)) -2 -3)
    (make-source 250 (make-point (/ screen-width 6) (/ screen-height 3)) -2 1)
    (make-source 250 (make-point (/ screen-width 5) (/ screen-height 2)) -3 2)))



(define (onkey k)
  (print "----------------------------------------")
  (#/lang/%print-stats)
  (#/lang/%clear-stats)
  (print "cleared stats"))

(fork-with-priority 50 (forever (sleep-ms 2000)
                                (print (thread-count))))

(request-display screen-width screen-height)
