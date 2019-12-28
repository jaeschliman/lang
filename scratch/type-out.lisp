(load-as "xvec" "./scratch/xvec.lisp")
(load-as "xvec" "./scratch/xvec-ext.lisp")

(define (mini a b) (if (<i a b) a b))
(define (maxi a b) (if (<i a b) b a))

(defmacro sync (semaphore & body)
  (let ((sem (gensym))
        (res (gensym)))
    `(let ((,sem ,semaphore)
           (,res '()))
       (semaphore-wait ,sem)
       (set! ,res (let () ,@body))
       (signal-semaphore ,sem)
       ,res)))

(define *text-lock (make-semaphore #t))

(define font (load-image "./res/another-font.png"))
(define font-start 0)
(define font-chars-per-row 16)
(define font-char-width 10)
(define font-char-height 10)
(define font-char-size (make-point font-char-width font-char-height))
(define font-letter-width 8)
(define font-letter-height 10)

(define %font-origins (make-array 256))
(define %font-extents (make-array 256))
(let loop ((raw-code 0))
     (when (<i raw-code 256)
       (let* ((code (-i raw-code font-start))
              (col (%i code font-chars-per-row))
              (row (/i code font-chars-per-row))
              (origin (make-point (*i col font-char-width)
                                  (*i row font-char-height))))
         (aset %font-origins raw-code origin)
         (aset %font-extents raw-code (point+ origin font-char-size))
         (loop (+i raw-code 1)))))

(define (blit-charcode-at output raw-code point color scale rotation)
  (when (<i raw-code 256)
    (fill-rect-with-mask
     color output font point font-char-size scale rotation
     (aget %font-origins raw-code)
     (aget %font-extents raw-code) scale rotation)))

(define *star-rot 0.0)

(define *cursor 0)

(define (draw-cursor output at w h)
  (fill-rect output at (point+ at (make-point (f->i w) (f->i h))) 0xffffff00))

(define (%display-xvec output at color scale rotation vec)
  (let* ((left 0.0) (top 0.0)
         (w (* font-letter-width scale))
         (h (* font-letter-height scale))
         (cnt (xvec/xvec-count vec))
         (here  (point+ at (make-point (f->i left) (f->i top)))))
    (xvec/each-with-index
     (ch idx vec)
     (when (eq *cursor idx) (draw-cursor output here w h))
     (cond ((eq ch #\Newline)
            (set! left 0.0)
            (set! top (+f top h)))
           (#t
            (blit-charcode-at
             output (char-code ch) here color scale (if (eq ch #\*) *star-rot rotation))
            (set! left (+f left w))))
     (set! here (point+ at (make-point (f->i left) (f->i top)))))
    (when (eq *cursor cnt)
      (draw-cursor output here w h))))

(define (draw-xvec output vec at-point color height rotation)
  (let ((scale (/ height (i->f font-char-height))))
    (%display-xvec output at-point color scale rotation vec)))

(define screen-w 500)
(define screen-h 800)
(define screen-size (make-point screen-w screen-h))
(define buffer (make-image screen-w screen-h))

(define (clear-screen!) (fill-rect buffer 0@0 screen-size 0xff000000))
(define (flip-buffer!) (blit-to-screen buffer 0@0 100 0))


(define *text (xvec/make-xvec))
;; (binding ((*standard-output* *text)) (print `(hello from: ,*text)))

(define (debug-dump)
  (xvec/each-with-index (ch i *text) (stream-write-char *standard-output* ch))
  (stream-write-char *standard-output* #\Newline))

(define (onleft)  (set '*cursor (maxi 0 (+i *cursor -1))))
(define (onright) (set '*cursor (mini (+i *cursor 1) (xvec/xvec-count *text))))

(define (onkey k)
  (sync *text-lock
        (cond
          ((eq k #\Dc1) (onleft))
          ((eq k #\Dc3) (onright))
          ((eq k #\Backspace)
           (when (>i *cursor 0)
             (set '*cursor (-i *cursor 1))
             (xvec/xvec-delete-range *text *cursor 1)))
          (#t
           (let ((c (char-code k)))
             (when (or (and (>i c 31) (<i c 128))
                       (eq k #\Newline) (eq k #\Return))
               (xvec/xvec-insert-at-index *text (if (eq k #\Return) #\Newline k) *cursor)
               (set '*cursor (+i *cursor 1))))))))

(define (draw!)
  (sync *text-lock
        (clear-screen!)
        (draw-xvec buffer *text 0@0 0xff00cc00 14.5 0.0)
        (flip-buffer!)))

(define onshow draw!)

(fork-with-priority 0 (forever
                       (sleep-ms 20)
                       (set '*star-rot (%f (+f 2.0 *star-rot) 360.0))
                       (draw!)))

(request-display screen-w screen-h)
