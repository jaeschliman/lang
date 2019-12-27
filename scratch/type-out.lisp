(load-as "xvec" "./scratch/xvec.lisp")

(define font (load-image "./res/charmap-futuristic_black.png"))
(define font-start 32)
(define font-chars-per-row 18)
(define font-char-width 7)
(define font-char-height 9)
(define font-char-size (make-point font-char-width font-char-height))

(define (blit-charcode-at output raw-code point color scale rotation)
  (let* ((code (- raw-code font-start))
         (col (% code font-chars-per-row))
         (row (/ code font-chars-per-row))
         (origin (make-point (* col font-char-width)
                             (* row font-char-height))))
    (fill-rect-with-mask
     color output font point font-char-size scale rotation
     origin (point+ origin font-char-size) scale rotation)))

(define (%display-xvec output at color scale rotation vec)
  (let ((left 0.0) (top 0.0)
        (w (* font-char-width scale))
        (h (* font-char-height scale)))
    (xvec/each-with-index
     (ch idx vec)
     (cond ((eq ch #\Newline)
            (set! left 0.0)
            (set! top (+ top h)))
           (#t
            (blit-charcode-at
             output (char-code ch)
             (point+ at (make-point (f->i left) (f->i top)))
             color scale rotation)
            (set! left (+ left w)))))))

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

(define (debug-dump)
  (xvec/each-with-index (ch i *text) (stream-write-char *standard-output* ch))
  (stream-write-char *standard-output* #\Newline))

(define (onkey k)
  (if (eq k #\Backspace)
      (xvec/xvec-pop *text)
      (let ((c (char-code k)))
        (when (or (and (>i c 31) (<i c 128))
                  (eq k #\Newline) (eq k #\Return))
          (xvec/xvec-push *text (if (eq k #\Return) #\Newline k)))))
  (draw!))

(define (draw!)
  (clear-screen!)
  (draw-xvec buffer *text 0@0 0xff00cc00 16.0 0.0)
  (flip-buffer!))

(define onshow draw!)

(request-display screen-w screen-h)
