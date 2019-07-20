(define screen-width 300)
(define screen-height 200)
(define screen-size (make-point screen-width screen-height))
(request-display screen-width screen-height)

(define font (load-image "./res/charmap-futuristic_white.png"))
(define font-start 32)
(define font-chars-per-row 18)
(define font-char-width 7)
(define font-char-height 9)
(define font-char-size (make-point font-char-width font-char-height))

(define output (make-image 500 500))
;; (fill-rect output 0@0 500@500 0xff00ff00)

(define (blit-charcode-at raw-code point scale rotation)
  (let ((code (- raw-code font-start)))
    (let ((col (% code font-chars-per-row))
          (row (/ code font-chars-per-row)))
      (let ((origin (make-point (* col font-char-width)
                                (* row font-char-height))))
        (blit
         font output point
         origin (point+ origin font-char-size)
         scale rotation)))))


(define (map-charcodes-with-index  str fn)
    (let ((i 0))
      (string-do-chars (ch str)
        (fn (char-code ch) i)
        (set! i (+ i 1)))))

(define (display-string at scale rotation str)
    (map-charcodes-with-index
     str
     (lambda (char idx)
       (let ((left (f->i (* idx font-char-width scale))))
         (blit-charcode-at
          char (point+ at (make-point left 0)) scale rotation))))
  (blit-to-screen output 0@0 100 0))

(define (clear-screen)
    (screen-fill-rect 0@0 screen-size 0xffffffff))

(define rot 0.0)
(define (step-rot!)
    (set-symbol-value 'rot (% (+ 0.1 rot) 360.0)))

(define (step!)
    (step-rot!)
  (display-string 0@0 3.3 (* (cos rot) 33.0) "hello, world!")
  (step-rot!)
  (display-string 0@30 1.3 (* (cos rot) 33.0) "(lambda (x) x) [ ^ self ]."))

(define (update)
    (clear-screen)
    (step!))

(fork (forever (update) (sleep-ms 10)))
