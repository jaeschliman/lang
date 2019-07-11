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
  (let ((code (-i raw-code font-start)))
    (let ((col (%i code font-chars-per-row))
          (row (/i code font-chars-per-row)))
      (let ((origin (make-point (*i col font-char-width )
                                (*i row font-char-height))))
        (blit
         font output point
         origin (point+ origin font-char-size)
         scale rotation)))))

(define strloop #f)
(define strloop (lambda (str fn idx len)
                  (if (<i idx len)
                      (let ()
                        (fn (char-code-at str idx) idx)
                        (strloop str fn (+i 1 idx) len)))))

(define (map-charcodes-with-index  str fn)
    (strloop str fn 0 (string-length str)))

(define (display-string at scale rotation str)
    (map-charcodes-with-index
     str
     (lambda (char idx)
       (let ((left (f->i
                    (*f (*f (i->f idx)
                            (i->f font-char-width))
                        scale))))
         (blit-charcode-at
          char (point+ at (make-point left 0)) scale rotation))))
  (blit-to-screen output 0@0 100 0))

(define (clear-screen)
    (screen-fill-rect 0@0 screen-size 0xffffffff))

(define rot 0.0)
(define (step-rot!)
    (set-symbol-value 'rot (%f (+f 1.0 rot) 360.0)))

(define (step!)
    (step-rot!)
  (display-string 0@0 3.3 rot "hello, world!")
  (step-rot!)
  (display-string 0@30 1.3 rot "(lambda (x) x) [ ^ self ]."))

(define (update)
    (clear-screen)
    (step!))

(fork (forever (update) (sleep-ms 10)))
