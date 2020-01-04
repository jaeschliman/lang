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
     color output font point (point+ point font-char-size) scale rotation
     (aget %font-origins raw-code)
     (aget %font-extents raw-code) scale rotation)))

