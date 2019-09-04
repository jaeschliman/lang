(define font (load-image "./res/charmap-futuristic_black.png"))
(define font-start 32)
(define font-chars-per-row 18)
(define font-char-width 7)
(define font-char-height 9)
(define font-char-size (make-point font-char-width font-char-height))

(define fill-buffer (make-image 100 100))
(fill-rect fill-buffer 0@0 100@100 0xffffffff)

(define (blit-charcode-at output raw-code point color scale rotation)
  (let* ((code (- raw-code font-start))
         (col (% code font-chars-per-row))
         (row (/ code font-chars-per-row))
         (origin (make-point (* col font-char-width)
                             (* row font-char-height))))
    (fill-rect-with-mask
     color output font point font-char-size scale rotation
     origin (point+ origin font-char-size) scale rotation)))

(defmacro %do-char-codes (binds & body)
  (let ((ch (gensym))
        (code (car binds))
        (idx (cadr binds))
        (str (caddr binds)))
    `(let ((,idx 0))
       (string-do-chars (,ch ,str)
                        (let ((,code (char-code ,ch))) ,@body)
                        (set! ,idx (+ ,idx 1))))))

(define (%display-string output at color scale rotation str)
  (let ((w (* font-char-width scale)))
    (%do-char-codes (code idx str)
       (let ((left (f->i (* idx w))))
         (blit-charcode-at
          output code (point+ at (make-point left 0)) color scale rotation)))))


(define (draw-string output str at-point color height rotation)
  (let ((scale (/ height (i->f font-char-height))))
    (%display-string output at-point color scale rotation str)))
