(use-package :font "./scratch/lib/font.lisp")

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
  (let ((w (* font/font-char-width scale)))
    (%do-char-codes (code idx str)
       (let ((left (f->i (* idx w))))
         (font/blit-charcode-at
          output code (point+ at (make-point left 0)) color scale rotation)))))


(define (draw-string output str at-point color height rotation)
  (let ((scale (/ height (i->f font/font-char-height))))
    (%display-string output at-point color scale rotation str)))
