(use-package :font "./scratch/lib/font.lisp")
(use-package :xvec "./scratch/xvec-pkg.lisp")

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

(define (draw-cursor output at w h)
  (fill-rect output at (point+ at (make-point (f->i w) (f->i h))) 0xff999900))

(define (%display-xvec output at color scale rotation cursor vec)
  (let* ((left 0.0) (top 0.0)
         (w (* font/font-letter-width scale))
         (h (* font/font-letter-height scale))
         (cnt (xvec/xvec-count vec))
         (here  (point+ at (make-point (f->i left) (f->i top)))))
    (xvec/each-with-index
     (ch idx vec)
     (when (eq cursor idx) (draw-cursor output here w h))
     (cond ((eq ch #\Newline)
            (set! left 0.0)
            (set! top (+f top h)))
           (#t
            (font/blit-charcode-at
             output (char-code ch) here color scale rotation)
            (set! left (+f left w))))
     (set! here (point+ at (make-point (f->i left) (f->i top)))))
    (when (eq cursor cnt)
      (draw-cursor output here w h))))

(define (draw-xvec output vec at-point color height rotation)
  (let ((scale (/ height (i->f font/font-char-height))))
    (%display-xvec output at-point color scale rotation -1 vec)))
