(define screen-width 500)
(define screen-height 500)
(define screen-size (make-point screen-width screen-height))

(request-display screen-width screen-height)

(set 'colors
     '(0xffff0000
       0xffffff00
       0xff00ff00
       0xff00ffff
       0xff0000ff
       0xffff00ff
       0xffffffff
       0xff000000
       rainbow))

(set 'draw-rainbow-loop #f)
(set 'draw-rainbow-loop
     (lambda (ul width height colors)
       (if (not (eq 'rainbow (car colors)))
           (let ()
             (screen-fill-rect ul (point+ ul (make-point width height)) (car colors))
             (draw-rainbow-loop (point+ ul (make-point 0 height))
                                width height
                                (cdr colors))))))

(set 'draw-rainbow
     (lambda (ul size)
       (let ((stripe-height (/i size 8)))
         (draw-rainbow-loop ul size stripe-height colors))))

(set 'draw-box
     (lambda (ul size color)
       (if (eq color 'rainbow)
           (draw-rainbow ul size)
           (screen-fill-rect ul (point+ ul (make-point size size)) color))))

;; this is awkward, but don't have self-referencing definitions yet
;; also don't have loops yet!
(set 'draw-boxes #f)
(set 'draw-boxes
     (lambda (top size colors)
       (if (not (nil? colors))
           (let ((color (car colors))
                 (ul (make-point 0 top))
                 (lr (make-point size (+i top size))))
             (draw-box ul size color)
             (draw-boxes (+i top size) size (cdr colors))))))


(set 'color-well-size (/i screen-height 9))

(set 'selected-color (car colors))

(set 'paint
     (lambda (p)
       (if (eq selected-color 'rainbow)
           (draw-box p 30 selected-color)
           (draw-box (point- p 8@8) 16 selected-color))))

(set 'choose-color
     (lambda (p)
       (let ((idx (/i (point-y p) color-well-size)))
         (let ((color (nth colors idx)))
           (if (not (nil? color))
               (set 'selected-color color))))))

(set 'scale (lambda (s v) (/i (*i s v) 100)))

(set 'screen-size #f)

(set 'mouse-handler
     (lambda (p)
       (if (<i (point-x p) color-well-size)
           (choose-color p)
           (let ((s (+i 50 (/i (point-y p) 24)))) 
             (paint p)))))

;;;;;;; register event handlers

(set 'onshow (lambda (sz)
               (draw-boxes 0 color-well-size colors)))

(set 'onmousedown mouse-handler)
(set 'onmousedrag mouse-handler)
