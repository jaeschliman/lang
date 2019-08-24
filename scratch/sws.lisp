;; simple widget system


(defparameter *translation* 0@0)
(defparameter *buffer* '())

(define (wget w k) (ht-at w k))
(define (wset w k v) (ht-at-put w k v))

(defmacro with-widget (binds & body)
  `(binding ((*translation* (point+ *translation* (wget ,(car binds) :pos))))
     ,@body))

(define (paint-rect origin size color)
  (fill-rect *buffer* (point+ origin *translation*)
             (point+ size (point+ origin *translation*))
             color))

(define (make-root w h)
  (let ((r (make-ht)))
    (wset r :buffer (make-image w h))
    (wset r :pos 0@0)
    (wset r :size (make-point w h))
    r))

(define (add-kid w k)
  (wset w :kids (cons k (wget w :kids))))

(define (draw-widget w)
  (with-widget (w)
    (let ((draw (wget w :draw)))
      (unless (nil? draw) (draw w))
      (dolist (k (wget w :kids)) (draw-widget k)))))

(define (draw-root root)
  (binding ((*buffer* (wget root :buffer)))
    (draw-widget root))
  (blit-to-screen (wget root :buffer) 0@0 100 0))


(define (%rect-draw w)
  (paint-rect 0@0 (wget w :size) (wget w :color)))

(define (make-rect x y w h color)
  (let ((r (make-ht)))
    (wset r :pos (make-point x y))
    (wset r :size (make-point w h))
    (wset r :color color)
    (wset r :draw %rect-draw)
    r))
