;; simple widget system
(use-package :text "./scratch/text.lisp")
(use-package :xvec "./scratch/xvec-pkg.lisp")
(use-package :font "./scratch/lib/font.lisp")

(defparameter *translation* 0@0)
(defparameter *widget* '())
(defparameter *buffer* '())
(defparameter *root* '())

(define (%binding-value b)
  (if (nil? b) '() (aget b 0)))
(define (%ensure-binding w k)
  (let ((exist (ht-at w k)))
    (when (nil? exist)
      (set! exist (vector '() #f))
      (ht-at-put w k exist))
    exist))

(define (wget w k) (%binding-value (ht-at w k)))

(define (wset w k v)
  (let ((b (%ensure-binding w k)))
    (unless (aget b 1)
      (aset b 1 #t)
      (aset b 0 v)
      (let ((observers (ht-at w 'observers)))
        (unless (nil? observers)
          (dolist (fn (ht-at observers k))
            (fn w k v))))
      (aset b 1 #f))))

(define make-widget
  (lambda plist
    (let ((r (make-ht)))
      (wset r :type (car plist))
      (let loop ((lst (cdr plist)))
           (unless (nil? lst)
             (wset r (car lst) (cadr lst))
             (loop (cddr lst))))
      r)))

(defmacro with-widget (binds & body)
  `(binding ((*translation* (point+ *translation* (wget ,(car binds) :pos)))
             (*widget* ,(car binds)))
     ,@body))

(define (paint-rect origin size color)
  (fill-rect *buffer* (point+ origin *translation*)
             (point+ size (point+ origin *translation*))
             color))

(define (add-observer w key fn)
  (when (nil? (ht-at w 'observers))
    (ht-at-put w 'observers (make-ht)))
  (let ((tbl (ht-at w 'observers)))
    (ht-at-put tbl key (cons fn (ht-at tbl key)))))

(define (make-root w h)
  (make-widget 
   'root
   :buffer (make-image w h)
   :pos 0@0
   :size (make-point w h)))

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

(define (make-rect x y w h color &opt (click '()))
  (make-widget
   'rect
   :pos (make-point x y)
   :size (make-point w h)
   :color color
   :draw %rect-draw
   :click click))

(define (point-in-bounds? p)
  (not (or (<i (point-x p) (point-x *translation*))
           (<i (point-y p) (point-y *translation*))
           (>i (point-x p) (point-x (point+ (wget *widget* :size) *translation*)))
           (>i (point-y p) (point-y (point+ (wget *widget* :size) *translation*))))))

(define (accept-click w p) (binding ((*root* w)) (%accept-click w p)))

(define (%accept-click w p)
  (with-widget (w)
    (when (point-in-bounds? p)
      (let ((fn (wget w :click)))
        (unless (nil? fn) (fn w (point- p *translation*))))
      (dolist (k (wget w :kids)) (%accept-click k p)))))

(define (accept-key w k)
  (let ((target (wget w :focused-control)))
    (unless (nil? target)
      (let ((fn (wget target :onkey)))
        (unless (nil? fn)
          (fn target k))))))

(define (%slider-draw w)
  (%rect-draw w)
  (let* ((range (- (wget w :max) (wget w :min)))
         (v (/ (- (wget w :val) (wget w :min)) range))
         (left (* v (point-x (wget w :size)))))
    (paint-rect (make-point (f->i left) 0)
                (make-point 2 (point-y (wget w :size)))
                0xffff0000)))

(define (%slider-click w p)
  (let* ((sz (point-x (wget w :size)))
         (ofs (point-x p))
         (pct (/ (i->f ofs) sz))
         (range (- (wget w :max) (wget w :min))))
    (wset w :val (+ (wget w :min) (* pct range)))))

(define (make-slider x y w h min max val)
  (make-widget
   'slider
   :pos (make-point x y)
   :size (make-point w h)
   :color 0xff00ff00
   :draw %slider-draw
   :click %slider-click
   :min min
   :max max
   :val val))

(define (%label-draw w)
  (%rect-draw w)
  (let ((s (wget w :val)))
    (unless (string? s)
      (set! s (with-output-to-string (stream)
                (print-object s stream))))
    (text/draw-string *buffer* s *translation* 0xff000000 (point-y (wget w :size)) 0.0)))

(define (make-label x y w h val)
  (make-widget
   'label
   :pos (make-point x y)
   :size (make-point w h)
   :color 0xffffffff
   :draw %label-draw
   :val val))

(define (%focus-control w p)
  (let ((exist (wget *root* :focused-control)))
    (unless (eq exist w)
      (unless (nil? exist)
        (wset exist :focused #f))
      (wset *root* :focused-control w)
      (wset w :focused #t))))

(define (%numeric-input-onkey w k)
  (let ((add-number (lambda (n)
                      (wset w :val (+ n (* 10 (wget w :val)))))))
    (cond
      ((eq k #\Backspace) (wset w :val (/ (wget w :val) 10)))
      ((eq k #\0) (add-number 0))
      ((eq k #\1) (add-number 1))
      ((eq k #\2) (add-number 2))
      ((eq k #\3) (add-number 3))
      ((eq k #\4) (add-number 4))
      ((eq k #\5) (add-number 5))
      ((eq k #\6) (add-number 6))
      ((eq k #\7) (add-number 7))
      ((eq k #\8) (add-number 8))
      ((eq k #\9) (add-number 9)))))

(define (make-numeric-input x y w h val)
  (make-widget
   'numeric-input
   :pos (make-point x y)
   :size (make-point w h)
   :color 0xffcccccc
   :focused #f
   :click %focus-control
   :onkey %numeric-input-onkey
   :draw %label-draw
   :val val))

(define (%text-input-onkey w k)
  (let ((vec (wget w :val))
        (code (char-code k))
        (curs (wget w :cursor)))
    (cond ((eq k #\Backspace)
           (when (>i (xvec/xvec-count vec) 0)
             (xvec/xvec-delete-range vec (-i curs 1) 1)
             (wset w :cursor (+i -1 curs))))
          ((eq k #\Dc1) ;;left
           (when (>i curs 0)
             (wset w :cursor (+i -1 curs))))
          ((eq k #\Dc3) ;;right
           (when (<i curs (xvec/xvec-count vec))
             (wset w :cursor (+i 1 curs))))
          ((and (>i code 31) (<i code 128))
           (xvec/xvec-insert-at-index vec k curs)
           (wset w :cursor (+i 1 curs))))))

(define (%text-input-draw w)
  (%rect-draw w)
  (let ((s (wget w :val))
        (cursor (if (wget w :focused) (wget w :cursor) -1)))
    (text/draw-xvec *buffer* s *translation* 0xff000000 (point-y (wget w :size)) 0.0 cursor)))

(define (%text-input-click w p)
  (%focus-control w)
  (let* ((max (xvec/xvec-count (wget w :val)))
         (width (font/letter-width-for-size (point-y (wget w :size))))
         (pos (f->i (/ (point-x p) width))))
    (wset w :cursor (if (>i pos max) max pos))))

(define (make-text-input x y w h val)
  (make-widget
   'text-input
   :pos (make-point x y)
   :size (make-point w h)
   :color 0xffccffcc
   :focused #f
   :click %text-input-click
   :onkey %text-input-onkey
   :draw %text-input-draw
   :val (xvec/make-xvec)
   :cursor 0))
