(use-package :sws "./scratch/sws.lisp")

(define (list-member? item lst)
  (if (nil? lst) #f
      (or (eq (car lst) item)
          (list-member? item (cdr lst)))))

(defparameter *cells* (make-ht))
(defparameter *active-cells* '())
(defparameter *current-cell* '())
(defparameter *updating-cells* '())
(define empty (list 'empty))

(defmacro formula (name & body)
  `(ht-at-put *cells* ',name
              (vector #f ;; not an input
                      empty ;; value
                      '() ;; listeners
                      #t ;; dirty
                      (lambda () ,@body) ;; body
                      )))

(defmacro input (name value)
  `(ht-at-put *cells* ',name
              (vector #t ;; is an input
                      ,value ;; value
                      '() ;; listeners
                      )))

(define (add-listener cell fn)
  (let* ((c (ht-at *cells* cell)))
    (aset c 2 (cons fn (aget c 2)))))

(define (%cell-meta-at cell key)
  (let ((h (ht-at *active-cells* cell)))
    (if (nil? h) '() (ht-at h key))))

(define (%cell-meta-at-put cell key val)
  (let ((h (ht-at *active-cells* cell)))
    (when (nil? h)
      (set! h (make-ht))
      (ht-at-put *active-cells* cell h))
    (ht-at-put h key val)))

(define (%note-read cell input)
  (unless (eq cell '%toplevel%)
    (let ((exist (%cell-meta-at cell 'inputs)))
      (unless (list-member? input exist)
        (%cell-meta-at-put cell 'inputs (cons input exist)))) 
    (let ((exist (%cell-meta-at input 'outputs)))
      (unless (list-member? cell exist)
        (%cell-meta-at-put input 'outputs (cons cell exist))))))

(define (%mark-dirty! cell)
  (let ((c (ht-at *cells* cell)))
    (unless (aget c 0)
      (aset c 3 #t))))

(defmacro with-updating-cell (cell & body)
  `(unless (list-member? ,cell *updating-cells*)
    (;binding ((*updating-cells* (cons ,cell *updating-cells*)))
     let ()
      (set '*updating-cells* (cons ,cell *updating-cells*))
      ,@body)))

(define (%update-dependents! cell)
  (let ((updated '()))
    (dolist (output (%cell-meta-at cell 'outputs))
      (with-updating-cell output
        (%mark-dirty! output)
        (set! updated (cons output updated))))
    (dolist (output (reverse-list updated))
      (%cell-value output))))

(define (%cell-value cell)
  (let ((c (ht-at *cells* cell)))
    (unless (aget c 0)
      (when (aget c 3)
        (binding ((*current-cell* cell))
          (%cell-meta-at-put cell 'inputs '())
          (aset c 1 ((aget c 4)))
          (aset c 3 #f)
          (dolist (fn (aget c 2)) (fn (aget c 1)))
          (%update-dependents! cell))))
    (aget c 1)))

;; TODO: cycle detection
(define (cell-value cell)
  (%note-read *current-cell* cell)
  (%cell-value cell))

(define (mappend fn lst)
  (#/lang/reduce-list append '() (mapcar fn lst)))

(define (%all-inputs cell)
  (let ((direct-inputs (%cell-meta-at cell 'inputs)))
    (append direct-inputs (mappend %all-inputs direct-inputs))))

(define (observe! cell)
  (binding ((*updating-cells* (list cell)))
    (%cell-value cell)))

(define (set-input! cell value)
  (unless (list-member? cell *updating-cells*)
    (let ((c (ht-at *cells* cell)))
      (aset c 1 value)
      (binding ((*updating-cells* (list cell)))
        (dolist (fn (aget c 2)) (fn (aget c 1)))
        (%update-dependents! cell)))))

(define (run context fn)
  (binding ((*current-cell* '%toplevel%)
            (*active-cells* context))
    (fn)))

(defmacro ? (cell) `(cell-value ',cell))

(input bonus 0.2)
(input earned-bonus #f)
(input salary 100000)
(input tax 0.06)

(define (taxed amount)
  (- amount (* amount (? tax))))

(formula take-home (if (? earned-bonus)
                       (taxed (+ (? salary) (* (? bonus) (? salary))))
                       (taxed (? salary))))

(formula monthly (/ (? take-home) 12.0))
(input rent 1000)
(input food 500)
(formula spending-money (- (? monthly) (? rent) (? food)))

(add-listener 'take-home (lambda (v) (print `(take-home updated to: ,v))))
(add-listener 'spending-money
              (lambda (v) (print `(spending money updated to: ,v
                                            inputs = ,(%all-inputs 'spending-money)))))

(define (clear-terminal)
  (stream-write-char *standard-output* #\Esc)
  (stream-write-string *standard-output* "[2J")
  (stream-write-char *standard-output* #\Esc)
  (stream-write-string *standard-output* "[;f"))

(clear-terminal)
(define *context (make-ht))

(run *context (lambda ()
                (observe! 'spending-money)
                (print '----------------------------------------)
                (set-input! 'tax 0.04)
                (print '----------------------------------------)
                (set-input! 'earned-bonus #t)))

(define (%bind-to-input input-cell widget)
  (add-listener input-cell (lambda (v) (sws/wset widget :val v)))
  (sws/add-observer widget :val (lambda (w k v) (run *context (lambda () (set-input! input-cell v))))))

(define (%bind-to-output output-cell widget)
  (add-listener output-cell (lambda (v) (sws/wset widget :val v))))

(define (bind cell-name widget)
  (let ((cell (ht-at *cells* cell-name)))
    (if (aget cell 0)
        (%bind-to-input cell-name widget)
        (%bind-to-output cell-name widget))
    (run *context (lambda () (sws/wset widget :val (observe! cell-name))))))

(define screen-width 200)
(define screen-height 200)
(define screen-size (make-point screen-width screen-height))

(define root-widget (sws/make-root screen-width screen-height))
(define slider (sws/make-slider 10 10 100 30 100.0 900.0 500.0))
(define slider-2 (sws/make-slider 10 50 100 30 100.0 900.0 500.0))

(define salary-input (sws/make-numeric-input 10 100 300 16 0))
(define results-label (sws/make-label 10 120 300 16 0.0))
(define text-input (sws/make-text-input 10 140 300 16 0.0))

(sws/add-kid root-widget slider)
(sws/add-kid root-widget slider-2)
(sws/add-kid root-widget salary-input)
(sws/add-kid root-widget results-label)
(sws/add-kid root-widget text-input)

(bind 'food slider)
(bind 'food slider-2)
(bind 'spending-money results-label)
(bind 'salary salary-input)

(let ((package *package*))
  (define (onmousedown p)
    (binding ((*package* package)) ;; so symbols will print nicely
      (sws/accept-click root-widget p)
      (sws/draw-root root-widget))))

(let ((package *package*))
  (define (onkey k)
    (binding ((*package* package)) ;; so symbols will print nicely
      (sws/accept-key root-widget k)
      (sws/draw-root root-widget))))

(define (onshow)
  (sws/draw-root root-widget))

(request-display screen-width screen-height)

