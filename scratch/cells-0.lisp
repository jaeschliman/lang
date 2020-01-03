(define (list-member? item lst)
  (if (nil? lst) #f
      (or (eq (car lst) item)
          (list-member? item (cdr lst)))))

(defparameter *cells* (make-ht))
(defparameter *active-cells* '())
(defparameter *current-cell* '())
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
 (let ((exist (%cell-meta-at cell 'inputs)))
    (unless (list-member? input exist)
      (%cell-meta-at-put cell 'inputs (cons input exist)))) 
 (let ((exist (%cell-meta-at input 'outputs)))
    (unless (list-member? cell exist)
      (%cell-meta-at-put input 'outputs (cons cell exist)))))

(define (%mark-dirty! cell)
  (let ((c (ht-at *cells* cell)))
    (unless (aget c 0)
      (aset c 3 #t))))

;; TODO: cycle detection
(define (cell-value cell)
  (%note-read *current-cell* cell)
  (let ((c (ht-at *cells* cell)))
    (unless (aget c 0)
      (when (aget c 3)
        (binding ((*current-cell* cell))
          (aset c 1 ((aget c 4)))
          (dolist (fn (aget c 2)) (fn (aget c 1)))
          (aset c 3 #f))))
    (aget c 1)))

(define (set-input! cell value)
  (let ((c (ht-at *cells* cell)))
    (aset c 1 value)
    (dolist (output (%cell-meta-at cell 'outputs))
      (%mark-dirty! output))
    (dolist (output (%cell-meta-at cell 'outputs))
      (cell-value output))))

(define (run fn)
  (binding ((*current-cell* '%toplevel%)
            (*active-cells* (make-ht)))
    (fn)))

(defmacro ? (cell) `(cell-value ',cell))

(input salary 100000)
(input tax 0.06)
(formula take-home (- (? salary) (* (? salary) (? tax))))

(add-listener 'take-home (lambda (v) (print `(take-home updated to: ,v))))

(run (lambda ()
       (cell-value 'take-home)
       (set-input! 'tax 0.04)))
