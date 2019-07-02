

(define run-reset #f)
(define (run-reset fn)
    (set-stack-mark 'reset)
  (let ((it (fn)))
    (if (continuation? it)
        (let* ((resume (lambda (val)
                         (let* ((thunk (lambda () (resume-stack-snapshot it val)))) 
                           (run-reset thunk))))
               (handler (continuation-value it)))
          (handler resume))
        it)))

(define (run-shift fn)
    (snapshot-to-stack-mark 'reset fn))

(defmacro reset (form)
  `(run-reset (lambda () ,form)))

(defmacro shift (var & body)
  `(run-shift (lambda (,var) ,@body)))


;; examples from https://en.wikipedia.org/wiki/Delimited_continuation

;; (* 2 (reset (+ 1 (shift k (k 5))))) => 12
(print (*i 2 (reset (+i 1 (shift k (k 5))))))

;; (+ 1 (reset (* 2 (shift k (k (k 4)))))) => 17
(print (+i 1 (reset (*i 2 (shift k (k (k 4)))))))
;; (reset
;;   (begin
;;     (shift k (cons 1 (k (void)))) ;; (1)
;;     null))
;; => (1)
(print (reset (let ()
                (shift k (cons 1 (k 'ignored)))
                '())))
;; (reset
;;   (begin
;;     (shift k (cons 1 (k (void))))
;;     (shift k (cons 2 (k (void))))
;;     null))
;; => (1 2)
(print (reset (let ()
                (shift k (cons 1 (k 'ignored)))
                (shift k (cons 2 (k 'ignored)))
                '())))

'done
