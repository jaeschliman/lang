
(defmacro push (val place)
  `(set! ,place (cons ,val ,place)))

(defmacro pop (place)
  `(let ((_result (car ,place)))
     (set! ,place (cdr ,place))
     _result))

(set 'run #f)
(set 'run-next #f)

(let ((amb-stack '()))

  (define (run-next)
      (if (nil? amb-stack)
          (print 'exhausted)
          (let* ((top (pop amb-stack))
                 (vals (car top))
                 (fn   (cdr top)))
            (if (nil? vals)
                (run-next)
                (let ((val (pop vals)))
                  (push (cons vals fn) amb-stack)
                  (let ((r (fn val)))
                    r))))))

  (define (assert bool)
      (when (not bool)
        (snapshot-to-stack-mark 'run run-next)))

  (define (run fn)
      (set-stack-mark 'run)
    (let ((it (fn)))
      (when (continuation? it)
        (if (eq (continuation-value it) run-next)
            (run run-next)
            ;; we have a new amb
            (let ((vals (continuation-value it))
                  (resume (lambda (val)
                            (let ((r (resume-stack-snapshot it val)))
                              r))))
              (push (cons vals resume) amb-stack)
              (let ((r (run run-next)))
                r)))))))



(define amb
    (lambda args
     (snapshot-to-stack-mark 'run args)))

(run (lambda ()
       (let* ((a (amb 1 2 3))
              (b (amb 4 5 6))
              (c (*i a b)))
         (print `(trying values a = ,a and b = ,b))
         (assert (>i c 15))
         (print `(found a number ,a * ,b = ,c which is greater than 15)))))


'done
