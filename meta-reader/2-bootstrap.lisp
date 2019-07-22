(define (match-1 rule string)
    (let* ((stream (make-stream string))
           (state  (make-initial-state stream))
           (fn (get-rule rule))
           (newstate (fn state)))
      (state-result newstate)))

(define (match-map xf rule string)
    (let* ((stream (make-stream string))
           (state  (make-initial-state stream))
           (fn (get-rule rule))
           (loop #f))
      (set! loop
            (lambda (state results)
              (let ((newstate (fn state)))
                (if (failure? newstate) (reverse-list results)
                    (let ((newresults (cons (xf (state-result newstate)) results)))
                      (if (stream-end? (state-stream newstate)) (reverse-list newresults)
                          (loop newstate newresults)))))))
      (loop state '())))

(define (match-all rule string)
    (match-map identity rule string))

(define (meta1-runfile path)
    (let ((input (slurp path)))
      (binding ((*meta-context* (list 'Meta)))
               (match-map eval 'meta-main input))))

(meta1-runfile "./meta-reader/meta-lisp-reader.lisp")
(meta1-runfile "./meta-reader/meta-meta-reader.lisp")

(define (load path)
    (let ((input (slurp path)))
      (binding ((*meta-context* (list 'meta)))
               (match-map eval 'main input))))

;; (binding ((*meta-context* (list 'lisp)))
;;   (match-map print 'expr (slurp "./cow-storm.lisp")))
;; (binding ((*meta-context* (list 'meta)))
;;   (match-map print 'main (slurp "./cow-storm.lisp")))

;; (binding ((*meta-context* (list 'meta)))
;;   (match-map print 'main (slurp "./meta-lisp-reader.lisp")))

;; (binding ((*meta-context* (list 'meta)))
;;   (match-map eval 'main (slurp "./meta-reader/meta-meta-reader.lisp")))
;; (binding ((*meta-context* (list 'meta)))
;;   (match-map print 'main (slurp "./meta-reader/meta-meta-reader.lisp")))
;; (print "----------------------------------------")
;; (binding ((*meta-context* (list 'meta)))
;;   (match-map eval 'main (slurp "./meta-reader/meta-meta-reader.lisp")))
;; (binding ((*meta-context* (list 'meta)))
;;   (match-map print 'main (slurp "./meta-reader/meta-meta-reader.lisp")))

;; use as default reader for the repl
;; (define (run-string input)
;;   (binding ((*meta-context* (list 'Meta)))
;;     (match-map eval 'meta-main input)))

'done
