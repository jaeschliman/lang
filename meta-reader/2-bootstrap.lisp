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
