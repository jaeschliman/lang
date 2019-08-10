(defparameter *current-file*)

(define (eval-with-source-location x)
    (let* ((pos (state-col-row *match-start*)))
      (binding ((*source-location* (list (if-nil? *current-file* "<anonymous>" *current-file*)
                                         (car pos) (cdr pos))))
        (eval x))))

(defparameter *load-evaluator* eval-with-source-location)

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
                (if (failure? newstate) (throw `(syntax error in ,*current-file* at ,(state-col-row state)))
                    (let ((newresults (binding ((*match-start* state)
                                                (*match-end*  newstate))
                                        (cons (xf (state-result newstate)) results))))
                      (if (stream-end? (state-stream newstate)) (reverse-list newresults)
                          (loop newstate newresults)))))))
      (loop state '())))

(define (match-all rule string)
    (match-map identity rule string))

;; (define (%source-location-string)
;;     (let ((pos (state-col-row *match-start*)))
;;       (with-output-to-string (s)
;;         (stream-write-string s (if-nil? *current-file* "<anonymous>" *current-file*))
;;         (stream-write-char s #\:)
;;         (print-object (car pos) s)
;;         (stream-write-char s #\:)
;;         (print-object (cdr pos) s))))

;; prints results in a format compatible with compilation-mode
;; (define (%eval-printing-source-location x)
;;     (stream-write-string *standard-output* (%source-location-string))
;;   (stream-write-char *standard-output* #\Newline)
;;   (eval x))

(define (meta1-runfile path)
    (let ((input (slurp path)))
      (binding ((*current-file* path)
                (*meta-context* (list 'Meta)))
               (match-map eval 'meta-main input))))

(meta1-runfile "./meta-reader/meta-lisp-reader.lisp")
(meta1-runfile "./meta-reader/meta-meta-reader.lisp")

(define (%load path)
    ;; (%print path)
    (let ((input (slurp path)))
      (binding ((*current-file* path)
                (*meta-context* (list 'meta)))
               ;; TODO: we don't need to keep the results
               (match-map *load-evaluator* 'main input))))

(define (load-as name path)
    (let ((pkg (make-user-package "anon")))
      (package-add-subpackage *package* pkg name)
      (binding ((*package* pkg))
        (%load path))))

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
