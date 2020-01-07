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
         (state  (make-initial-state stream)))
    (let loop ((state state))
         (let* ((fn (get-rule rule))
                (newstate (fn state)))
           (if (failure? newstate) (throw `(syntax error in ,*current-file* at ,(state-col-row state)))
               (let ((r (binding ((*match-start* state)
                                  (*match-end*  newstate))
                          (xf (state-result newstate)))))
                 (if (stream-end? (state-stream newstate)) r
                     (loop newstate))))))))

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

(at-boot (%print `(loading meta lisp reader)))
(at-boot (meta1-runfile "./meta-reader/meta-lisp-reader.lisp"))
(at-boot (%print `(loading meta meta reader)))
(at-boot (meta1-runfile "./meta-reader/meta-meta-reader.lisp"))

(at-boot (define meta-entry-point (intern "main" %meta-package)))
(at-boot (define meta-meta-name 'meta))

(define (parse-input-from-position meta-name meta-input position)
  (let* ((stream (make-meta-stream-from-input meta-input position))
         (state  (make-initial-state stream)))
    (binding ((*meta-context* (list meta-name)))
      (let* ((fn (get-rule meta-entry-point))
             (newstate (fn state)))
        (if (failure? newstate) newstate
            (state-result newstate))))))

(define (parse-input meta-name meta-input)
  (parse-input-from-position meta-name meta-input 0))

(define (parse-failure? result) (failure? result))

(define (%load path)
  ;; (%print path)
  (let ((input (slurp path)))
    (binding ((*current-file* path)
              (*meta-context* (list meta-meta-name)))
      ;; TODO: we don't need to keep the results
      (match-map *load-evaluator* meta-entry-point input))))

(when *recompiling*
  (%print `(reloading meta lisp reader))
  (%load "./meta-reader/meta-lisp-reader.lisp")
  (%print `(reloading meta meta reader))
  (%load "./meta-reader/meta-meta-reader.lisp"))

(define (load-as name path)
  (let ((exist (package-find-subpackage *package* name)))
    (if (nil? exist)
        (let ((pkg (make-user-package "anon")))
          (package-add-subpackage *package* pkg name)
          (binding ((*package* pkg))
            (%load path)))
        (binding ((*package* exist)) (%load path)))))

(define (import-file! path) (binding ((*package* *package*)) (%load path)))

(define *required-files (make-ht))

(at-boot (define *module-package (make-user-package "anon")))
(define (use-package symbol path)
  (let ((module-name (intern path *module-package)))
    (when (nil? (ht-at *required-files module-name))
      (let ((pkg (make-user-package "anon")))
        (binding ((*package* pkg)) (import-file! path))
        (ht-at-put *required-files module-name pkg)))
    (package-add-subpackage *package* (ht-at *required-files module-name) (symbol-name symbol))))

(ht-at-put meta-by-name 'Meta '())
(ht-at-put meta-by-name 'Lisp '())

;; use as default reader for the repl
;; (define (run-string input)
;;   (binding ((*meta-context* (list 'Meta)))
;;     (match-map eval 'meta-main input)))

'done
