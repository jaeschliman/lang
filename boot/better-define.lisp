;; support for optional args in define
;; TODO: optional argc checking
;; TODO: support for & (rest) bindings
;; eventually we can move this behavior into lambda itself

(define (%parse-define-arglist args)
    (let ((normals '())
          (optionals '())
          (saw-opt #f))
      (dolist (s args)
        (cond (saw-opt (set! optionals (cons s optionals)))
              ((eq s '&opt) (set! saw-opt #t))
              (#t (set! normals (cons s normals)))))
      (list (reverse-list normals) (reverse-list optionals))))

(define (%prepare-optional-arg-bindings opts initial-idx)
    (let ((i (-i initial-idx 1))
          (argc (gensym)))
      (cons `(,argc (%argument-count))
            (mapcar (lambda (b)
                      (let ((default (if (pair? b) (second b) '()))
                            (var (if (pair? b) (first b) b)))
                        (set! i (+i 1 i))
                        `(,var (if (>i ,argc ,i) (%load-arg ,i) ,default))))
                    opts))))

(define (%transform-define head body)
    (if (symbol? head) `(set-symbol-value ',head ,@body)
        (let* ((name (car head))
               (args (%parse-define-arglist (cdr head)))
               (norms (first args))
               (opts (second args)))
          (if (nil? opts) `(set-symbol-value ',name (%nlambda ,name ,norms ,@body))
              (let ((opt-bindings (%prepare-optional-arg-bindings opts (list-length norms))))
                `(set-symbol-value ',name (%nlambda ,name ,norms
                                                    ;; XXX hack to identify lambdas with optional
                                                    ;; arguments. this method won't last, since
                                                    ;; we'll eventually optimize this out.
                                                    '%%has-optionals
                                                    (let* ,opt-bindings ,@body))))))))

(defmacro define (head & body) (%transform-define head body))

'done
