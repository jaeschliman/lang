(use-package :xvec "./scratch/xvec-pkg.lisp")

(define *x (xvec/make-xvec))
(define *read-stream (xvec/make-meta-xvec-input *x))

(print-object '(+ 2 2) *x)
(print (parse-input 'meta *read-stream))

(xvec/xvec-reset! *x)

(print-object '(+ 3 4) *x)
(print (parse-input 'meta *read-stream))

(xvec/xvec-reset! *x)

(print-object '(lambda (x) x) *x)
(print (parse-input 'meta *read-stream))
