(define (export sym)
    (package-extern-symbol *package* sym))

(define old-package *package*)
(define my-package (make-user-package "my package"))
(package-use-package my-package *package*)

(export 'old-package)

(define (myfun) (print "hello from lang!"))

(set '*package* my-package)

;; import works?
(print "hello, world")

;; macro import works?
(define (myfun) (print "hello from my package!"))
(myfun)

;; macro expansion to internal symbols of other package works?
(unless #f
  (print "expansion worked!"))

(set '*package* old-package)

;; we got our original definition back?
(myfun)

'done
