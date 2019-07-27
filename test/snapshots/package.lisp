(define (export sym)
    (package-extern-symbol *package* sym))
(define (export-all syms)
    (mapcar export syms))


(define old-package *package*)
(define my-package (make-user-package "my package"))
;;TODO: add *package* to use-list of my-package

(export-all
 '(print
   unless
   eq
   define
   *package*
   set
   old-package))

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
