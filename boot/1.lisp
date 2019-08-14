(%load "./boot/better-define.lisp")
(%load "./boot/generic-functions.lisp")
(%load "./boot/math.lisp")
(%load "./boot/list.lisp")
(%load "./boot/string.lisp")
(%load "./boot/char.lisp")
(%load "./boot/string-output-stream.lisp")
(%load "./boot/printing.lisp")
(%load "./boot/interaction-support.lisp")
(%load "./boot/exports.lisp")

(when #t
  ;; load the new compiler
  (let ((%l (lambda (f) (print `(loading file ,f)) (%load f)))) 
    (%l "./scratch/compiler.lisp") 

    (binding ((*recompiling* #t) (*trace-eval* #f)
              )
             (%l "./scratch/compiler.lisp") ;; compiler, compile thyself
             ;; reload everything with the new compiler
             ;; (%load  "./boot/built-in-classes.lisp")
             (%l  "./boot/0.lisp")
             (%l  "./boot/0-package.lisp")
             (%l  "./meta-reader/0-compiler.lisp")
             ;; (%l  "./meta-reader/1-lisp-handwritten.lisp") ;; already using meta reader
             ;; (%l  "./meta-reader/1-meta-handwritten.lisp")
             (%l  "./meta-reader/2-bootstrap.lisp")
             ;; (%l  "./boot/1.lisp") ;; infinite loop
             (%l "./boot/better-define.lisp")
             (%l "./boot/generic-functions.lisp")
             ;; (%load "./boot/math.lisp") ;; no need
             (%l "./boot/list.lisp")
             (%l "./boot/string.lisp")
             (%l "./boot/char.lisp")
             (%l "./boot/string-output-stream.lisp")
             (%l "./boot/printing.lisp")
             (%l "./boot/interaction-support.lisp"))))
