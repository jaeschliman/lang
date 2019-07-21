;; repro steps
;; build an image with ./build/amber cmdline.lisp
;; run this file with said image: ./build/img ./cmdline.amb ./bugs/1.lisp
;; observe some kind of stack corruption

(fork-with-priority 100 (forever (+ 2 2) (sleep-ms 10)))
