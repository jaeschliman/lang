(defmacro repeat (count & forms)
  `(let ((loop #f))
     (set! loop (lambda (c) ,@forms
                        (when (>i c 0)
                          (loop (-i c 1)))))
     (loop ,count)))

(defmacro repeat-slowly (delay count & forms)
  `(repeat ,count ,@forms (sleep-ms ,delay)))

(fork (repeat-slowly 200 10 (print 'background)))
;; (+f 1.0 3) ;; kills main thread now
;; (throw 'goodbye!) ;; kills main thread now
;; (kill-thread (current-thread)) ;; no result
(print `(should never see this))
