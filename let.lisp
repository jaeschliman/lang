;; a place to collect tricky bugs around let binding and closures etc.

(set-symbol-value 'test-let-binding-0
                  (lambda ()
                    (let ((x 'x-value))
                      (let ((f (lambda () x)))
                        x))))

(print (test-let-binding-0))
