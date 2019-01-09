(define (test-snap0)
    (print (list '=> (snapshot-to-stack-mark 'test) '<=)))

(define (test-mark0)
    (set-stack-mark 'test)
  (let ((result (test-snap0)))
    result))

(define (test0)
    (let ((snapshot (test-mark0))
          (result '()))
      (set! result
            (resume-stack-snapshot snapshot 4))
      result))

(print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
(print (test0))
(print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")

(define (test1)
    (let ((snapshot (test-mark0))
          (result '()))
      (set! result
            (mapcar (lambda (x)
                      (resume-stack-snapshot snapshot x))
                    '(1 2 3 4 5)))
      result))

(print "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
(print (test1))
(print "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
