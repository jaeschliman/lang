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

(define (call-with-tag tag body handler)
    (set-stack-mark tag)
    (let* ((snapshot (body)) ;; TODO: how to tell if it is a snapshot or normal value?
           (resume (lambda (v) (resume-stack-snapshot snapshot v)))
           (result (handler resume)))
      result))


(define (test2)
    (print
     (call-with-tag
      'foo
      (lambda ()
        ;; (mapcar (lambda (v)
        ;;           ;; would be nice if we could pass value along with 'foo
        ;;           (if (eq v 3) (snapshot-to-stack-mark 'foo)
        ;;               v))
        ;;         '(1 2 3 4 5))
        (+i 7 (snapshot-to-stack-mark 'foo))
        )
      (lambda (k) (k 6)))))

(print "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
(print (test2))
(print "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")

;;; Bummer, this doesn't work yet.
(define (test3)
    (print
     (call-with-tag
      'foo
      (lambda ()
        (mapcar (lambda (v)
                  ;; would be nice if we could pass value along with 'foo
                  (if (eq v 3) (snapshot-to-stack-mark 'foo)
                      v))
                '(1 2 3 4 5))
        ;; (+i 7 (snapshot-to-stack-mark 'foo))
        )
      (lambda (k) (k 6)))))

(print "++++++++++++++++++++++++++++++++++++++++++++++++++")
(print (test3))
(print "++++++++++++++++++++++++++++++++++++++++++++++++++")
