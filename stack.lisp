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

(define (id x) x)

(define (call-with-tag tag body handler)
    (set-stack-mark tag)
    (let* ((snapshot (body)) ;; TODO: how to tell if it is a snapshot or normal value?
           (resume (lambda (v)
                     ;; FIXME: appears to be necc. to avoid TCO when resuming
                     (let ((r (resume-stack-snapshot snapshot v)))
                       r)))
           (result (handler resume)))
      result))


(define (test2)
    (print
     (call-with-tag
      'foo
      (lambda ()
        ;; would be nice if we could pass value along with 'foo
        (+i 7 (snapshot-to-stack-mark 'foo))
        )
      (lambda (k) (k 6)))))

(print "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
(print (test2))
(print "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")

(define mapcar2 #f)
(define (mapcar2 f lst)
    (let ((mark 'mapcar2))
      (if (nil? lst) lst
          (let* ((hd (f (car lst)))
                 (tl (mapcar2 f (cdr lst))))
            (cons hd tl)))))

(define (test3)
    (print
     (call-with-tag
      'foo
      (lambda ()
        (let ((r '()))
          (set! r (mapcar2 (lambda (v)
                          ;; would be nice if we could pass value along with 'foo
                             (let ((m 'hello)
                                   (r '()))
                               (set! r 
                                     (print
                                      ;;let ((x 'hello))
                                      (if (eq v 3) (snapshot-to-stack-mark 'foo)
                                          v)))
                               r)
                          )
                          '(1 2 3 4 5)))
          r))
      (lambda (k) (k 6)))))

(print "++++++++++++++++++++++++++++++++++++++++++++++++++")
(print (test3))
(print "++++++++++++++++++++++++++++++++++++++++++++++++++")

(define mapcar3 #f)
(define (mapcar3 f lst)
    (let ((mark 'mapcar3))
      (if (nil? lst) lst
          (cons (f (car lst)) (mapcar3 f (cdr lst))))))

(define (test4)
    (print
     (call-with-tag
      'foo
      (lambda ()
        (mapcar3 (lambda (v)
                   (if (eq v 3) (snapshot-to-stack-mark 'foo)
                       v))
                 '(1 2 3 4 5)))
      (lambda (k) (k 6)))))

(print "++++++++++++++++++++++++++++++++++++++++++++++++++")
(print (test4))
(print "++++++++++++++++++++++++++++++++++++++++++++++++++")

(define (test5)
    (print
     (call-with-tag
      'foo
      (lambda ()
        (mapcar3 (lambda (v)
                   (if (eq v 3) (snapshot-to-stack-mark 'foo)
                       v))
                 '(1 2 3 4 5)))
      (lambda (k)
        (list
         (list (k 6) (k 7) (k 8))
         (mapcar k '(6 7 8))
         (list (k 6) (k 7) (k 8)))))))

(print "++++++++++++++++++++++++++++++++++++++++++++++++++")
(print (test5))
(print "++++++++++++++++++++++++++++++++++++++++++++++++++")
