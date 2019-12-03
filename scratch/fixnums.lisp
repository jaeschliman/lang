(print (* 10 10))
(print 576460752303423487)
(print "576460752303423487")
(print -1)
(print -576460752303423487)

;; (let ((n -576460752303423487))
;;   (try-catch (lambda () (* n n))
;;              (lambda (ex) (print `(got expected exception: ,ex))))
;;   (try-catch (lambda () (* n 2))
;;              (lambda (ex) (print `(got expected exception: ,ex))))
;;   (try-catch (lambda () (* n 3))
;;              (lambda (ex) (print `(got expected exception: ,ex))))
;;   (try-catch (lambda () (* n 4))
;;              (lambda (ex) (print `(got expected exception: ,ex))))
;;   'ok)

;; (let ((n 576460752303423487))
;;   (try-catch (lambda () (* n n))
;;              (lambda (ex) (print `(got expected exception: ,ex))))
;;   (try-catch (lambda () (* n 2))
;;              (lambda (ex) (print `(got expected exception: ,ex))))
;;   (try-catch (lambda () (* n 3))
;;              (lambda (ex) (print `(got expected exception: ,ex))))
;;   (try-catch (lambda () (* n 4))
;;              (lambda (ex) (print `(got expected exception: ,ex))))
;;   'ok)

(#/lang/%print 500000000000000000)
(#/lang/%print (* 2 500000000000000000))
(#/lang/%print (* 2 576460752303423487))
(#/lang/%print (* 2 -576460752303423487))
(let ((n 576460752303423487))
  (#/lang/%print (+ 2 n))
  (#/lang/%print (+ n n)))

(let ((n -500000000000000000))
  (#/lang/%print (* 2 n))
  (#/lang/%print (* -2 n))
  (#/lang/%print n)
  (#/lang/%print (* n n)))

(print '-------------------------------)

(let ((n (* 2 500000000000000000)))
  (#/lang/%print n)
  (print 'self-add)
  (#/lang/%print (#/lang/+b n n)))

(print '-------------------------------)

(let ((n (* 2 -500000000000000000)))
  (#/lang/%print n)
  (print 'self-add)
  (#/lang/%print (#/lang/+b n n)))

(print '-------------------------------)

(let ((a (* 2 -500000000000000000))
      (b (* 2 -500000000000000001)))
  (#/lang/%print a)
  (#/lang/%print b)
  (print 'summed)
  (#/lang/%print (#/lang/+b a b)))

(print '-------------------------------)

(let ((a (* 2 -500000000000000000)))
  (#/lang/%print a)
  (#/lang/%print 'negated)
  (#/lang/%print (#/lang/%negate-bignum a))
  (#/lang/%print (#/lang/%negate-bignum (#/lang/%negate-bignum a))))

(print '-------------------------------)

(let ((a (* 2 500000000000000000)))
  (#/lang/%print a)
  (#/lang/%print 'negated)
  (#/lang/%print (#/lang/%negate-bignum a))
  (#/lang/%print (#/lang/%negate-bignum (#/lang/%negate-bignum a))))

(print '-------------------------------)

(let ((a (* 2 512341234123412340)))
  (#/lang/%print a)
  (#/lang/%print 'negated)
  (#/lang/%print (#/lang/%negate-bignum a))
  (#/lang/%print (#/lang/%negate-bignum (#/lang/%negate-bignum a))))

(when #f
  (print '-------------------------------)

  (let ((a (* 2 -500000000000000000))
        (b (* 2  500000000000000000)))
    (#/lang/%print a)
    (#/lang/%print b)
    (#/lang/%print (#/lang/+b a b)))

  (print '-------------------------------)

  (let ((a (* 2 -500000000000000000))
        (b (* 2  500000000000000001)))
    (#/lang/%print a)
    (#/lang/%print b)
    (#/lang/%print (#/lang/+b a b)))

  (print '-------------------------------)

  (let ((a (* 2 -500000000000000001))
        (b (* 2  500000000000000000)))
    (#/lang/%print a)
    (#/lang/%print b)
    (#/lang/%print (#/lang/+b a b))))


(print '-------------------------------)
(let ((a (#/lang/i->b 5))
      (b (#/lang/i->b -7)))
  (#/lang/%print a)
  (#/lang/%print b)
  (#/lang/%print (#/lang/+b a b)))
(print '-------------------------------)

(print '-------------------------------)
(define (check a b)
  (let ((a (if (fixnum? a) (#/lang/i->b a) a))
        (b (if (fixnum? b) (#/lang/i->b b) b)))
    (#/lang/%print a)
    (#/lang/%print b)
    (#/lang/%print `((+ a b) = ,(#/lang/+b a b)))
    (#/lang/%print `((- a b) = ,(#/lang/-b a b))))
  (print '-------------------------------))

(check 1024 -2048)

(check (* 16 1024) (* 16 -2048))

(check (* 16 1024) (* 16 -2048))
(check (* 2 500000000) -5)
(check (* 2 500000000000000000) -5)

(check -3 -5)
(print `(+ 0xff 0xff = ,(+ 0xff 0xff)))
(check 0xff 0xff)

(define (check2 a b)
  (let ((a (if (fixnum? a) (#/lang/i->b a) a))
        (b (if (fixnum? b) (#/lang/i->b b) b)))
    (#/lang/%print a)
    (#/lang/%print b)
    (#/lang/%print `((+ a b) = ,(#/lang/+b a b)))
    (#/lang/%print `((* a b) = ,(#/lang/*b a b))))
  (print '-------------------------------))

(check2 2 3)
(check2 200 300)
(check2 200000 300000)
(check2 200000000 300000000)
(check2 -2 3)
(check2 200 -300)
(check2 -200000 300000)
(check2 200000000 -300000000)
(check2 -2 -3)
(check2 -200 -300)
(check2 -200000 -300000)
(check2 -200000000 -300000000)

(print '(reading a bignum:))
(#/lang/%print 50000000000000000000000001)

(print 'done)

