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
  (#/lang/%print (#/lang/+b n n)))

(print '-------------------------------)

(let ((n (* 2 -500000000000000000)))
  (#/lang/%print n)
  (#/lang/%print (#/lang/+b n n)))

(print '-------------------------------)

(let ((a (* 2 -500000000000000000))
      (b (* 2 -500000000000000001)))
  (#/lang/%print a)
  (#/lang/%print b)
  (#/lang/%print (#/lang/+b a b)))

(print '-------------------------------)

(let ((a (* 2 -500000000000000000)))
  (#/lang/%print a)
  (#/lang/%print 'negated)
  (#/lang/%print (#/lang/%negate-bignum a)))

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

(print 'done)

