;; smoke test built-ins for special variable support. needs tests for multiple threads.

(define *my-var* 10)
(print `(expecting 10))
(print *my-var*)

(mark-symbol-as-special '*my-var*)
(print `(expecting 10))
(print *my-var*)

(defparameter *my-other-var* 20)
(defparameter *my-third-var* 30)

(print `(expecting 20))
(print (with-special-binding *my-var* 20 *my-var*))

(define (show-my-var) (print *my-var*) 'done)

(print `(expecting 10 done))
(print (show-my-var))

(print `(expecting 20 done))
(print (with-special-binding *my-var* 20 (show-my-var)))

(print `(expecting 10 done))
(print (show-my-var))

(print `(expecting 20 30 40 10 done))
(print (let ()
         (with-special-binding *my-var* 20 (show-my-var))
         (with-special-binding *my-var* 30 (show-my-var))
         (with-special-binding *my-var* 40 (show-my-var))
         (show-my-var)))

(print `(expecting 10 done))
(print (show-my-var))

(print `(expecting 10))
(try-catch (lambda ()
             (with-special-binding *my-var* 20 (throw 'whoops)))
           (lambda (ex)
             (print *my-var*)))

(print `(expecting 10))
(print *my-var*)

(print `(expecting 20 30 40 10))
(let ()
  (with-special-binding
      *my-var* 40
      (try-catch (lambda ()
                   (with-special-binding *my-var* 30
                                         (let ()
                                           (try-catch
                                            (lambda ()
                                              (with-special-binding
                                                  *my-var* 20
                                                  (let ()
                                                    (show-my-var)
                                                    (throw 'ouch))))
                                            (lambda (ex)
                                              (show-my-var)
                                              (throw 'oopsy))))))
                 (lambda (ex)
                   (show-my-var))))
  (show-my-var))



(define call (reset-tag 't (with-special-binding
                            *my-var* 'my-var
                            (let ()
                              (shift-tag 't k k)
                              (list *my-var* *my-other-var* *my-third-var*)))))

(print `(expecting my-var 20 30))
(print (call))

(print `(expecting my-var my-other-var 30))
(print (with-special-binding *my-other-var* 'my-other-var (call)))

(define call2 (reset-tag 't (with-special-binding
                                *my-var* 'should-not-see
                                (let ()
                                  (shift-tag 't k k)
                                  (call)))))

(print `(expecting my-var 20 30))
(print (call2))

(print `(expecting my-var my-other-var 30))
(print (with-special-binding *my-other-var* 'my-other-var (call2)))

(define (call3) (with-special-binding *my-other-var* 'my-other-var (call2)))
(define (call4) (with-special-binding *my-third-var* 'my-third-var (call3)))

(print `(expecting my-var my-other-var my-third-var))
(print (call4))

(print `(expecting 80))
(binding ((*my-var* 80))
         (print *my-var*))

(print `(expecting (90 100)))
(binding ((*my-var* 90)
          (*my-other-var* 100))
         (print (list *my-var* *my-other-var*)))
