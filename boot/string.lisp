(set 'string-byte-length string-length) ;; TODO replace uses of string-length

(defmacro string-do-chars (binding & body)
  (let ((loop (gensym))
        (str (gensym))
        (idx (gensym))
        (max (gensym))
        (init-str (gensym))
        (ch  (car binding)))
    `(let ((,init-str ,(cadr binding))
           (,loop #f))
       (set! ,loop (lambda (,str ,idx ,max)
                     (when (<i ,idx ,max)
                       (let ((,ch (char-at ,str ,idx)))
                         ,@body
                         (,loop ,str (+i (char-width ,ch) ,idx) ,max)))))
       (,loop ,init-str 0 (string-byte-length ,init-str)))))

(define (charlist-to-string lst-of-chars)
    (let* ((byte-len (reduce-list (lambda (acc ch) (+i acc (char-width ch))) 0 lst-of-chars))
           (str (make-string byte-len #\Space)))
      (reduce-list (lambda (idx chr)
                     (char-at-put str idx chr)
                     (+i (char-width chr) idx))
                   0 lst-of-chars)
      str))

(define (string-to-charlist str)
    (let ((result '()))
      (string-do-chars (ch str)
         (set! result (cons ch result)))
      (reverse-list result)))

(define (implode lst-of-chars) (intern (charlist-to-string lst-of-chars) *package*))


'done
