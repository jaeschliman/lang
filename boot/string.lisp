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

'done
