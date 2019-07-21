(defmacro export (& syms)
  (let ((exports (mapcar (lambda (s) `(package-extern-symbol *package* ',s)) syms)))
    `(let () ,@exports)))

(export let lambda define set-symbol-value binding set! *package* defparameter)

(export defmacro gensym intern)

(export
 eq if not when unless cond case)

(export
 list cons car cdr nil? nth
 first second third)

(export
 make-ht make-st ht-at ht-at-put)

(export
 make-array aget aset)

(export
 = <= >= * / + - i->f f->i)

'done
