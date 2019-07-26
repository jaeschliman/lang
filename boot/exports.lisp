(defmacro export (& syms)
  (let ((exports (mapcar (lambda (s) `(package-extern-symbol *package* ',s)) syms)))
    `(let () ,@exports)))

(export let lambda define set-symbol-value binding set set! *package* defparameter)

(export defmacro gensym intern)

(export
 eq if not when unless cond case)

(export
 list cons car cdr nil? nth
 first second third)

(export
 caar cadr cdar cddr caaar caddr cdaar cdddr caaar caddr cdaar cdddr)

(export
 make-ht make-st ht-at ht-at-put)

(export
 make-array aget aset)

(export
 = <= >= * / % + - i->f f->i)

(export
 +i -i +f -f /i /f %i %f *i *f)

(export
 sin cos tan floor ceil rem pow log abs)

(export
 *standard-output* stream-write-char stream-write-string print-object print)

(export
 fork fork-with-priority forever sleep-ms)

(export
 onmousemove onmousedown onmousedrag onkey onshow request-display)

(export
 load-image blit-to-screen screen-fill-rect)

'done
