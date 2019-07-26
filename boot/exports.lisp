(defmacro export (& syms)
  (let ((exports (mapcar (lambda (s) `(package-extern-symbol *package* ',s)) syms)))
    `(let () ,@exports)))

(export apply let lambda let* define set-symbol-value binding set set! *package* defparameter symbol-package)

(export defmacro gensym intern & &opt)

(export
 eq if not when unless cond case and or)

(export
 list cons append nil? nth
 first second third)

(export
 car cdr caar cadr cdar cddr caaar caddr cdaar cdddr caaar caddr cdaar cdddr)

(export
 make-ht make-st ht-at ht-at-put)

(export
 make-array aget aset)

(export
 make-point point-x point-y point+ point- point-rotate)

(export
 = <= >= * / % + - i->f f->i)

(export
 +i -i +f -f /i /f %i %f *i *f >i >f <i <f)

(export
 sin cos tan floor ceil rem pow log abs)

(export
 *standard-output* stream-write-char stream-write-string print-object print)

(export
 fork fork-with-priority forever sleep-ms)

(export
 onmousemove onmousedown onmousedrag onkey onshow request-display)

(export
 make-image load-image image-width image-height blit-to-screen screen-fill-rect fill-rect)

(export set-stack-mark snapshot-to-stack-mark resume-stack-snapshot)

(export continuation? continuation-value)

(export save-snapshot save-snapshot-and-exit)

(export kill-thread current-thread)

'done
