(defmacro export (& syms)
  (let ((exports (mapcar (lambda (s) `(package-extern-symbol *package* ',s)) syms)))
    `(let () ,@exports)))

(export apply let lambda let* define set-symbol-value binding set set!
        *package* defparameter symbol-package forward)

(export defmacro gensym intern & &opt quasiquote-expand macroexpand eval lambda-bind
        special-symbol? mark-symbol-as-special with-special-binding symbol-bound?)

(export
 class? bool? closure? primop? float? fixnum? pair? nil? ht? array? symbol? char?
 image? point? continuation? string? semaphore? thread? package?)

(export dotimes)

(export
 create-class instantiate-class class-set-applicator instance-get-ivar instance-set-ivar)

(export defgeneric generic-function-add-method)

(export make-user-package package-extern-symbol package-add-subpackage package-use-package)

(export reset-tag shift-tag)

(export
 eq if not when unless cond case and or)

(export
 list cons append nil? nth
 first second third fourth
 mapcar reverse-list list-every
 dolist
 list->array
 plist-get)

(export
 car cdr caar cadr cdar cddr caaar caddr cdaar cdddr caaar caddr cdaar cdddr)

(export
 make-ht make-st ht-at ht-at-put)

(export
 make-array aget aset array->list)

(export
 make-array-u16 aget-u16 aset-u16 array-length-u16)

(export
 bytecode->closure make-bytecode)

(export
 point? make-point point-x point-y point+ point- point-rotate)

(export
 = < > <= >= * / % + - i->f f->i)

(export
 +i -i +f -f /i /f %i %f *i *f >i >f <i <f)

(export
 sin cos tan floor ceil rem pow log abs atan2f sqrtf random)

(export ash bit-and bit-or)

(export
 string-do-chars string-equal string->list
 string-byte-length string->char-array string-substr-bytes
 with-output-to-string make-string-output-stream string-output-stream-get-string)

(export
 char-code char-at char-code-at char-array->string)

(export
 *standard-output* stream-write-char stream-write-string print-object print)

(export vector length)

(export
 fork fork-with-priority forever sleep-ms)

(export
 onmousemove onmousedown onmousedrag onkey onshow request-display)

(export
 make-image load-image image-width image-height blit-to-screen screen-fill-rect fill-rect)

(export blit blitq)

(export set-stack-mark snapshot-to-stack-mark resume-stack-snapshot)

(export continuation? continuation-value)

(export save-snapshot save-snapshot-and-exit)

(export kill-thread current-thread list-all-threads thread-count
        make-semaphore signal-semaphore semaphore-wait)

(export deep-eq? quote quasiquote unquote unquote-splicing)

(export try-catch throw)

(export newline *print-base* print-integer print-float)

(export start-additional-event-loop update-display)

(export exit)

'done
