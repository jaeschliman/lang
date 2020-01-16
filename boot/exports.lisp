(defmacro export (& syms)
  (let ((exports (mapcar (lambda (s) `(package-extern-symbol *package* ',s)) syms)))
    `(let () ,@exports)))

(export lang load-as ;; TODO: have load-as take a lang param
        meta lisp self)
(export import-file! use-package)
(export parse-input parse-failure? make-meta-string-input parse-input-from-position)

(export apply let lambda let* define symbol-value set-symbol-value binding set set!
        *meta-context* ;; TODO: unexport this
        *package* defparameter symbol-package forward)

(export defmacro gensym intern & &opt quasiquote-expand macroexpand eval lambda-bind
        special-symbol? mark-symbol-as-special with-special-binding symbol-bound?)

(export
 class? bool? closure? primop? float? fixnum? pair? nil? ht? array? symbol? char?
 image? point? continuation? string? semaphore? thread? package?)

(export dotimes)

(export
 create-class instantiate-class class-set-applicator instance-get-ivar instance-set-ivar
 class-has-slot?)

(export defgeneric generic-function-add-method)

(export make-user-package package-extern-symbol package-add-subpackage package-use-package)

(export reset-tag shift-tag)

(export
 eq if not when unless cond case and or)

(export
 list cons append nil? nth list-length
 first second third fourth
 mapcar reverse-list list-every
 dolist
 list->array
 plist-get)

(export
 car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr)


(export
 make-ht make-st ht-at ht-contains? ht-at-put)

(export
 make-array aget aset array->list array-length)

(export iget iset)

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
 char-code code-char char-at char-code-at char-array->string)

(export
 *standard-output* stream-write-char stream-write-string print-object print)

(export vector length)

(export
 fork fork-with-priority forever sleep-ms)

(export
 onmousemove onmousedown onmouseup onmousedrag onkey onshow request-display)

(export
 make-image load-image image-width image-height blit-to-screen screen-fill-rect fill-rect
 fill-rect-with-mask)

(export blit blitq blit-with-mask)

(export set-stack-mark snapshot-to-stack-mark resume-stack-snapshot)

(export continuation? continuation-value)

(export save-snapshot save-snapshot-and-exit)

(export kill-thread current-thread list-all-threads thread-count
        make-semaphore signal-semaphore semaphore-wait)

(export deep-eq? quote quasiquote unquote unquote-splicing)

(export try-catch throw)

(export newline *print-base* print-integer print-float)

(export start-additional-event-loop update-display)

(export Continuation Fixnum Float String Symbol Closure
 PrimOp Image Point Array HashTable Null Cons Boolean Character
 Semaphore Thread FileOutputStream Package Class Bignum)

(export exit)

'done
