(set '*package* (symbol-package 'define))

(define (dbg rule str)
  (stream-write-string *standard-output* str)
  (newline)
  (try-catch (lambda () 
               (binding ((*meta-context* '(chitchat)))
                        (match-map print (rulename rule) str)))
             (lambda (ex) (print `(whoops! ,ex))))
  (stream-write-string *standard-output* "================================\n"))

(define (dbgx rule str)
  (stream-write-string *standard-output* str)
  (newline)
  (try-catch (lambda ()
                (binding ((*meta-context* '(chitchat)))
                         (match-map (lambda (e) (print (compiler e))) (rulename rule) str)))
             (lambda (ex) (print `(whoops! ,x))))

  (stream-write-string *standard-output* "================================\n"))

(define (dbge rule str)
  (stream-write-string *standard-output* str)
  (newline)
  (binding ((*meta-context* '(chitchat)))
    (match-map eval (rulename rule) str))
  (stream-write-string *standard-output* "================================\n"))

(dbg 'expr "1")
(dbg 'expr "(1)")
(dbg 'expr "1 + 2")
(dbg 'expr "(1 + 2)")
(dbg 'expr "1 + 2 + 3")
(dbg 'expr "1 + (2 + 3)")
(dbg 'expr "(1 + 2) + 3")
(dbg 'expr "foo print")
(dbg 'expr "foo printWith: 10")
(dbg 'expr "baz do: (x + 5) with: Color blue")
(dbg 'stmt "^ x")
(dbg 'stmt "^ x + 2")
(dbg 'expr "[ x + 2 ]")
(dbg 'expr "[ ^ x ]")
(dbg 'body " 
     x := AThing becomeRelevant.
     y := self fooWith: x.
     self frobnicate: [ ^ y ]
")
(dbg 'body " |x y|
     x := AThing becomeRelevant.
     y := self fooWith: x.
     self frobnicate: [ :arg | self fooWith:arg. ^ y ]
")
(dbg 'body "
     | y |
     y := 5.
     ^ [ :x | x + y ]
")

(define LazyTable (create-class 'LazyTable '(storage)))

(dbgx 'file-in "
Array>>first [ ^ self at: 0 ]
String>>first [ ^ self at: 0 ]
LazyTable>>at:x put:y [
  storage ifNil: [ storage := HashTable new ].
  ^ storage at: x put:y
]
Cons>>collect: block [ ^ `(mapcar (lambda (it) (@send block it '#/st/value:)) self) ]
Fixnum>>pi [ ^ `*pi* ]
")


(dbge 'file-in "
Fixnum>>+ other [ ^ `(+ self other) ]
Closure>>value [ ^ `(self) ]
Boolean>>ifTrue: then ifFalse: else [
 ^ `(if self (@send then '#/st/value) (@send else '#/st/value))
]
Fixnum>>isEven [ ^ `(= 0 (% self 2)) ]
Fixnum>>aCheck [
  (self isEven) ifTrue: [ ^ 'yes, even' ] ifFalse: [ ^ 'no, not even' ]
]
Fixnum>>callIfEven: block [
  (self isEven) ifTrue: [ block value ] ifFalse: [ false ]
]
Fixnum>>anotherCheck [
  self callIfEven: [ ^ 'early exit' ].
  ^ 'was not even'
]
")

(defmacro ! (msg & args)
  `(@send ,@args ',msg))

(print (! #/st/+ 2 3))
(print (! #/st/value (lambda () 10)))
(print (! #/st/isEven 4))
(print '#/st/ifTrue:ifFalse:)
(let ((even? (! #/st/isEven 4)))
  (print `(even? ,even?))
  (print (! #/st/ifTrue:ifFalse: even? (lambda () "ok") (lambda () "not ok"))))
(print (! #/st/ifTrue:ifFalse: (! #/st/isEven 4) (lambda () "ok") (lambda () "not ok")))
(print (! #/st/aCheck 4))
(print `(expecting early exit))
(print (! #/st/anotherCheck 4))
(print `(expecting was not even))
(print (! #/st/anotherCheck 3))

(print 'done)
