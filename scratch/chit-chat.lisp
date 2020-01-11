(set '*package* (symbol-package 'define))

(define %message-package (%make-package "messages"))
(package-add-subpackage %root-package %message-package "st")

(define (as-message-send charlist)
  (let ((r (intern (charlist-to-string charlist) %message-package)))
    (print r)
    r))

(define (rulename rule) (intern (symbol-name rule) %meta-package))

(define (compose-msg-send rcvr parts)
  (let ((msg (as-message-send (reduce-list (lambda (acc pair)
                                             (append acc (car pair)))
                                           '() parts)))
        (args (mapcar cdr parts)))
    `(send ',msg ,rcvr ,@args)))

(define (compose-hdr parts)
  (let ((msg (as-message-send (reduce-list (lambda (acc pair)
                                     (append acc (car pair)))
                                   '() parts)))
        (args (mapcar cdr parts)))
    (list :name msg :args args)))

(print 'begin)

(import-file! "./scratch/chit-chat.grammar.lisp")

(print 'done-meta)

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


(import-file! "./scratch/chit-chat.compiler.lisp")

(dbgx 'file-in "
Array>>first [ ^ self at: 0 ]
String>>first [ ^ self at: 0 ]
LazyTable>>at:x put:y [
  storage ifNil: [ storage := HashTable new ].
  ^ storage at: x put:y
]
Cons>>collect: block [ ^ `(mapcar (lambda (it) (@send '#/st/value: #/st/block it)) self) ]
Fixnum>>pi [ ^ `*pi* ]
")


(dbge 'file-in "
Fixnum>>+ other [ ^ `(+ self #/st/other) ]
Closure>>value [ ^ `(self) ]
Boolean>>ifTrue: then ifFalse: else [
 ^ `(if self (@send '#/st/value #/st/then) (@send '#/st/value #/st/else))
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

(print (@send '#/st/+ 2 3))
(print (@send '#/st/value (lambda () 10)))
(print (@send '#/st/isEven 4))
(print '#/st/ifTrue:ifFalse:)
(let ((even? (@send '#/st/isEven 4)))
  (print `(even? ,even?))
  (print (@send '#/st/ifTrue:ifFalse: even? (lambda () "ok") (lambda () "not ok"))))
(print (@send '#/st/ifTrue:ifFalse: (@send '#/st/isEven 4) (lambda () "ok") (lambda () "not ok")))
(print (@send '#/st/aCheck 4))
(print `(expecting early exit))
(print (@send '#/st/anotherCheck 4))
(print `(expecting was not even))
(print (@send '#/st/anotherCheck 3))

(print 'done)
