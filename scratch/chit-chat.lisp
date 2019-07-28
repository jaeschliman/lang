(set '*package* (symbol-package 'define))

(define (compose-msg-send rcvr parts)
  (let ((msg (implode (reduce-list (lambda (acc pair)
                                     (append acc (car pair)))
                                   '() parts)))
        (args (mapcar cdr parts)))
    `(send ',msg ,rcvr ,@args)))

(define (compose-hdr parts)
  (let ((msg (implode (reduce-list (lambda (acc pair)
                                     (append acc (car pair)))
                                   '() parts)))
        (args (mapcar cdr parts)))
    (list :name msg :args args)))

(print 'begin)

meta chitchat {
  escaped-char = "\\" any:x -> x
  comment      = "\"" (~[\"\\] any | escaped-char)* "\"" 
  string       = "'" (~['\\] any | escaped-char)*:chs "'" -> (charlist-to-string chs)
  ws           = ([ \r\n] | comment)*
  binop-ident  = "+" -> '+ | "-" -> '- | "/" -> '/ | "*" -> '*
  unary-ident  = [a-zA-Z]:fst [a-zA-Z0-9]*:rst -> (implode (cons fst rst))
  nary-part    = [a-zA-Z]:fst [a-zA-Z0-9]*:rst ":" -> (append (cons fst rst) '(#\:))
  keyword      = "self" -> 'self | "true" -> #t | "false" -> #f
  unary-send   = rcvr:r ws unary-ident:msg ~":" -> `(send ',msg ,r)
  binary-send  = rcvr:r ws binop-ident:msg ws expr:arg -> `(send ',msg ,r ,arg)
  nary-send    = rcvr:r (ws nary-part:m ws subexpr:a -> (cons m a))+:msg -> (compose-msg-send r msg)
  rcvr         = atom | group
  local        = unary-ident:x -> `(load ,x)
  lisp         = "`" lisp.expr:x -> `(lisp ,x)
  atom         = keyword | lisp | local | lisp.integer | lisp.float | string | block
  group        = "(" expr:x ")" -> x
  arglist      = (ws ":" unary-ident)*:vs ws "|" -> vs
  block        = "[" arglist?:args body:b "]" -> `(block :args ,args ,@b)
  subexpr      = unary-send | binary-send | atom | group 
  expr         = ws (nary-send | subexpr):x ws -> x
  assign       = unary-ident:var ws ":=" expr:val -> `(set! ,var ,val)
  return       = "^" ws expr:x -> `(return ,x)
  stmt         = return | assign | expr
  stmts        = ws stmt:s (ws "." ws stmt)*:ss -> (cons s ss)
  vars         = "|" (ws ":" unary-ident)*:vars ws "|" -> vars
  body         = ws vars?:vars stmts:stmts -> `(:vars ,vars :body ,stmts) 

  nary-hdr     = (nary-part:m ws unary-ident:a ws -> (cons m a))+:hdr -> (compose-hdr hdr)
  unary-hdr    = unary-ident:m -> (list :name m :args '())
  binary-hdr   = binop-ident:m ws unary-ident:arg -> (list :name m :args (list arg))
  method-hdr   = nary-hdr | unary-hdr  | binary-hdr
  method-defn  = unary-ident:cls ">>" method-hdr:h ws "[" body:b "]" -> `(method :class ,cls ,@h ,@b)
  file-in      = (ws method-defn)+:ms -> `(chitchat-methods ,ms)
}

(print 'done-meta)

(define (dbg rule str)
  (stream-write-string *standard-output* str)
  (newline)
  (binding ((*meta-context* '(chitchat))
            ;(*meta-memo* #t)
            ;(*meta-trace* #t)
            )
    (match-map print rule str))
  (stream-write-string *standard-output* "================================\n"))

(define (dbgx rule str)
  (stream-write-string *standard-output* str)
  (newline)
  (binding ((*meta-context* '(chitchat))
            ;(*meta-memo* #t)
            ;(*meta-trace* #t)
            )
    (match-map (lambda (e) (print (compiler e))) rule str))
  (stream-write-string *standard-output* "================================\n"))

(define (dbge rule str)
  (stream-write-string *standard-output* str)
  (newline)
  (binding ((*meta-context* '(chitchat))
            ;(*meta-memo* #t)
            ;(*meta-trace* #t)
            )
    (match-map eval rule str))
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
(dbg 'body " |:x :y|
     x := AThing becomeRelevant.
     y := self fooWith: x.
     self frobnicate: [ :arg | self fooWith:arg. ^ y ]
")
(dbg 'body "
     | :y |
     y := 5.
     ^ [ :x | x + y ]
")

(%load "./scratch/chit-chat.compiler.lisp")

(dbgx 'file-in "
Array>>first [ ^ self at: 0 ]
String>>first [ ^ self at: 0 ]
LazyTable>>at:x put:y [
  storage ifNil: [ storage := HashTable new ].
  ^ storage at: x put:y
]
Cons>>collect: block [ ^ `(mapcar (lambda (it) (@send 'value: block it)) self) ]
Fixnum>>pi [ ^ `*pi* ]
")

(dbge 'file-in "
Fixnum>>+ other [ ^ `(+ self other) ]
Closure>>value [ ^ `(self) ]
Boolean>>ifTrue: then ifFalse: else [
 ^ `(if self (@send 'value then) (@send 'value else))
]
Fixnum>>isEven [ ^ `(= 0 (% self 2)) ]
Fixnum>>aCheck [
  (self isEven) ifTrue: [ ^ 'yes, even' ] ifFalse: [ ^ 'no, not even' ]
]
")

(print (@send '+ 2 3))
(print (@send 'value (lambda () 10)))
(print (@send 'isEven 4))
(print 'ifTrue:ifFalse:)
(let ((even? (@send 'isEven 4)))
  (print `(even? ,even?))
  (print (@send 'ifTrue:ifFalse: even? (lambda () "ok") (lambda () "not ok"))))
(print (@send 'ifTrue:ifFalse: (@send 'isEven 4) (lambda () "ok") (lambda () "not ok")))
(print (@send 'aCheck 4))

(print 'done)
