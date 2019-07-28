(set '*package* (symbol-package 'define))

(define (compose-msg-send rcvr parts)
  (let ((msg (implode (reduce-list (lambda (acc pair)
                                     (append acc (car pair)))
                                   '() parts)))
        (args (mapcar cdr parts)))
    `(@send ',msg ,@args)))

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
  unary-send   = rcvr:r ws unary-ident:msg ~":" -> `(@send ',msg ,r)
  binary-send  = rcvr:r ws binop-ident:msg ws expr:arg -> `(@send ',msg ,r ,arg)
  nary-send    = rcvr:r (ws nary-part:m ws subexpr:a -> (cons m a))+:msg -> (compose-msg-send r msg)
  rcvr         = atom | group
  local        = unary-ident:x -> `(load ,x)
  atom         = keyword | local | lisp.integer | lisp.float | string | block
  group        = "(" expr:x ")" -> x
  arglist      = (ws ":" unary-ident)*:vs ws "|" -> vs
  block        = "[" arglist?:args body:b "]" -> `(block (:args ,args) ,@b)
  subexpr      = unary-send | binary-send | atom | group 
  expr         = ws (nary-send | subexpr):x ws -> x
  assign       = unary-ident:var ws ":=" expr:val -> `(set! ,var ,val)
  return       = "^" ws expr:x -> `(return ,x)
  stmt         = return | assign | expr
  stmts        = ws stmt:s (ws "." ws stmt)*:ss -> (cons s ss)
  vars         = "|" (ws ":" unary-ident)*:vars ws "|" -> vars
  body         = ws vars?:vars stmts:stmts -> `((:vars ,vars) (:body ,stmts)) 
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

(print 'done)
