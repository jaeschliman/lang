(set '*package* (symbol-package 'define))

(define (compose-msg-send rcvr parts) (list '@send rcvr parts))

(print 'begin)

meta chitchat {
  escaped-char = "\\" any:x -> x
  comment      = "\"" (~[\"\\] any | escaped-char)* "\"" 
  string       = "'" (~['\\] any | escaped-char)*:chs "'" -> (charlist-to-string chs)
  ws           = (space | comment)*
  binop-ident  = "+" -> '+ | "-" -> '- | "/" -> '/ | "*" -> '*
  unary-ident  = [a-zA-Z]:fst [a-zA-Z0-9]*:rst -> (implode (cons fst rst))
  nary-part    = [a-zA-Z]:fst [a-zA-Z0-9]*:rst ":" -> (append (cons fst rst) '(#\:))
  keyword      = "self" -> 'self | "true" -> #t | "false" -> #f
  unary-send   = expr:rcvr ws unary-ident:msg -> `(@send ',msg ,rcvr)
  binary-send  = expr:rcvr ws binop-ident:msg ws expr:arg -> `(@send ',msg ,rcvr ,arg)
  nary-send    = expr:rcvr (ws nary-part:m ws expr:a -> (cons m a))+:msg -> (compose-msg-send rcvr msg)
  atom         = keyword | unary-ident | lisp.integer | lisp.float 
  group        = "(" expr:x ")" -> x
  inner-expr   = nary-send | binary-send | unary-send | atom | group 
  expr         = ws (nary-send | binary-send | unary-send | atom | group):x ws -> x
  stmt         = expr:x "." -> x
}

(print 'done-meta)

(define (dbg rule str)
  (stream-write-string *standard-output* str)
  (newline)
  (binding ((*meta-context* '(chitchat))
            (*meta-memo* #t)
            (*meta-trace* #t))
    (match-map print rule str))
    (stream-write-string *standard-output* "================================\n")
  (stream-write-string *standard-output* str)
  (newline)
  (stream-write-string *standard-output* "--------------------------------\n"))

;(dbg 'expr "1")
;(dbg 'expr "(1)")
;(dbg 'expr "1 + 2")
(dbg 'expr "(1 + 2)")
;(dbg 'expr "1 + 2 + 3")
;(dbg 'expr "1 + (2 + 3)")
;(dbg 'expr "(1 + 2) + 3")
;(dbg 'expr "foo print")
;(dbg 'expr "foo printWith: 10")

(print 'done)
