;; -*- mode: fundamental -*-

meta chitchat {
  escaped-char = "\\" any:x -> x
  comment      = "\"" (~[\"\\] any | escaped-char)* "\"" 
  string       = "'" (~['\\] any | escaped-char)*:chs "'" -> (charlist-to-string chs)
  ws           = (space | comment)*
  capture      = [a-zA-Z]:fst [a-zA-Z0-9]*:rst -> (implode (cons fst rst))
  binop-ident  = ("+"|"-"|"*"|"/"):x -> (as-message-send (list x))
  unary-ident  = [a-zA-Z]:fst [a-zA-Z0-9]*:rst -> (as-message-send (cons fst rst))
  nary-part    = [a-zA-Z]:fst [a-zA-Z0-9]*:rst ":" -> (append (cons fst rst) '(#\:))
  keyword      = "self" -> 'self | "true" -> #t | "false" -> #f
  unary-send   = rcvr:r ws unary-ident:msg ~":" -> `(send ',msg ,r)
  binary-send  = rcvr:r ws binop-ident:msg ws expr:arg -> `(send ',msg ,r ,arg)
  nary-send    = rcvr:r (ws nary-part:m ws subexpr:a -> (cons m a))+:msg -> (compose-msg-send r msg)
  rcvr         = atom | group
  local        = capture:x -> `(load ,x)
  lisp         = "`" lisp.expr:x -> `(lisp ,x)
  atom         = keyword | lisp | local | lisp.integer | lisp.float | string | block
  group        = "(" expr:x ")" -> x
  arglist      = (ws ":" capture)*:vs ws "|" -> vs
  block        = "[" arglist?:args body:b "]" -> `(block :args ,args ,@b)
  subexpr      = unary-send | binary-send | atom | group 
  expr         = ws (nary-send | subexpr):x ws -> x
  assign       = capture:var ws ":=" expr:val -> `(set! ,var ,val)
  return       = "^" ws expr:x -> `(return ,x)
  stmt         = return | assign | expr
  stmts        = ws stmt:s (ws "." ws stmt)*:ss -> (cons s ss)
  vars         = "|" (ws capture)*:vars ws "|" -> vars
  body         = ws vars?:vars stmts:stmts ws "."? ws -> `(:vars ,vars :body ,stmts) 
}

meta chitchat-methods < chitchat {
  nary-hdr     = (nary-part:m ws capture:a ws -> (cons m a))+:hdr -> (compose-hdr hdr)
  unary-hdr    = unary-ident:m -> (list :name m :args '())
  binary-hdr   = binop-ident:m ws capture:arg -> (list :name m :args (list arg))
  method-hdr   = nary-hdr | unary-hdr  | binary-hdr
  method-defn  = method-hdr:h ws "[" body:b "]" -> `(,@h ,@b)
  inst-method  = capture:cls ">>" method-defn:d -> `(instance :class ,cls ,@d)
  class-method = capture:cls " class>>" method-defn:d -> `(class :class ,cls ,@d)
  file-method  = inst-method | class-method
  file-in      = (ws file-method)+:ms ws -> `(chitchat-methods ,ms)

  main = file-in 
}

meta chitchat-script {
  main = chitchat.body:b -> `(,(cc-compile-script b) '())
}