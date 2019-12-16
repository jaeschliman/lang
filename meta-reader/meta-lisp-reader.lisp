meta lisp {
  eol          = [\r\n] | nothing
  comment      = ";" (~eol any)* eol
  ws           = (space | comment)*
  nonterm-char = ~(space | [(){}\[\]]) any
  charname     = any:ch nonterm-char*:chs -> (charlist-to-string (cons ch chs))
  character    = "#\\" charname:name ?(character-name? name) -> (char-by-name name)
  non-quote    = ~["\\] any
  escaped-char = "\\" any:x -> (escaped-char-character x)
  string       = "\"" (non-quote | escaped-char)*:chs "\"" -> (charlist-to-string chs)
  pkg-name     = [a-zA-Z\-]+:chs -> (charlist-to-string chs)
  pkg-path     = "#/"?:gl (pkg-name:n "/" -> n)+:parts  -> (if-nil? gl parts (cons 'root parts))
  pkg-prefix   = ":" -> '(root "keyword") | pkg-path
  symbol-char  = any:x ?(symbol-char x) -> x
  symbol-name  = ~[0-9] symbol-char+:xs -> (charlist-to-string xs)
  symbol       = pkg-prefix?:pfx symbol-name:name -> (intern-with-package-prefix pfx name)
  integer      = "-"?:s digit+:xs -> (* (digits-to-integer xs) (if-nil? s 1 -1))
  float        = "-"?:s digit+:xs "." digit+:ys -> (*f (digits-to-float xs ys) (if-nil? s 1.0 -1.0))
  point        = integer:x "@" integer:y -> (make-point x y)
  hex-integer  = "0x" [0-9a-f]+:chs -> (hex-chars-to-integer chs)
  true         = "#t" -> #t
  false        = "#f" -> #f
  boolean      = true | false
  atom         = boolean | character | hex-integer | float | point | integer | symbol | string
  quoted       = "'"  ws expr:x -> (list 'quote x)
  quasiquoted  = "`"  ws expr:x -> (list 'quasiquote x)
  unq-splicing = ",@" ws expr:x -> (list 'unquote-splicing x)
  unquoted     = ","  ws expr:x -> (list 'unquote x)
  quotation    = quoted | quasiquoted | unq-splicing | unquoted
  expr         = ws ( "(" expr*:x ")" -> x | quotation | atom ):x ws -> x
}

