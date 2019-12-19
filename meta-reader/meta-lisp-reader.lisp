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

  pkg-name     = [a-zA-Z\-]+
  pkg-path     = "#/"?:gl (pkg-name:n "/" -> n)*:parts  -> (if-nil? gl parts (cons 'root parts))
  pkg-prefix   = ":" -> '(root "keyword") | pkg-path

  simbol-char = any:x ?(symbol-char-no-slash x) -> x
  simbol-lead = (~[\-0-9#:] simbol-char) | ("-" ~[0-9]) | "/"
  simbol-name = simbol-lead:x simbol-char*:xs ~"/" -> (charlist-to-string (cons x xs))
  simbol      = simbol-name:n -> (intern-with-charlist-prefix '() n)

  psymbol = pkg-prefix:pfx simbol-name:n -> (intern-with-charlist-prefix pfx n)
  symbol  = simbol | psymbol

  integer      = "-"?:s digit+:xs -> (* (digits-to-integer xs) (if-nil? s 1 -1))
  float        = "-"?:s digit+:xs "." digit+:ys -> (*f (digits-to-float xs ys) (if-nil? s 1.0 -1.0))
  point        = integer:x "@" integer:y -> (make-point x y)
  hex-integer  = "0x" [0-9a-f]+:chs -> (hex-chars-to-integer chs)
  true         = "#t" -> #t
  false        = "#f" -> #f
  boolean      = true | false
  atom         = symbol | hex-integer | float | point | integer | string | boolean | character 
  quoted       = "'"  ws expr:x -> (list 'quote x)
  quasiquoted  = "`"  ws expr:x -> (list 'quasiquote x)
  unq-splicing = ",@" ws expr:x -> (list 'unquote-splicing x)
  unquoted     = ","  ws expr:x -> (list 'unquote x)
  quotation    = quoted | quasiquoted | unq-splicing | unquoted
  expr         = ws ( "(" expr*:x ")" -> x | quotation | atom ):x ws -> x
}

