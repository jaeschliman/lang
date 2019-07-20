meta lisp {
  eol          = [\r\n] | nothing
  non-eol      = ~eol any
  comment      = ";" non-eol* eol
  ignorable    = space | comment
  ws           = ignorable*
  constr       = constituent+:chs -> (charlist-to-string chs)
  character    = "#\\" constr+:name ?(character-name? name) -> (char-by-name name)
  non-quote    = ~["\\] any
  escaped-char = "\\" any:x -> (escaped-char-character x)
  string-char  = non-quote | escaped-char
  string       = "\"" string-char*:chs "\"" -> (charlist-to-string chs)
  symbol-char  = any:x ?(symbol-char x) -> x
  symbol       = ~[0-9] symbol-char+:xs -> (implode xs)
  integer      = "-"?:sign digit+:xs -> (* (digits-to-integer xs) (if (nil? sign) 1 -1))
  float        = "-"?:sign digit+:xs "." digit+:ys -> (* (digits-to-float xs ys) (if (nil? sign) 1 -1))
  point        = integer:x "@" integer:y -> (make-point x y)
  hex-char     = [0-9a-f]
  hex-integer  = "0x" hex-char+:chs -> (hex-chars-to-integer chs)
  true         = "#t" -> #t
  false        = "#f" -> #f
  boolean      = true | false
  atom        = boolean | character | hex-integer | float | point | integer | symbol | string
  quoted       = "'" ws expr:x -> (list 'quote x)
  quasiquoted  = "`" ws expr:x -> (list 'quasiquote x)
  unq-splicing = ",@" ws expr:x -> (list 'unquote-splicing x)
  unquoted     = "," ws expr:x -> (list 'unquoted x)
  quotation    = quoted | quasiquoted | unq-splicing | unquoted
  expr         = ws "(" expr*:x")" ws -> x | quotation | ws atom:x ws -> x
}

