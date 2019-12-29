meta meta {
  mod = [*?+]:x -> (implode (list x))
  sym = ~"->" (alpha | "-")+:xs -> (implode xs)

  string-lit = lisp.string:x -> (cons 'seq (string-to-charlist x))

  br-body-char  = ~[\\\]] any
  br-esc-char   = "\\" any:x -> (bracket-escaped-char-character x)
  br-char       = br-body-char | br-esc-char
  br-char-range = br-char:a "-" br-char:b -> (make-bracket-char-range a b)
  bracket-lit   = "[" (br-char-range | br-char)+:chs "]" -> (cons 'or chs)

  extern = sym:ns "." sym:rule -> `(extern ,ns ,rule)

  grouped = "(" rule-body:b ")" -> b

  lit     = extern | sym | string-lit | bracket-lit | grouped
  lit-mod = lit:lit mod:mod -> (list mod lit)

  atom = "~"?:negated (lit-mod | lit):item -> (if-nil? negated item `(not ,item))

  pred   = "?" ws lisp.expr:it -> `(where ,it)
  bind   = atom:rule ws ":" ident:as -> `(set! ,as ,rule)
  app    = bind | atom | pred
  result = "->" ws lisp.expr:it -> `(return ,it)

  rule-app       = " "* app
  rule-branch    = rule-app+:as " "* result?:r -> (cons 'seq (append as (if-nil? r r (list r))))
  rule-body-list = rule-branch:fst ws ("|" rule-body-list)?:rst -> (cons fst rst)
  rule-body      = rule-body-list:rs -> (cons 'or rs)

  rule = ws sym:name ws "=" ws rule-body:b -> `(define-rule ,name ,b)

  par   = "<" ws sym:par -> par
  block = ws "meta" ws sym:n ws par?:mp ws "{" ws rule+:rs ws "}" -> (make-meta-definition n mp rs)

  main = lisp.ws (block | lisp.expr):x lisp.ws -> x
}
