meta meta {
  mod = [*?+]:x -> (implode (list x))
  sym = ~"->" (alpha | "-")+:xs -> (implode xs)

  string-lit = lisp.string:x -> (cons 'seq (string-to-charlist x))

  br-body-char  = ~[\\\]] any
  br-esc-char   = "\\" x:any -> (bracket-escaped-char-character x)
  br-char       = br-body-char | br-esc-char
  br-char-range = br-char:a "-" br-char:b -> (make-bracket-char-range a b)
  bracket-lit   = "[" (br-char-range | br-char)+:chs "]" -> (cons 'or chs)

  extern = sym:ns "." sym:rule -> `(extern ,ns ,rule)

  grouped = "(" rule-body:b ")" -> b

  lit     = extern | sym | string-lit | bracket-lit | grouped
  lit-mod = meta-lit:lit mod:mod -> (list mod lit)

  atom = "~"?:negated (lit-mod | lit):item -> (if (nil? negated) item `(not ,item))

  pred   = "?" ws lisp.expr:it -> `(where ,it)
  app    = (atom:rule ws ":" ident:as -> `(set! ,as ,rule)) | atom | pred
  result = "->" ws lisp.expr:it -> `(return ,it)

  rule-branch         = app+:as " "* result?:r -> (cons 'seq (append as (if (nil? r) '() (list r))))
  rule-body-list      = rule-branch:fst ws more-rule-body-list?:rst -> (cons fst rst)
  more-rule-body-list = "|" rule-body-list:rs -> rs
  rule-body           = rule-body-list:rs -> (cons 'or rs)

  rule = ws sym:name ws "=" rule-body:b -> `(define-rule ,name ,b)

  block = ws "meta" ws sym:n ws "{" ws rule+:rs ws "}" -> (make-meta-definition n rs)
}
