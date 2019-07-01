(print `(hello from testfile))

meta testfile {
  space   = any:ch ?(whitespace-char? ch) -> ch
  ws      = space*:chs                    -> chs
  digit   = any:ch ?(digit-char? ch)      -> (char-to-digit ch)
  int     = ws digit+:ds ws               -> (make-integer ds)
}

(print `(we have characters #\a #\b #c #\; #\*))

(print `(so long from testfile))
