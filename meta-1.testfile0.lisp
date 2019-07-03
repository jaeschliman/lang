(print `(hello from testfile))

meta testfile {
  space   = any:ch ?(whitespace-char? ch) -> ch
  ws      = space*
  digit   = any:ch ?(digit-char? ch)      -> (char-to-digit ch)
  int     = ws digit+:ds ws               -> (make-integer ds)
  alpha   = any:ch ?(alpha-char? ch)      -> ch
  ident   = ws alpha+:chs ws              -> (implode chs)
  main    = int | ident
}

;; test comment
(print `(we have characters #\a #\b #c #\; #\*)) ; test comment 2

(print "single-line a string!") 

(print "multi
line
string
!")

(print "string with \"quotation\"!")
(print "string with \n some escape \t chars like \\n and \\t")

(print `(got nil ,(nil? '())))
(print `(got some booleans #t #f #f #t))
(print `(got some ints 1234 -1 -30 3))
(print `(got some floats 0.1 1.2345 -0.5 876.1234 3.14159))
(print `(got some points 0@0 234@-456))
(print `(read some hex 0x0 0xf 0x10 0xff 0x100))

(print `(so long from ; internal test comment
            testfile))
