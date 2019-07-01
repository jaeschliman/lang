(print `(hello from testfile))

meta testfile {
  space   = any:ch ?(whitespace-char? ch) -> ch
  ws      = space*:chs                    -> chs
  digit   = any:ch ?(digit-char? ch)      -> (char-to-digit ch)
  int     = ws digit+:ds ws               -> (make-integer ds)
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

(print `(got some floats 0.1 1.2345 0.5 876.1234 3.14159))
(print `(got some points 0@0 234@456))

(print `(so long from ; internal test comment
            testfile))
