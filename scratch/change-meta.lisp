(set '*package* (symbol-package 'define))

meta mymeta < meta {
  main = mylisp.ws ( block | mylisp.expr ):x mylisp.ws -> x
}

(print '-------------------------------------------------------------------)
(define (banger l r)
  (if (pair? l) `(aset ,(second l) ,(third l) ,r)
      `(set! ,l ,r)))

meta mylisp < lisp {
  aref = symbol:s "[" expr:e "]" -> `(aget ,s ,e)
  lhs = aref | symbol
  assign = lhs:l ws ":=" ws expr:r -> (banger l r)
  my-expr = ws (assign | aref):x ws -> x
  expr-inner =  my-expr | quotation | atom | read-eval 
}

(print *meta-context*)
(print '-------------------------------------------------------------------)
(set '*meta-context* (list 'mymeta))
(print '-------------------------------------------------------------------)
(define *x (vector 1 2 3))
(print '-------------------------------------------------------------------)
*x[1]
(print *x)
(print *x[1])
*x[1] := 'hello
(print '-------------------------------------------------------------------)
(print "hello again")
(print *x)
