(set 'print-2 (lambda (a b) (print (list a b))))
(set 'print-2-reversed (lambda (a b) (print (list b a))))
(set 'list-2 (lambda (a b) (list a b)))

(apply print (list 42))
(apply print-2 (list 'hello 'world))
(apply print-2-reversed (list 'world 'hello))
(print (apply list '(hello world)))
(print (apply list (list 'hello 'world)))
(print (apply list-2 (list 'hello 'world)))
(apply print (list (apply list-2 (list 'hello 'world))))
(apply print (list (apply list (list 'hello 'world))))
(apply print-2 (apply list-2 (list 'hello 'world)))
(apply print-2 (apply list (list 'hello 'world)))
