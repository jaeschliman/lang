
(dolist (x '(1 2 3 4 5))
  (print x))

(print (list->array '(hello how are you?)))

(print (array->list (list->array '(a b c d e f g))))
