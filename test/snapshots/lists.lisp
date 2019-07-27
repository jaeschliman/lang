
(dolist (x '(1 2 3 4 5))
  (print x))

(print (list->array '(hello how are you?)))

(print (array->list (list->array '(a b c d e f g))))

(print (plist-get :hello   '(:hello 10 :goodbye 20)))
(print (plist-get :goodbye '(:hello 10 :goodbye 20)))
(print (plist-get :missing '(:hello 10 :goodbye 20)))
