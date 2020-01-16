(define MyPair (create-class 'MyPair '(a b)))
(lang #/lang/chitchat-methods)

MyPair class>>withFirst: a andRest: b [
   | result |
   result := `(instantiate-class MyPair).
   result first: a.
   result rest: b.
   ^ result
]

MyPair>>first: v [ a := v ]
MyPair>>rest: v [ b := v ]
MyPair>>first [ ^ a ]
MyPair>>rest [ ^ b ]