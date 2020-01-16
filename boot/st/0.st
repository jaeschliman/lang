(lang #/lang/chitchat-methods)

FileOutputStream>>printObject: it [
  `(print-object it self)
]

FileOutputStream>>newline [
  `(stream-write-string self "\n")
]

FileOutputStream class>>stdout [
  ^ `*standard-output*
]

HashTable class>>new [
  ^ `(make-ht)
]

HashTable>>at:k [
  ^ `(ht-at self k)
]

HashTable>>at:k put:v [
  `(ht-at-put self k v)
]

Array class>>withCapacity: n [
  ^ `(make-array n)
]

Array>>at: n [ ^ `(aget self n) ]

Array>>at: n put: v [ `(aset self n v) ]