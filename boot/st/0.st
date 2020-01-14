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