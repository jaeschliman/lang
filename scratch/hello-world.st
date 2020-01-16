(import-file! "./scratch/my-pair.st")
(lang #/lang/chitchat-script)

| stdout a p |

stdout := FileOutputStream stdout.

stdout printObject: 'hello, world!'.
stdout newline.

a := Array withCapacity: 3.

a at: 0 put: 'hello'.
a at: 1 put: 'again'.
a at: 2 put: 'world'.

stdout printObject: a.
stdout newline.

p := MyPair withFirst: 'hello' andRest: 'world'.

stdout printObject: p first.
stdout printObject: p rest.
stdout newline.