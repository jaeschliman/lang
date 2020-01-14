(lang #/lang/chitchat-script)

| stdout |

stdout := FileOutputStream stdout.

stdout printObject: 'hello, world!'.
stdout newline;