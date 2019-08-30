## contents
### amb.lisp
an implementation of McCarthy's `amb` operator using the languages primitive delimited continuation functions
### bitmap-font.lisp
testing out functionality with simple bitmap font
### blitq-2.lisp
testing functionality blitting from arbitrary quad to arbitrary quad
(`blitq`). defines two regions which may be altered interactively and
blits between them
### blitq.lisp
basically fuzz testing for `blitq`
### blitwork.lisp
run a magnifying glass over an image of a cow
### bouncers-2.lisp
interactively spawn thousands of green threads which move points
around the screen, and draw colored trails for them
### bouncers.lisp
interactively spawn thousands of coroutines which spawn other
coroutines etc (and eventually die), and draw them
### brush.lisp
beginning of a motion paint program
### chit-chat.compiler.lisp
compiler for a small message-passing class-based language
### chit-chat.lisp
reader and some tests for a small message-passing class-based language
### classes.lisp
small tests for the class system
### cmdline.lisp
small test for command line args
### compiler.lisp
the self-hosted bytecode compiler. will be moved elsewhere in time.
### cons-alot.lisp
stress the GC
### coroutines.lisp
a messy first implementation of coroutines using delimited continuations
### cow-storm.lisp
one of the first graphic demos. translate and rotate many images of a cow
### crowd.lisp
stress test `blitq` a bit
### debug-thread.lisp
test for `thread-get-debug-info`
### jump-opt.lisp
some WIP tests for compiler optimizations (convert self tail-calls to
jumps, inline let-bound lambdas when possible)
### letrec-opt.lisp
some WIP tests for compiler optimizations (inline letrec-bound lambdas when possible, allowing for jump opts. this handles the common case of named let for looping)
### macroexpand.lisp
the "new" self-hosted macro expander that respects lexical scope. will be moved elsewhere in time. still to come are compiler macros.
### masking.lisp
a test of blit with mask. move an image around 'behind' a mask
### ms.lisp
a mini implementation of a minesweeper-like game
### paint.lisp
a mini paint program
### queue.lisp
library implementation of a queue. will be moved elsewhere in time
### send.lisp
test built-in message sending function
### shift-reset.lisp
an old implementation of shift/reset over the langauge's delimited
continuations. this is superceded by the versions (which allow for
tags) included in boot
### snapshot-test.lisp
basic test of the suspend/resume functionality. suspend/resume is now a part of the build and test process.
### sws.lisp
start of a Simple Widget System to build UIs
### text.lisp
start of a test drawing facility
### threads.lisp
simple test of the green threads
### ufos.lisp
watch UFOs play tag over the city. tests various blitting facilities

