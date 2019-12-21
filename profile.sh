
# http://cmdlinelinux.blogspot.com/2018/04/profiling-c-code-with-clang-using.html
make instrumented
make profimage
/usr/local/opt/llvm/bin/llvm-profdata merge --output=llvm.merge --instr ./llvm.prof
/usr/local/opt/llvm/bin/llvm-cov show ./build/boot -instr-profile=llvm.merge | less
