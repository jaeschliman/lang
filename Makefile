.PHONY : release debug image debugvm vm

INCLUDE=/Users/jsn/Library/Frameworks
FRAMEWORKS= -framework Cocoa -framework SDL2 -framework SDL2_image
WARN_FLAGS= -Wall -Wpedantic -Wno-c99-extensions -Wno-vla-extension -Werror
OPTIMIZE= -Ofast -mllvm -unroll-count=4
COVERAGE=-g -fprofile-instr-generate -fcoverage-mapping

release :
	mkdir -p build
	rm -rf build
	mkdir -p build
	clang++ main.cpp $(WARN_FLAGS) -std=c++17 $(OPTIMIZE) -F$(INCLUDE) $(FRAMEWORKS) -o build/boot
	cd build && ln boot repl && ln boot img
	./build/boot ./boot/_cmdline-loader.lisp ./build/latest.image

vm :
	- rm ./build/boot
	- rm ./build/repl
	- rm ./build/img
	clang++ main.cpp $(WARN_FLAGS) -std=c++17 $(OPTIMIZE) -F$(INCLUDE) $(FRAMEWORKS) -o build/boot
	cd build && ln boot repl && ln boot img

debug :
	mkdir -p build
	rm -rf build
	mkdir -p build
	clang++ main.cpp $(WARN_FLAGS) -std=c++17 -O0 -g -F$(INCLUDE) $(FRAMEWORKS) -o build/boot
	cd build && ln boot repl && ln boot img
	./build/boot ./boot/_cmdline-loader.lisp ./build/latest.image

debugvm :
	- rm ./build/boot
	- rm ./build/repl
	- rm ./build/img
	clang++ main.cpp $(WARN_FLAGS) -std=c++17 -O0 -g -F$(INCLUDE) $(FRAMEWORKS) -o build/boot
	cd build && ln boot repl && ln boot img

instrumented :
	- rm ./build/boot
	- rm ./build/repl
	- rm ./build/img
	clang++ main.cpp $(WARN_FLAGS) $(COVERAGE) -std=c++17 $(OPTIMIZE) -F$(INCLUDE) $(FRAMEWORKS) -o build/boot
	cd build && ln boot repl && ln boot img

image :
	./build/boot ./boot/_cmdline-loader.lisp ./build/latest.image

profimage :
	LLVM_PROFILE_FILE=./llvm.prof ./build/boot ./boot/_cmdline-loader.lisp ./build/latest.image

profdata :
	llvm-profdata merge -output=llvm.merge -instr=./llvm.prof
