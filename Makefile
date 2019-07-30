.PHONY : release debug image debugvm vm

INCLUDE=/Users/jsn/Library/Frameworks
FRAMEWORKS= -framework Cocoa -framework SDL2 -framework SDL2_image

release :
	mkdir -p build
	rm -rf build
	mkdir -p build
	g++ main.cpp -Wall -Werror -Ofast -std=c++14 -mllvm -unroll-count=4  -F$(INCLUDE) $(FRAMEWORKS) -o build/boot
	cd build && ln boot repl && ln boot img
	./build/boot ./boot/_cmdline-loader.lisp ./build/latest.image

vm :
	rm ./build/boot
	rm ./build/repl
	rm ./build/img
	g++ main.cpp -Wall -Werror -Ofast -std=c++14 -mllvm -unroll-count=4  -F$(INCLUDE) $(FRAMEWORKS) -o build/boot
	cd build && ln boot repl && ln boot img

debug :
	mkdir -p build
	rm -rf build
	mkdir -p build
	clang++ main.cpp -Werror -O0 -g -std=c++14 -F$(INCLUDE) $(FRAMEWORKS) -o build/boot
	cd build && ln boot repl && ln boot img
	./build/boot ./boot/_cmdline-loader.lisp ./build/latest.image

debugvm :
	rm ./build/boot
	rm ./build/repl
	rm ./build/img
	clang++ main.cpp -Werror -O0 -g -std=c++14 -F$(INCLUDE) $(FRAMEWORKS) -o build/boot
	cd build && ln boot repl && ln boot img

image :
	./build/boot ./boot/_cmdline-loader.lisp ./build/latest.image
