.PHONY : release debug

INCLUDE=/Users/jsn/Library/Frameworks
FRAMEWORKS= -framework Cocoa -framework SDL2 -framework SDL2_image

release :
	mkdir -p build
	rm -rf build
	mkdir -p build
	g++ main.cpp -Wall -Werror -Ofast -std=c++14 -mllvm -unroll-count=4  -F$(INCLUDE) $(FRAMEWORKS) -o build/amber
	cd build && ln amber repl && ln amber img

debug :
	mkdir -p build
	rm -rf build
	mkdir -p build
	clang++ main.cpp -Werror -O0 -g -std=c++14 -F$(INCLUDE) $(FRAMEWORKS) -o build/amber
	cd build && ln amber repl && ln amber img
