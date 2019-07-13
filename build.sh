#!/bin/bash
set -e
set -x

# requires SDL2 and SDL2_image frameworks
INCLUDE=/Users/jsn/Library/Frameworks

mkdir -p build
rm -rf build
mkdir -p build
g++ main.cpp -Wall -Werror -Ofast -std=c++14 -mllvm -unroll-count=4 -I$INCLUDE -F$INCLUDE -framework Cocoa -framework SDL2 -framework SDL2_image -o build/amber -v
#clang++ main.cpp -Werror -O0 -g -std=c++14 -I$INCLUDE -F$INCLUDE -framework Cocoa -framework SDL2 -framework SDL2_image -o build/amber -v
cd build
ln amber repl

# alternatively:
# https://stackoverflow.com/questions/20277477/how-do-you-include-files-in-c-from-the-library-framework-folder-in-mac
