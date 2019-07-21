#!/bin/bash
set -e
set -x

# requires SDL2 and SDL2_image frameworks

INCLUDE=~/Library/Frameworks

mkdir -p build
rm -rf build
mkdir -p build

g++ main.cpp -Wall -Werror -Ofast -std=c++14 -mllvm -unroll-count=4  -F$INCLUDE -framework Cocoa -framework SDL2 -framework SDL2_image -o build/amber -v
# clang++ main.cpp -Werror -O0 -g -std=c++14 -F$INCLUDE -framework Cocoa -framework SDL2 -framework SDL2_image -o build/amber -v

cd build
ln amber repl
ln amber img
