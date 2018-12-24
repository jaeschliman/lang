#!/bin/bash
set -e
set -x

INCLUDE=/Users/jsn/Library/Frameworks

mkdir -p build
rm -rf build
mkdir -p build
g++ main.cpp -Werror -O0 -std=c++14 -I$INCLUDE -F$INCLUDE -framework Cocoa -framework SDL2 -o build/run-file -v
cd build
ln run-file soak
ln run-file repl
ln run-file events

# alternatively:
# https://stackoverflow.com/questions/20277477/how-do-you-include-files-in-c-from-the-library-framework-folder-in-mac
