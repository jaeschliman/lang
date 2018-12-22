#!/bin/bash
set -e
set -x

mkdir -p build
rm -rf build
mkdir -p build
g++ main.cpp -Werror -O0 -std=c++14 -o build/run-file
cd build
ln run-file soak
