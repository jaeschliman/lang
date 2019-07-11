#!/bin/bash

function run_snapshot() {
    local file_name="$1"
    local snap_name="$1.snapshot"
    diff $snap_name <(./build/amber $file_name 2>/dev/null) >/dev/null \
        && echo " OK $file_name" || echo " FAIL $file_name"
}

find ./test/snapshots -iname '*.lisp'  | while read line; do
   run_snapshot $line
done
