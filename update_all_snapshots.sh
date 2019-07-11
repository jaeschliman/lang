#!/bin/bash

find ./test/snapshots -iname '*.lisp'  | while read line; do
   ./update_snapshot.sh $line
done
