#!/bin/bash

find ./test/snapshots -iname '*.lisp'  | while read line; do
    echo "updating $line..."
   ./update-snapshot.sh $line
done
