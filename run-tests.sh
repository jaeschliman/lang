#!/bin/bash

return_value=0

function update_return_value() {
    if [ $1 -ne 0 ]; then
        return $1
    fi
    return $return_value
}

function run_snapshot() {
    local file_name="$1"
    local snap_name="$1.snapshot"
    local out_file=`mktemp`

    ./build/amber $file_name 2>/dev/null > $out_file
    local status=$?

    if [ $status -eq 0 ]; then
        diff $snap_name $out_file >/dev/null
        status=$? 
    fi

    if [ $status -eq 0 ];
    then
        echo " OK $file_name"
    else
        echo " FAIL $file_name"
    fi

    rm $out_file
    update_return_value $status
    return_value=$?
    return $return_value
}

find ./test/snapshots -iname '*.lisp'  | while read line; do
   run_snapshot $line
done

exit $?
