#!/bin/bash
file_name="$1"
snap_name="$1.snapshot"
./amber $file_name > $snap_name 2>/dev/null
