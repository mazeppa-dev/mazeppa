#!/bin/bash

set -e

$CC main.c ../sds.c -lgc -o main $COMMON_OPTIONS
if ! ./main 2>&1 >/dev/null \
    | grep -q 'Execution panic: "out of range"'; then
    echo "Expected a panic."
fi
