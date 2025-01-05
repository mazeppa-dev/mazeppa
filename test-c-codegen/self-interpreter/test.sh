#!/bin/bash

# The fact that the self-interpreter uses almost all the language features makes
# it a good example for testing.

set -e

cat program.mz \
    | ../../scripts/mazeppa.sh translate --language C --entry run \
    | $CC -c -o program.o $COMMON_OPTIONS -xc -I.. -
$CC main.c program.o ../sds.c -lgc -o main $COMMON_OPTIONS
./main
