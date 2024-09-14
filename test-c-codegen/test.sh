#!/bin/bash

set -e

if [ -z $CC ]; then
    CC=gcc
fi

common_options="-Wall -Werror -Wno-unused-variable -Wno-infinite-recursion"
common_options="$common_options -std=gnu11"

cp -r ../c/deps/sds* .

# All the examples must at least compile with no errors.
for example in ../examples/*/; do
    cat "${example}main.mz" \
        | ../scripts/mazeppa.sh translate --language C --entry run \
            --dump-header-to . \
        | $CC -c -o /dev/null $common_options -xc -
done

# Compile and execute the self-interpreter example (convenient for testing).
cp ../examples/self-interpreter/main.mz main.mz
cat main.mz \
    | ../scripts/mazeppa.sh translate --language C --entry run \
        --dump-header-to . \
    | $CC -c -o program.o $common_options -xc -
$CC main.c program.o sds.c -lgc -o main $common_options
./main

# Make sure that run-time panics work as expected.
$CC panic.c sds.c -lgc -o panic $common_options
if ! ./panic 2>&1 >/dev/null \
    | grep -q 'Execution panic: "out of range"'; then
    echo "Expected a panic."
fi
