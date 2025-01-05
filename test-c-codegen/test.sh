#!/bin/bash

set -e

if [ -z $CC ]; then
    CC=gcc
fi

common_options="-Wall -Werror -Wno-unused-variable -Wno-infinite-recursion"
common_options="$common_options -std=gnu11"

# All the examples must at least compile with no errors.
for example in ../examples/*/; do
    cat "${example}main.mz" \
        | ../scripts/mazeppa.sh translate --language C --entry run \
        | $CC -c -o /dev/null $common_options -xc -
done

for test_dir in */; do
    echo "Testing '$test_dir'..."
    cd $test_dir
    COMMON_OPTIONS=$common_options ./test.sh
    cd ..
done
