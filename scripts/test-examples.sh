#!/bin/bash

set -e

for example in examples/*/; do
    echo "Testing '$example'..."
    cd $example
    ../../scripts/mazeppa.sh run --target-dir test --inspect
    cd ../..
    diff --recursive "${example}target/" "${example}test/"
    rm -rf $example/test
done
