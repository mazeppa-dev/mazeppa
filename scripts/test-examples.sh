#!/bin/bash

set -e

for example_dir in examples/*/; do
    echo "Testing '$example_dir'..."
    cd $example_dir
    ../../scripts/mazeppa.sh run --target-dir test --inspect
    cd ../..
    diff --recursive "${example_dir}target/" "${example_dir}test/"
    rm -rf $example_dir/test
done
