#!/bin/bash

set -ex

cd test-c-codegen
CC=gcc ./test.sh
CC=clang ./test.sh
