#!/bin/bash

set -e

dune build @fmt --auto-promote
find c \
    \( -path c/deps \) -prune -false -o \
    \( -iname "*.h" \) -or \( -iname "*.c" \) | xargs clang-format -i
