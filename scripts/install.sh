#!/bin/bash

set -e

opam install . --deps-only -y
dune build --release
dune install --release mazeppa
