#!/bin/bash

set -e

echo "Installing the dependencies..."
opam install . --deps-only -y

echo "Building Mazeppa..."
dune build --release

echo "Installing Mazeppa..."
dune install --release mazeppa
