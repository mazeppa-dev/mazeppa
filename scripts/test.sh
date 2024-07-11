#!/bin/bash

set -e

find . -name '*.coverage' | xargs rm -f
dune test --instrument-with bisect_ppx --force
bisect-ppx-report summary
bisect-ppx-report html
