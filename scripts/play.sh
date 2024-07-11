#!/bin/bash

set -e

ulimit -s unlimited
cd playground && ../scripts/mazeppa.sh run --inspect
dot -Tsvg target/graph.dot > target/graph.svg
