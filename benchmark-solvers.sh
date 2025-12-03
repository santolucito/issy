#!/bin/bash
# benchmark-solvers.sh - Compare Z3 vs CVC5 performance

set -e

ISSY="stack exec issy --"
SPECS=("docs/sample.llissy" "docs/sample-moddified-product.llissy")
ITERATIONS=3

echo "Z3 vs CVC5 Performance Comparison"
echo "=================================="
echo ""

for spec in "${SPECS[@]}"; do
    echo "Spec: $spec"
    echo "---"

    echo -n "Z3:   "
    for i in $(seq 1 $ITERATIONS); do
        t=$( { time $ISSY --solver z3 --quiet "$spec" > /dev/null; } 2>&1 | grep real | awk '{print $2}')
        echo -n "$t "
    done
    echo ""

    echo -n "CVC5: "
    for i in $(seq 1 $ITERATIONS); do
        t=$( { time $ISSY --solver cvc5 --quiet "$spec" > /dev/null; } 2>&1 | grep real | awk '{print $2}')
        echo -n "$t "
    done
    echo ""
    echo ""
done
