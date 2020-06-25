#!/usr/bin/env bash

set -e

for layout in "10" "20" "30" "10x10" "15x15" "20x10" "20x20" "15x10x5"; do
    ./out/recognition/launcher/dest/run train --step 2.5 --batch-size 100 --max-iters 500 --inertia 0.5 \
                    --features data/features/train/ \
                    --layout $layout \
                    --output "data/nn-$layout"
done

for i in {1..10}; do
    for net in "nn-10" "nn-20" "nn-30" "nn-10x10" "nn-15x15" "nn-20x10" "nn-20x20" "nn-15x10x5"; do
        echo $i $net
        ./out/recognition/launcher/dest/run train --step 4.0 --max-iters 5000 --inertia 0.5 \
            --features data/features/train/ \
            --test-features data/features/test/ \
            --test-moving-average 1000 \
            --load "data/${net}" \
            --output "data/${net}"
    done
done
