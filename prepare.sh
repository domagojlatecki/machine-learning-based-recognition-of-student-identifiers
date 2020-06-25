#!/usr/bin/env bash

set -e

for folder in data/images/*; do
    cd ${folder}

    for subset in *; do
        path_to_get="$folder/$subset"
        cd ../../..
        ./out/training/launcher/dest/run prepare \
            --images ${path_to_get} \
            --n-per-image 10 \
            --output "data/features/$subset/${folder/*\/}-${subset}.hs" \
            --debug data/debug/
        cd - >/dev/null
    done

    cd ../../..
done
