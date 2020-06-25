#!/usr/bin/env bash

set -e

ALL_NNS=("trained-nns/nn-10" "trained-nns/nn-20" "trained-nns/nn-30" "trained-nns/nn-10x10" "trained-nns/nn-15x15"
         "trained-nns/nn-20x10" "trained-nns/nn-20x20" "trained-nns/nn-15x10x5")
TEST_ARGS=() #("--ensemble" "--print-misses" "--detail-by-number")
echo
echo "Train:"
./out/recognition/launcher/dest/run test --features data/features/train/ ${TEST_ARGS[@]} --nn-paths ${ALL_NNS[@]}
echo
echo "Test:"
./out/recognition/launcher/dest/run test --features data/features/test/ ${TEST_ARGS[@]} --nn-paths ${ALL_NNS[@]}
echo
