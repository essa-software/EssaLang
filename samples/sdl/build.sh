#!/bin/bash

set -e

SELF=$(dirname "$(basename "$0")")
ELC=$(realpath $SELF/../../rust/target/debug/elc)
echo ELC=$ELC

# build elc
$ELC src/main.esl \
    --c-include 'external/sdl.h' \
    --c-include-dir $SELF \
    --c-source 'external/sdl.cpp' \
    --cc-arg -lSDL3 \

build/out
