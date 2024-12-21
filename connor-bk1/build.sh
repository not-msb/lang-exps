#!/bin/sh

set -xe

if [ "$1" == "debug" ]; then
    zig build-exe main.zig
fi

if [ "$1" == "releaseFast" ]; then
    zig build-exe main.zig -OReleaseFast
fi

if [ "$1" == "releaseSmall" ]; then
    zig build-exe main.zig -OReleaseSmall
fi

if [ "$2" == "run" ]; then
    ./main
fi
