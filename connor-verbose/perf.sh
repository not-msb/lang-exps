#!/bin/sh

mkdir -p build
perf record -g zig-out/bin/connor
perf script > build/out.perf

stackcollapse-perf.pl build/out.perf > build/out.folded
flamegraph.pl build/out.folded > build/kernel.svg
