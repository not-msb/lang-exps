#!/bin/sh

set -xe

zig build run | qbe > temp.S
#zig build-exe temp.S lib.S -femit-bin=main
zig build-exe -lc temp.S lib.zig -femit-bin=main
rm main.o temp.S
