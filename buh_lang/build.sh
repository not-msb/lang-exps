#!/bin/sh

set -xe

#zig build run | cat cbind.asm - > tmp.asm
zig build run | cat basic.asm - > tmp.asm
nasm tmp.asm -felf64
#zig cc tmp.o
ld tmp.o
rm tmp.*
