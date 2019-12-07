#!/bin/sh

#name=$1
name=a
tcc src/compiler.c -DNDEBUG -run > output/$name.ll
llvm-as output/$name.ll -o output/$name.bc
llc output/$name.bc -o output/$name.s
gcc output/$name.s -o output/$name
./output/$name
echo Result: $?
